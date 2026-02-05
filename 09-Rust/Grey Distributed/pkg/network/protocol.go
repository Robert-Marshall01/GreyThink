// Package network implements Grey Protocol v2 for inter-node communication.
//
// # Design Goals
//
// 1. Deterministic: Message ordering guarantees for consensus
// 2. Efficient: Minimal overhead, zero-copy where possible
// 3. Resilient: Built-in backpressure, retry, and circuit breaking
// 4. Observable: Every message is traceable
//
// # Grey Protocol v2 Frame Format
//
// ┌────────────────────────────────────────────────────────────┐
// │  [4] Magic: "GREY"                                         │
// │  [2] Version: 0x0002                                       │
// │  [2] Flags: Compressed|Encrypted|Priority|Idempotent       │
// │  [8] Message ID: Unique, monotonic per connection          │
// │  [8] Correlation ID: Links requests to responses           │
// │  [8] Timestamp: Unix nanos                                 │
// │  [2] Message Type                                          │
// │  [4] Payload Length                                        │
// │  [N] Payload (MessagePack encoded)                         │
// │  [4] CRC32 Checksum                                        │
// └────────────────────────────────────────────────────────────┘
//
// # Flow Control
//
// We use credit-based flow control to prevent sender from overwhelming receiver:
// - Receiver advertises available buffer space (credits)
// - Sender tracks outstanding bytes, cannot exceed credits
// - Credits replenished as receiver processes messages
// - Prevents unbounded memory growth during slowdowns
//
// # Multiplexing
//
// Single TCP connection carries multiple logical streams:
// - Stream 0: Control (heartbeats, flow control)
// - Stream 1: Consensus (Raft messages)
// - Stream 2: Data (storage replication)
// - Stream 3: Tasks (scheduler coordination)
// - Custom streams for extensions
package network

import (
	"context"
	"encoding/binary"
	"errors"
	"hash/crc32"
	"io"
	"net"
	"sync"
	"sync/atomic"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// PROTOCOL CONSTANTS
// =============================================================================

// Protocol magic bytes
var protocolMagic = [4]byte{'G', 'R', 'E', 'Y'}

const (
	// Protocol version
	protocolVersion uint16 = 0x0002

	// Header size in bytes
	headerSize = 4 + 2 + 2 + 8 + 8 + 8 + 2 + 4 // 38 bytes

	// Maximum message size (16MB)
	maxMessageSize = 16 * 1024 * 1024

	// Default credits (1MB)
	defaultCredits = 1024 * 1024
)

// Flags for message processing
type Flags uint16

const (
	FlagCompressed  Flags = 1 << 0
	FlagEncrypted   Flags = 1 << 1
	FlagHighPriority Flags = 1 << 2
	FlagIdempotent  Flags = 1 << 3
	FlagOneWay      Flags = 1 << 4 // No response expected
)

// Stream IDs for multiplexing
const (
	StreamControl   uint16 = 0
	StreamConsensus uint16 = 1
	StreamData      uint16 = 2
	StreamTask      uint16 = 3
)

// =============================================================================
// CONFIGURATION
// =============================================================================

// Config contains network layer configuration.
type Config struct {
	// ListenAddress for incoming connections
	ListenAddress string

	// MaxConnections limits concurrent connections
	MaxConnections int

	// ReadTimeout for reading a complete message
	ReadTimeout time.Duration

	// WriteTimeout for writing a complete message
	WriteTimeout time.Duration

	// HeartbeatInterval for connection keepalive
	HeartbeatInterval time.Duration

	// InitialCredits for new connections
	InitialCredits int64

	// CircuitBreakerThreshold: failures before opening circuit
	CircuitBreakerThreshold int

	// CircuitBreakerTimeout: how long circuit stays open
	CircuitBreakerTimeout time.Duration

	// RetryConfig for automatic retries
	Retry RetryConfig

	// Compression settings
	CompressionEnabled     bool
	CompressionThreshold   int // Compress if payload > this
}

// RetryConfig defines retry behavior.
type RetryConfig struct {
	MaxAttempts     int
	InitialBackoff  time.Duration
	MaxBackoff      time.Duration
	BackoffMultiplier float64
	Jitter          float64
}

// DefaultConfig returns production defaults.
func DefaultConfig() Config {
	return Config{
		ListenAddress:           ":7654",
		MaxConnections:          1000,
		ReadTimeout:             30 * time.Second,
		WriteTimeout:            30 * time.Second,
		HeartbeatInterval:       10 * time.Second,
		InitialCredits:          defaultCredits,
		CircuitBreakerThreshold: 5,
		CircuitBreakerTimeout:   30 * time.Second,
		CompressionEnabled:      true,
		CompressionThreshold:    1024,
		Retry: RetryConfig{
			MaxAttempts:       3,
			InitialBackoff:    100 * time.Millisecond,
			MaxBackoff:        10 * time.Second,
			BackoffMultiplier: 2.0,
			Jitter:            0.1,
		},
	}
}

// =============================================================================
// MESSAGE
// =============================================================================

// Message represents a Grey Protocol message.
type Message struct {
	// Header fields
	Flags         Flags
	MessageID     uint64
	CorrelationID uint64
	Timestamp     int64
	Type          types.MessageType
	
	// Payload
	Payload []byte
	
	// Metadata (not serialized)
	SenderID  types.NodeID
	ReceivedAt time.Time
}

// Serialize writes the message to wire format.
func (m *Message) Serialize(w io.Writer) error {
	// Calculate total size
	payloadLen := len(m.Payload)
	totalLen := headerSize + payloadLen + 4 // +4 for CRC

	buf := make([]byte, totalLen)
	offset := 0

	// Magic
	copy(buf[offset:], protocolMagic[:])
	offset += 4

	// Version
	binary.BigEndian.PutUint16(buf[offset:], protocolVersion)
	offset += 2

	// Flags
	binary.BigEndian.PutUint16(buf[offset:], uint16(m.Flags))
	offset += 2

	// Message ID
	binary.BigEndian.PutUint64(buf[offset:], m.MessageID)
	offset += 8

	// Correlation ID
	binary.BigEndian.PutUint64(buf[offset:], m.CorrelationID)
	offset += 8

	// Timestamp
	binary.BigEndian.PutUint64(buf[offset:], uint64(m.Timestamp))
	offset += 8

	// Message Type
	binary.BigEndian.PutUint16(buf[offset:], uint16(m.Type))
	offset += 2

	// Payload Length
	binary.BigEndian.PutUint32(buf[offset:], uint32(payloadLen))
	offset += 4

	// Payload
	copy(buf[offset:], m.Payload)
	offset += payloadLen

	// CRC32 of everything before it
	checksum := crc32.ChecksumIEEE(buf[:offset])
	binary.BigEndian.PutUint32(buf[offset:], checksum)

	_, err := w.Write(buf)
	return err
}

// Deserialize reads a message from wire format.
func (m *Message) Deserialize(r io.Reader) error {
	// Read header
	header := make([]byte, headerSize)
	if _, err := io.ReadFull(r, header); err != nil {
		return err
	}

	offset := 0

	// Verify magic
	if header[0] != 'G' || header[1] != 'R' || header[2] != 'E' || header[3] != 'Y' {
		return ErrInvalidMagic
	}
	offset += 4

	// Verify version
	version := binary.BigEndian.Uint16(header[offset:])
	if version != protocolVersion {
		return ErrUnsupportedVersion
	}
	offset += 2

	// Parse flags
	m.Flags = Flags(binary.BigEndian.Uint16(header[offset:]))
	offset += 2

	// Parse message ID
	m.MessageID = binary.BigEndian.Uint64(header[offset:])
	offset += 8

	// Parse correlation ID
	m.CorrelationID = binary.BigEndian.Uint64(header[offset:])
	offset += 8

	// Parse timestamp
	m.Timestamp = int64(binary.BigEndian.Uint64(header[offset:]))
	offset += 8

	// Parse message type
	m.Type = types.MessageType(binary.BigEndian.Uint16(header[offset:]))
	offset += 2

	// Parse payload length
	payloadLen := binary.BigEndian.Uint32(header[offset:])
	if payloadLen > maxMessageSize {
		return ErrMessageTooLarge
	}

	// Read payload + CRC
	payloadAndCRC := make([]byte, payloadLen+4)
	if _, err := io.ReadFull(r, payloadAndCRC); err != nil {
		return err
	}

	m.Payload = payloadAndCRC[:payloadLen]

	// Verify CRC
	expectedCRC := binary.BigEndian.Uint32(payloadAndCRC[payloadLen:])
	actualCRC := crc32.ChecksumIEEE(append(header, m.Payload...))
	if expectedCRC != actualCRC {
		return ErrChecksumMismatch
	}

	m.ReceivedAt = time.Now()
	return nil
}

// =============================================================================
// CONNECTION
// =============================================================================

// Connection represents a connection to another node.
type Connection struct {
	mu sync.Mutex

	// Connection identity
	remoteNodeID types.NodeID
	localNodeID  types.NodeID

	// Underlying connection
	conn net.Conn

	// Message ID generator
	nextMessageID uint64

	// Pending requests (waiting for response)
	pending map[uint64]chan *Message
	pendingMu sync.Mutex

	// Flow control
	sendCredits   int64
	recvCredits   int64

	// Deduplication (for idempotent messages)
	seenMessages map[uint64]time.Time
	seenMu       sync.Mutex

	// Circuit breaker state
	consecutiveFailures int
	circuitOpen         bool
	circuitOpenUntil    time.Time

	// Configuration
	config Config

	// Metrics
	metrics *ConnectionMetrics

	// Lifecycle
	closed  bool
	closeCh chan struct{}
}

// NewConnection creates a connection wrapper.
func NewConnection(conn net.Conn, localNodeID, remoteNodeID types.NodeID, config Config) *Connection {
	return &Connection{
		conn:          conn,
		localNodeID:   localNodeID,
		remoteNodeID:  remoteNodeID,
		nextMessageID: 1,
		pending:       make(map[uint64]chan *Message),
		sendCredits:   config.InitialCredits,
		recvCredits:   config.InitialCredits,
		seenMessages:  make(map[uint64]time.Time),
		config:        config,
		metrics:       NewConnectionMetrics(),
		closeCh:       make(chan struct{}),
	}
}

// Send sends a message and waits for response.
func (c *Connection) Send(ctx context.Context, msg *Message) (*Message, error) {
	if c.closed {
		return nil, ErrConnectionClosed
	}

	// Check circuit breaker
	if c.circuitOpen {
		if time.Now().Before(c.circuitOpenUntil) {
			return nil, ErrCircuitOpen
		}
		// Half-open state - allow one request through
	}

	// Assign message ID
	msg.MessageID = atomic.AddUint64(&c.nextMessageID, 1)
	msg.Timestamp = time.Now().UnixNano()

	// Check flow control
	payloadSize := int64(len(msg.Payload))
	if atomic.LoadInt64(&c.sendCredits) < payloadSize {
		return nil, ErrBackpressure
	}
	atomic.AddInt64(&c.sendCredits, -payloadSize)

	// Create response channel
	responseCh := make(chan *Message, 1)
	c.pendingMu.Lock()
	c.pending[msg.MessageID] = responseCh
	c.pendingMu.Unlock()

	defer func() {
		c.pendingMu.Lock()
		delete(c.pending, msg.MessageID)
		c.pendingMu.Unlock()
	}()

	// Send message
	c.mu.Lock()
	_ = c.conn.SetWriteDeadline(time.Now().Add(c.config.WriteTimeout))
	err := msg.Serialize(c.conn)
	c.mu.Unlock()

	if err != nil {
		c.recordFailure()
		return nil, err
	}

	c.metrics.MessagesSent.Inc()
	c.metrics.BytesSent.Add(int64(headerSize + len(msg.Payload)))

	// Wait for response
	if msg.Flags&FlagOneWay != 0 {
		c.recordSuccess()
		return nil, nil
	}

	select {
	case response := <-responseCh:
		c.recordSuccess()
		return response, nil
	case <-ctx.Done():
		c.recordFailure()
		return nil, ctx.Err()
	case <-c.closeCh:
		return nil, ErrConnectionClosed
	}
}

// SendOneWay sends a message without waiting for response.
func (c *Connection) SendOneWay(msg *Message) error {
	msg.Flags |= FlagOneWay
	_, err := c.Send(context.Background(), msg)
	return err
}

// receiveLoop reads messages from the connection.
func (c *Connection) receiveLoop(handler MessageHandler) {
	for {
		msg := &Message{}
		_ = c.conn.SetReadDeadline(time.Now().Add(c.config.ReadTimeout))
		err := msg.Deserialize(c.conn)

		if err != nil {
			if !c.closed {
				// Connection error
				c.Close()
			}
			return
		}

		c.metrics.MessagesReceived.Inc()
		c.metrics.BytesReceived.Add(int64(headerSize + len(msg.Payload)))

		// Replenish sender's credits
		atomic.AddInt64(&c.recvCredits, int64(len(msg.Payload)))

		// Deduplicate idempotent messages
		if msg.Flags&FlagIdempotent != 0 {
			if c.isDuplicate(msg.MessageID) {
				continue
			}
		}

		// Check if this is a response to a pending request
		if msg.CorrelationID != 0 {
			c.pendingMu.Lock()
			ch, ok := c.pending[msg.CorrelationID]
			c.pendingMu.Unlock()

			if ok {
				select {
				case ch <- msg:
				default:
				}
				continue
			}
		}

		// Handle as new request
		go handler.HandleMessage(c, msg)
	}
}

// isDuplicate checks if we've seen this message recently.
func (c *Connection) isDuplicate(messageID uint64) bool {
	c.seenMu.Lock()
	defer c.seenMu.Unlock()

	if _, seen := c.seenMessages[messageID]; seen {
		return true
	}

	c.seenMessages[messageID] = time.Now()

	// Garbage collect old entries
	cutoff := time.Now().Add(-5 * time.Minute)
	for id, t := range c.seenMessages {
		if t.Before(cutoff) {
			delete(c.seenMessages, id)
		}
	}

	return false
}

func (c *Connection) recordSuccess() {
	c.consecutiveFailures = 0
	c.circuitOpen = false
}

func (c *Connection) recordFailure() {
	c.consecutiveFailures++
	if c.consecutiveFailures >= c.config.CircuitBreakerThreshold {
		c.circuitOpen = true
		c.circuitOpenUntil = time.Now().Add(c.config.CircuitBreakerTimeout)
		c.metrics.CircuitBreakerTrips.Inc()
	}
}

// Close terminates the connection.
func (c *Connection) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()

	if c.closed {
		return nil
	}

	c.closed = true
	close(c.closeCh)
	return c.conn.Close()
}

// MessageHandler processes incoming messages.
type MessageHandler interface {
	HandleMessage(conn *Connection, msg *Message)
}

// =============================================================================
// TRANSPORT
// =============================================================================

// Transport manages all network connections.
type Transport struct {
	config    Config
	localID   types.NodeID
	listener  net.Listener
	
	connections   map[types.NodeID]*Connection
	connectionsMu sync.RWMutex

	handler MessageHandler

	// Metrics
	metrics *TransportMetrics

	// Lifecycle
	closed  bool
	closeCh chan struct{}
}

// NewTransport creates a new transport.
func NewTransport(config Config, localID types.NodeID, handler MessageHandler) (*Transport, error) {
	listener, err := net.Listen("tcp", config.ListenAddress)
	if err != nil {
		return nil, err
	}

	t := &Transport{
		config:      config,
		localID:     localID,
		listener:    listener,
		connections: make(map[types.NodeID]*Connection),
		handler:     handler,
		metrics:     NewTransportMetrics(),
		closeCh:     make(chan struct{}),
	}

	go t.acceptLoop()

	return t, nil
}

// acceptLoop handles incoming connections.
func (t *Transport) acceptLoop() {
	for {
		conn, err := t.listener.Accept()
		if err != nil {
			if t.closed {
				return
			}
			continue
		}

		t.metrics.ConnectionsAccepted.Inc()

		// Check connection limit
		t.connectionsMu.RLock()
		connCount := len(t.connections)
		t.connectionsMu.RUnlock()

		if connCount >= t.config.MaxConnections {
			conn.Close()
			t.metrics.ConnectionsRejected.Inc()
			continue
		}

		// Connection handshake will identify the remote node
		go t.handleNewConnection(conn)
	}
}

func (t *Transport) handleNewConnection(netConn net.Conn) {
	// TODO: Implement handshake to exchange node IDs
	// For now, placeholder
	remoteID := types.NodeID(0)

	conn := NewConnection(netConn, t.localID, remoteID, t.config)

	t.connectionsMu.Lock()
	t.connections[remoteID] = conn
	t.connectionsMu.Unlock()

	conn.receiveLoop(t.handler)

	// Connection closed, clean up
	t.connectionsMu.Lock()
	delete(t.connections, remoteID)
	t.connectionsMu.Unlock()
}

// Connect establishes a connection to another node.
func (t *Transport) Connect(ctx context.Context, nodeID types.NodeID, address string) (*Connection, error) {
	t.connectionsMu.RLock()
	existing, ok := t.connections[nodeID]
	t.connectionsMu.RUnlock()

	if ok && !existing.closed {
		return existing, nil
	}

	dialer := net.Dialer{Timeout: t.config.WriteTimeout}
	netConn, err := dialer.DialContext(ctx, "tcp", address)
	if err != nil {
		return nil, err
	}

	conn := NewConnection(netConn, t.localID, nodeID, t.config)

	t.connectionsMu.Lock()
	t.connections[nodeID] = conn
	t.connectionsMu.Unlock()

	go conn.receiveLoop(t.handler)

	t.metrics.ConnectionsEstablished.Inc()

	return conn, nil
}

// GetConnection returns an existing connection to a node.
func (t *Transport) GetConnection(nodeID types.NodeID) (*Connection, bool) {
	t.connectionsMu.RLock()
	defer t.connectionsMu.RUnlock()

	conn, ok := t.connections[nodeID]
	if ok && !conn.closed {
		return conn, true
	}
	return nil, false
}

// Close shuts down the transport.
func (t *Transport) Close() error {
	t.closed = true
	close(t.closeCh)

	t.connectionsMu.Lock()
	for _, conn := range t.connections {
		conn.Close()
	}
	t.connectionsMu.Unlock()

	return t.listener.Close()
}

// =============================================================================
// METRICS
// =============================================================================

// ConnectionMetrics tracks per-connection metrics.
type ConnectionMetrics struct {
	MessagesSent        AtomicCounter
	MessagesReceived    AtomicCounter
	BytesSent           AtomicCounter
	BytesReceived       AtomicCounter
	CircuitBreakerTrips AtomicCounter
}

func NewConnectionMetrics() *ConnectionMetrics {
	return &ConnectionMetrics{}
}

// TransportMetrics tracks transport-level metrics.
type TransportMetrics struct {
	ConnectionsAccepted    AtomicCounter
	ConnectionsEstablished AtomicCounter
	ConnectionsRejected    AtomicCounter
	ConnectionsClosed      AtomicCounter
}

func NewTransportMetrics() *TransportMetrics {
	return &TransportMetrics{}
}

type AtomicCounter struct {
	value int64
}

func (c *AtomicCounter) Inc() {
	atomic.AddInt64(&c.value, 1)
}

func (c *AtomicCounter) Add(delta int64) {
	atomic.AddInt64(&c.value, delta)
}

func (c *AtomicCounter) Value() int64 {
	return atomic.LoadInt64(&c.value)
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrInvalidMagic       = errors.New("invalid protocol magic")
	ErrUnsupportedVersion = errors.New("unsupported protocol version")
	ErrMessageTooLarge    = errors.New("message exceeds maximum size")
	ErrChecksumMismatch   = errors.New("checksum mismatch")
	ErrConnectionClosed   = errors.New("connection closed")
	ErrCircuitOpen        = errors.New("circuit breaker open")
	ErrBackpressure       = errors.New("backpressure: out of credits")
)
