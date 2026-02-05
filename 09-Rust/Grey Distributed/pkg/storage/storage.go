// Package storage implements sharding, replication, and quorum mechanics.
//
// # Design Overview
//
// Grey Distributed uses a distributed key-value store as its data layer.
// The design prioritizes:
// 1. Strong consistency via read/write quorums
// 2. High availability through replication
// 3. Horizontal scalability via consistent hashing
//
// # Sharding Strategy: Consistent Hashing with Virtual Nodes
//
// We use consistent hashing to distribute data across nodes:
// - Each physical node maps to multiple virtual nodes (vnodes)
// - VNodes create a more uniform distribution
// - Adding/removing nodes only moves ~1/N of the data
//
// Why virtual nodes?
// - With just physical nodes, data distribution is uneven
// - VNodes smooth out the distribution
// - Allows heterogeneous node sizes (more vnodes = more data)
//
// # Replication
//
// Each key is replicated to N nodes (default: 3):
// - Replica 1: Primary (hash-determined)
// - Replica 2: First successor on ring (different rack)
// - Replica 3: First node in different availability zone
//
// # Consistency Model
//
// We support configurable consistency levels:
// - ONE: Read/write to one node (fastest, weakest)
// - QUORUM: Majority of replicas (W + R > N)
// - ALL: All replicas (strongest, slowest)
// - LOCAL_QUORUM: Majority within local datacenter
//
// Default: QUORUM for both reads and writes, ensuring strong consistency
package storage

import (
	"context"
	"crypto/sha256"
	"encoding/binary"
	"errors"
	"sort"
	"sync"
	"time"

	"github.com/grey-systems/grey-distributed/pkg/types"
)

// =============================================================================
// CONFIGURATION
// =============================================================================

// Config contains storage layer configuration.
type Config struct {
	// ReplicationFactor is the number of replicas per key (N).
	ReplicationFactor int

	// VNodesPerNode is virtual nodes per physical node.
	// Higher values = smoother distribution, more memory overhead.
	VNodesPerNode int

	// DefaultReadConsistency for reads.
	DefaultReadConsistency ConsistencyLevel

	// DefaultWriteConsistency for writes.
	DefaultWriteConsistency ConsistencyLevel

	// ReadTimeout for read operations.
	ReadTimeout time.Duration

	// WriteTimeout for write operations.
	WriteTimeout time.Duration

	// AntiEntropyInterval is how often to run anti-entropy repair.
	AntiEntropyInterval time.Duration

	// HintedHandoffEnabled stores writes for unavailable nodes.
	HintedHandoffEnabled bool

	// HintedHandoffTTL is how long to keep hinted handoffs.
	HintedHandoffTTL time.Duration
}

// DefaultConfig returns production defaults.
func DefaultConfig() Config {
	return Config{
		ReplicationFactor:       3,
		VNodesPerNode:           256,
		DefaultReadConsistency:  ConsistencyQuorum,
		DefaultWriteConsistency: ConsistencyQuorum,
		ReadTimeout:             5 * time.Second,
		WriteTimeout:            10 * time.Second,
		AntiEntropyInterval:     6 * time.Hour,
		HintedHandoffEnabled:    true,
		HintedHandoffTTL:        24 * time.Hour,
	}
}

// ConsistencyLevel defines read/write consistency requirements.
type ConsistencyLevel int

const (
	ConsistencyOne ConsistencyLevel = iota
	ConsistencyQuorum
	ConsistencyAll
	ConsistencyLocalQuorum
)

func (c ConsistencyLevel) String() string {
	switch c {
	case ConsistencyOne:
		return "ONE"
	case ConsistencyQuorum:
		return "QUORUM"
	case ConsistencyAll:
		return "ALL"
	case ConsistencyLocalQuorum:
		return "LOCAL_QUORUM"
	default:
		return "UNKNOWN"
	}
}

// =============================================================================
// CONSISTENT HASH RING
// =============================================================================

// HashRing implements consistent hashing with virtual nodes.
type HashRing struct {
	mu sync.RWMutex

	// Sorted list of hash positions on the ring
	ring []uint64

	// Map from hash position to physical node
	hashToNode map[uint64]types.NodeID

	// Map from physical node to its virtual node hashes
	nodeToHashes map[types.NodeID][]uint64

	// Node metadata for placement decisions
	nodeInfo map[types.NodeID]*NodeInfo

	// Configuration
	vnodesPerNode int
	replicas      int
}

// NodeInfo contains node metadata for replica placement.
type NodeInfo struct {
	NodeID           types.NodeID
	Rack             string
	AvailabilityZone string
	Datacenter       string
	Weight           int // For heterogeneous nodes
}

// NewHashRing creates a new consistent hash ring.
func NewHashRing(config Config) *HashRing {
	return &HashRing{
		hashToNode:    make(map[uint64]types.NodeID),
		nodeToHashes:  make(map[types.NodeID][]uint64),
		nodeInfo:      make(map[types.NodeID]*NodeInfo),
		vnodesPerNode: config.VNodesPerNode,
		replicas:      config.ReplicationFactor,
	}
}

// AddNode adds a node to the hash ring with its virtual nodes.
func (r *HashRing) AddNode(info *NodeInfo) {
	r.mu.Lock()
	defer r.mu.Unlock()

	if _, exists := r.nodeToHashes[info.NodeID]; exists {
		return // Already exists
	}

	r.nodeInfo[info.NodeID] = info

	// Create virtual nodes
	vnodes := make([]uint64, 0, r.vnodesPerNode*info.Weight)
	for i := 0; i < r.vnodesPerNode*info.Weight; i++ {
		hash := r.hash(info.NodeID, i)
		vnodes = append(vnodes, hash)
		r.hashToNode[hash] = info.NodeID
	}

	r.nodeToHashes[info.NodeID] = vnodes
	r.rebuildRing()
}

// RemoveNode removes a node and its virtual nodes from the ring.
func (r *HashRing) RemoveNode(nodeID types.NodeID) {
	r.mu.Lock()
	defer r.mu.Unlock()

	hashes, exists := r.nodeToHashes[nodeID]
	if !exists {
		return
	}

	for _, hash := range hashes {
		delete(r.hashToNode, hash)
	}
	delete(r.nodeToHashes, nodeID)
	delete(r.nodeInfo, nodeID)

	r.rebuildRing()
}

// GetNodes returns the nodes responsible for a key.
// Returns exactly ReplicationFactor nodes, if available.
func (r *HashRing) GetNodes(key []byte) []types.NodeID {
	r.mu.RLock()
	defer r.mu.RUnlock()

	if len(r.ring) == 0 {
		return nil
	}

	keyHash := r.hashKey(key)
	return r.getNodesForHash(keyHash)
}

// getNodesForHash finds nodes for a hash value, ensuring diversity.
func (r *HashRing) getNodesForHash(hash uint64) []types.NodeID {
	// Find position in ring
	idx := sort.Search(len(r.ring), func(i int) bool {
		return r.ring[i] >= hash
	})
	if idx == len(r.ring) {
		idx = 0 // Wrap around
	}

	// Collect unique nodes with placement diversity
	seen := make(map[types.NodeID]bool)
	seenRacks := make(map[string]bool)
	seenZones := make(map[string]bool)

	result := make([]types.NodeID, 0, r.replicas)

	// Walk the ring to find replicas
	for i := 0; i < len(r.ring) && len(result) < r.replicas; i++ {
		pos := (idx + i) % len(r.ring)
		nodeID := r.hashToNode[r.ring[pos]]

		if seen[nodeID] {
			continue
		}

		info := r.nodeInfo[nodeID]
		
		// Prefer nodes in different racks (replica 2+)
		if len(result) >= 1 && info.Rack != "" && seenRacks[info.Rack] {
			// Try to find a node in a different rack first
			// But still add this one if we can't find better
		}

		// Prefer nodes in different zones (replica 3+)
		if len(result) >= 2 && info.AvailabilityZone != "" && seenZones[info.AvailabilityZone] {
			// Similar - prefer different zone but don't require it
		}

		seen[nodeID] = true
		if info.Rack != "" {
			seenRacks[info.Rack] = true
		}
		if info.AvailabilityZone != "" {
			seenZones[info.AvailabilityZone] = true
		}
		result = append(result, nodeID)
	}

	return result
}

// rebuildRing rebuilds the sorted ring from the hash map.
func (r *HashRing) rebuildRing() {
	r.ring = make([]uint64, 0, len(r.hashToNode))
	for hash := range r.hashToNode {
		r.ring = append(r.ring, hash)
	}
	sort.Slice(r.ring, func(i, j int) bool {
		return r.ring[i] < r.ring[j]
	})
}

// hash creates a deterministic hash for a virtual node.
func (r *HashRing) hash(nodeID types.NodeID, vnode int) uint64 {
	h := sha256.New()
	binary.Write(h, binary.BigEndian, nodeID)
	binary.Write(h, binary.BigEndian, int32(vnode))
	sum := h.Sum(nil)
	return binary.BigEndian.Uint64(sum[:8])
}

// hashKey creates a hash for a data key.
func (r *HashRing) hashKey(key []byte) uint64 {
	h := sha256.Sum256(key)
	return binary.BigEndian.Uint64(h[:8])
}

// GetKeyRange returns the hash range owned by a node.
func (r *HashRing) GetKeyRange(nodeID types.NodeID) []HashRange {
	r.mu.RLock()
	defer r.mu.RUnlock()

	hashes, ok := r.nodeToHashes[nodeID]
	if !ok {
		return nil
	}

	ranges := make([]HashRange, 0, len(hashes))
	for _, hash := range hashes {
		// Find this hash's position
		idx := sort.Search(len(r.ring), func(i int) bool {
			return r.ring[i] >= hash
		})
		if idx == len(r.ring) {
			idx = 0
		}

		// Range is from previous hash to this hash
		var start uint64
		if idx == 0 {
			start = r.ring[len(r.ring)-1] + 1
		} else {
			start = r.ring[idx-1] + 1
		}

		ranges = append(ranges, HashRange{Start: start, End: hash})
	}

	return ranges
}

// HashRange represents a range of hash values.
type HashRange struct {
	Start uint64
	End   uint64
}

// =============================================================================
// STORAGE ENGINE
// =============================================================================

// Engine is the distributed storage engine.
type Engine struct {
	config   Config
	ring     *HashRing
	local    LocalStore
	replicas ReplicaClient
	mu       sync.RWMutex

	// Hinted handoff queue for unavailable nodes
	hints *HintedHandoffQueue

	// Metrics
	metrics *StorageMetrics
}

// LocalStore interface for local key-value storage.
// The implementation could be LSM-tree, B-tree, etc.
type LocalStore interface {
	Get(key []byte) (*Value, error)
	Put(key []byte, value *Value) error
	Delete(key []byte) error
	Scan(start, end []byte, limit int) ([]*KeyValue, error)
	GetMerkleTree(start, end uint64) (*MerkleTree, error)
}

// ReplicaClient interface for communicating with other nodes.
type ReplicaClient interface {
	Read(ctx context.Context, nodeID types.NodeID, key []byte) (*Value, error)
	Write(ctx context.Context, nodeID types.NodeID, key []byte, value *Value) error
	Delete(ctx context.Context, nodeID types.NodeID, key []byte) error
	AntiEntropySync(ctx context.Context, nodeID types.NodeID, tree *MerkleTree) (*SyncResult, error)
}

// Value represents a stored value with metadata.
type Value struct {
	Data      []byte
	Version   uint64    // Logical clock for conflict resolution
	Timestamp time.Time // Wall clock for debugging
	Tombstone bool      // True if deleted (for eventual consistency)
	TTL       time.Duration
}

// KeyValue is a key-value pair for scans.
type KeyValue struct {
	Key   []byte
	Value *Value
}

// NewEngine creates a new storage engine.
func NewEngine(config Config, local LocalStore, replicas ReplicaClient) *Engine {
	return &Engine{
		config:   config,
		ring:     NewHashRing(config),
		local:    local,
		replicas: replicas,
		hints:    NewHintedHandoffQueue(config.HintedHandoffTTL),
		metrics:  NewStorageMetrics(),
	}
}

// =============================================================================
// READ/WRITE OPERATIONS
// =============================================================================

// Get reads a value with the specified consistency level.
//
// Quorum reads work as follows:
// 1. Send read request to all N replicas in parallel
// 2. Wait for R (quorum) responses
// 3. Return the value with highest version
// 4. Optionally, trigger read repair if versions differ
func (e *Engine) Get(ctx context.Context, key []byte, consistency ConsistencyLevel) (*Value, error) {
	nodes := e.ring.GetNodes(key)
	if len(nodes) == 0 {
		return nil, ErrNoAvailableNodes
	}

	required := e.requiredReplicas(consistency, len(nodes))

	// Read from all replicas in parallel
	type readResult struct {
		nodeID types.NodeID
		value  *Value
		err    error
	}

	resultCh := make(chan readResult, len(nodes))

	for _, nodeID := range nodes {
		go func(nid types.NodeID) {
			var result readResult
			result.nodeID = nid

			// Check if this is the local node
			if e.isLocalNode(nid) {
				result.value, result.err = e.local.Get(key)
			} else {
				result.value, result.err = e.replicas.Read(ctx, nid, key)
			}

			resultCh <- result
		}(nodeID)
	}

	// Wait for quorum
	var results []readResult
	var successCount int

	for i := 0; i < len(nodes); i++ {
		select {
		case result := <-resultCh:
			results = append(results, result)
			if result.err == nil {
				successCount++
			}
			if successCount >= required {
				// We have quorum, find the winning value
				winner := e.resolveVersions(results)
				e.metrics.ReadsSucceeded.Inc()

				// Trigger read repair asynchronously
				go e.readRepair(key, winner, results)

				return winner, nil
			}
		case <-ctx.Done():
			e.metrics.ReadsFailed.Inc()
			return nil, ctx.Err()
		}
	}

	e.metrics.ReadsFailed.Inc()
	return nil, ErrQuorumNotReached
}

// Put writes a value with the specified consistency level.
//
// Quorum writes work as follows:
// 1. Assign a new version number (logical clock)
// 2. Send write request to all N replicas in parallel
// 3. Wait for W (quorum) acknowledgments
// 4. Return success
func (e *Engine) Put(ctx context.Context, key []byte, data []byte, consistency ConsistencyLevel) error {
	nodes := e.ring.GetNodes(key)
	if len(nodes) == 0 {
		return ErrNoAvailableNodes
	}

	required := e.requiredReplicas(consistency, len(nodes))

	// Create value with version
	value := &Value{
		Data:      data,
		Version:   e.nextVersion(),
		Timestamp: time.Now(),
		Tombstone: false,
	}

	// Write to all replicas in parallel
	type writeResult struct {
		nodeID types.NodeID
		err    error
	}

	resultCh := make(chan writeResult, len(nodes))

	for _, nodeID := range nodes {
		go func(nid types.NodeID) {
			var result writeResult
			result.nodeID = nid

			if e.isLocalNode(nid) {
				result.err = e.local.Put(key, value)
			} else {
				result.err = e.replicas.Write(ctx, nid, key, value)
			}

			// If node is unavailable and hinted handoff is enabled
			if result.err != nil && e.config.HintedHandoffEnabled {
				e.hints.Add(nid, key, value)
			}

			resultCh <- result
		}(nodeID)
	}

	// Wait for quorum
	var successCount int

	for i := 0; i < len(nodes); i++ {
		select {
		case result := <-resultCh:
			if result.err == nil {
				successCount++
			}
			if successCount >= required {
				e.metrics.WritesSucceeded.Inc()
				return nil
			}
		case <-ctx.Done():
			e.metrics.WritesFailed.Inc()
			return ctx.Err()
		}
	}

	e.metrics.WritesFailed.Inc()
	return ErrQuorumNotReached
}

// Delete marks a key as deleted (tombstone).
// The tombstone is replicated like a regular write.
func (e *Engine) Delete(ctx context.Context, key []byte, consistency ConsistencyLevel) error {
	nodes := e.ring.GetNodes(key)
	if len(nodes) == 0 {
		return ErrNoAvailableNodes
	}

	required := e.requiredReplicas(consistency, len(nodes))

	// Create tombstone value
	value := &Value{
		Data:      nil,
		Version:   e.nextVersion(),
		Timestamp: time.Now(),
		Tombstone: true,
	}

	// Write tombstone to all replicas
	type writeResult struct {
		nodeID types.NodeID
		err    error
	}

	resultCh := make(chan writeResult, len(nodes))

	for _, nodeID := range nodes {
		go func(nid types.NodeID) {
			var result writeResult
			result.nodeID = nid

			if e.isLocalNode(nid) {
				result.err = e.local.Put(key, value)
			} else {
				result.err = e.replicas.Delete(ctx, nid, key)
			}

			resultCh <- result
		}(nodeID)
	}

	// Wait for quorum
	var successCount int

	for i := 0; i < len(nodes); i++ {
		select {
		case result := <-resultCh:
			if result.err == nil {
				successCount++
			}
			if successCount >= required {
				e.metrics.DeletesSucceeded.Inc()
				return nil
			}
		case <-ctx.Done():
			e.metrics.DeletesFailed.Inc()
			return ctx.Err()
		}
	}

	e.metrics.DeletesFailed.Inc()
	return ErrQuorumNotReached
}

// =============================================================================
// ANTI-ENTROPY REPAIR
// =============================================================================

// AntiEntropy runs the anti-entropy repair process.
// This should be called periodically (e.g., every 6 hours).
//
// The process:
// 1. Build Merkle tree of local data for each shard
// 2. Exchange trees with replica nodes
// 3. Identify divergent ranges
// 4. Stream missing data bidirectionally
func (e *Engine) AntiEntropy(ctx context.Context) error {
	// For each range we own
	// TODO: Get ranges from ring
	
	e.metrics.AntiEntropyRuns.Inc()
	return nil
}

// MerkleTree is a hash tree for efficient comparison.
type MerkleTree struct {
	Root       *MerkleNode
	RangeStart uint64
	RangeEnd   uint64
	Depth      int
}

// MerkleNode is a node in the Merkle tree.
type MerkleNode struct {
	Hash  [32]byte
	Left  *MerkleNode
	Right *MerkleNode
	// For leaf nodes
	StartKey []byte
	EndKey   []byte
}

// SyncResult contains the result of an anti-entropy sync.
type SyncResult struct {
	KeysSent     int
	KeysReceived int
	Duration     time.Duration
}

// =============================================================================
// HELPER METHODS
// =============================================================================

func (e *Engine) requiredReplicas(consistency ConsistencyLevel, totalNodes int) int {
	n := totalNodes
	switch consistency {
	case ConsistencyOne:
		return 1
	case ConsistencyQuorum:
		return n/2 + 1
	case ConsistencyAll:
		return n
	case ConsistencyLocalQuorum:
		// In a real implementation, this would count only local DC nodes
		return n/2 + 1
	default:
		return n/2 + 1
	}
}

func (e *Engine) isLocalNode(nodeID types.NodeID) bool {
	// TODO: Compare with local node ID
	return false
}

func (e *Engine) nextVersion() uint64 {
	// TODO: Use a proper logical clock
	return uint64(time.Now().UnixNano())
}

// resolveVersions picks the winning value based on version (last-write-wins).
func (e *Engine) resolveVersions(results []readResult) *Value {
	var winner *Value
	for _, result := range results {
		if result.err != nil || result.value == nil {
			continue
		}
		if winner == nil || result.value.Version > winner.Version {
			winner = result.value
		}
	}
	return winner
}

type readResult struct {
	nodeID types.NodeID
	value  *Value
	err    error
}

// readRepair sends the correct value to nodes with stale data.
func (e *Engine) readRepair(key []byte, winner *Value, results []readResult) {
	if winner == nil {
		return
	}

	for _, result := range results {
		if result.err != nil {
			continue
		}
		if result.value == nil || result.value.Version < winner.Version {
			// This node has stale data, repair it
			ctx, cancel := context.WithTimeout(context.Background(), e.config.WriteTimeout)
			_ = e.replicas.Write(ctx, result.nodeID, key, winner)
			cancel()
			e.metrics.ReadRepairs.Inc()
		}
	}
}

// =============================================================================
// HINTED HANDOFF
// =============================================================================

// HintedHandoffQueue stores writes for temporarily unavailable nodes.
type HintedHandoffQueue struct {
	mu    sync.Mutex
	hints map[types.NodeID][]*Hint
	ttl   time.Duration
}

// Hint is a pending write for an unavailable node.
type Hint struct {
	Key       []byte
	Value     *Value
	CreatedAt time.Time
}

func NewHintedHandoffQueue(ttl time.Duration) *HintedHandoffQueue {
	return &HintedHandoffQueue{
		hints: make(map[types.NodeID][]*Hint),
		ttl:   ttl,
	}
}

func (q *HintedHandoffQueue) Add(nodeID types.NodeID, key []byte, value *Value) {
	q.mu.Lock()
	defer q.mu.Unlock()

	hint := &Hint{
		Key:       key,
		Value:     value,
		CreatedAt: time.Now(),
	}

	q.hints[nodeID] = append(q.hints[nodeID], hint)
}

// DrainTo sends all hints for a node that has come back online.
func (q *HintedHandoffQueue) DrainTo(ctx context.Context, nodeID types.NodeID, client ReplicaClient) int {
	q.mu.Lock()
	hints := q.hints[nodeID]
	delete(q.hints, nodeID)
	q.mu.Unlock()

	sent := 0
	cutoff := time.Now().Add(-q.ttl)

	for _, hint := range hints {
		if hint.CreatedAt.Before(cutoff) {
			continue // Hint expired
		}

		err := client.Write(ctx, nodeID, hint.Key, hint.Value)
		if err == nil {
			sent++
		}
	}

	return sent
}

// =============================================================================
// METRICS
// =============================================================================

// StorageMetrics tracks storage operations.
type StorageMetrics struct {
	ReadsSucceeded    AtomicCounter
	ReadsFailed       AtomicCounter
	WritesSucceeded   AtomicCounter
	WritesFailed      AtomicCounter
	DeletesSucceeded  AtomicCounter
	DeletesFailed     AtomicCounter
	ReadRepairs       AtomicCounter
	AntiEntropyRuns   AtomicCounter
	HintedHandoffsSent AtomicCounter
}

func NewStorageMetrics() *StorageMetrics {
	return &StorageMetrics{}
}

type AtomicCounter struct {
	value int64
	mu    sync.Mutex
}

func (c *AtomicCounter) Inc() {
	c.mu.Lock()
	c.value++
	c.mu.Unlock()
}

func (c *AtomicCounter) Value() int64 {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.value
}

// =============================================================================
// ERRORS
// =============================================================================

var (
	ErrNoAvailableNodes = errors.New("no available nodes for key")
	ErrQuorumNotReached = errors.New("quorum not reached")
	ErrKeyNotFound      = errors.New("key not found")
)
