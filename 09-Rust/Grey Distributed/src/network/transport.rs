//! # Transport Layer
//!
//! Connection management and transport protocols.
//!
//! ## Transport Options
//!
//! 1. **TCP**: Reliable, ordered delivery
//!    - Best for: Most messages
//!    - Trade-off: Higher latency than UDP
//!
//! 2. **UDP**: Low latency, unreliable
//!    - Best for: Heartbeats, gossip
//!    - Trade-off: May lose messages
//!
//! 3. **QUIC**: Modern, multiplexed streams
//!    - Best for: High connection count
//!    - Trade-off: Newer, less battle-tested
//!
//! ## Connection Lifecycle
//!
//! ```text
//! Disconnected → Connecting → Connected ⟷ Draining → Disconnected
//!       ↑                         |
//!       └─────── Failed ──────────┘
//! ```

use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::{Mutex, RwLock};
use tokio::sync::mpsc;

use super::protocol::{Message, MessageCodec, PeerId};
use super::NetworkError;

// ============================================================================
// Connection Types
// ============================================================================

/// Connection identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConnectionId(pub u64);

/// Connection state
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ConnectionState {
    Disconnected,
    Connecting,
    Connected,
    Draining,
    Failed,
}

/// Transport protocol
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Transport {
    Tcp,
    Udp,
    Quic,
}

/// Connection metadata
#[derive(Debug, Clone)]
pub struct Connection {
    pub id: ConnectionId,
    pub peer_id: Option<PeerId>,
    pub remote_addr: SocketAddr,
    pub local_addr: SocketAddr,
    pub state: ConnectionState,
    pub transport: Transport,
    pub established_at: Option<Instant>,
    pub last_activity: Instant,
    pub bytes_sent: u64,
    pub bytes_received: u64,
    pub messages_sent: u64,
    pub messages_received: u64,
}

impl Connection {
    pub fn new(id: ConnectionId, remote_addr: SocketAddr, local_addr: SocketAddr, transport: Transport) -> Self {
        Self {
            id,
            peer_id: None,
            remote_addr,
            local_addr,
            state: ConnectionState::Disconnected,
            transport,
            established_at: None,
            last_activity: Instant::now(),
            bytes_sent: 0,
            bytes_received: 0,
            messages_sent: 0,
            messages_received: 0,
        }
    }
    
    pub fn is_active(&self) -> bool {
        matches!(self.state, ConnectionState::Connected | ConnectionState::Draining)
    }
    
    pub fn latency(&self, now: Instant) -> Duration {
        now.duration_since(self.last_activity)
    }
}

// ============================================================================
// Connection Pool
// ============================================================================

/// Manages a pool of connections
///
/// ## Goals
///
/// - Reuse connections to reduce overhead
/// - Load balance across connections
/// - Handle failures gracefully
/// - Limit resource usage
pub struct ConnectionPool {
    /// All connections
    connections: RwLock<HashMap<ConnectionId, Connection>>,
    
    /// Connections by peer
    by_peer: RwLock<HashMap<PeerId, Vec<ConnectionId>>>,
    
    /// Connections by address
    by_addr: RwLock<HashMap<SocketAddr, ConnectionId>>,
    
    /// Configuration
    config: PoolConfig,
    
    /// Next connection ID
    next_id: AtomicU64,
    
    /// Metrics
    metrics: PoolMetrics,
}

#[derive(Debug, Clone)]
pub struct PoolConfig {
    /// Maximum connections per peer
    pub max_per_peer: usize,
    
    /// Maximum total connections
    pub max_total: usize,
    
    /// Idle connection timeout
    pub idle_timeout: Duration,
    
    /// Connection timeout
    pub connect_timeout: Duration,
    
    /// Enable connection reuse
    pub reuse: bool,
}

impl Default for PoolConfig {
    fn default() -> Self {
        Self {
            max_per_peer: 4,
            max_total: 1024,
            idle_timeout: Duration::from_secs(300),
            connect_timeout: Duration::from_secs(10),
            reuse: true,
        }
    }
}

#[derive(Default)]
struct PoolMetrics {
    connections_created: AtomicU64,
    connections_closed: AtomicU64,
    connections_failed: AtomicU64,
    current_active: AtomicU64,
}

impl ConnectionPool {
    pub fn new(config: PoolConfig) -> Self {
        Self {
            connections: RwLock::new(HashMap::new()),
            by_peer: RwLock::new(HashMap::new()),
            by_addr: RwLock::new(HashMap::new()),
            config,
            next_id: AtomicU64::new(1),
            metrics: PoolMetrics::default(),
        }
    }
    
    /// Create or get a connection to an address
    pub fn get_or_create(&self, addr: SocketAddr, transport: Transport) -> Result<ConnectionId, NetworkError> {
        // Check for existing connection
        if self.config.reuse {
            if let Some(id) = self.by_addr.read().get(&addr) {
                let connections = self.connections.read();
                if let Some(conn) = connections.get(id) {
                    if conn.is_active() {
                        return Ok(*id);
                    }
                }
            }
        }
        
        // Check limits
        let current = self.metrics.current_active.load(Ordering::Relaxed);
        if current as usize >= self.config.max_total {
            return Err(NetworkError::ConnectionFailed {
                peer_id: 0,
                reason: "Connection limit reached".into(),
            });
        }
        
        // Create new connection
        let id = ConnectionId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let local_addr = "0.0.0.0:0".parse().unwrap(); // Will be set on connect
        
        let mut conn = Connection::new(id, addr, local_addr, transport);
        conn.state = ConnectionState::Connecting;
        
        self.connections.write().insert(id, conn);
        self.by_addr.write().insert(addr, id);
        self.metrics.connections_created.fetch_add(1, Ordering::Relaxed);
        self.metrics.current_active.fetch_add(1, Ordering::Relaxed);
        
        Ok(id)
    }
    
    /// Mark connection as established
    pub fn mark_connected(&self, id: ConnectionId, peer_id: Option<PeerId>) {
        let mut connections = self.connections.write();
        
        if let Some(conn) = connections.get_mut(&id) {
            conn.state = ConnectionState::Connected;
            conn.established_at = Some(Instant::now());
            conn.last_activity = Instant::now();
            
            if let Some(pid) = peer_id {
                conn.peer_id = Some(pid);
                self.by_peer.write()
                    .entry(pid)
                    .or_insert_with(Vec::new)
                    .push(id);
            }
        }
    }
    
    /// Mark connection as failed
    pub fn mark_failed(&self, id: ConnectionId) {
        let mut connections = self.connections.write();
        
        if let Some(conn) = connections.get_mut(&id) {
            conn.state = ConnectionState::Failed;
            self.metrics.connections_failed.fetch_add(1, Ordering::Relaxed);
        }
    }
    
    /// Close a connection
    pub fn close(&self, id: ConnectionId) {
        let mut connections = self.connections.write();
        
        if let Some(conn) = connections.remove(&id) {
            // Remove from peer map
            if let Some(peer_id) = conn.peer_id {
                if let Some(peers) = self.by_peer.write().get_mut(&peer_id) {
                    peers.retain(|&cid| cid != id);
                }
            }
            
            // Remove from address map
            self.by_addr.write().remove(&conn.remote_addr);
            
            self.metrics.connections_closed.fetch_add(1, Ordering::Relaxed);
            self.metrics.current_active.fetch_sub(1, Ordering::Relaxed);
        }
    }
    
    /// Get a connection by ID
    pub fn get(&self, id: ConnectionId) -> Option<Connection> {
        self.connections.read().get(&id).cloned()
    }
    
    /// Get connections for a peer
    pub fn get_for_peer(&self, peer_id: PeerId) -> Vec<ConnectionId> {
        self.by_peer.read()
            .get(&peer_id)
            .cloned()
            .unwrap_or_default()
    }
    
    /// Update activity timestamp
    pub fn touch(&self, id: ConnectionId) {
        if let Some(conn) = self.connections.write().get_mut(&id) {
            conn.last_activity = Instant::now();
        }
    }
    
    /// Record bytes transferred
    pub fn record_transfer(&self, id: ConnectionId, sent: u64, received: u64) {
        if let Some(conn) = self.connections.write().get_mut(&id) {
            conn.bytes_sent += sent;
            conn.bytes_received += received;
            if sent > 0 {
                conn.messages_sent += 1;
            }
            if received > 0 {
                conn.messages_received += 1;
            }
        }
    }
    
    /// Close idle connections
    pub fn close_idle(&self) -> Vec<ConnectionId> {
        let now = Instant::now();
        let mut to_close = Vec::new();
        
        {
            let connections = self.connections.read();
            for (&id, conn) in connections.iter() {
                if conn.is_active() && now.duration_since(conn.last_activity) > self.config.idle_timeout {
                    to_close.push(id);
                }
            }
        }
        
        for id in &to_close {
            self.close(*id);
        }
        
        to_close
    }
    
    /// Get pool stats
    pub fn stats(&self) -> PoolStats {
        let connections = self.connections.read();
        
        PoolStats {
            total: connections.len(),
            active: connections.values().filter(|c| c.state == ConnectionState::Connected).count(),
            connecting: connections.values().filter(|c| c.state == ConnectionState::Connecting).count(),
            failed: connections.values().filter(|c| c.state == ConnectionState::Failed).count(),
            created: self.metrics.connections_created.load(Ordering::Relaxed),
            closed: self.metrics.connections_closed.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PoolStats {
    pub total: usize,
    pub active: usize,
    pub connecting: usize,
    pub failed: usize,
    pub created: u64,
    pub closed: u64,
}

// ============================================================================
// Transport Manager
// ============================================================================

/// Manages transport layer operations
pub struct TransportManager {
    /// Connection pool
    pool: Arc<ConnectionPool>,
    
    /// Message codec
    codec: MessageCodec,
    
    /// Outbound message queue
    outbound: Mutex<Vec<(ConnectionId, Message)>>,
    
    /// Inbound message handlers
    handlers: RwLock<Vec<Box<dyn MessageHandler + Send + Sync>>>,
    
    /// Transport configuration
    config: TransportConfig,
}

#[derive(Debug, Clone)]
pub struct TransportConfig {
    /// Read buffer size
    pub read_buffer_size: usize,
    
    /// Write buffer size  
    pub write_buffer_size: usize,
    
    /// Enable Nagle's algorithm
    pub tcp_nodelay: bool,
    
    /// TCP keepalive
    pub tcp_keepalive: Option<Duration>,
    
    /// Maximum concurrent sends
    pub max_concurrent_sends: usize,
}

impl Default for TransportConfig {
    fn default() -> Self {
        Self {
            read_buffer_size: 64 * 1024,
            write_buffer_size: 64 * 1024,
            tcp_nodelay: true,
            tcp_keepalive: Some(Duration::from_secs(30)),
            max_concurrent_sends: 100,
        }
    }
}

/// Handler for incoming messages
pub trait MessageHandler: Send + Sync {
    fn handle(&self, conn: &Connection, message: Message);
}

impl TransportManager {
    pub fn new(config: TransportConfig, pool_config: PoolConfig) -> Self {
        Self {
            pool: Arc::new(ConnectionPool::new(pool_config)),
            codec: MessageCodec::new(),
            outbound: Mutex::new(Vec::new()),
            handlers: RwLock::new(Vec::new()),
            config,
        }
    }
    
    /// Get the connection pool
    pub fn pool(&self) -> Arc<ConnectionPool> {
        self.pool.clone()
    }
    
    /// Register a message handler
    pub fn on_message<H: MessageHandler + 'static>(&self, handler: H) {
        self.handlers.write().push(Box::new(handler));
    }
    
    /// Queue a message for sending
    pub fn send(&self, conn_id: ConnectionId, message: Message) -> Result<(), NetworkError> {
        // Verify connection exists and is active
        let conn = self.pool.get(conn_id)
            .ok_or(NetworkError::ConnectionClosed)?;
        
        if !conn.is_active() {
            return Err(NetworkError::ConnectionClosed);
        }
        
        // Queue the message
        self.outbound.lock().push((conn_id, message));
        
        Ok(())
    }
    
    /// Send to a peer by ID
    pub fn send_to_peer(&self, peer_id: PeerId, message: Message) -> Result<(), NetworkError> {
        let connections = self.pool.get_for_peer(peer_id);
        
        if connections.is_empty() {
            return Err(NetworkError::PeerNotFound(peer_id.0));
        }
        
        // Use first available connection
        for conn_id in connections {
            if let Some(conn) = self.pool.get(conn_id) {
                if conn.is_active() {
                    return self.send(conn_id, message);
                }
            }
        }
        
        Err(NetworkError::PeerNotFound(peer_id.0))
    }
    
    /// Get pending outbound messages
    pub fn drain_outbound(&self) -> Vec<(ConnectionId, Vec<u8>)> {
        let messages = std::mem::take(&mut *self.outbound.lock());
        
        messages
            .into_iter()
            .filter_map(|(id, msg)| {
                self.codec.encode(&msg).ok().map(|data| (id, data))
            })
            .collect()
    }
    
    /// Process an inbound message
    pub fn receive(&self, conn_id: ConnectionId, data: &[u8]) -> Result<(), NetworkError> {
        let message = self.codec.decode(data)?;
        
        let conn = self.pool.get(conn_id);
        
        if let Some(conn) = conn {
            self.pool.touch(conn_id);
            self.pool.record_transfer(conn_id, 0, data.len() as u64);
            
            let handlers = self.handlers.read();
            for handler in handlers.iter() {
                handler.handle(&conn, message.clone());
            }
        }
        
        Ok(())
    }
    
    /// Close all connections
    pub fn shutdown(&self) {
        let connections: Vec<_> = self.pool.connections.read().keys().copied().collect();
        for id in connections {
            self.pool.close(id);
        }
    }
}

// ============================================================================
// Backpressure
// ============================================================================

/// Flow control for preventing overload
pub struct FlowController {
    /// Current window size
    window: AtomicU64,
    
    /// Maximum window size
    max_window: u64,
    
    /// Minimum window size
    min_window: u64,
    
    /// Outstanding bytes
    outstanding: AtomicU64,
}

impl FlowController {
    pub fn new(initial_window: u64, max_window: u64, min_window: u64) -> Self {
        Self {
            window: AtomicU64::new(initial_window),
            max_window,
            min_window,
            outstanding: AtomicU64::new(0),
        }
    }
    
    /// Check if we can send more data
    pub fn can_send(&self, size: u64) -> bool {
        let outstanding = self.outstanding.load(Ordering::Relaxed);
        let window = self.window.load(Ordering::Relaxed);
        outstanding + size <= window
    }
    
    /// Consume window for sending
    pub fn consume(&self, size: u64) -> bool {
        if self.can_send(size) {
            self.outstanding.fetch_add(size, Ordering::Relaxed);
            true
        } else {
            false
        }
    }
    
    /// Acknowledge received data (release window)
    pub fn acknowledge(&self, size: u64) {
        self.outstanding.fetch_sub(size.min(self.outstanding.load(Ordering::Relaxed)), Ordering::Relaxed);
    }
    
    /// Update window size (e.g., based on RTT)
    pub fn update_window(&self, new_window: u64) {
        let clamped = new_window.max(self.min_window).min(self.max_window);
        self.window.store(clamped, Ordering::Relaxed);
    }
    
    /// Get current availability
    pub fn available(&self) -> u64 {
        let window = self.window.load(Ordering::Relaxed);
        let outstanding = self.outstanding.load(Ordering::Relaxed);
        window.saturating_sub(outstanding)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_connection_pool() {
        let pool = ConnectionPool::new(PoolConfig::default());
        
        let addr: SocketAddr = "127.0.0.1:8080".parse().unwrap();
        let id = pool.get_or_create(addr, Transport::Tcp).unwrap();
        
        assert!(pool.get(id).is_some());
        assert_eq!(pool.get(id).unwrap().state, ConnectionState::Connecting);
        
        pool.mark_connected(id, Some(PeerId(1)));
        assert_eq!(pool.get(id).unwrap().state, ConnectionState::Connected);
        
        pool.close(id);
        assert!(pool.get(id).is_none());
    }
    
    #[test]
    fn test_flow_controller() {
        let flow = FlowController::new(1000, 10000, 100);
        
        assert!(flow.can_send(500));
        assert!(flow.consume(500));
        assert!(flow.can_send(500));
        assert!(!flow.can_send(600));
        
        flow.acknowledge(500);
        assert!(flow.can_send(1000));
    }
    
    #[test]
    fn test_transport_manager() {
        let manager = TransportManager::new(
            TransportConfig::default(),
            PoolConfig::default(),
        );
        
        let addr: SocketAddr = "127.0.0.1:9090".parse().unwrap();
        let conn_id = manager.pool.get_or_create(addr, Transport::Tcp).unwrap();
        manager.pool.mark_connected(conn_id, Some(PeerId(1)));
        
        let msg = Message::new(
            super::super::protocol::MessageType::Ping,
            PeerId(0),
            vec![],
        );
        
        assert!(manager.send(conn_id, msg).is_ok());
        
        let outbound = manager.drain_outbound();
        assert_eq!(outbound.len(), 1);
    }
}
