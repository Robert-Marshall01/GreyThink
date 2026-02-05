//! Grey Distributed — Minimal Network Implementation
//!
//! A minimal but runnable transport layer demonstrating:
//! - Reliable message delivery with retries
//! - Backpressure handling
//! - Connection management
//!
//! # Design Tradeoffs
//!
//! - **TCP-based**: Reliability over raw performance
//! - **Simple framing**: Length-prefixed messages
//! - **In-process simulation**: No actual network (add TCP for production)

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{mpsc, Mutex, RwLock, Semaphore};

// ============================================================================
// Types
// ============================================================================

pub type NodeId = u64;
pub type MessageId = u64;

#[derive(Debug, Clone)]
pub struct Message {
    pub id: MessageId,
    pub from: NodeId,
    pub to: NodeId,
    pub payload: Vec<u8>,
    pub created_at: Instant,
    pub retry_count: u32,
}

#[derive(Debug, Clone)]
pub struct NetworkConfig {
    pub node_id: NodeId,
    pub max_retries: u32,
    pub retry_delay: Duration,
    pub send_timeout: Duration,
    pub max_inflight: usize,
    pub max_queue_size: usize,
}

impl Default for NetworkConfig {
    fn default() -> Self {
        Self {
            node_id: 0,
            max_retries: 3,
            retry_delay: Duration::from_millis(100),
            send_timeout: Duration::from_secs(5),
            max_inflight: 100,
            max_queue_size: 1000,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SendResult {
    Success,
    Timeout,
    QueueFull,
    NodeUnreachable,
    Shutdown,
}

#[derive(Debug, Clone)]
pub struct NetworkStats {
    pub messages_sent: u64,
    pub messages_received: u64,
    pub messages_dropped: u64,
    pub retries: u64,
    pub avg_latency_us: u64,
}

// ============================================================================
// Connection
// ============================================================================

struct Connection {
    peer_id: NodeId,
    outbound: mpsc::Sender<Message>,
    pending_acks: HashMap<MessageId, PendingMessage>,
    last_activity: Instant,
    is_healthy: bool,
}

struct PendingMessage {
    message: Message,
    sent_at: Instant,
    ack_tx: Option<mpsc::Sender<bool>>,
}

impl Connection {
    fn new(peer_id: NodeId, outbound: mpsc::Sender<Message>) -> Self {
        Self {
            peer_id,
            outbound,
            pending_acks: HashMap::new(),
            last_activity: Instant::now(),
            is_healthy: true,
        }
    }
}

// ============================================================================
// Network Transport
// ============================================================================

pub struct NetworkTransport {
    config: NetworkConfig,
    connections: RwLock<HashMap<NodeId, Connection>>,
    inbound_tx: mpsc::Sender<Message>,
    inbound_rx: Mutex<mpsc::Receiver<Message>>,
    message_counter: AtomicU64,
    inflight_semaphore: Semaphore,
    
    // Metrics
    messages_sent: AtomicU64,
    messages_received: AtomicU64,
    messages_dropped: AtomicU64,
    retries: AtomicU64,
    total_latency_us: AtomicU64,
}

impl NetworkTransport {
    pub fn new(config: NetworkConfig) -> Arc<Self> {
        let (inbound_tx, inbound_rx) = mpsc::channel(config.max_queue_size);
        
        Arc::new(Self {
            inflight_semaphore: Semaphore::new(config.max_inflight),
            config,
            connections: RwLock::new(HashMap::new()),
            inbound_tx,
            inbound_rx: Mutex::new(inbound_rx),
            message_counter: AtomicU64::new(0),
            messages_sent: AtomicU64::new(0),
            messages_received: AtomicU64::new(0),
            messages_dropped: AtomicU64::new(0),
            retries: AtomicU64::new(0),
            total_latency_us: AtomicU64::new(0),
        })
    }
    
    /// Connect to a peer node.
    pub async fn connect(&self, peer_id: NodeId, outbound: mpsc::Sender<Message>) {
        let mut connections = self.connections.write().await;
        connections.insert(peer_id, Connection::new(peer_id, outbound));
    }
    
    /// Disconnect from a peer node.
    pub async fn disconnect(&self, peer_id: NodeId) {
        let mut connections = self.connections.write().await;
        connections.remove(&peer_id);
    }
    
    /// Check if connected to a peer.
    pub async fn is_connected(&self, peer_id: NodeId) -> bool {
        let connections = self.connections.read().await;
        connections.get(&peer_id).map(|c| c.is_healthy).unwrap_or(false)
    }
    
    /// Send a message to a peer with retries.
    pub async fn send(&self, to: NodeId, payload: Vec<u8>) -> SendResult {
        let message_id = self.message_counter.fetch_add(1, Ordering::Relaxed);
        
        let message = Message {
            id: message_id,
            from: self.config.node_id,
            to,
            payload,
            created_at: Instant::now(),
            retry_count: 0,
        };
        
        self.send_with_retry(message).await
    }
    
    async fn send_with_retry(&self, mut message: Message) -> SendResult {
        // Acquire semaphore permit for backpressure
        let permit = match self.inflight_semaphore.try_acquire() {
            Ok(p) => p,
            Err(_) => return SendResult::QueueFull,
        };
        
        let mut attempts = 0;
        let start = Instant::now();
        
        loop {
            let result = self.send_once(&message).await;
            
            match result {
                SendResult::Success => {
                    let latency = start.elapsed().as_micros() as u64;
                    self.messages_sent.fetch_add(1, Ordering::Relaxed);
                    self.total_latency_us.fetch_add(latency, Ordering::Relaxed);
                    drop(permit);
                    return SendResult::Success;
                }
                SendResult::Timeout | SendResult::NodeUnreachable => {
                    attempts += 1;
                    
                    if attempts > self.config.max_retries {
                        self.messages_dropped.fetch_add(1, Ordering::Relaxed);
                        drop(permit);
                        return result;
                    }
                    
                    self.retries.fetch_add(1, Ordering::Relaxed);
                    message.retry_count += 1;
                    
                    // Exponential backoff
                    let delay = self.config.retry_delay * (1 << attempts.min(4));
                    tokio::time::sleep(delay).await;
                }
                _ => {
                    drop(permit);
                    return result;
                }
            }
        }
    }
    
    async fn send_once(&self, message: &Message) -> SendResult {
        let connections = self.connections.read().await;
        
        let connection = match connections.get(&message.to) {
            Some(c) if c.is_healthy => c,
            Some(_) => return SendResult::NodeUnreachable,
            None => return SendResult::NodeUnreachable,
        };
        
        match tokio::time::timeout(
            self.config.send_timeout,
            connection.outbound.send(message.clone()),
        ).await {
            Ok(Ok(())) => SendResult::Success,
            Ok(Err(_)) => SendResult::NodeUnreachable,
            Err(_) => SendResult::Timeout,
        }
    }
    
    /// Receive the next inbound message.
    pub async fn receive(&self) -> Option<Message> {
        let mut rx = self.inbound_rx.lock().await;
        let msg = rx.recv().await;
        
        if msg.is_some() {
            self.messages_received.fetch_add(1, Ordering::Relaxed);
        }
        
        msg
    }
    
    /// Deliver a message to this transport (called by network simulator).
    pub async fn deliver(&self, message: Message) -> bool {
        self.inbound_tx.send(message).await.is_ok()
    }
    
    /// Mark a peer as unhealthy.
    pub async fn mark_unhealthy(&self, peer_id: NodeId) {
        let mut connections = self.connections.write().await;
        if let Some(conn) = connections.get_mut(&peer_id) {
            conn.is_healthy = false;
        }
    }
    
    /// Mark a peer as healthy.
    pub async fn mark_healthy(&self, peer_id: NodeId) {
        let mut connections = self.connections.write().await;
        if let Some(conn) = connections.get_mut(&peer_id) {
            conn.is_healthy = true;
            conn.last_activity = Instant::now();
        }
    }
    
    /// Get network statistics.
    pub fn stats(&self) -> NetworkStats {
        let sent = self.messages_sent.load(Ordering::Relaxed);
        let total_latency = self.total_latency_us.load(Ordering::Relaxed);
        
        NetworkStats {
            messages_sent: sent,
            messages_received: self.messages_received.load(Ordering::Relaxed),
            messages_dropped: self.messages_dropped.load(Ordering::Relaxed),
            retries: self.retries.load(Ordering::Relaxed),
            avg_latency_us: if sent > 0 { total_latency / sent } else { 0 },
        }
    }
}

// ============================================================================
// Network Simulator
// ============================================================================

/// Simulates a network for testing without real TCP connections.
pub struct NetworkSimulator {
    nodes: RwLock<HashMap<NodeId, Arc<NetworkTransport>>>,
    latency: Duration,
    packet_loss_rate: f64,
    partitions: RwLock<Vec<(NodeId, NodeId)>>,
}

impl NetworkSimulator {
    pub fn new(latency: Duration, packet_loss_rate: f64) -> Arc<Self> {
        Arc::new(Self {
            nodes: RwLock::new(HashMap::new()),
            latency,
            packet_loss_rate,
            partitions: RwLock::new(Vec::new()),
        })
    }
    
    /// Add a node to the simulation.
    pub async fn add_node(&self, transport: Arc<NetworkTransport>) {
        let node_id = transport.config.node_id;
        let mut nodes = self.nodes.write().await;
        nodes.insert(node_id, transport);
    }
    
    /// Connect all nodes to each other.
    pub async fn connect_all(&self) {
        let nodes = self.nodes.read().await;
        let node_ids: Vec<NodeId> = nodes.keys().copied().collect();
        
        for &a in &node_ids {
            for &b in &node_ids {
                if a != b {
                    if let (Some(transport_a), Some(transport_b)) = (nodes.get(&a), nodes.get(&b)) {
                        let (tx, mut rx) = mpsc::channel::<Message>(100);
                        transport_a.connect(b, tx).await;
                        
                        // Simulate delivery
                        let transport_b = transport_b.clone();
                        let latency = self.latency;
                        let packet_loss = self.packet_loss_rate;
                        
                        tokio::spawn(async move {
                            while let Some(msg) = rx.recv().await {
                                // Simulate latency
                                tokio::time::sleep(latency).await;
                                
                                // Simulate packet loss
                                if rand::random::<f64>() >= packet_loss {
                                    transport_b.deliver(msg).await;
                                }
                            }
                        });
                    }
                }
            }
        }
    }
    
    /// Create a network partition between two nodes.
    pub async fn partition(&self, a: NodeId, b: NodeId) {
        let mut partitions = self.partitions.write().await;
        partitions.push((a, b));
        partitions.push((b, a));
        
        let nodes = self.nodes.read().await;
        if let Some(transport) = nodes.get(&a) {
            transport.mark_unhealthy(b).await;
        }
        if let Some(transport) = nodes.get(&b) {
            transport.mark_unhealthy(a).await;
        }
    }
    
    /// Heal a network partition.
    pub async fn heal(&self, a: NodeId, b: NodeId) {
        let mut partitions = self.partitions.write().await;
        partitions.retain(|&(x, y)| !((x == a && y == b) || (x == b && y == a)));
        
        let nodes = self.nodes.read().await;
        if let Some(transport) = nodes.get(&a) {
            transport.mark_healthy(b).await;
        }
        if let Some(transport) = nodes.get(&b) {
            transport.mark_healthy(a).await;
        }
    }
    
    /// Check if two nodes are partitioned.
    pub async fn is_partitioned(&self, a: NodeId, b: NodeId) -> bool {
        let partitions = self.partitions.read().await;
        partitions.contains(&(a, b))
    }
}

// ============================================================================
// Backpressure Controller
// ============================================================================

pub struct BackpressureController {
    max_pending: usize,
    pending: AtomicU64,
    high_watermark: usize,
    low_watermark: usize,
    is_throttled: std::sync::atomic::AtomicBool,
}

impl BackpressureController {
    pub fn new(max_pending: usize) -> Self {
        Self {
            max_pending,
            pending: AtomicU64::new(0),
            high_watermark: (max_pending as f64 * 0.8) as usize,
            low_watermark: (max_pending as f64 * 0.5) as usize,
            is_throttled: std::sync::atomic::AtomicBool::new(false),
        }
    }
    
    /// Try to acquire a slot for sending.
    pub fn try_acquire(&self) -> bool {
        let current = self.pending.fetch_add(1, Ordering::Relaxed) as usize;
        
        if current >= self.max_pending {
            self.pending.fetch_sub(1, Ordering::Relaxed);
            return false;
        }
        
        if current >= self.high_watermark {
            self.is_throttled.store(true, Ordering::Relaxed);
        }
        
        true
    }
    
    /// Release a slot after completion.
    pub fn release(&self) {
        let current = self.pending.fetch_sub(1, Ordering::Relaxed) as usize;
        
        if current <= self.low_watermark {
            self.is_throttled.store(false, Ordering::Relaxed);
        }
    }
    
    /// Check if currently throttled.
    pub fn is_throttled(&self) -> bool {
        self.is_throttled.load(Ordering::Relaxed)
    }
    
    /// Get current pending count.
    pub fn pending_count(&self) -> usize {
        self.pending.load(Ordering::Relaxed) as usize
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_basic_send_receive() {
        let config1 = NetworkConfig { node_id: 1, ..Default::default() };
        let config2 = NetworkConfig { node_id: 2, ..Default::default() };
        
        let transport1 = NetworkTransport::new(config1);
        let transport2 = NetworkTransport::new(config2);
        
        // Create channels
        let (tx1, mut rx1) = mpsc::channel::<Message>(100);
        let (tx2, mut rx2) = mpsc::channel::<Message>(100);
        
        transport1.connect(2, tx2).await;
        transport2.connect(1, tx1).await;
        
        // Spawn receiver
        let t2 = transport2.clone();
        tokio::spawn(async move {
            while let Some(msg) = rx2.recv().await {
                t2.deliver(msg).await;
            }
        });
        
        // Send message
        let result = transport1.send(2, vec![1, 2, 3]).await;
        assert_eq!(result, SendResult::Success);
        
        // Receive
        let received = transport2.receive().await;
        assert!(received.is_some());
        assert_eq!(received.unwrap().payload, vec![1, 2, 3]);
    }
    
    #[tokio::test]
    async fn test_backpressure() {
        let bp = BackpressureController::new(10);
        
        // Acquire up to high watermark
        for _ in 0..8 {
            assert!(bp.try_acquire());
        }
        
        assert!(bp.is_throttled());
        
        // Release below low watermark
        for _ in 0..4 {
            bp.release();
        }
        
        assert!(!bp.is_throttled());
    }
    
    #[tokio::test]
    async fn test_network_simulator() {
        let sim = NetworkSimulator::new(Duration::from_millis(1), 0.0);
        
        let config1 = NetworkConfig { node_id: 1, ..Default::default() };
        let config2 = NetworkConfig { node_id: 2, ..Default::default() };
        
        let t1 = NetworkTransport::new(config1);
        let t2 = NetworkTransport::new(config2);
        
        sim.add_node(t1.clone()).await;
        sim.add_node(t2.clone()).await;
        sim.connect_all().await;
        
        // Give connections time to establish
        tokio::time::sleep(Duration::from_millis(10)).await;
        
        // Send should work
        let result = t1.send(2, vec![42]).await;
        assert_eq!(result, SendResult::Success);
    }
}
