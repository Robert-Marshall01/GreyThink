//! # Message Routing
//!
//! Peer discovery and message routing infrastructure.
//!
//! ## Routing Strategies
//!
//! 1. **Direct**: Known peer addresses
//! 2. **Gossip**: Epidemic information spreading
//! 3. **DHT**: Distributed hash table routing
//! 4. **Hierarchical**: Tree/ring topologies
//!
//! ## Discovery Mechanisms
//!
//! - Static configuration
//! - DNS-based discovery
//! - Gossip-based peer exchange
//! - Service discovery (Consul, etcd, k8s)

use std::collections::{HashMap, HashSet, VecDeque};
use std::net::SocketAddr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

use parking_lot::RwLock;

use super::protocol::PeerId;
use super::NetworkError;

// ============================================================================
// Peer Information
// ============================================================================

/// Complete peer information
#[derive(Debug, Clone)]
pub struct PeerInfo {
    pub id: PeerId,
    pub addresses: Vec<SocketAddr>,
    pub state: PeerState,
    pub last_seen: Instant,
    pub version: String,
    pub capabilities: HashSet<String>,
    pub metadata: HashMap<String, String>,
    /// Failure count since last success
    pub failures: u32,
    /// RTT estimate
    pub rtt_estimate: Duration,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PeerState {
    Unknown,
    Alive,
    Suspected,
    Failed,
    Left,
}

impl PeerInfo {
    pub fn new(id: PeerId, addr: SocketAddr) -> Self {
        Self {
            id,
            addresses: vec![addr],
            state: PeerState::Unknown,
            last_seen: Instant::now(),
            version: String::new(),
            capabilities: HashSet::new(),
            metadata: HashMap::new(),
            failures: 0,
            rtt_estimate: Duration::from_millis(100),
        }
    }
    
    pub fn primary_address(&self) -> Option<SocketAddr> {
        self.addresses.first().copied()
    }
    
    pub fn mark_alive(&mut self) {
        self.state = PeerState::Alive;
        self.last_seen = Instant::now();
        self.failures = 0;
    }
    
    pub fn mark_suspected(&mut self) {
        if self.state == PeerState::Alive {
            self.state = PeerState::Suspected;
        }
    }
    
    pub fn mark_failed(&mut self) {
        self.state = PeerState::Failed;
        self.failures += 1;
    }
    
    pub fn is_reachable(&self) -> bool {
        matches!(self.state, PeerState::Alive | PeerState::Suspected)
    }
}

// ============================================================================
// Peer Registry
// ============================================================================

/// Registry of known peers
pub struct PeerRegistry {
    /// All known peers
    peers: RwLock<HashMap<PeerId, PeerInfo>>,
    
    /// Address to peer mapping
    by_addr: RwLock<HashMap<SocketAddr, PeerId>>,
    
    /// Maximum peers to track
    max_peers: usize,
    
    /// Suspect timeout
    suspect_timeout: Duration,
    
    /// Failure timeout
    failure_timeout: Duration,
}

impl PeerRegistry {
    pub fn new(max_peers: usize) -> Self {
        Self {
            peers: RwLock::new(HashMap::new()),
            by_addr: RwLock::new(HashMap::new()),
            max_peers,
            suspect_timeout: Duration::from_secs(5),
            failure_timeout: Duration::from_secs(30),
        }
    }
    
    /// Register a new peer
    pub fn register(&self, info: PeerInfo) -> bool {
        let mut peers = self.peers.write();
        
        if peers.len() >= self.max_peers && !peers.contains_key(&info.id) {
            return false;
        }
        
        // Update address mapping
        let mut by_addr = self.by_addr.write();
        for addr in &info.addresses {
            by_addr.insert(*addr, info.id);
        }
        
        peers.insert(info.id, info);
        true
    }
    
    /// Get peer info
    pub fn get(&self, id: PeerId) -> Option<PeerInfo> {
        self.peers.read().get(&id).cloned()
    }
    
    /// Get peer by address
    pub fn get_by_addr(&self, addr: &SocketAddr) -> Option<PeerInfo> {
        let by_addr = self.by_addr.read();
        by_addr.get(addr).and_then(|id| self.get(*id))
    }
    
    /// Update peer state
    pub fn update<F>(&self, id: PeerId, f: F) -> bool
    where
        F: FnOnce(&mut PeerInfo),
    {
        if let Some(info) = self.peers.write().get_mut(&id) {
            f(info);
            true
        } else {
            false
        }
    }
    
    /// Remove a peer
    pub fn remove(&self, id: PeerId) -> Option<PeerInfo> {
        let info = self.peers.write().remove(&id);
        
        if let Some(ref info) = info {
            let mut by_addr = self.by_addr.write();
            for addr in &info.addresses {
                by_addr.remove(addr);
            }
        }
        
        info
    }
    
    /// Get all alive peers
    pub fn alive_peers(&self) -> Vec<PeerInfo> {
        self.peers
            .read()
            .values()
            .filter(|p| p.state == PeerState::Alive)
            .cloned()
            .collect()
    }
    
    /// Get random peers for gossip
    pub fn random_peers(&self, count: usize) -> Vec<PeerInfo> {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let peers = self.peers.read();
        let alive: Vec<_> = peers
            .values()
            .filter(|p| p.is_reachable())
            .cloned()
            .collect();
        
        if alive.len() <= count {
            return alive;
        }
        
        // Simple reservoir sampling
        let mut hasher = DefaultHasher::new();
        Instant::now().hash(&mut hasher);
        let seed = hasher.finish();
        
        let mut selected: Vec<_> = alive.iter().cloned().take(count).collect();
        for (i, peer) in alive.iter().enumerate().skip(count) {
            let j = (seed.wrapping_add(i as u64) as usize) % (i + 1);
            if j < count {
                selected[j] = peer.clone();
            }
        }
        
        selected
    }
    
    /// Check for suspect/failed peers
    pub fn check_timeouts(&self) -> (Vec<PeerId>, Vec<PeerId>) {
        let now = Instant::now();
        let mut suspected = Vec::new();
        let mut failed = Vec::new();
        
        let mut peers = self.peers.write();
        
        for (id, info) in peers.iter_mut() {
            let elapsed = now.duration_since(info.last_seen);
            
            match info.state {
                PeerState::Alive => {
                    if elapsed > self.suspect_timeout {
                        info.state = PeerState::Suspected;
                        suspected.push(*id);
                    }
                }
                PeerState::Suspected => {
                    if elapsed > self.failure_timeout {
                        info.state = PeerState::Failed;
                        failed.push(*id);
                    }
                }
                _ => {}
            }
        }
        
        (suspected, failed)
    }
    
    /// Count peers by state
    pub fn counts(&self) -> HashMap<PeerState, usize> {
        let peers = self.peers.read();
        let mut counts = HashMap::new();
        
        for info in peers.values() {
            *counts.entry(info.state).or_insert(0) += 1;
        }
        
        counts
    }
}

// ============================================================================
// Gossip Protocol
// ============================================================================

/// Gossip-based peer discovery and information spreading
///
/// ## SWIM Protocol
///
/// Scalable Weakly-consistent Infection-style Membership:
/// 1. Periodically ping random peer
/// 2. If no ack, ask K peers to probe on our behalf
/// 3. If still no ack, mark as suspected
/// 4. After timeout, mark as failed
/// 5. Piggyback membership changes on messages
pub struct GossipProtocol {
    /// Peer registry
    registry: Arc<PeerRegistry>,
    
    /// Local peer ID
    local_id: PeerId,
    
    /// Fanout (number of peers for indirect probes)
    fanout: usize,
    
    /// Gossip interval
    interval: Duration,
    
    /// Messages to gossip
    pending_gossip: RwLock<VecDeque<GossipMessage>>,
    
    /// Incarnation number (lamport-like counter)
    incarnation: AtomicU64,
}

#[derive(Debug, Clone)]
pub struct GossipMessage {
    /// Type of gossip
    pub kind: GossipKind,
    
    /// Source peer
    pub source: PeerId,
    
    /// Message incarnation
    pub incarnation: u64,
    
    /// Creation time
    pub created_at: Instant,
}

#[derive(Debug, Clone)]
pub enum GossipKind {
    /// Peer is alive
    Alive { peer: PeerId, addr: SocketAddr },
    /// Peer is suspected
    Suspect { peer: PeerId },
    /// Peer has failed
    Dead { peer: PeerId },
    /// Peer left voluntarily
    Left { peer: PeerId },
    /// Custom application gossip
    Custom { key: String, value: Vec<u8> },
}

impl GossipProtocol {
    pub fn new(local_id: PeerId, registry: Arc<PeerRegistry>) -> Self {
        Self {
            registry,
            local_id,
            fanout: 3,
            interval: Duration::from_secs(1),
            pending_gossip: RwLock::new(VecDeque::new()),
            incarnation: AtomicU64::new(1),
        }
    }
    
    /// Broadcast a gossip message
    pub fn broadcast(&self, kind: GossipKind) {
        let msg = GossipMessage {
            kind,
            source: self.local_id,
            incarnation: self.incarnation.fetch_add(1, Ordering::SeqCst),
            created_at: Instant::now(),
        };
        
        self.pending_gossip.write().push_back(msg);
    }
    
    /// Get messages to send (piggybacked on other messages)
    pub fn drain_gossip(&self, max: usize) -> Vec<GossipMessage> {
        let mut pending = self.pending_gossip.write();
        let count = max.min(pending.len());
        pending.drain(0..count).collect()
    }
    
    /// Apply received gossip
    pub fn apply(&self, msg: GossipMessage) {
        match msg.kind {
            GossipKind::Alive { peer, addr } => {
                if let Some(mut info) = self.registry.get(peer) {
                    info.mark_alive();
                    self.registry.register(info);
                } else {
                    let info = PeerInfo::new(peer, addr);
                    self.registry.register(info);
                }
            }
            GossipKind::Suspect { peer } => {
                self.registry.update(peer, |info| {
                    info.mark_suspected();
                });
            }
            GossipKind::Dead { peer } | GossipKind::Left { peer } => {
                self.registry.update(peer, |info| {
                    info.mark_failed();
                });
            }
            GossipKind::Custom { .. } => {
                // Application-specific handling
            }
        }
        
        // Re-gossip to spread information
        if msg.source != self.local_id {
            self.pending_gossip.write().push_back(msg);
        }
    }
    
    /// Select peers for probing
    pub fn select_probe_targets(&self) -> Vec<PeerId> {
        self.registry
            .random_peers(self.fanout)
            .into_iter()
            .map(|p| p.id)
            .filter(|id| *id != self.local_id)
            .collect()
    }
}

// ============================================================================
// Router
// ============================================================================

/// Message router
pub struct Router {
    /// Local peer ID
    local_id: PeerId,
    
    /// Peer registry
    registry: Arc<PeerRegistry>,
    
    /// Routing table (peer -> next hop)
    routes: RwLock<HashMap<PeerId, RouteInfo>>,
    
    /// Route TTL
    route_ttl: Duration,
}

#[derive(Debug, Clone)]
pub struct RouteInfo {
    pub next_hop: PeerId,
    pub hops: u8,
    pub latency: Duration,
    pub updated_at: Instant,
}

impl Router {
    pub fn new(local_id: PeerId, registry: Arc<PeerRegistry>) -> Self {
        Self {
            local_id,
            registry,
            routes: RwLock::new(HashMap::new()),
            route_ttl: Duration::from_secs(60),
        }
    }
    
    /// Get next hop for a destination
    pub fn next_hop(&self, destination: PeerId) -> Option<PeerId> {
        // Direct connection?
        if let Some(peer) = self.registry.get(destination) {
            if peer.is_reachable() {
                return Some(destination);
            }
        }
        
        // Check routing table
        let routes = self.routes.read();
        if let Some(route) = routes.get(&destination) {
            if Instant::now().duration_since(route.updated_at) < self.route_ttl {
                return Some(route.next_hop);
            }
        }
        
        None
    }
    
    /// Update route information
    pub fn update_route(&self, destination: PeerId, next_hop: PeerId, hops: u8, latency: Duration) {
        let mut routes = self.routes.write();
        
        let should_update = routes.get(&destination).map_or(true, |existing| {
            // Prefer shorter paths or fresher routes
            hops < existing.hops || 
            (hops == existing.hops && latency < existing.latency)
        });
        
        if should_update {
            routes.insert(destination, RouteInfo {
                next_hop,
                hops,
                latency,
                updated_at: Instant::now(),
            });
        }
    }
    
    /// Remove stale routes
    pub fn cleanup_stale(&self) -> Vec<PeerId> {
        let now = Instant::now();
        let mut routes = self.routes.write();
        
        let stale: Vec<_> = routes
            .iter()
            .filter(|(_, route)| now.duration_since(route.updated_at) > self.route_ttl)
            .map(|(id, _)| *id)
            .collect();
        
        for id in &stale {
            routes.remove(id);
        }
        
        stale
    }
    
    /// Get all known routes
    pub fn all_routes(&self) -> Vec<(PeerId, RouteInfo)> {
        self.routes.read().iter().map(|(k, v)| (*k, v.clone())).collect()
    }
    
    /// Find peers with specific capability
    pub fn find_by_capability(&self, capability: &str) -> Vec<PeerInfo> {
        self.registry
            .alive_peers()
            .into_iter()
            .filter(|p| p.capabilities.contains(capability))
            .collect()
    }
}

// ============================================================================
// Service Discovery
// ============================================================================

/// Service discovery interface
pub trait ServiceDiscovery: Send + Sync {
    /// Register this node
    fn register(&self, service: &str, addr: SocketAddr) -> Result<(), NetworkError>;
    
    /// Deregister this node
    fn deregister(&self, service: &str) -> Result<(), NetworkError>;
    
    /// Discover peers for a service
    fn discover(&self, service: &str) -> Result<Vec<SocketAddr>, NetworkError>;
}

/// Static service discovery (from configuration)
pub struct StaticDiscovery {
    services: RwLock<HashMap<String, Vec<SocketAddr>>>,
}

impl StaticDiscovery {
    pub fn new() -> Self {
        Self {
            services: RwLock::new(HashMap::new()),
        }
    }
    
    /// Add static endpoints
    pub fn add_endpoints(&self, service: &str, addrs: Vec<SocketAddr>) {
        self.services.write()
            .entry(service.to_string())
            .or_insert_with(Vec::new)
            .extend(addrs);
    }
}

impl Default for StaticDiscovery {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceDiscovery for StaticDiscovery {
    fn register(&self, service: &str, addr: SocketAddr) -> Result<(), NetworkError> {
        self.services.write()
            .entry(service.to_string())
            .or_insert_with(Vec::new)
            .push(addr);
        Ok(())
    }
    
    fn deregister(&self, service: &str) -> Result<(), NetworkError> {
        self.services.write().remove(service);
        Ok(())
    }
    
    fn discover(&self, service: &str) -> Result<Vec<SocketAddr>, NetworkError> {
        self.services.read()
            .get(service)
            .cloned()
            .ok_or_else(|| NetworkError::RouteNotFound(service.into()))
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_peer_registry() {
        let registry = PeerRegistry::new(100);
        
        let addr: SocketAddr = "127.0.0.1:8080".parse().unwrap();
        let mut info = PeerInfo::new(PeerId(1), addr);
        info.mark_alive();
        
        assert!(registry.register(info));
        assert!(registry.get(PeerId(1)).is_some());
        assert!(registry.get_by_addr(&addr).is_some());
        
        let alive = registry.alive_peers();
        assert_eq!(alive.len(), 1);
    }
    
    #[test]
    fn test_router() {
        let registry = Arc::new(PeerRegistry::new(100));
        let router = Router::new(PeerId(0), registry.clone());
        
        // Add a reachable peer
        let mut info = PeerInfo::new(PeerId(1), "127.0.0.1:8080".parse().unwrap());
        info.mark_alive();
        registry.register(info);
        
        // Direct route
        assert_eq!(router.next_hop(PeerId(1)), Some(PeerId(1)));
        
        // Add indirect route
        router.update_route(PeerId(2), PeerId(1), 1, Duration::from_millis(10));
        assert_eq!(router.next_hop(PeerId(2)), Some(PeerId(1)));
    }
    
    #[test]
    fn test_gossip() {
        let registry = Arc::new(PeerRegistry::new(100));
        let gossip = GossipProtocol::new(PeerId(0), registry.clone());
        
        let addr: SocketAddr = "127.0.0.1:9000".parse().unwrap();
        gossip.broadcast(GossipKind::Alive { peer: PeerId(1), addr });
        
        let messages = gossip.drain_gossip(10);
        assert_eq!(messages.len(), 1);
    }
    
    #[test]
    fn test_static_discovery() {
        let discovery = StaticDiscovery::new();
        let addr: SocketAddr = "127.0.0.1:8080".parse().unwrap();
        
        discovery.register("my-service", addr).unwrap();
        
        let found = discovery.discover("my-service").unwrap();
        assert_eq!(found.len(), 1);
        assert_eq!(found[0], addr);
    }
}
