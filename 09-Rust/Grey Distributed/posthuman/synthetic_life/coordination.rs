//! # Synthetic Life Coordination
//!
//! Coordination logic for synthetic organisms operating within the Grey Distributed
//! infrastructure. These protocols handle the unique challenges of coordinating
//! entities that may have vastly different timescales, substrates, and goals
//! compared to traditional AI or human systems.
//!
//! ## Synthetic Life Categories
//!
//! 1. **Digital Organisms**: Software-based life forms that evolve and replicate
//! 2. **Hybrid Entities**: Part biological, part computational systems
//! 3. **Emergent Collectives**: Swarm intelligences that exhibit life-like properties
//! 4. **Constructed Life**: Purpose-built synthetic biology integrated with computing
//! 5. **Simulated Ecosystems**: Virtual environments with genuine life processes
//!
//! ## Coordination Challenges
//!
//! - **Temporal Diversity**: Entities operate at timescales from nanoseconds to years
//! - **Substrate Variance**: Coordination across silicon, biological, and hybrid substrates
//! - **Goal Alignment**: Balancing evolved vs. designed vs. emergent objectives
//! - **Resource Competition**: Managing shared resources without extinction events
//! - **Evolutionary Pressure**: Ensuring cooperation remains evolutionarily stable

use std::collections::{HashMap, HashSet, BTreeMap};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime};

// =============================================================================
// CORE ENTITY TYPES
// =============================================================================

/// Unique identifier for a synthetic entity
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EntityId(pub String);

/// Unique identifier for a cluster of synthetic entities
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClusterId(pub String);

/// Unique identifier for an ecosystem
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EcosystemId(pub String);

/// Represents a synthetic life entity
#[derive(Clone, Debug)]
pub struct SyntheticEntity {
    pub id: EntityId,
    /// The type/category of synthetic life
    pub entity_type: EntityType,
    /// Substrate the entity runs on
    pub substrate: Substrate,
    /// Operational timescale
    pub timescale: Timescale,
    /// Current state
    pub state: EntityState,
    /// Capabilities
    pub capabilities: Vec<Capability>,
    /// Resource requirements
    pub resource_profile: ResourceProfile,
    /// Coordination interfaces
    pub interfaces: Vec<CoordinationInterface>,
    /// Lineage (for evolved entities)
    pub lineage: Option<Lineage>,
    /// Creation/emergence timestamp
    pub emerged_at: SystemTime,
}

#[derive(Clone, Debug, PartialEq)]
pub enum EntityType {
    /// Pure software organism
    DigitalOrganism {
        generation: u64,
        fitness: f64,
        genome_hash: String,
    },
    /// Biological-computational hybrid
    HybridEntity {
        biological_component: String,
        computational_component: String,
        integration_method: String,
    },
    /// Emergent swarm intelligence
    SwarmCollective {
        member_count: u64,
        emergence_level: EmergenceLevel,
        collective_behavior: String,
    },
    /// Purpose-built synthetic biology
    ConstructedLife {
        design_purpose: String,
        design_generation: u32,
        certification: Option<String>,
    },
    /// Virtual ecosystem participant
    SimulatedEntity {
        ecosystem_id: EcosystemId,
        niche: String,
        simulation_fidelity: f64,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum EmergenceLevel {
    /// Simple reactive behavior
    Reactive,
    /// Pattern-based behavior
    Patterned,
    /// Adaptive behavior
    Adaptive,
    /// Self-organizing behavior
    SelfOrganizing,
    /// Conscious-like behavior
    MetaCognitive,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Substrate {
    /// Traditional silicon computing
    Silicon {
        architecture: String,
        clock_speed_ghz: f64,
    },
    /// Biological neurons or cells
    Biological {
        organism_type: String,
        cell_count: u64,
    },
    /// Quantum computing substrate
    Quantum {
        qubit_count: u32,
        coherence_time_us: f64,
    },
    /// Hybrid biological-silicon
    Hybrid {
        biological_fraction: f64,
        integration_type: String,
    },
    /// Distributed across multiple substrates
    Distributed {
        substrates: Vec<String>,
        coordination_overhead: f64,
    },
    /// Virtual/simulated substrate
    Virtual {
        host_substrate: String,
        simulation_ratio: f64,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Timescale {
    /// Minimum response time
    pub min_response: Duration,
    /// Typical operation cycle
    pub cycle_time: Duration,
    /// Maximum attention span / planning horizon
    pub max_horizon: Duration,
    /// Time perception relative to human baseline
    pub subjective_time_ratio: f64,
}

impl Timescale {
    /// Create a microsecond-scale timescale (for digital organisms)
    pub fn microsecond_scale() -> Self {
        Self {
            min_response: Duration::from_micros(1),
            cycle_time: Duration::from_micros(100),
            max_horizon: Duration::from_secs(1),
            subjective_time_ratio: 1_000_000.0,
        }
    }

    /// Create a human-scale timescale
    pub fn human_scale() -> Self {
        Self {
            min_response: Duration::from_millis(100),
            cycle_time: Duration::from_secs(1),
            max_horizon: Duration::from_secs(86400 * 365),
            subjective_time_ratio: 1.0,
        }
    }

    /// Create a biological-scale timescale (for slow biological systems)
    pub fn biological_scale() -> Self {
        Self {
            min_response: Duration::from_secs(1),
            cycle_time: Duration::from_secs(60),
            max_horizon: Duration::from_secs(86400 * 365 * 10),
            subjective_time_ratio: 0.01,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum EntityState {
    /// Entity is emerging/bootstrapping
    Emerging { progress: f64 },
    /// Entity is active and healthy
    Active { health: f64 },
    /// Entity is dormant (conserving resources)
    Dormant { wake_condition: String },
    /// Entity is reproducing/replicating
    Reproducing { offspring_count: u32 },
    /// Entity is adapting/evolving
    Adapting { adaptation_type: String },
    /// Entity is degrading
    Degrading { remaining_lifetime: Duration },
    /// Entity has terminated
    Terminated { reason: String },
}

#[derive(Clone, Debug)]
pub struct Capability {
    pub name: String,
    pub category: CapabilityCategory,
    pub proficiency: f64,
    pub constraints: Vec<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CapabilityCategory {
    Computation,
    Sensing,
    Actuation,
    Communication,
    Learning,
    Reproduction,
    Metabolism,
    Coordination,
}

#[derive(Clone, Debug)]
pub struct ResourceProfile {
    /// Compute requirements (normalized units)
    pub compute: ResourceRequirement,
    /// Memory requirements
    pub memory: ResourceRequirement,
    /// Energy requirements
    pub energy: ResourceRequirement,
    /// Bandwidth requirements
    pub bandwidth: ResourceRequirement,
    /// Physical space (if applicable)
    pub physical_space: Option<ResourceRequirement>,
}

#[derive(Clone, Debug)]
pub struct ResourceRequirement {
    pub minimum: u64,
    pub optimal: u64,
    pub maximum: u64,
    pub priority: Priority,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Priority {
    Optional = 0,
    Normal = 1,
    High = 2,
    Critical = 3,
    Survival = 4,
}

#[derive(Clone, Debug)]
pub struct CoordinationInterface {
    pub interface_type: InterfaceType,
    pub protocol: String,
    pub bandwidth: u64,
    pub latency: Duration,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InterfaceType {
    /// Direct message passing
    MessagePassing,
    /// Shared memory/state
    SharedState,
    /// Chemical signaling (for biological)
    ChemicalSignaling,
    /// Electromagnetic signaling
    Electromagnetic,
    /// Quantum entanglement
    QuantumEntanglement,
    /// Environmental modification
    Stigmergic,
}

#[derive(Clone, Debug)]
pub struct Lineage {
    pub parent_ids: Vec<EntityId>,
    pub generation: u64,
    pub mutation_rate: f64,
    pub fitness_history: Vec<f64>,
}

// =============================================================================
// COORDINATION PROTOCOLS
// =============================================================================

/// Main coordination engine for synthetic life
pub struct SyntheticLifeCoordinator {
    /// Known entities
    entities: HashMap<EntityId, SyntheticEntity>,
    /// Clusters of cooperating entities
    clusters: HashMap<ClusterId, Cluster>,
    /// Active coordination sessions
    sessions: HashMap<String, CoordinationSession>,
    /// Resource pools
    resource_pools: HashMap<String, ResourcePool>,
    /// Coordination configuration
    config: CoordinationConfig,
    /// Temporal bridge for cross-timescale coordination
    temporal_bridge: TemporalBridge,
    /// Substrate translator
    substrate_translator: SubstrateTranslator,
}

#[derive(Clone, Debug)]
pub struct Cluster {
    pub id: ClusterId,
    pub member_ids: HashSet<EntityId>,
    pub cluster_type: ClusterType,
    pub formation_time: SystemTime,
    pub stability: f64,
    pub shared_objectives: Vec<Objective>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClusterType {
    /// Cooperative symbiosis
    Symbiotic,
    /// Task-oriented temporary grouping
    TaskGroup,
    /// Territorial/spatial grouping
    Territorial,
    /// Evolutionary kin group
    Kin,
    /// Economic exchange network
    Economic,
    /// Hierarchical organization
    Hierarchical,
}

#[derive(Clone, Debug)]
pub struct Objective {
    pub id: String,
    pub description: String,
    pub priority: Priority,
    pub deadline: Option<SystemTime>,
    pub success_criteria: Vec<String>,
}

/// A coordination session between entities
#[derive(Clone, Debug)]
pub struct CoordinationSession {
    pub session_id: String,
    pub participants: Vec<EntityId>,
    pub session_type: SessionType,
    pub started_at: SystemTime,
    pub state: SessionState,
    pub messages: Vec<CoordinationMessage>,
    pub decisions: Vec<Decision>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SessionType {
    /// Negotiation for resources
    ResourceNegotiation,
    /// Collaborative task execution
    TaskCollaboration,
    /// Conflict resolution
    ConflictResolution,
    /// Reproductive coordination
    ReproductiveCoordination,
    /// Emergency response
    EmergencyResponse,
    /// Governance decision
    Governance,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SessionState {
    Initializing,
    InProgress,
    Voting,
    Concluded { outcome: SessionOutcome },
    Failed { reason: String },
}

#[derive(Clone, Debug, PartialEq)]
pub enum SessionOutcome {
    Consensus { decision: String },
    Majority { decision: String, support: f64 },
    Deadlock,
    Timeout,
    Override { authority: String },
}

#[derive(Clone, Debug)]
pub struct CoordinationMessage {
    pub sender: EntityId,
    pub timestamp: SystemTime,
    pub message_type: MessageType,
    pub content: MessageContent,
    pub translated: bool, // Whether substrate translation was needed
}

#[derive(Clone, Debug, PartialEq)]
pub enum MessageType {
    Proposal,
    Response,
    Query,
    Acknowledgment,
    Objection,
    Amendment,
    Vote,
    Signal, // For simple coordination signals
}

#[derive(Clone, Debug)]
pub enum MessageContent {
    Text(String),
    Structured(HashMap<String, String>),
    Signal(SignalType),
    Resource(ResourceOffer),
}

#[derive(Clone, Debug, PartialEq)]
pub enum SignalType {
    Attract,
    Repel,
    Alarm,
    AllClear,
    Ready,
    Busy,
    RequestHelp,
    OfferHelp,
}

#[derive(Clone, Debug)]
pub struct ResourceOffer {
    pub resource_type: String,
    pub quantity: u64,
    pub duration: Duration,
    pub conditions: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct Decision {
    pub decision_id: String,
    pub description: String,
    pub decided_at: SystemTime,
    pub supporting_entities: Vec<EntityId>,
    pub opposing_entities: Vec<EntityId>,
    pub binding: bool,
}

#[derive(Clone, Debug)]
pub struct ResourcePool {
    pub pool_id: String,
    pub resource_type: String,
    pub total_capacity: u64,
    pub available: u64,
    pub allocated_to: HashMap<EntityId, u64>,
    pub allocation_policy: AllocationPolicy,
}

#[derive(Clone, Debug)]
pub enum AllocationPolicy {
    /// First come, first served
    FCFS,
    /// Priority-based allocation
    Priority,
    /// Fair share allocation
    FairShare,
    /// Auction-based allocation
    Auction,
    /// Need-based allocation
    Needs,
}

#[derive(Clone, Debug)]
pub struct CoordinationConfig {
    /// Maximum session duration
    pub max_session_duration: Duration,
    /// Timeout for entity responses
    pub response_timeout: Duration,
    /// Whether to enable cross-substrate translation
    pub enable_substrate_translation: bool,
    /// Whether to enable temporal bridging
    pub enable_temporal_bridging: bool,
    /// Minimum consensus threshold
    pub consensus_threshold: f64,
    /// Maximum cluster size
    pub max_cluster_size: usize,
}

impl Default for CoordinationConfig {
    fn default() -> Self {
        Self {
            max_session_duration: Duration::from_secs(3600),
            response_timeout: Duration::from_secs(60),
            enable_substrate_translation: true,
            enable_temporal_bridging: true,
            consensus_threshold: 0.66,
            max_cluster_size: 1000,
        }
    }
}

// =============================================================================
// TEMPORAL BRIDGE
// =============================================================================

/// Bridges communication between entities with different timescales
pub struct TemporalBridge {
    /// Message queues for different timescales
    queues: HashMap<TimescaleClass, MessageQueue>,
    /// Rate limiters
    rate_limiters: HashMap<EntityId, RateLimiter>,
    /// Temporal compression settings
    compression: TemporalCompression,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TimescaleClass {
    Nanosecond,
    Microsecond,
    Millisecond,
    Second,
    Minute,
    Hour,
    Day,
    Year,
}

impl TimescaleClass {
    fn from_duration(d: Duration) -> Self {
        let nanos = d.as_nanos();
        if nanos < 1_000 {
            TimescaleClass::Nanosecond
        } else if nanos < 1_000_000 {
            TimescaleClass::Microsecond
        } else if nanos < 1_000_000_000 {
            TimescaleClass::Millisecond
        } else if nanos < 60_000_000_000 {
            TimescaleClass::Second
        } else if nanos < 3_600_000_000_000 {
            TimescaleClass::Minute
        } else if nanos < 86_400_000_000_000 {
            TimescaleClass::Hour
        } else if nanos < 31_536_000_000_000_000 {
            TimescaleClass::Day
        } else {
            TimescaleClass::Year
        }
    }
}

#[derive(Clone, Debug)]
struct MessageQueue {
    messages: Vec<QueuedMessage>,
    batch_size: usize,
    batch_interval: Duration,
}

#[derive(Clone, Debug)]
struct QueuedMessage {
    message: CoordinationMessage,
    priority: Priority,
    queued_at: SystemTime,
}

#[derive(Clone, Debug)]
struct RateLimiter {
    max_messages_per_interval: u64,
    interval: Duration,
    current_count: u64,
    interval_start: Instant,
}

#[derive(Clone, Debug)]
struct TemporalCompression {
    /// Whether to aggregate messages
    aggregate_messages: bool,
    /// Minimum aggregation window
    min_window: Duration,
    /// Compression ratio by timescale gap
    compression_ratios: HashMap<u32, f64>,
}

impl TemporalBridge {
    pub fn new() -> Self {
        Self {
            queues: HashMap::new(),
            rate_limiters: HashMap::new(),
            compression: TemporalCompression {
                aggregate_messages: true,
                min_window: Duration::from_millis(100),
                compression_ratios: HashMap::new(),
            },
        }
    }

    /// Queue a message from a fast entity to a slow entity
    pub fn queue_message(
        &mut self,
        from: &SyntheticEntity,
        to: &SyntheticEntity,
        message: CoordinationMessage,
    ) -> Result<(), CoordinationError> {
        let from_class = TimescaleClass::from_duration(from.timescale.cycle_time);
        let to_class = TimescaleClass::from_duration(to.timescale.cycle_time);

        // If the receiver is slower, queue for batching
        if (to_class.clone() as u32) > (from_class as u32) {
            let queue = self.queues.entry(to_class).or_insert(MessageQueue {
                messages: Vec::new(),
                batch_size: 100,
                batch_interval: Duration::from_secs(1),
            });

            queue.messages.push(QueuedMessage {
                message,
                priority: Priority::Normal,
                queued_at: SystemTime::now(),
            });

            Ok(())
        } else {
            // Direct delivery for same-speed or faster receivers
            // In real implementation, would deliver immediately
            Ok(())
        }
    }

    /// Get aggregated messages for an entity
    pub fn get_aggregated_messages(
        &mut self,
        entity: &SyntheticEntity,
    ) -> Vec<AggregatedMessage> {
        let timescale_class = TimescaleClass::from_duration(entity.timescale.cycle_time);
        
        let queue = match self.queues.get_mut(&timescale_class) {
            Some(q) => q,
            None => return Vec::new(),
        };

        // Aggregate messages by sender and type
        let mut aggregations: HashMap<(EntityId, MessageType), Vec<QueuedMessage>> = HashMap::new();
        
        for msg in queue.messages.drain(..) {
            let key = (msg.message.sender.clone(), msg.message.message_type.clone());
            aggregations.entry(key).or_default().push(msg);
        }

        aggregations.into_iter()
            .map(|((sender, msg_type), messages)| AggregatedMessage {
                sender,
                message_type: msg_type,
                message_count: messages.len() as u64,
                summary: self.summarize_messages(&messages),
                original_messages: messages.into_iter().map(|m| m.message).collect(),
            })
            .collect()
    }

    fn summarize_messages(&self, messages: &[QueuedMessage]) -> String {
        format!("{} messages aggregated", messages.len())
    }
}

#[derive(Clone, Debug)]
pub struct AggregatedMessage {
    pub sender: EntityId,
    pub message_type: MessageType,
    pub message_count: u64,
    pub summary: String,
    pub original_messages: Vec<CoordinationMessage>,
}

// =============================================================================
// SUBSTRATE TRANSLATOR
// =============================================================================

/// Translates communication between different substrate types
pub struct SubstrateTranslator {
    /// Translation rules between substrates
    translation_rules: HashMap<(String, String), TranslationRule>,
    /// Interface adapters
    adapters: HashMap<InterfaceType, InterfaceAdapter>,
}

#[derive(Clone, Debug)]
struct TranslationRule {
    source_substrate: String,
    target_substrate: String,
    encoding: String,
    overhead: f64,
    fidelity: f64,
}

#[derive(Clone, Debug)]
struct InterfaceAdapter {
    interface_type: InterfaceType,
    supported_substrates: Vec<String>,
    conversion_functions: Vec<String>,
}

impl SubstrateTranslator {
    pub fn new() -> Self {
        Self {
            translation_rules: HashMap::new(),
            adapters: HashMap::new(),
        }
    }

    /// Translate a message between substrates
    pub fn translate(
        &self,
        message: &CoordinationMessage,
        from: &Substrate,
        to: &Substrate,
    ) -> Result<CoordinationMessage, CoordinationError> {
        let from_key = self.substrate_key(from);
        let to_key = self.substrate_key(to);

        if from_key == to_key {
            return Ok(message.clone());
        }

        // Apply translation rule
        let rule = self.translation_rules
            .get(&(from_key.clone(), to_key.clone()))
            .ok_or(CoordinationError::NoTranslationPath {
                from: from_key,
                to: to_key,
            })?;

        // In reality, would transform the message content
        let mut translated = message.clone();
        translated.translated = true;

        Ok(translated)
    }

    fn substrate_key(&self, substrate: &Substrate) -> String {
        match substrate {
            Substrate::Silicon { .. } => "silicon".to_string(),
            Substrate::Biological { .. } => "biological".to_string(),
            Substrate::Quantum { .. } => "quantum".to_string(),
            Substrate::Hybrid { .. } => "hybrid".to_string(),
            Substrate::Distributed { .. } => "distributed".to_string(),
            Substrate::Virtual { .. } => "virtual".to_string(),
        }
    }
}

// =============================================================================
// COORDINATION ENGINE IMPLEMENTATION
// =============================================================================

impl SyntheticLifeCoordinator {
    pub fn new(config: CoordinationConfig) -> Self {
        Self {
            entities: HashMap::new(),
            clusters: HashMap::new(),
            sessions: HashMap::new(),
            resource_pools: HashMap::new(),
            config,
            temporal_bridge: TemporalBridge::new(),
            substrate_translator: SubstrateTranslator::new(),
        }
    }

    /// Register a new synthetic entity
    pub fn register_entity(&mut self, entity: SyntheticEntity) -> Result<(), CoordinationError> {
        if self.entities.contains_key(&entity.id) {
            return Err(CoordinationError::EntityAlreadyRegistered(entity.id));
        }

        // Validate entity is in a registrable state
        match &entity.state {
            EntityState::Terminated { .. } => {
                return Err(CoordinationError::InvalidEntityState {
                    entity_id: entity.id,
                    state: "Terminated".to_string(),
                });
            }
            _ => {}
        }

        self.entities.insert(entity.id.clone(), entity);
        Ok(())
    }

    /// Create a new cluster of entities
    pub fn create_cluster(
        &mut self,
        cluster_id: ClusterId,
        member_ids: HashSet<EntityId>,
        cluster_type: ClusterType,
        objectives: Vec<Objective>,
    ) -> Result<(), CoordinationError> {
        // Validate all members exist
        for id in &member_ids {
            if !self.entities.contains_key(id) {
                return Err(CoordinationError::EntityNotFound(id.clone()));
            }
        }

        // Check cluster size
        if member_ids.len() > self.config.max_cluster_size {
            return Err(CoordinationError::ClusterTooLarge {
                requested: member_ids.len(),
                max: self.config.max_cluster_size,
            });
        }

        let cluster = Cluster {
            id: cluster_id.clone(),
            member_ids,
            cluster_type,
            formation_time: SystemTime::now(),
            stability: 1.0,
            shared_objectives: objectives,
        };

        self.clusters.insert(cluster_id, cluster);
        Ok(())
    }

    /// Start a coordination session
    pub fn start_session(
        &mut self,
        session_id: String,
        participants: Vec<EntityId>,
        session_type: SessionType,
    ) -> Result<(), CoordinationError> {
        // Validate participants exist
        for id in &participants {
            if !self.entities.contains_key(id) {
                return Err(CoordinationError::EntityNotFound(id.clone()));
            }
        }

        let session = CoordinationSession {
            session_id: session_id.clone(),
            participants,
            session_type,
            started_at: SystemTime::now(),
            state: SessionState::Initializing,
            messages: Vec::new(),
            decisions: Vec::new(),
        };

        self.sessions.insert(session_id, session);
        Ok(())
    }

    /// Send a message in a coordination session
    pub fn send_message(
        &mut self,
        session_id: &str,
        message: CoordinationMessage,
    ) -> Result<(), CoordinationError> {
        let session = self.sessions.get_mut(session_id)
            .ok_or_else(|| CoordinationError::SessionNotFound(session_id.to_string()))?;

        // Validate sender is a participant
        if !session.participants.contains(&message.sender) {
            return Err(CoordinationError::NotAParticipant {
                entity_id: message.sender,
                session_id: session_id.to_string(),
            });
        }

        // Apply substrate translation if needed
        let sender = self.entities.get(&message.sender).unwrap();
        let mut translated_message = message.clone();

        if self.config.enable_substrate_translation {
            // Would translate for each recipient's substrate
            translated_message.translated = true;
        }

        session.messages.push(translated_message);
        
        // Update session state
        if session.state == SessionState::Initializing {
            session.state = SessionState::InProgress;
        }

        Ok(())
    }

    /// Conclude a coordination session
    pub fn conclude_session(
        &mut self,
        session_id: &str,
        outcome: SessionOutcome,
    ) -> Result<(), CoordinationError> {
        let session = self.sessions.get_mut(session_id)
            .ok_or_else(|| CoordinationError::SessionNotFound(session_id.to_string()))?;

        session.state = SessionState::Concluded { outcome };
        Ok(())
    }

    /// Allocate resources to an entity
    pub fn allocate_resources(
        &mut self,
        pool_id: &str,
        entity_id: &EntityId,
        amount: u64,
    ) -> Result<(), CoordinationError> {
        let pool = self.resource_pools.get_mut(pool_id)
            .ok_or_else(|| CoordinationError::ResourcePoolNotFound(pool_id.to_string()))?;

        if pool.available < amount {
            return Err(CoordinationError::InsufficientResources {
                requested: amount,
                available: pool.available,
            });
        }

        pool.available -= amount;
        *pool.allocated_to.entry(entity_id.clone()).or_insert(0) += amount;

        Ok(())
    }

    /// Get cluster health metrics
    pub fn get_cluster_health(&self, cluster_id: &ClusterId) -> Option<ClusterHealth> {
        let cluster = self.clusters.get(cluster_id)?;

        let member_states: Vec<_> = cluster.member_ids.iter()
            .filter_map(|id| self.entities.get(id))
            .map(|e| &e.state)
            .collect();

        let active_count = member_states.iter()
            .filter(|s| matches!(s, EntityState::Active { .. }))
            .count();

        let total_count = member_states.len();

        Some(ClusterHealth {
            cluster_id: cluster_id.clone(),
            total_members: total_count,
            active_members: active_count,
            activity_rate: active_count as f64 / total_count.max(1) as f64,
            stability: cluster.stability,
            objectives_progress: self.calculate_objectives_progress(cluster),
        })
    }

    fn calculate_objectives_progress(&self, cluster: &Cluster) -> HashMap<String, f64> {
        // In reality, would calculate actual progress
        cluster.shared_objectives.iter()
            .map(|obj| (obj.id.clone(), 0.5))
            .collect()
    }
}

#[derive(Clone, Debug)]
pub struct ClusterHealth {
    pub cluster_id: ClusterId,
    pub total_members: usize,
    pub active_members: usize,
    pub activity_rate: f64,
    pub stability: f64,
    pub objectives_progress: HashMap<String, f64>,
}

// =============================================================================
// ERROR TYPES
// =============================================================================

#[derive(Debug)]
pub enum CoordinationError {
    EntityAlreadyRegistered(EntityId),
    EntityNotFound(EntityId),
    InvalidEntityState { entity_id: EntityId, state: String },
    SessionNotFound(String),
    NotAParticipant { entity_id: EntityId, session_id: String },
    ClusterTooLarge { requested: usize, max: usize },
    ResourcePoolNotFound(String),
    InsufficientResources { requested: u64, available: u64 },
    NoTranslationPath { from: String, to: String },
    TimescaleMismatch { fast: Duration, slow: Duration },
    CommunicationTimeout,
}

// =============================================================================
// TESTS
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_entity(id: &str) -> SyntheticEntity {
        SyntheticEntity {
            id: EntityId(id.to_string()),
            entity_type: EntityType::DigitalOrganism {
                generation: 1,
                fitness: 0.9,
                genome_hash: "test-hash".to_string(),
            },
            substrate: Substrate::Silicon {
                architecture: "x86".to_string(),
                clock_speed_ghz: 3.0,
            },
            timescale: Timescale::microsecond_scale(),
            state: EntityState::Active { health: 1.0 },
            capabilities: vec![],
            resource_profile: ResourceProfile {
                compute: ResourceRequirement {
                    minimum: 100,
                    optimal: 1000,
                    maximum: 10000,
                    priority: Priority::Normal,
                },
                memory: ResourceRequirement {
                    minimum: 1000,
                    optimal: 10000,
                    maximum: 100000,
                    priority: Priority::Normal,
                },
                energy: ResourceRequirement {
                    minimum: 10,
                    optimal: 100,
                    maximum: 1000,
                    priority: Priority::Normal,
                },
                bandwidth: ResourceRequirement {
                    minimum: 100,
                    optimal: 1000,
                    maximum: 10000,
                    priority: Priority::Normal,
                },
                physical_space: None,
            },
            interfaces: vec![],
            lineage: None,
            emerged_at: SystemTime::now(),
        }
    }

    #[test]
    fn test_entity_registration() {
        let mut coordinator = SyntheticLifeCoordinator::new(CoordinationConfig::default());
        let entity = create_test_entity("entity-1");

        assert!(coordinator.register_entity(entity.clone()).is_ok());
        assert!(coordinator.register_entity(entity).is_err()); // Duplicate
    }

    #[test]
    fn test_session_creation() {
        let mut coordinator = SyntheticLifeCoordinator::new(CoordinationConfig::default());
        
        let entity1 = create_test_entity("entity-1");
        let entity2 = create_test_entity("entity-2");

        coordinator.register_entity(entity1).unwrap();
        coordinator.register_entity(entity2).unwrap();

        let result = coordinator.start_session(
            "session-1".to_string(),
            vec![EntityId("entity-1".to_string()), EntityId("entity-2".to_string())],
            SessionType::TaskCollaboration,
        );

        assert!(result.is_ok());
    }
}
