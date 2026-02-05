//! # Core State Machine
//!
//! Implements event-sourced state with deterministic reducers.
//!
//! ## Design Philosophy
//!
//! 1. **Event Sourcing**: State is the result of applying a sequence of events.
//!    Never mutate state directly; always go through events.
//!
//! 2. **Deterministic Reducers**: Given the same events, always produce the same
//!    state. This enables replay, debugging, and distributed consistency.
//!
//! 3. **Immutable History**: Events are append-only. Never modify or delete events.
//!    This provides auditability and enables time-travel debugging.
//!
//! ## Key Invariants
//!
//! 1. State version always increases with each applied event
//! 2. Events are applied in order by version number
//! 3. Reducers are pure functions (no side effects)

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Instant;

use parking_lot::{Mutex, RwLock};

// ============================================================================
// State Machine Trait
// ============================================================================

/// Core state machine interface
///
/// Implementations define:
/// - Initial state
/// - Event application logic
/// - State queries
pub trait StateMachine: Send + Sync {
    /// Event type for this state machine
    type Event: Clone + Send + Sync;

    /// Create initial state
    fn initial_state() -> Self
    where
        Self: Sized;

    /// Apply an event to produce new state
    ///
    /// # Determinism Requirement
    /// This method MUST be deterministic. Given the same state and event,
    /// it must always produce the same new state. No randomness, no time,
    /// no external I/O.
    fn apply(&mut self, event: &Self::Event) -> Result<(), super::StateError>;

    /// Get current state version
    fn version(&self) -> u64;

    /// Serialize state for snapshotting
    fn serialize(&self) -> Result<Vec<u8>, super::StateError>;

    /// Deserialize state from snapshot
    fn deserialize(data: &[u8]) -> Result<Self, super::StateError>
    where
        Self: Sized;
}

// ============================================================================
// Event Store
// ============================================================================

/// Event with metadata
#[derive(Debug, Clone)]
pub struct StoredEvent<E> {
    /// Event sequence number (unique, monotonic)
    pub sequence: u64,
    /// Event version (state version after applying)
    pub version: u64,
    /// The event payload
    pub event: E,
    /// Timestamp when event was stored
    pub timestamp: u64,
    /// Optional correlation ID for tracing
    pub correlation_id: Option<String>,
    /// Aggregate/entity this event belongs to
    pub aggregate_id: String,
}

/// Event persistence layer
pub struct EventStore<E> {
    /// Events stored in memory (would be persistent in production)
    events: RwLock<Vec<StoredEvent<E>>>,
    /// Next sequence number
    next_sequence: AtomicU64,
    /// Index by aggregate ID for efficient queries
    by_aggregate: RwLock<HashMap<String, Vec<u64>>>,
}

impl<E: Clone + Send + Sync> EventStore<E> {
    pub fn new() -> Self {
        Self {
            events: RwLock::new(Vec::new()),
            next_sequence: AtomicU64::new(1),
            by_aggregate: RwLock::new(HashMap::new()),
        }
    }

    /// Store an event
    ///
    /// # Returns
    /// The sequence number assigned to this event
    pub fn store(
        &self,
        aggregate_id: String,
        event: E,
        version: u64,
        correlation_id: Option<String>,
    ) -> Result<u64, super::StateError> {
        let sequence = self.next_sequence.fetch_add(1, Ordering::SeqCst);

        let stored = StoredEvent {
            sequence,
            version,
            event,
            timestamp: Self::now(),
            correlation_id,
            aggregate_id: aggregate_id.clone(),
        };

        self.events.write().push(stored);

        // Update index
        self.by_aggregate
            .write()
            .entry(aggregate_id)
            .or_insert_with(Vec::new)
            .push(sequence);

        Ok(sequence)
    }

    /// Get events for an aggregate from a specific version
    pub fn get_events(
        &self,
        aggregate_id: &str,
        from_version: u64,
    ) -> Vec<StoredEvent<E>> {
        let events = self.events.read();
        let by_aggregate = self.by_aggregate.read();

        if let Some(sequences) = by_aggregate.get(aggregate_id) {
            sequences
                .iter()
                .filter_map(|&seq| {
                    events
                        .iter()
                        .find(|e| e.sequence == seq && e.version >= from_version)
                        .cloned()
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Get all events after a sequence number
    pub fn get_events_after(&self, sequence: u64) -> Vec<StoredEvent<E>> {
        self.events
            .read()
            .iter()
            .filter(|e| e.sequence > sequence)
            .cloned()
            .collect()
    }

    /// Get total event count
    pub fn count(&self) -> usize {
        self.events.read().len()
    }

    fn now() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_micros() as u64
    }
}

// ============================================================================
// State Manager
// ============================================================================

/// Manages state machine with event sourcing
pub struct StateManager<S: StateMachine> {
    /// Current state
    state: RwLock<S>,
    /// Event store
    events: Arc<EventStore<S::Event>>,
    /// Last applied sequence
    last_applied: AtomicU64,
    /// Aggregate ID for this state
    aggregate_id: String,
}

impl<S: StateMachine> StateManager<S> {
    pub fn new(aggregate_id: String, events: Arc<EventStore<S::Event>>) -> Self {
        Self {
            state: RwLock::new(S::initial_state()),
            events,
            last_applied: AtomicU64::new(0),
            aggregate_id,
        }
    }

    /// Apply an event to the state
    ///
    /// 1. Validate event (optional)
    /// 2. Store event in event store
    /// 3. Apply event to state
    pub fn apply_event(&self, event: S::Event) -> Result<u64, super::StateError> {
        let mut state = self.state.write();
        
        // Apply to state
        state.apply(&event)?;
        let version = state.version();
        drop(state);

        // Store event
        let sequence = self.events.store(
            self.aggregate_id.clone(),
            event,
            version,
            None,
        )?;

        self.last_applied.store(sequence, Ordering::SeqCst);

        Ok(sequence)
    }

    /// Replay events to rebuild state
    ///
    /// Used for:
    /// - Recovery from snapshot
    /// - Catching up after restart
    /// - Time-travel debugging
    pub fn replay(&self, from_version: u64) -> Result<(), super::StateError> {
        let events = self.events.get_events(&self.aggregate_id, from_version);
        let mut state = self.state.write();

        for stored in events {
            state.apply(&stored.event)?;
            self.last_applied.store(stored.sequence, Ordering::SeqCst);
        }

        Ok(())
    }

    /// Get read access to current state
    pub fn read<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&S) -> R,
    {
        f(&*self.state.read())
    }

    /// Get current version
    pub fn version(&self) -> u64 {
        self.state.read().version()
    }

    /// Create a snapshot of current state
    pub fn snapshot(&self) -> Result<super::Snapshot, super::StateError> {
        let state = self.state.read();
        let data = state.serialize()?;
        let version = state.version();
        drop(state);

        Ok(super::Snapshot {
            version,
            data,
            created_at: Self::now(),
            aggregate_id: self.aggregate_id.clone(),
        })
    }

    /// Restore from a snapshot
    pub fn restore(&self, snapshot: &super::Snapshot) -> Result<(), super::StateError> {
        let new_state = S::deserialize(&snapshot.data)?;
        *self.state.write() = new_state;
        Ok(())
    }

    fn now() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_micros() as u64
    }
}

// ============================================================================
// Projection
// ============================================================================

/// A read-optimized view derived from events
///
/// Projections are denormalized views of event-sourced state,
/// optimized for specific query patterns.
pub trait Projection: Send + Sync {
    /// Event type this projection processes
    type Event;

    /// Update projection with an event
    fn apply(&mut self, event: &Self::Event);

    /// Reset projection to initial state
    fn reset(&mut self);
}

/// Manages multiple projections
pub struct ProjectionManager<E> {
    projections: RwLock<Vec<Box<dyn Projection<Event = E>>>>,
    last_processed: AtomicU64,
}

impl<E: Clone> ProjectionManager<E> {
    pub fn new() -> Self {
        Self {
            projections: RwLock::new(Vec::new()),
            last_processed: AtomicU64::new(0),
        }
    }

    /// Register a projection
    pub fn register(&self, projection: Box<dyn Projection<Event = E>>) {
        self.projections.write().push(projection);
    }

    /// Process events through all projections
    pub fn process(&self, events: &[StoredEvent<E>]) {
        let mut projections = self.projections.write();

        for event in events {
            for projection in projections.iter_mut() {
                projection.apply(&event.event);
            }
            self.last_processed.store(event.sequence, Ordering::SeqCst);
        }
    }

    /// Rebuild all projections from events
    pub fn rebuild(&self, events: &[StoredEvent<E>]) {
        let mut projections = self.projections.write();

        // Reset all projections
        for projection in projections.iter_mut() {
            projection.reset();
        }

        // Replay events
        for event in events {
            for projection in projections.iter_mut() {
                projection.apply(&event.event);
            }
        }
    }
}

// ============================================================================
// Sample Implementation
// ============================================================================

/// Example: Key-Value store as a state machine
pub mod kv {
    use super::*;

    /// Key-value store events
    #[derive(Debug, Clone)]
    pub enum KvEvent {
        Put { key: String, value: Vec<u8> },
        Delete { key: String },
    }

    /// Key-value store state
    #[derive(Debug, Clone)]
    pub struct KvState {
        data: HashMap<String, Vec<u8>>,
        version: u64,
    }

    impl StateMachine for KvState {
        type Event = KvEvent;

        fn initial_state() -> Self {
            Self {
                data: HashMap::new(),
                version: 0,
            }
        }

        fn apply(&mut self, event: &KvEvent) -> Result<(), super::super::StateError> {
            match event {
                KvEvent::Put { key, value } => {
                    self.data.insert(key.clone(), value.clone());
                }
                KvEvent::Delete { key } => {
                    self.data.remove(key);
                }
            }
            self.version += 1;
            Ok(())
        }

        fn version(&self) -> u64 {
            self.version
        }

        fn serialize(&self) -> Result<Vec<u8>, super::super::StateError> {
            // TODO: Proper serialization
            Ok(Vec::new())
        }

        fn deserialize(_data: &[u8]) -> Result<Self, super::super::StateError> {
            Ok(Self::initial_state())
        }
    }

    impl KvState {
        pub fn get(&self, key: &str) -> Option<&Vec<u8>> {
            self.data.get(key)
        }

        pub fn keys(&self) -> impl Iterator<Item = &String> {
            self.data.keys()
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::kv::*;
    use super::*;

    #[test]
    fn test_event_store() {
        let store: EventStore<KvEvent> = EventStore::new();

        store
            .store(
                "test".to_string(),
                KvEvent::Put {
                    key: "foo".to_string(),
                    value: b"bar".to_vec(),
                },
                1,
                None,
            )
            .unwrap();

        assert_eq!(store.count(), 1);
    }

    #[test]
    fn test_state_machine() {
        let mut state = KvState::initial_state();

        state
            .apply(&KvEvent::Put {
                key: "foo".to_string(),
                value: b"bar".to_vec(),
            })
            .unwrap();

        assert_eq!(state.version(), 1);
        assert_eq!(state.get("foo"), Some(&b"bar".to_vec()));
    }

    #[test]
    fn test_state_manager() {
        let events = Arc::new(EventStore::new());
        let manager: StateManager<KvState> = StateManager::new("test".to_string(), events);

        manager
            .apply_event(KvEvent::Put {
                key: "foo".to_string(),
                value: b"bar".to_vec(),
            })
            .unwrap();

        let value = manager.read(|s| s.get("foo").cloned());
        assert_eq!(value, Some(b"bar".to_vec()));
    }
}
