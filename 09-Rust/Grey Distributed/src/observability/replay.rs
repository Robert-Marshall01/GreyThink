//! # Request Replay and Debugging
//!
//! Record and replay capabilities for distributed debugging.
//!
//! ## Philosophy
//!
//! Distributed systems are notoriously hard to debug. By recording
//! all inputs and allowing deterministic replay, we can:
//! - Reproduce production issues locally
//! - Step through execution in time
//! - Compare different code paths
//!
//! ## Recording Modes
//!
//! 1. **Full Recording**: All requests and responses
//! 2. **Sampled**: Record N% of requests
//! 3. **Triggered**: Start recording on specific conditions
//!
//! ## Replay Modes
//!
//! 1. **Real-time**: Replay at original speed
//! 2. **Fast-forward**: Skip delays
//! 3. **Step**: Manual advancement

use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use parking_lot::{Mutex, RwLock};

use super::tracing::TraceId;

// ============================================================================
// Recording Types
// ============================================================================

/// Unique recording identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RecordingId(u64);

impl RecordingId {
    pub fn new() -> Self {
        use std::sync::atomic::AtomicU64;
        static COUNTER: AtomicU64 = AtomicU64::new(1);
        Self(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for RecordingId {
    fn default() -> Self {
        Self::new()
    }
}

/// A recorded event in the system
#[derive(Debug, Clone)]
pub struct RecordedEvent {
    /// Event sequence number
    pub sequence: u64,
    
    /// When the event occurred
    pub timestamp: SystemTime,
    
    /// Event type
    pub event_type: EventType,
    
    /// Source node
    pub source_node: String,
    
    /// Target node (for messages)
    pub target_node: Option<String>,
    
    /// Trace context
    pub trace_id: Option<TraceId>,
    
    /// Event payload
    pub payload: Payload,
    
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub enum EventType {
    /// Incoming request
    RequestReceived,
    /// Outgoing response
    ResponseSent,
    /// Internal message sent
    MessageSent,
    /// Internal message received
    MessageReceived,
    /// State mutation
    StateMutation,
    /// Timer/timeout fired
    TimerFired,
    /// External IO (disk, network)
    ExternalIO,
    /// Custom event
    Custom(String),
}

#[derive(Debug, Clone)]
pub enum Payload {
    /// Binary data
    Bytes(Vec<u8>),
    /// JSON string
    Json(String),
    /// Structured fields
    Fields(HashMap<String, String>),
    /// Empty
    Empty,
}

// ============================================================================
// Recorder
// ============================================================================

/// Records system events for later replay
pub struct EventRecorder {
    /// Recording ID
    id: RecordingId,
    
    /// Recording state
    state: RwLock<RecorderState>,
    
    /// Recorded events
    events: Mutex<Vec<RecordedEvent>>,
    
    /// Event sequence counter
    sequence: AtomicU64,
    
    /// Configuration
    config: RecorderConfig,
    
    /// Metrics
    metrics: RecorderMetrics,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RecorderState {
    Idle,
    Recording,
    Paused,
    Finished,
}

#[derive(Debug, Clone)]
pub struct RecorderConfig {
    /// Maximum events to record
    pub max_events: usize,
    
    /// Maximum recording duration
    pub max_duration: Duration,
    
    /// Event types to record
    pub event_filter: Vec<EventType>,
    
    /// Sample rate (0.0 to 1.0)
    pub sample_rate: f64,
    
    /// Include payload in recording
    pub record_payloads: bool,
}

impl Default for RecorderConfig {
    fn default() -> Self {
        Self {
            max_events: 100_000,
            max_duration: Duration::from_secs(3600), // 1 hour
            event_filter: Vec::new(), // All events
            sample_rate: 1.0, // 100%
            record_payloads: true,
        }
    }
}

#[derive(Default)]
struct RecorderMetrics {
    events_recorded: AtomicU64,
    events_dropped: AtomicU64,
    events_filtered: AtomicU64,
}

impl EventRecorder {
    pub fn new(config: RecorderConfig) -> Self {
        Self {
            id: RecordingId::new(),
            state: RwLock::new(RecorderState::Idle),
            events: Mutex::new(Vec::new()),
            sequence: AtomicU64::new(0),
            config,
            metrics: RecorderMetrics::default(),
        }
    }
    
    /// Start recording
    pub fn start(&self) {
        *self.state.write() = RecorderState::Recording;
    }
    
    /// Pause recording
    pub fn pause(&self) {
        *self.state.write() = RecorderState::Paused;
    }
    
    /// Resume recording
    pub fn resume(&self) {
        *self.state.write() = RecorderState::Recording;
    }
    
    /// Stop recording
    pub fn stop(&self) {
        *self.state.write() = RecorderState::Finished;
    }
    
    /// Record an event
    pub fn record(&self, mut event: RecordedEvent) -> bool {
        // Check state
        if *self.state.read() != RecorderState::Recording {
            return false;
        }
        
        // Apply sampling
        if self.config.sample_rate < 1.0 {
            let sample: f64 = rand::random();
            if sample > self.config.sample_rate {
                self.metrics.events_filtered.fetch_add(1, Ordering::Relaxed);
                return false;
            }
        }
        
        // Apply event filter
        if !self.config.event_filter.is_empty() {
            let type_matches = self.config.event_filter.iter().any(|t| {
                std::mem::discriminant(t) == std::mem::discriminant(&event.event_type)
            });
            if !type_matches {
                self.metrics.events_filtered.fetch_add(1, Ordering::Relaxed);
                return false;
            }
        }
        
        let mut events = self.events.lock();
        
        // Check limits
        if events.len() >= self.config.max_events {
            self.metrics.events_dropped.fetch_add(1, Ordering::Relaxed);
            return false;
        }
        
        // Assign sequence number
        event.sequence = self.sequence.fetch_add(1, Ordering::SeqCst);
        
        // Strip payload if not recording
        if !self.config.record_payloads {
            event.payload = Payload::Empty;
        }
        
        events.push(event);
        self.metrics.events_recorded.fetch_add(1, Ordering::Relaxed);
        
        true
    }
    
    /// Get all recorded events
    pub fn get_events(&self) -> Vec<RecordedEvent> {
        self.events.lock().clone()
    }
    
    /// Get recording stats
    pub fn stats(&self) -> RecordingStats {
        RecordingStats {
            id: self.id,
            events_recorded: self.metrics.events_recorded.load(Ordering::Relaxed),
            events_dropped: self.metrics.events_dropped.load(Ordering::Relaxed),
            events_filtered: self.metrics.events_filtered.load(Ordering::Relaxed),
            state: *self.state.read(),
        }
    }
    
    /// Export recording to bytes
    ///
    /// # Why Custom Format
    /// We could use protobuf/flatbuffers, but a simple format
    /// is easier to debug and doesn't require code generation.
    pub fn export(&self) -> Vec<u8> {
        // Simple format: just JSON for now
        let events = self.events.lock();
        let json = format!(
            r#"{{"id":{},"events":{}}}"#,
            self.id.0,
            events.len(),
        );
        json.into_bytes()
    }
}

#[derive(Debug, Clone)]
pub struct RecordingStats {
    pub id: RecordingId,
    pub events_recorded: u64,
    pub events_dropped: u64,
    pub events_filtered: u64,
    state: RecorderState,
}

// ============================================================================
// Replay Engine
// ============================================================================

/// Replays recorded events
pub struct ReplayEngine {
    /// Events to replay
    events: VecDeque<RecordedEvent>,
    
    /// Current position
    position: AtomicU64,
    
    /// Replay state
    state: RwLock<ReplayState>,
    
    /// Configuration
    config: ReplayConfig,
    
    /// Event handlers
    handlers: RwLock<Vec<Box<dyn ReplayHandler + Send + Sync>>>,
    
    /// Time tracking
    replay_start: Mutex<Option<Instant>>,
    original_start: Mutex<Option<SystemTime>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReplayState {
    Ready,
    Playing,
    Paused,
    Finished,
}

#[derive(Debug, Clone)]
pub struct ReplayConfig {
    /// Replay speed (1.0 = real-time, 0.0 = instant)
    pub speed: f64,
    
    /// Start from this sequence number
    pub start_sequence: u64,
    
    /// Stop at this sequence number (0 = no limit)
    pub end_sequence: u64,
    
    /// Filter to specific event types
    pub event_filter: Vec<EventType>,
    
    /// Filter to specific trace
    pub trace_filter: Option<TraceId>,
}

impl Default for ReplayConfig {
    fn default() -> Self {
        Self {
            speed: 0.0, // Fast-forward
            start_sequence: 0,
            end_sequence: 0,
            event_filter: Vec::new(),
            trace_filter: None,
        }
    }
}

impl ReplayEngine {
    pub fn new(events: Vec<RecordedEvent>, config: ReplayConfig) -> Self {
        Self {
            events: VecDeque::from(events),
            position: AtomicU64::new(0),
            state: RwLock::new(ReplayState::Ready),
            config,
            handlers: RwLock::new(Vec::new()),
            replay_start: Mutex::new(None),
            original_start: Mutex::new(None),
        }
    }
    
    /// Add a replay handler
    pub fn add_handler<H: ReplayHandler + 'static>(&self, handler: H) {
        self.handlers.write().push(Box::new(handler));
    }
    
    /// Start replay
    pub fn start(&self) {
        *self.state.write() = ReplayState::Playing;
        *self.replay_start.lock() = Some(Instant::now());
        
        // Find original start time
        if let Some(first) = self.events.front() {
            *self.original_start.lock() = Some(first.timestamp);
        }
    }
    
    /// Pause replay
    pub fn pause(&self) {
        *self.state.write() = ReplayState::Paused;
    }
    
    /// Resume replay
    pub fn resume(&self) {
        *self.state.write() = ReplayState::Playing;
    }
    
    /// Step to next event
    ///
    /// Returns the event if available.
    /// This is useful for debugging - step through events one at a time.
    pub fn step(&self) -> Option<RecordedEvent> {
        let pos = self.position.load(Ordering::SeqCst) as usize;
        
        if pos >= self.events.len() {
            *self.state.write() = ReplayState::Finished;
            return None;
        }
        
        let event = self.events.get(pos).cloned();
        
        if let Some(ref evt) = event {
            // Check filters
            if !self.should_replay(evt) {
                self.position.fetch_add(1, Ordering::SeqCst);
                return self.step(); // Skip and try next
            }
            
            // Dispatch to handlers
            let handlers = self.handlers.read();
            for handler in handlers.iter() {
                handler.on_event(evt);
            }
        }
        
        self.position.fetch_add(1, Ordering::SeqCst);
        event
    }
    
    /// Check if event should be replayed
    fn should_replay(&self, event: &RecordedEvent) -> bool {
        // Check sequence range
        if self.config.start_sequence > 0 && event.sequence < self.config.start_sequence {
            return false;
        }
        if self.config.end_sequence > 0 && event.sequence > self.config.end_sequence {
            return false;
        }
        
        // Check event type filter
        if !self.config.event_filter.is_empty() {
            let matches = self.config.event_filter.iter().any(|t| {
                std::mem::discriminant(t) == std::mem::discriminant(&event.event_type)
            });
            if !matches {
                return false;
            }
        }
        
        // Check trace filter
        if let Some(trace) = &self.config.trace_filter {
            if event.trace_id.as_ref() != Some(trace) {
                return false;
            }
        }
        
        true
    }
    
    /// Get current position
    pub fn position(&self) -> u64 {
        self.position.load(Ordering::Relaxed)
    }
    
    /// Get total events
    pub fn total_events(&self) -> usize {
        self.events.len()
    }
    
    /// Get current state
    pub fn state(&self) -> ReplayState {
        *self.state.read()
    }
    
    /// Seek to position
    pub fn seek(&self, position: u64) {
        self.position.store(position, Ordering::SeqCst);
    }
}

/// Handles replayed events
pub trait ReplayHandler: Send + Sync {
    fn on_event(&self, event: &RecordedEvent);
}

// ============================================================================
// Time Travel Debugging
// ============================================================================

/// Enables time travel debugging with snapshots
pub struct TimeTravelDebugger {
    /// Event history
    events: Vec<RecordedEvent>,
    
    /// State snapshots
    snapshots: Vec<StateSnapshot>,
    
    /// Current position
    position: usize,
    
    /// Snapshot interval
    snapshot_interval: usize,
}

#[derive(Debug, Clone)]
pub struct StateSnapshot {
    /// Sequence number when taken
    pub sequence: u64,
    
    /// Serialized state
    pub state: Vec<u8>,
    
    /// State checksum
    pub checksum: u64,
}

impl TimeTravelDebugger {
    pub fn new(events: Vec<RecordedEvent>, snapshot_interval: usize) -> Self {
        Self {
            events,
            snapshots: Vec::new(),
            position: 0,
            snapshot_interval,
        }
    }
    
    /// Add a state snapshot
    pub fn add_snapshot(&mut self, snapshot: StateSnapshot) {
        self.snapshots.push(snapshot);
    }
    
    /// Move forward one step
    pub fn forward(&mut self) -> Option<&RecordedEvent> {
        if self.position < self.events.len() {
            let event = &self.events[self.position];
            self.position += 1;
            Some(event)
        } else {
            None
        }
    }
    
    /// Move backward one step
    ///
    /// # Tricky Part
    /// Going backward requires restoring state from a snapshot
    /// and replaying events up to the target position.
    pub fn backward(&mut self) -> Option<&RecordedEvent> {
        if self.position > 0 {
            self.position -= 1;
            Some(&self.events[self.position])
        } else {
            None
        }
    }
    
    /// Jump to specific position
    ///
    /// Uses nearest snapshot to minimize replay.
    pub fn goto(&mut self, position: usize) -> Option<&RecordedEvent> {
        if position >= self.events.len() {
            return None;
        }
        
        // Find nearest snapshot before position
        let target_seq = self.events[position].sequence;
        let snapshot = self.snapshots.iter()
            .rev()
            .find(|s| s.sequence <= target_seq);
        
        // Would restore from snapshot here...
        
        self.position = position;
        Some(&self.events[position])
    }
    
    /// Get current position
    pub fn position(&self) -> usize {
        self.position
    }
    
    /// Get event at current position
    pub fn current(&self) -> Option<&RecordedEvent> {
        self.events.get(self.position)
    }
}

// ============================================================================
// Request Recorder
// ============================================================================

/// Records request/response pairs for replay
pub struct RequestRecorder {
    /// Recorded requests
    requests: Mutex<HashMap<String, RecordedRequest>>,
    
    /// Sequence counter
    sequence: AtomicU64,
    
    /// Recording enabled
    enabled: std::sync::atomic::AtomicBool,
}

#[derive(Debug, Clone)]
pub struct RecordedRequest {
    /// Request ID
    pub id: String,
    
    /// Sequence number
    pub sequence: u64,
    
    /// Timestamp
    pub timestamp: SystemTime,
    
    /// Trace ID
    pub trace_id: Option<TraceId>,
    
    /// Request data
    pub request: Vec<u8>,
    
    /// Response data
    pub response: Option<Vec<u8>>,
    
    /// Latency
    pub latency: Option<Duration>,
}

impl RequestRecorder {
    pub fn new() -> Self {
        Self {
            requests: Mutex::new(HashMap::new()),
            sequence: AtomicU64::new(0),
            enabled: std::sync::atomic::AtomicBool::new(true),
        }
    }
    
    /// Enable/disable recording
    pub fn set_enabled(&self, enabled: bool) {
        self.enabled.store(enabled, Ordering::SeqCst);
    }
    
    /// Record a request
    pub fn record_request(
        &self,
        id: String,
        request: Vec<u8>,
        trace_id: Option<TraceId>,
    ) {
        if !self.enabled.load(Ordering::Relaxed) {
            return;
        }
        
        let recorded = RecordedRequest {
            id: id.clone(),
            sequence: self.sequence.fetch_add(1, Ordering::SeqCst),
            timestamp: SystemTime::now(),
            trace_id,
            request,
            response: None,
            latency: None,
        };
        
        self.requests.lock().insert(id, recorded);
    }
    
    /// Record response for a request
    pub fn record_response(&self, id: &str, response: Vec<u8>, latency: Duration) {
        if let Some(recorded) = self.requests.lock().get_mut(id) {
            recorded.response = Some(response);
            recorded.latency = Some(latency);
        }
    }
    
    /// Get all recorded requests
    pub fn get_requests(&self) -> Vec<RecordedRequest> {
        self.requests.lock().values().cloned().collect()
    }
    
    /// Clear recordings
    pub fn clear(&self) {
        self.requests.lock().clear();
    }
}

impl Default for RequestRecorder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_event_recorder() {
        let config = RecorderConfig::default();
        let recorder = EventRecorder::new(config);
        
        recorder.start();
        
        let event = RecordedEvent {
            sequence: 0,
            timestamp: SystemTime::now(),
            event_type: EventType::RequestReceived,
            source_node: "node1".into(),
            target_node: None,
            trace_id: None,
            payload: Payload::Empty,
            metadata: HashMap::new(),
        };
        
        assert!(recorder.record(event));
        
        recorder.stop();
        
        let events = recorder.get_events();
        assert_eq!(events.len(), 1);
    }
    
    #[test]
    fn test_replay_engine() {
        let events = vec![
            RecordedEvent {
                sequence: 0,
                timestamp: SystemTime::now(),
                event_type: EventType::RequestReceived,
                source_node: "node1".into(),
                target_node: None,
                trace_id: None,
                payload: Payload::Empty,
                metadata: HashMap::new(),
            },
            RecordedEvent {
                sequence: 1,
                timestamp: SystemTime::now(),
                event_type: EventType::ResponseSent,
                source_node: "node1".into(),
                target_node: None,
                trace_id: None,
                payload: Payload::Empty,
                metadata: HashMap::new(),
            },
        ];
        
        let config = ReplayConfig::default();
        let engine = ReplayEngine::new(events, config);
        
        engine.start();
        
        let e1 = engine.step();
        assert!(e1.is_some());
        assert_eq!(e1.unwrap().sequence, 0);
        
        let e2 = engine.step();
        assert!(e2.is_some());
        assert_eq!(e2.unwrap().sequence, 1);
        
        let e3 = engine.step();
        assert!(e3.is_none());
    }
    
    #[test]
    fn test_time_travel() {
        let events = (0..10).map(|i| RecordedEvent {
            sequence: i,
            timestamp: SystemTime::now(),
            event_type: EventType::StateMutation,
            source_node: "node1".into(),
            target_node: None,
            trace_id: None,
            payload: Payload::Empty,
            metadata: HashMap::new(),
        }).collect();
        
        let mut debugger = TimeTravelDebugger::new(events, 5);
        
        // Move forward
        debugger.forward();
        debugger.forward();
        assert_eq!(debugger.position(), 2);
        
        // Move backward
        debugger.backward();
        assert_eq!(debugger.position(), 1);
        
        // Jump to position
        debugger.goto(5);
        assert_eq!(debugger.position(), 5);
    }
    
    #[test]
    fn test_request_recorder() {
        let recorder = RequestRecorder::new();
        
        recorder.record_request(
            "req1".into(),
            b"GET /api".to_vec(),
            None,
        );
        
        recorder.record_response(
            "req1",
            b"OK".to_vec(),
            Duration::from_millis(50),
        );
        
        let requests = recorder.get_requests();
        assert_eq!(requests.len(), 1);
        assert!(requests[0].response.is_some());
        assert!(requests[0].latency.is_some());
    }
}
