//! # Network Protocol
//!
//! Message framing, serialization, and protocol handling.
//!
//! ## Design Goals
//!
//! - **Efficiency**: Minimize serialization overhead
//! - **Extensibility**: Support versioning and evolution
//! - **Safety**: Validate all incoming messages
//! - **Observability**: Track message flow
//!
//! ## Message Format
//!
//! ```text
//! ┌─────────────┬──────────────┬─────────────┬─────────────────┐
//! │ Magic (4B)  │ Version (2B) │ Length (4B) │ Header (var)    │
//! ├─────────────┴──────────────┴─────────────┴─────────────────┤
//! │ Payload (var)                                              │
//! ├────────────────────────────────────────────────────────────┤
//! │ Checksum (4B)                                              │
//! └────────────────────────────────────────────────────────────┘
//! ```

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use parking_lot::RwLock;

use super::NetworkError;

// ============================================================================
// Constants
// ============================================================================

/// Magic bytes identifying Grey protocol
pub const PROTOCOL_MAGIC: [u8; 4] = [0x47, 0x52, 0x45, 0x59]; // "GREY"

/// Current protocol version
pub const PROTOCOL_VERSION: u16 = 1;

/// Maximum message size (16 MB)
pub const MAX_MESSAGE_SIZE: usize = 16 * 1024 * 1024;

/// Maximum header size
pub const MAX_HEADER_SIZE: usize = 4096;

// ============================================================================
// Message Types
// ============================================================================

/// Unique message identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MessageId(pub u64);

/// Peer identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PeerId(pub u64);

/// Message categories
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum MessageType {
    // Consensus messages
    VoteRequest = 1,
    VoteResponse = 2,
    AppendEntries = 3,
    AppendResponse = 4,
    
    // Replication messages
    ReplicateData = 10,
    ReplicateAck = 11,
    
    // Cluster management
    JoinCluster = 20,
    LeaveCluster = 21,
    Heartbeat = 22,
    HeartbeatAck = 23,
    
    // Client requests
    ClientRequest = 30,
    ClientResponse = 31,
    
    // Internal
    Ping = 40,
    Pong = 41,
    Error = 50,
}

/// Message priority levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Priority {
    Low = 0,
    Normal = 1,
    High = 2,
    Critical = 3,
}

impl Default for Priority {
    fn default() -> Self {
        Priority::Normal
    }
}

// ============================================================================
// Message
// ============================================================================

/// Network message
#[derive(Debug, Clone)]
pub struct Message {
    /// Unique message ID
    pub id: MessageId,
    
    /// Message type
    pub message_type: MessageType,
    
    /// Sender peer ID
    pub sender: PeerId,
    
    /// Target peer ID (None for broadcast)
    pub target: Option<PeerId>,
    
    /// Priority
    pub priority: Priority,
    
    /// Correlation ID (for request-response)
    pub correlation_id: Option<MessageId>,
    
    /// Headers (key-value metadata)
    pub headers: HashMap<String, String>,
    
    /// Payload
    pub payload: Vec<u8>,
    
    /// Timestamp (micros since epoch)
    pub timestamp: u64,
    
    /// Time-to-live in hops
    pub ttl: u8,
}

impl Message {
    /// Create a new message
    pub fn new(message_type: MessageType, sender: PeerId, payload: Vec<u8>) -> Self {
        static NEXT_ID: AtomicU64 = AtomicU64::new(1);
        
        Self {
            id: MessageId(NEXT_ID.fetch_add(1, Ordering::SeqCst)),
            message_type,
            sender,
            target: None,
            priority: Priority::Normal,
            correlation_id: None,
            headers: HashMap::new(),
            payload,
            timestamp: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_micros() as u64,
            ttl: 10,
        }
    }
    
    /// Set target peer
    pub fn to(mut self, target: PeerId) -> Self {
        self.target = Some(target);
        self
    }
    
    /// Set priority
    pub fn with_priority(mut self, priority: Priority) -> Self {
        self.priority = priority;
        self
    }
    
    /// Set correlation ID
    pub fn in_reply_to(mut self, id: MessageId) -> Self {
        self.correlation_id = Some(id);
        self
    }
    
    /// Add header
    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }
    
    /// Create a response to this message
    pub fn reply(&self, message_type: MessageType, payload: Vec<u8>) -> Self {
        let mut response = Message::new(message_type, self.target.unwrap_or(PeerId(0)), payload);
        response.target = Some(self.sender);
        response.correlation_id = Some(self.id);
        response
    }
}

// ============================================================================
// Serialization
// ============================================================================

/// Message serializer/deserializer
pub struct MessageCodec {
    /// Maximum allowed message size
    max_size: usize,
    
    /// Compression threshold
    compression_threshold: usize,
}

impl MessageCodec {
    pub fn new() -> Self {
        Self {
            max_size: MAX_MESSAGE_SIZE,
            compression_threshold: 4096,
        }
    }
    
    /// Serialize a message to bytes
    pub fn encode(&self, message: &Message) -> Result<Vec<u8>, NetworkError> {
        let mut buffer = Vec::with_capacity(256);
        
        // Magic
        buffer.extend_from_slice(&PROTOCOL_MAGIC);
        
        // Version
        buffer.extend_from_slice(&PROTOCOL_VERSION.to_be_bytes());
        
        // Placeholder for length (will fill later)
        let length_pos = buffer.len();
        buffer.extend_from_slice(&0u32.to_be_bytes());
        
        // Message ID
        buffer.extend_from_slice(&message.id.0.to_be_bytes());
        
        // Type, priority, TTL
        buffer.push(message.message_type as u8);
        buffer.push(message.priority as u8);
        buffer.push(message.ttl);
        
        // Sender and target
        buffer.extend_from_slice(&message.sender.0.to_be_bytes());
        buffer.extend_from_slice(&message.target.map(|t| t.0).unwrap_or(0).to_be_bytes());
        
        // Correlation ID
        buffer.extend_from_slice(&message.correlation_id.map(|c| c.0).unwrap_or(0).to_be_bytes());
        
        // Timestamp
        buffer.extend_from_slice(&message.timestamp.to_be_bytes());
        
        // Headers count and data
        buffer.extend_from_slice(&(message.headers.len() as u16).to_be_bytes());
        for (key, value) in &message.headers {
            let key_bytes = key.as_bytes();
            let value_bytes = value.as_bytes();
            buffer.push(key_bytes.len() as u8);
            buffer.extend_from_slice(key_bytes);
            buffer.extend_from_slice(&(value_bytes.len() as u16).to_be_bytes());
            buffer.extend_from_slice(value_bytes);
        }
        
        // Payload length and data
        buffer.extend_from_slice(&(message.payload.len() as u32).to_be_bytes());
        buffer.extend_from_slice(&message.payload);
        
        // Compute and append checksum
        let checksum = self.compute_checksum(&buffer[4..]); // Skip magic
        buffer.extend_from_slice(&checksum.to_be_bytes());
        
        // Fill in length
        let length = (buffer.len() - 10) as u32; // Exclude magic, version, length field
        buffer[length_pos..length_pos + 4].copy_from_slice(&length.to_be_bytes());
        
        if buffer.len() > self.max_size {
            return Err(NetworkError::MessageTooLarge {
                size: buffer.len(),
                max: self.max_size,
            });
        }
        
        Ok(buffer)
    }
    
    /// Deserialize a message from bytes
    pub fn decode(&self, data: &[u8]) -> Result<Message, NetworkError> {
        if data.len() < 10 {
            return Err(NetworkError::ProtocolError("Message too short".into()));
        }
        
        // Verify magic
        if &data[0..4] != PROTOCOL_MAGIC {
            return Err(NetworkError::ProtocolError("Invalid magic bytes".into()));
        }
        
        // Check version
        let version = u16::from_be_bytes([data[4], data[5]]);
        if version > PROTOCOL_VERSION {
            return Err(NetworkError::ProtocolError(format!(
                "Unsupported version: {}",
                version
            )));
        }
        
        // Read length
        let length = u32::from_be_bytes([data[6], data[7], data[8], data[9]]) as usize;
        if data.len() < 10 + length {
            return Err(NetworkError::ProtocolError("Incomplete message".into()));
        }
        
        // Verify checksum
        let checksum_start = 10 + length - 4;
        let expected_checksum = u32::from_be_bytes([
            data[checksum_start],
            data[checksum_start + 1],
            data[checksum_start + 2],
            data[checksum_start + 3],
        ]);
        let actual_checksum = self.compute_checksum(&data[4..checksum_start]);
        
        if expected_checksum != actual_checksum {
            return Err(NetworkError::ProtocolError("Checksum mismatch".into()));
        }
        
        // Parse fields
        let mut pos = 10;
        
        let id = MessageId(u64::from_be_bytes(data[pos..pos + 8].try_into().unwrap()));
        pos += 8;
        
        let message_type = self.parse_message_type(data[pos])?;
        pos += 1;
        
        let priority = self.parse_priority(data[pos])?;
        pos += 1;
        
        let ttl = data[pos];
        pos += 1;
        
        let sender = PeerId(u64::from_be_bytes(data[pos..pos + 8].try_into().unwrap()));
        pos += 8;
        
        let target_raw = u64::from_be_bytes(data[pos..pos + 8].try_into().unwrap());
        let target = if target_raw == 0 { None } else { Some(PeerId(target_raw)) };
        pos += 8;
        
        let corr_raw = u64::from_be_bytes(data[pos..pos + 8].try_into().unwrap());
        let correlation_id = if corr_raw == 0 { None } else { Some(MessageId(corr_raw)) };
        pos += 8;
        
        let timestamp = u64::from_be_bytes(data[pos..pos + 8].try_into().unwrap());
        pos += 8;
        
        // Headers
        let header_count = u16::from_be_bytes([data[pos], data[pos + 1]]) as usize;
        pos += 2;
        
        let mut headers = HashMap::new();
        for _ in 0..header_count {
            let key_len = data[pos] as usize;
            pos += 1;
            let key = String::from_utf8_lossy(&data[pos..pos + key_len]).to_string();
            pos += key_len;
            
            let value_len = u16::from_be_bytes([data[pos], data[pos + 1]]) as usize;
            pos += 2;
            let value = String::from_utf8_lossy(&data[pos..pos + value_len]).to_string();
            pos += value_len;
            
            headers.insert(key, value);
        }
        
        // Payload
        let payload_len = u32::from_be_bytes(data[pos..pos + 4].try_into().unwrap()) as usize;
        pos += 4;
        let payload = data[pos..pos + payload_len].to_vec();
        
        Ok(Message {
            id,
            message_type,
            sender,
            target,
            priority,
            correlation_id,
            headers,
            payload,
            timestamp,
            ttl,
        })
    }
    
    fn compute_checksum(&self, data: &[u8]) -> u32 {
        let mut hash: u32 = 0;
        for byte in data {
            hash = hash.wrapping_mul(31).wrapping_add(*byte as u32);
        }
        hash
    }
    
    fn parse_message_type(&self, byte: u8) -> Result<MessageType, NetworkError> {
        match byte {
            1 => Ok(MessageType::VoteRequest),
            2 => Ok(MessageType::VoteResponse),
            3 => Ok(MessageType::AppendEntries),
            4 => Ok(MessageType::AppendResponse),
            10 => Ok(MessageType::ReplicateData),
            11 => Ok(MessageType::ReplicateAck),
            20 => Ok(MessageType::JoinCluster),
            21 => Ok(MessageType::LeaveCluster),
            22 => Ok(MessageType::Heartbeat),
            23 => Ok(MessageType::HeartbeatAck),
            30 => Ok(MessageType::ClientRequest),
            31 => Ok(MessageType::ClientResponse),
            40 => Ok(MessageType::Ping),
            41 => Ok(MessageType::Pong),
            50 => Ok(MessageType::Error),
            _ => Err(NetworkError::ProtocolError(format!("Unknown message type: {}", byte))),
        }
    }
    
    fn parse_priority(&self, byte: u8) -> Result<Priority, NetworkError> {
        match byte {
            0 => Ok(Priority::Low),
            1 => Ok(Priority::Normal),
            2 => Ok(Priority::High),
            3 => Ok(Priority::Critical),
            _ => Ok(Priority::Normal),
        }
    }
}

impl Default for MessageCodec {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Request-Response Pattern
// ============================================================================

/// Tracks pending requests awaiting responses
pub struct RequestTracker {
    /// Pending requests
    pending: RwLock<HashMap<MessageId, PendingRequest>>,
    
    /// Default timeout
    timeout: Duration,
}

struct PendingRequest {
    sent_at: std::time::Instant,
    timeout: Duration,
    response_type: MessageType,
}

impl RequestTracker {
    pub fn new(timeout: Duration) -> Self {
        Self {
            pending: RwLock::new(HashMap::new()),
            timeout,
        }
    }
    
    /// Register a pending request
    pub fn register(&self, id: MessageId, response_type: MessageType) {
        self.pending.write().insert(id, PendingRequest {
            sent_at: std::time::Instant::now(),
            timeout: self.timeout,
            response_type,
        });
    }
    
    /// Check if a message is a valid response
    pub fn check_response(&self, message: &Message) -> bool {
        if let Some(corr_id) = message.correlation_id {
            let pending = self.pending.read();
            if let Some(req) = pending.get(&corr_id) {
                return message.message_type == req.response_type;
            }
        }
        false
    }
    
    /// Complete a pending request
    pub fn complete(&self, correlation_id: MessageId) -> bool {
        self.pending.write().remove(&correlation_id).is_some()
    }
    
    /// Get expired requests
    pub fn expired(&self) -> Vec<MessageId> {
        let now = std::time::Instant::now();
        let pending = self.pending.read();
        
        pending
            .iter()
            .filter(|(_, req)| now.duration_since(req.sent_at) > req.timeout)
            .map(|(id, _)| *id)
            .collect()
    }
    
    /// Clean up expired requests
    pub fn cleanup(&self) -> Vec<MessageId> {
        let expired = self.expired();
        let mut pending = self.pending.write();
        
        for id in &expired {
            pending.remove(id);
        }
        
        expired
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_message_creation() {
        let msg = Message::new(MessageType::Ping, PeerId(1), vec![1, 2, 3])
            .to(PeerId(2))
            .with_priority(Priority::High)
            .with_header("key", "value");
        
        assert_eq!(msg.sender, PeerId(1));
        assert_eq!(msg.target, Some(PeerId(2)));
        assert_eq!(msg.priority, Priority::High);
        assert_eq!(msg.headers.get("key"), Some(&"value".to_string()));
    }
    
    #[test]
    fn test_codec_roundtrip() {
        let codec = MessageCodec::new();
        
        let original = Message::new(MessageType::ClientRequest, PeerId(1), b"hello world".to_vec())
            .to(PeerId(2))
            .with_header("trace-id", "abc123");
        
        let encoded = codec.encode(&original).unwrap();
        let decoded = codec.decode(&encoded).unwrap();
        
        assert_eq!(decoded.id, original.id);
        assert_eq!(decoded.message_type as u8, original.message_type as u8);
        assert_eq!(decoded.sender, original.sender);
        assert_eq!(decoded.target, original.target);
        assert_eq!(decoded.payload, original.payload);
        assert_eq!(decoded.headers.get("trace-id"), Some(&"abc123".to_string()));
    }
    
    #[test]
    fn test_request_tracker() {
        let tracker = RequestTracker::new(Duration::from_secs(5));
        let id = MessageId(1);
        
        tracker.register(id, MessageType::Pong);
        
        let response = Message::new(MessageType::Pong, PeerId(2), vec![])
            .in_reply_to(id);
        
        assert!(tracker.check_response(&response));
        assert!(tracker.complete(id));
    }
}
