//! # Network Module
//!
//! Distributed networking infrastructure.
//!
//! ## Module Structure
//!
//! - `protocol.rs` - Message protocols and serialization
//! - `transport.rs` - Connection and transport management
//! - `routing.rs` - Message routing and discovery

mod protocol;
mod transport;
mod routing;

pub use protocol::*;
pub use transport::*;
pub use routing::*;

use thiserror::Error;

/// Errors that can occur in network operations
#[derive(Error, Debug, Clone)]
pub enum NetworkError {
    #[error("Connection failed to peer {peer_id}: {reason}")]
    ConnectionFailed { peer_id: u64, reason: String },

    #[error("Peer not found: {0}")]
    PeerNotFound(u64),

    #[error("Message too large: {size} bytes (max: {max})")]
    MessageTooLarge { size: usize, max: usize },

    #[error("Protocol error: {0}")]
    ProtocolError(String),

    #[error("Serialization error: {0}")]
    SerializationError(String),

    #[error("Timeout waiting for response")]
    Timeout,

    #[error("Connection closed")]
    ConnectionClosed,

    #[error("Route not found: {0}")]
    RouteNotFound(String),

    #[error("TLS error: {0}")]
    TlsError(String),
}
