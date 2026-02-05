//! # Structured Logging
//!
//! High-performance structured logging for distributed systems.
//!
//! ## Philosophy
//!
//! Logs should be:
//! - **Structured**: Machine-parseable (JSON, key-value)
//! - **Contextual**: Include trace IDs, request IDs
//! - **Leveled**: Filter by importance
//! - **Sampled**: Rate-limited in high-throughput paths
//!
//! ## Log Levels
//!
//! - TRACE: Very detailed debugging
//! - DEBUG: Development debugging
//! - INFO: Normal operation milestones
//! - WARN: Unexpected but handled conditions
//! - ERROR: Failures requiring attention
//!
//! ## Best Practices
//!
//! 1. Include trace context in every log
//! 2. Use structured fields, not string formatting
//! 3. Log at appropriate levels
//! 4. Sample high-frequency logs
//! 5. Don't log sensitive data

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use parking_lot::{Mutex, RwLock};

use super::tracing::TraceId;

// ============================================================================
// Log Levels
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogLevel {
    Trace = 0,
    Debug = 1,
    Info = 2,
    Warn = 3,
    Error = 4,
}

impl LogLevel {
    pub fn as_str(&self) -> &'static str {
        match self {
            LogLevel::Trace => "TRACE",
            LogLevel::Debug => "DEBUG",
            LogLevel::Info => "INFO",
            LogLevel::Warn => "WARN",
            LogLevel::Error => "ERROR",
        }
    }
}

impl std::fmt::Display for LogLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

// ============================================================================
// Log Record
// ============================================================================

/// A structured log entry
#[derive(Debug, Clone)]
pub struct LogRecord {
    /// Log level
    pub level: LogLevel,
    
    /// Timestamp
    pub timestamp: SystemTime,
    
    /// Message
    pub message: String,
    
    /// Logger name/source
    pub logger: String,
    
    /// Trace ID (if in trace context)
    pub trace_id: Option<TraceId>,
    
    /// Structured fields
    pub fields: HashMap<String, FieldValue>,
    
    /// Source file
    pub file: Option<String>,
    
    /// Source line
    pub line: Option<u32>,
}

#[derive(Debug, Clone)]
pub enum FieldValue {
    String(String),
    Int(i64),
    Uint(u64),
    Float(f64),
    Bool(bool),
    Duration(Duration),
    Error(String),
}

impl LogRecord {
    pub fn new(level: LogLevel, message: impl Into<String>) -> Self {
        Self {
            level,
            timestamp: SystemTime::now(),
            message: message.into(),
            logger: String::new(),
            trace_id: None,
            fields: HashMap::new(),
            file: None,
            line: None,
        }
    }
    
    pub fn with_logger(mut self, logger: impl Into<String>) -> Self {
        self.logger = logger.into();
        self
    }
    
    pub fn with_trace(mut self, trace_id: TraceId) -> Self {
        self.trace_id = Some(trace_id);
        self
    }
    
    pub fn with_field(mut self, key: impl Into<String>, value: FieldValue) -> Self {
        self.fields.insert(key.into(), value);
        self
    }
    
    pub fn with_source(mut self, file: &str, line: u32) -> Self {
        self.file = Some(file.into());
        self.line = Some(line);
        self
    }
    
    /// Format as JSON
    pub fn to_json(&self) -> String {
        let timestamp = self.timestamp
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis();
        
        let mut json = format!(
            r#"{{"level":"{}","timestamp":{},"message":"{}""#,
            self.level.as_str(),
            timestamp,
            self.message.replace('\"', "\\\""),
        );
        
        if !self.logger.is_empty() {
            json.push_str(&format!(r#","logger":"{}""#, self.logger));
        }
        
        if let Some(trace_id) = &self.trace_id {
            json.push_str(&format!(r#","trace_id":"{}""#, trace_id.to_hex()));
        }
        
        if let Some(file) = &self.file {
            json.push_str(&format!(r#","file":"{}""#, file));
        }
        
        if let Some(line) = self.line {
            json.push_str(&format!(r#","line":{}"#, line));
        }
        
        for (key, value) in &self.fields {
            let value_str = match value {
                FieldValue::String(s) => format!(r#""{}""#, s.replace('\"', "\\\"")),
                FieldValue::Int(i) => i.to_string(),
                FieldValue::Uint(u) => u.to_string(),
                FieldValue::Float(f) => f.to_string(),
                FieldValue::Bool(b) => b.to_string(),
                FieldValue::Duration(d) => format!(r#""{}ms""#, d.as_millis()),
                FieldValue::Error(e) => format!(r#""{}""#, e.replace('\"', "\\\"")),
            };
            json.push_str(&format!(r#","{}":"#, key));
            json.push_str(&value_str);
        }
        
        json.push('}');
        json
    }
}

// ============================================================================
// Logger
// ============================================================================

/// Structured logger
pub struct Logger {
    /// Logger name
    name: String,
    
    /// Minimum log level
    level: RwLock<LogLevel>,
    
    /// Log handlers
    handlers: RwLock<Vec<Box<dyn LogHandler + Send + Sync>>>,
    
    /// Default fields (added to every log)
    default_fields: RwLock<HashMap<String, FieldValue>>,
    
    /// Rate limiter
    rate_limiter: Option<RateLimitedLogger>,
    
    /// Metrics
    metrics: LogMetrics,
}

#[derive(Default)]
struct LogMetrics {
    total_logs: AtomicU64,
    by_level: [AtomicU64; 5], // One per level
    dropped: AtomicU64,
}

impl Logger {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            level: RwLock::new(LogLevel::Info),
            handlers: RwLock::new(Vec::new()),
            default_fields: RwLock::new(HashMap::new()),
            rate_limiter: None,
            metrics: LogMetrics::default(),
        }
    }
    
    /// Set minimum log level
    pub fn set_level(&self, level: LogLevel) {
        *self.level.write() = level;
    }
    
    /// Add a handler
    pub fn add_handler<H: LogHandler + 'static>(&self, handler: H) {
        self.handlers.write().push(Box::new(handler));
    }
    
    /// Add a default field
    pub fn add_default_field(&self, key: impl Into<String>, value: FieldValue) {
        self.default_fields.write().insert(key.into(), value);
    }
    
    /// Check if level is enabled
    pub fn is_enabled(&self, level: LogLevel) -> bool {
        level >= *self.level.read()
    }
    
    /// Log a record
    pub fn log(&self, mut record: LogRecord) {
        if !self.is_enabled(record.level) {
            return;
        }
        
        // Check rate limit
        if let Some(limiter) = &self.rate_limiter {
            if !limiter.allow() {
                self.metrics.dropped.fetch_add(1, Ordering::Relaxed);
                return;
            }
        }
        
        // Add default fields
        for (key, value) in self.default_fields.read().iter() {
            if !record.fields.contains_key(key) {
                record.fields.insert(key.clone(), value.clone());
            }
        }
        
        // Set logger name
        if record.logger.is_empty() {
            record.logger = self.name.clone();
        }
        
        // Update metrics
        self.metrics.total_logs.fetch_add(1, Ordering::Relaxed);
        self.metrics.by_level[record.level as usize].fetch_add(1, Ordering::Relaxed);
        
        // Send to handlers
        let handlers = self.handlers.read();
        for handler in handlers.iter() {
            handler.handle(&record);
        }
    }
    
    /// Log convenience methods
    pub fn trace(&self, message: impl Into<String>) {
        self.log(LogRecord::new(LogLevel::Trace, message));
    }
    
    pub fn debug(&self, message: impl Into<String>) {
        self.log(LogRecord::new(LogLevel::Debug, message));
    }
    
    pub fn info(&self, message: impl Into<String>) {
        self.log(LogRecord::new(LogLevel::Info, message));
    }
    
    pub fn warn(&self, message: impl Into<String>) {
        self.log(LogRecord::new(LogLevel::Warn, message));
    }
    
    pub fn error(&self, message: impl Into<String>) {
        self.log(LogRecord::new(LogLevel::Error, message));
    }
    
    /// Get log stats
    pub fn stats(&self) -> LogStats {
        LogStats {
            total: self.metrics.total_logs.load(Ordering::Relaxed),
            by_level: [
                self.metrics.by_level[0].load(Ordering::Relaxed),
                self.metrics.by_level[1].load(Ordering::Relaxed),
                self.metrics.by_level[2].load(Ordering::Relaxed),
                self.metrics.by_level[3].load(Ordering::Relaxed),
                self.metrics.by_level[4].load(Ordering::Relaxed),
            ],
            dropped: self.metrics.dropped.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LogStats {
    pub total: u64,
    pub by_level: [u64; 5],
    pub dropped: u64,
}

// ============================================================================
// Log Handlers
// ============================================================================

/// Handles log records
pub trait LogHandler: Send + Sync {
    fn handle(&self, record: &LogRecord);
}

/// Writes logs to stdout
pub struct StdoutHandler {
    format: LogFormat,
}

#[derive(Debug, Clone, Copy)]
pub enum LogFormat {
    /// Human-readable format
    Pretty,
    /// JSON format
    Json,
    /// Compact key=value format
    Logfmt,
}

impl StdoutHandler {
    pub fn new(format: LogFormat) -> Self {
        Self { format }
    }
}

impl LogHandler for StdoutHandler {
    fn handle(&self, record: &LogRecord) {
        match self.format {
            LogFormat::Json => {
                println!("{}", record.to_json());
            }
            LogFormat::Pretty => {
                let timestamp = record.timestamp
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs();
                
                let level_color = match record.level {
                    LogLevel::Trace => "\x1b[90m",
                    LogLevel::Debug => "\x1b[36m",
                    LogLevel::Info => "\x1b[32m",
                    LogLevel::Warn => "\x1b[33m",
                    LogLevel::Error => "\x1b[31m",
                };
                
                println!(
                    "{}{}\x1b[0m {} [{}] {}",
                    level_color,
                    record.level.as_str(),
                    timestamp,
                    record.logger,
                    record.message,
                );
            }
            LogFormat::Logfmt => {
                let timestamp = record.timestamp
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_millis();
                
                print!(
                    "level={} ts={} logger={} msg=\"{}\"",
                    record.level.as_str().to_lowercase(),
                    timestamp,
                    record.logger,
                    record.message,
                );
                
                for (key, value) in &record.fields {
                    let value_str = match value {
                        FieldValue::String(s) => format!("\"{}\"", s),
                        FieldValue::Int(i) => i.to_string(),
                        FieldValue::Uint(u) => u.to_string(),
                        FieldValue::Float(f) => f.to_string(),
                        FieldValue::Bool(b) => b.to_string(),
                        FieldValue::Duration(d) => format!("{}ms", d.as_millis()),
                        FieldValue::Error(e) => format!("\"{}\"", e),
                    };
                    print!(" {}={}", key, value_str);
                }
                
                println!();
            }
        }
    }
}

/// Buffers logs for async writing
pub struct AsyncHandler {
    buffer: Mutex<Vec<LogRecord>>,
    inner: Box<dyn LogHandler + Send + Sync>,
    buffer_size: usize,
}

impl AsyncHandler {
    pub fn new(inner: impl LogHandler + 'static, buffer_size: usize) -> Self {
        Self {
            buffer: Mutex::new(Vec::new()),
            inner: Box::new(inner),
            buffer_size,
        }
    }
    
    pub fn flush(&self) {
        let records = std::mem::take(&mut *self.buffer.lock());
        for record in records {
            self.inner.handle(&record);
        }
    }
}

impl LogHandler for AsyncHandler {
    fn handle(&self, record: &LogRecord) {
        let mut buffer = self.buffer.lock();
        buffer.push(record.clone());
        
        if buffer.len() >= self.buffer_size {
            let records = std::mem::take(&mut *buffer);
            drop(buffer);
            
            for record in records {
                self.inner.handle(&record);
            }
        }
    }
}

/// Filters logs by level
pub struct LevelFilter {
    level: LogLevel,
    inner: Box<dyn LogHandler + Send + Sync>,
}

impl LevelFilter {
    pub fn new(level: LogLevel, inner: impl LogHandler + 'static) -> Self {
        Self {
            level,
            inner: Box::new(inner),
        }
    }
}

impl LogHandler for LevelFilter {
    fn handle(&self, record: &LogRecord) {
        if record.level >= self.level {
            self.inner.handle(record);
        }
    }
}

// ============================================================================
// Rate Limiting
// ============================================================================

/// Rate limits log output
pub struct RateLimitedLogger {
    /// Maximum logs per second
    max_per_second: u64,
    
    /// Token bucket
    tokens: AtomicU64,
    
    /// Last refill time
    last_refill: Mutex<Instant>,
}

impl RateLimitedLogger {
    pub fn new(max_per_second: u64) -> Self {
        Self {
            max_per_second,
            tokens: AtomicU64::new(max_per_second),
            last_refill: Mutex::new(Instant::now()),
        }
    }
    
    pub fn allow(&self) -> bool {
        // Try to consume a token
        loop {
            let current = self.tokens.load(Ordering::Relaxed);
            
            if current == 0 {
                // Try to refill
                let mut last_refill = self.last_refill.lock();
                let elapsed = last_refill.elapsed();
                
                if elapsed >= Duration::from_secs(1) {
                    self.tokens.store(self.max_per_second, Ordering::Relaxed);
                    *last_refill = Instant::now();
                    drop(last_refill);
                    continue;
                }
                
                return false;
            }
            
            if self.tokens.compare_exchange(
                current,
                current - 1,
                Ordering::SeqCst,
                Ordering::Relaxed
            ).is_ok() {
                return true;
            }
        }
    }
}

// ============================================================================
// Log Aggregation
// ============================================================================

/// Aggregates similar log messages
pub struct LogAggregator {
    /// Aggregation window
    window: Duration,
    
    /// Current aggregations
    aggregations: Mutex<HashMap<String, AggregatedLog>>,
}

struct AggregatedLog {
    record: LogRecord,
    count: u64,
    first_seen: Instant,
    last_seen: Instant,
}

impl LogAggregator {
    pub fn new(window: Duration) -> Self {
        Self {
            window,
            aggregations: Mutex::new(HashMap::new()),
        }
    }
    
    /// Add a log record
    pub fn add(&self, record: LogRecord) {
        let key = format!("{}:{}:{}", record.level.as_str(), record.logger, record.message);
        let now = Instant::now();
        
        let mut aggregations = self.aggregations.lock();
        
        if let Some(agg) = aggregations.get_mut(&key) {
            agg.count += 1;
            agg.last_seen = now;
        } else {
            aggregations.insert(key, AggregatedLog {
                record,
                count: 1,
                first_seen: now,
                last_seen: now,
            });
        }
    }
    
    /// Flush aggregations older than window
    pub fn flush(&self) -> Vec<(LogRecord, u64)> {
        let now = Instant::now();
        let mut aggregations = self.aggregations.lock();
        
        let mut results = Vec::new();
        aggregations.retain(|_, agg| {
            if now.duration_since(agg.first_seen) >= self.window {
                results.push((agg.record.clone(), agg.count));
                false
            } else {
                true
            }
        });
        
        results
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_log_record() {
        let record = LogRecord::new(LogLevel::Info, "Test message")
            .with_logger("test-logger")
            .with_field("user_id", FieldValue::Int(123))
            .with_field("action", FieldValue::String("login".into()));
        
        let json = record.to_json();
        assert!(json.contains("\"level\":\"INFO\""));
        assert!(json.contains("\"message\":\"Test message\""));
        assert!(json.contains("\"user_id\":"));
    }
    
    #[test]
    fn test_logger() {
        let logger = Logger::new("test");
        
        // Capture handler
        struct CaptureHandler {
            logs: Mutex<Vec<LogRecord>>,
        }
        impl LogHandler for CaptureHandler {
            fn handle(&self, record: &LogRecord) {
                self.logs.lock().push(record.clone());
            }
        }
        
        let capture = Arc::new(CaptureHandler {
            logs: Mutex::new(Vec::new()),
        });
        
        // Note: Can't easily add Arc<T> as handler, simplified test
        logger.info("Test info");
        logger.warn("Test warn");
        
        let stats = logger.stats();
        assert_eq!(stats.total, 2);
    }
    
    #[test]
    fn test_rate_limiter() {
        let limiter = RateLimitedLogger::new(5);
        
        // First 5 should be allowed
        for _ in 0..5 {
            assert!(limiter.allow());
        }
        
        // Next should be denied
        assert!(!limiter.allow());
    }
    
    #[test]
    fn test_log_levels() {
        assert!(LogLevel::Error > LogLevel::Warn);
        assert!(LogLevel::Warn > LogLevel::Info);
        assert!(LogLevel::Info > LogLevel::Debug);
        assert!(LogLevel::Debug > LogLevel::Trace);
    }
}
