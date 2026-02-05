// Grey Distributed — Archival Sync Protocol
// Synchronizes historical archives with live systems, ensuring past data
// remains accessible and coherent with present-day operations.

use std::collections::{HashMap, HashSet, VecDeque};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Archive identifier with temporal metadata.
/// Each archive spans a specific time range and contains immutable historical data.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArchiveId {
    /// Unique archive identifier
    pub id: String,
    /// Start of archive coverage (timestamp)
    pub coverage_start: u64,
    /// End of archive coverage (timestamp)
    pub coverage_end: u64,
    /// Archive generation (incremented on major restructuring)
    pub generation: u32,
    /// Canonical location of archive
    pub canonical_location: String,
}

/// Archive record representing a single historical entry.
#[derive(Debug, Clone)]
pub struct ArchiveRecord {
    /// Record identifier (unique within archive)
    pub record_id: String,
    /// Original creation timestamp
    pub created_at: u64,
    /// Last modification timestamp (for mutable-era records)
    pub modified_at: u64,
    /// Archival timestamp (when record became read-only)
    pub archived_at: u64,
    /// Record type classification
    pub record_type: RecordType,
    /// Record content in canonical format
    pub content: ArchiveContent,
    /// Provenance chain tracking record history
    pub provenance: ProvenanceChain,
    /// Cross-references to related records
    pub cross_refs: Vec<CrossReference>,
}

/// Record type classification for archival processing.
#[derive(Debug, Clone)]
pub enum RecordType {
    /// System configuration from a specific era
    Configuration { system: String, version: String },
    /// Transaction log entry
    Transaction { transaction_type: String },
    /// Governance decision record
    Governance { decision_type: String, authority: String },
    /// Data snapshot at a point in time
    Snapshot { entity: String, snapshot_type: String },
    /// Audit trail entry
    Audit { event_type: String, severity: AuditSeverity },
    /// Cultural or educational artifact
    Cultural { artifact_type: String, era: String },
}

/// Audit severity levels for archival audit records.
#[derive(Debug, Clone)]
pub enum AuditSeverity {
    /// Informational event
    Info,
    /// Warning condition
    Warning,
    /// Error condition
    Error,
    /// Critical security event
    Critical,
}

/// Archive content in canonical format.
#[derive(Debug, Clone)]
pub struct ArchiveContent {
    /// Content format identifier
    pub format: ContentFormat,
    /// Raw content bytes
    pub data: Vec<u8>,
    /// Content hash for integrity verification
    pub content_hash: [u8; 32],
    /// Compression method (if any)
    pub compression: Option<CompressionMethod>,
    /// Encryption status
    pub encryption: EncryptionStatus,
}

/// Content format for archival records.
#[derive(Debug, Clone)]
pub enum ContentFormat {
    /// Plain UTF-8 text
    PlainText,
    /// JSON with schema version
    Json { schema_version: u32 },
    /// Protocol Buffers with schema ID
    Protobuf { schema_id: String },
    /// Binary blob with type hint
    Binary { type_hint: String },
    /// Self-describing format (includes schema)
    SelfDescribing { format_name: String },
}

/// Compression methods for archived content.
#[derive(Debug, Clone)]
pub enum CompressionMethod {
    /// No compression
    None,
    /// ZSTD compression with level
    Zstd { level: u8 },
    /// LZ4 compression
    Lz4,
    /// Brotli compression with quality
    Brotli { quality: u8 },
}

/// Encryption status of archived content.
#[derive(Debug, Clone)]
pub enum EncryptionStatus {
    /// Unencrypted (public archive)
    Unencrypted,
    /// Encrypted with key ID (for access control)
    Encrypted { key_id: String, algorithm: String },
    /// Time-locked encryption (decryptable after timestamp)
    TimeLocked { unlock_timestamp: u64, algorithm: String },
}

/// Provenance chain tracking record history.
#[derive(Debug, Clone)]
pub struct ProvenanceChain {
    /// Original creator of the record
    pub creator: String,
    /// Creation context (system, version, location)
    pub creation_context: CreationContext,
    /// Chain of custody entries
    pub custody_chain: Vec<CustodyEntry>,
    /// Verification status
    pub verified: bool,
    /// Last verification timestamp
    pub last_verified: u64,
}

/// Context of record creation.
#[derive(Debug, Clone)]
pub struct CreationContext {
    /// System that created the record
    pub system: String,
    /// System version at creation time
    pub system_version: String,
    /// Geographic location (if applicable)
    pub location: Option<String>,
    /// Temporal epoch at creation
    pub epoch: String,
}

/// Entry in custody chain.
#[derive(Debug, Clone)]
pub struct CustodyEntry {
    /// Entity holding custody
    pub custodian: String,
    /// Start of custody period
    pub custody_start: u64,
    /// End of custody period (None if current)
    pub custody_end: Option<u64>,
    /// Custody transfer attestation
    pub attestation: Option<Vec<u8>>,
}

/// Cross-reference to related archive records.
#[derive(Debug, Clone)]
pub struct CrossReference {
    /// Target archive ID
    pub target_archive: String,
    /// Target record ID
    pub target_record: String,
    /// Relationship type
    pub relationship: RelationshipType,
    /// Reference validity period
    pub valid_from: u64,
    pub valid_until: Option<u64>,
}

/// Relationship types for cross-references.
#[derive(Debug, Clone)]
pub enum RelationshipType {
    /// Record supersedes another
    Supersedes,
    /// Record is superseded by another
    SupersededBy,
    /// Record references another
    References,
    /// Record is referenced by another
    ReferencedBy,
    /// Records are related (bidirectional)
    RelatedTo,
    /// Record is a child/component of another
    ChildOf,
    /// Record is a parent/container of another
    ParentOf,
}

/// Sync strategy for archival synchronization.
#[derive(Debug, Clone)]
pub enum SyncStrategy {
    /// Full synchronization (all records)
    Full,
    /// Incremental synchronization (since last sync)
    Incremental { since: u64 },
    /// Selective synchronization (specific record types)
    Selective { record_types: Vec<String> },
    /// Priority synchronization (critical records first)
    Priority { priority_threshold: u32 },
}

/// Sync status for tracking synchronization progress.
#[derive(Debug, Clone)]
pub struct SyncStatus {
    /// Sync operation ID
    pub sync_id: String,
    /// Sync start timestamp
    pub started_at: u64,
    /// Records synchronized
    pub records_synced: u64,
    /// Records pending
    pub records_pending: u64,
    /// Sync state
    pub state: SyncState,
    /// Errors encountered
    pub errors: Vec<SyncError>,
}

/// State of synchronization operation.
#[derive(Debug, Clone)]
pub enum SyncState {
    /// Sync in progress
    InProgress,
    /// Sync completed successfully
    Completed,
    /// Sync failed
    Failed,
    /// Sync paused
    Paused,
    /// Sync cancelled
    Cancelled,
}

/// Sync error record.
#[derive(Debug, Clone)]
pub struct SyncError {
    /// Record that failed to sync
    pub record_id: String,
    /// Error type
    pub error_type: SyncErrorType,
    /// Error message
    pub message: String,
    /// Timestamp of error
    pub occurred_at: u64,
    /// Retry count
    pub retry_count: u32,
}

/// Types of synchronization errors.
#[derive(Debug, Clone)]
pub enum SyncErrorType {
    /// Record not found in source
    RecordNotFound,
    /// Checksum mismatch
    ChecksumMismatch,
    /// Format incompatibility
    FormatIncompatible,
    /// Access denied
    AccessDenied,
    /// Network timeout
    Timeout,
    /// Storage failure
    StorageFailure,
}

/// Conflict resolution strategy for archival sync.
#[derive(Debug, Clone)]
pub enum ConflictResolution {
    /// Prefer source record
    PreferSource,
    /// Prefer destination record
    PreferDestination,
    /// Keep both versions
    KeepBoth,
    /// Merge records (if compatible)
    Merge,
    /// Manual resolution required
    ManualResolution,
}

/// Archival Sync Engine: Core system for synchronizing archives with live systems.
pub struct ArchivalSync {
    /// Registered archives
    archives: HashMap<String, ArchiveId>,
    /// Archive indexes for fast lookup
    indexes: HashMap<String, ArchiveIndex>,
    /// Active sync operations
    active_syncs: HashMap<String, SyncStatus>,
    /// Sync history for audit
    sync_history: VecDeque<SyncStatus>,
    /// Maximum sync history entries
    max_history: usize,
    /// Conflict resolution strategy
    conflict_strategy: ConflictResolution,
}

/// Archive index for fast record lookup.
#[derive(Debug, Clone)]
pub struct ArchiveIndex {
    /// Archive ID
    pub archive_id: String,
    /// Record count
    pub record_count: u64,
    /// Index by record ID
    pub by_id: HashMap<String, ArchiveRecordMeta>,
    /// Index by timestamp (sorted)
    pub by_timestamp: Vec<(u64, String)>,
    /// Index by record type
    pub by_type: HashMap<String, Vec<String>>,
    /// Last index update
    pub last_updated: u64,
}

/// Metadata for indexed archive records.
#[derive(Debug, Clone)]
pub struct ArchiveRecordMeta {
    /// Record ID
    pub record_id: String,
    /// Record type (stringified)
    pub record_type: String,
    /// Creation timestamp
    pub created_at: u64,
    /// Content size in bytes
    pub size_bytes: u64,
    /// Content hash
    pub content_hash: [u8; 32],
}

impl ArchivalSync {
    /// Create a new Archival Sync engine.
    pub fn new(conflict_strategy: ConflictResolution) -> Self {
        ArchivalSync {
            archives: HashMap::new(),
            indexes: HashMap::new(),
            active_syncs: HashMap::new(),
            sync_history: VecDeque::new(),
            max_history: 1000,
            conflict_strategy,
        }
    }
    
    /// Register an archive for synchronization.
    pub fn register_archive(&mut self, archive: ArchiveId) -> Result<(), String> {
        if self.archives.contains_key(&archive.id) {
            return Err(format!("Archive already registered: {}", archive.id));
        }
        
        // Initialize empty index
        self.indexes.insert(archive.id.clone(), ArchiveIndex {
            archive_id: archive.id.clone(),
            record_count: 0,
            by_id: HashMap::new(),
            by_timestamp: Vec::new(),
            by_type: HashMap::new(),
            last_updated: Self::current_timestamp(),
        });
        
        self.archives.insert(archive.id.clone(), archive);
        Ok(())
    }
    
    /// Start synchronization from archive to live system.
    pub fn start_sync(
        &mut self,
        archive_id: &str,
        strategy: SyncStrategy,
    ) -> Result<String, String> {
        if !self.archives.contains_key(archive_id) {
            return Err(format!("Archive not found: {}", archive_id));
        }
        
        let sync_id = format!("sync-{}-{}", archive_id, Self::current_timestamp());
        
        let status = SyncStatus {
            sync_id: sync_id.clone(),
            started_at: Self::current_timestamp(),
            records_synced: 0,
            records_pending: self.get_sync_scope(archive_id, &strategy),
            state: SyncState::InProgress,
            errors: Vec::new(),
        };
        
        self.active_syncs.insert(sync_id.clone(), status);
        Ok(sync_id)
    }
    
    /// Get number of records in sync scope.
    fn get_sync_scope(&self, archive_id: &str, strategy: &SyncStrategy) -> u64 {
        let index = match self.indexes.get(archive_id) {
            Some(idx) => idx,
            None => return 0,
        };
        
        match strategy {
            SyncStrategy::Full => index.record_count,
            SyncStrategy::Incremental { since } => {
                index.by_timestamp.iter()
                    .filter(|(ts, _)| *ts > *since)
                    .count() as u64
            }
            SyncStrategy::Selective { record_types } => {
                record_types.iter()
                    .filter_map(|rt| index.by_type.get(rt))
                    .map(|ids| ids.len() as u64)
                    .sum()
            }
            SyncStrategy::Priority { .. } => index.record_count / 2, // Estimate
        }
    }
    
    /// Sync a single record from archive to live system.
    pub fn sync_record(
        &mut self,
        sync_id: &str,
        record: ArchiveRecord,
    ) -> Result<SyncRecordResult, SyncError> {
        let status = self.active_syncs.get_mut(sync_id)
            .ok_or(SyncError {
                record_id: record.record_id.clone(),
                error_type: SyncErrorType::RecordNotFound,
                message: "Sync operation not found".to_string(),
                occurred_at: Self::current_timestamp(),
                retry_count: 0,
            })?;
        
        // Verify record integrity
        let computed_hash = self.compute_content_hash(&record.content.data);
        if computed_hash != record.content.content_hash {
            let error = SyncError {
                record_id: record.record_id.clone(),
                error_type: SyncErrorType::ChecksumMismatch,
                message: "Content hash mismatch".to_string(),
                occurred_at: Self::current_timestamp(),
                retry_count: 0,
            };
            status.errors.push(error.clone());
            return Err(error);
        }
        
        // Update sync status
        status.records_synced += 1;
        status.records_pending = status.records_pending.saturating_sub(1);
        
        Ok(SyncRecordResult {
            record_id: record.record_id,
            synced_at: Self::current_timestamp(),
            conflicts: vec![],
            resolution: None,
        })
    }
    
    /// Complete synchronization operation.
    pub fn complete_sync(&mut self, sync_id: &str) -> Result<SyncStatus, String> {
        let mut status = self.active_syncs.remove(sync_id)
            .ok_or(format!("Sync operation not found: {}", sync_id))?;
        
        status.state = if status.errors.is_empty() {
            SyncState::Completed
        } else {
            SyncState::Failed
        };
        
        // Add to history
        self.sync_history.push_front(status.clone());
        if self.sync_history.len() > self.max_history {
            self.sync_history.pop_back();
        }
        
        Ok(status)
    }
    
    /// Query archive records by time range.
    pub fn query_by_time_range(
        &self,
        archive_id: &str,
        start: u64,
        end: u64,
    ) -> Vec<String> {
        let index = match self.indexes.get(archive_id) {
            Some(idx) => idx,
            None => return vec![],
        };
        
        index.by_timestamp.iter()
            .filter(|(ts, _)| *ts >= start && *ts <= end)
            .map(|(_, id)| id.clone())
            .collect()
    }
    
    /// Query archive records by type.
    pub fn query_by_type(
        &self,
        archive_id: &str,
        record_type: &str,
    ) -> Vec<String> {
        let index = match self.indexes.get(archive_id) {
            Some(idx) => idx,
            None => return vec![],
        };
        
        index.by_type.get(record_type)
            .cloned()
            .unwrap_or_default()
    }
    
    /// Verify provenance of an archive record.
    pub fn verify_provenance(&self, record: &ArchiveRecord) -> ProvenanceVerification {
        let mut verification = ProvenanceVerification {
            record_id: record.record_id.clone(),
            creator_verified: false,
            custody_chain_complete: true,
            attestations_valid: true,
            verification_timestamp: Self::current_timestamp(),
            issues: vec![],
        };
        
        // Verify creator exists and is recognized
        if record.provenance.creator.is_empty() {
            verification.issues.push("Missing creator".to_string());
        } else {
            verification.creator_verified = true;
        }
        
        // Verify custody chain continuity
        let mut prev_end: Option<u64> = None;
        for entry in &record.provenance.custody_chain {
            if let Some(prev) = prev_end {
                if entry.custody_start != prev {
                    verification.custody_chain_complete = false;
                    verification.issues.push(format!(
                        "Custody gap: {} to {}",
                        prev, entry.custody_start
                    ));
                }
            }
            prev_end = entry.custody_end;
        }
        
        // Verify attestations
        for entry in &record.provenance.custody_chain {
            if entry.custody_end.is_some() && entry.attestation.is_none() {
                verification.attestations_valid = false;
                verification.issues.push(format!(
                    "Missing attestation for custody transfer from {}",
                    entry.custodian
                ));
            }
        }
        
        verification
    }
    
    /// Reconcile archive with live system.
    pub fn reconcile(
        &self,
        archive_id: &str,
        live_records: &[String],
    ) -> ReconciliationResult {
        let index = match self.indexes.get(archive_id) {
            Some(idx) => idx,
            None => return ReconciliationResult {
                archive_id: archive_id.to_string(),
                in_archive_only: vec![],
                in_live_only: live_records.to_vec(),
                in_both: vec![],
                conflicts: vec![],
            },
        };
        
        let live_set: HashSet<&String> = live_records.iter().collect();
        let archive_set: HashSet<&String> = index.by_id.keys().collect();
        
        ReconciliationResult {
            archive_id: archive_id.to_string(),
            in_archive_only: archive_set.difference(&live_set)
                .map(|s| (*s).clone())
                .collect(),
            in_live_only: live_set.difference(&archive_set)
                .map(|s| (*s).clone())
                .collect(),
            in_both: archive_set.intersection(&live_set)
                .map(|s| (*s).clone())
                .collect(),
            conflicts: vec![], // Would require content comparison
        }
    }
    
    /// Add record to archive index.
    pub fn index_record(&mut self, archive_id: &str, record: &ArchiveRecord) {
        let index = match self.indexes.get_mut(archive_id) {
            Some(idx) => idx,
            None => return,
        };
        
        let meta = ArchiveRecordMeta {
            record_id: record.record_id.clone(),
            record_type: format!("{:?}", record.record_type),
            created_at: record.created_at,
            size_bytes: record.content.data.len() as u64,
            content_hash: record.content.content_hash,
        };
        
        // Update indexes
        index.by_id.insert(record.record_id.clone(), meta);
        index.by_timestamp.push((record.created_at, record.record_id.clone()));
        
        let type_key = format!("{:?}", record.record_type);
        index.by_type.entry(type_key)
            .or_insert_with(Vec::new)
            .push(record.record_id.clone());
        
        index.record_count += 1;
        index.last_updated = Self::current_timestamp();
    }
    
    /// Compute content hash (simplified - would use proper SHA-256).
    fn compute_content_hash(&self, data: &[u8]) -> [u8; 32] {
        let mut hash = [0u8; 32];
        for (i, byte) in data.iter().enumerate() {
            hash[i % 32] ^= byte;
        }
        hash
    }
    
    /// Get current timestamp.
    fn current_timestamp() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or(Duration::ZERO)
            .as_secs()
    }
}

/// Result of syncing a single record.
#[derive(Debug)]
pub struct SyncRecordResult {
    /// Record ID
    pub record_id: String,
    /// Sync timestamp
    pub synced_at: u64,
    /// Conflicts detected
    pub conflicts: Vec<RecordConflict>,
    /// Resolution applied (if any)
    pub resolution: Option<ConflictResolution>,
}

/// Record conflict during sync.
#[derive(Debug)]
pub struct RecordConflict {
    /// Field with conflict
    pub field: String,
    /// Source value
    pub source_value: String,
    /// Destination value
    pub destination_value: String,
}

/// Result of provenance verification.
#[derive(Debug)]
pub struct ProvenanceVerification {
    /// Record ID
    pub record_id: String,
    /// Whether creator was verified
    pub creator_verified: bool,
    /// Whether custody chain is complete
    pub custody_chain_complete: bool,
    /// Whether all attestations are valid
    pub attestations_valid: bool,
    /// Verification timestamp
    pub verification_timestamp: u64,
    /// Issues found
    pub issues: Vec<String>,
}

/// Result of reconciliation.
#[derive(Debug)]
pub struct ReconciliationResult {
    /// Archive ID
    pub archive_id: String,
    /// Records only in archive
    pub in_archive_only: Vec<String>,
    /// Records only in live system
    pub in_live_only: Vec<String>,
    /// Records in both
    pub in_both: Vec<String>,
    /// Conflicts detected
    pub conflicts: Vec<RecordConflict>,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_archive_registration() {
        let mut sync = ArchivalSync::new(ConflictResolution::PreferSource);
        let archive = ArchiveId {
            id: "test-archive".to_string(),
            coverage_start: 0,
            coverage_end: 1000000,
            generation: 1,
            canonical_location: "/archives/test".to_string(),
        };
        
        assert!(sync.register_archive(archive.clone()).is_ok());
        assert!(sync.register_archive(archive).is_err()); // Duplicate
    }
    
    #[test]
    fn test_sync_start() {
        let mut sync = ArchivalSync::new(ConflictResolution::PreferSource);
        let archive = ArchiveId {
            id: "test-archive".to_string(),
            coverage_start: 0,
            coverage_end: 1000000,
            generation: 1,
            canonical_location: "/archives/test".to_string(),
        };
        
        sync.register_archive(archive).unwrap();
        let sync_id = sync.start_sync("test-archive", SyncStrategy::Full);
        assert!(sync_id.is_ok());
    }
    
    #[test]
    fn test_provenance_verification() {
        let sync = ArchivalSync::new(ConflictResolution::PreferSource);
        let record = ArchiveRecord {
            record_id: "test-record".to_string(),
            created_at: 1000,
            modified_at: 1000,
            archived_at: 2000,
            record_type: RecordType::Snapshot {
                entity: "test".to_string(),
                snapshot_type: "full".to_string(),
            },
            content: ArchiveContent {
                format: ContentFormat::PlainText,
                data: b"test content".to_vec(),
                content_hash: [0; 32],
                compression: None,
                encryption: EncryptionStatus::Unencrypted,
            },
            provenance: ProvenanceChain {
                creator: "test-creator".to_string(),
                creation_context: CreationContext {
                    system: "test-system".to_string(),
                    system_version: "1.0".to_string(),
                    location: None,
                    epoch: "DISTRIBUTED-ERA".to_string(),
                },
                custody_chain: vec![],
                verified: false,
                last_verified: 0,
            },
            cross_refs: vec![],
        };
        
        let verification = sync.verify_provenance(&record);
        assert!(verification.creator_verified);
    }
}
