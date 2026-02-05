//! Fork Protocol for Grey Distributed
//!
//! This module implements the protocol for safe forks and divergence,
//! enabling controlled branching of the Grey Distributed ecosystem.

use std::collections::HashMap;
use std::time::{Duration, SystemTime};

// ============================================================================
// Core Types
// ============================================================================

/// Unique identifier for a fork
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ForkId(pub String);

impl ForkId {
    pub fn new(name: &str, version: &str) -> Self {
        let id = format!("{}-{}-{}", name, version, Self::generate_suffix());
        ForkId(id)
    }

    fn generate_suffix() -> String {
        // Generate unique suffix based on timestamp and random bytes
        let timestamp = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        format!("{:x}", timestamp)
    }
}

/// Type of fork being created
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForkType {
    /// Experimental fork for testing new features
    Experimental {
        expiry: Option<SystemTime>,
        auto_merge: bool,
    },
    /// Long-term fork with independent governance
    LongTerm {
        governance_model: GovernanceModel,
        reconciliation_policy: ReconciliationPolicy,
    },
    /// Hard fork with no intention to reconcile
    Hard {
        reason: String,
        divergence_point: String,
    },
    /// Regional fork for sovereignty requirements
    Regional {
        region: String,
        compliance_requirements: Vec<String>,
    },
    /// Vendor fork for enterprise customizations
    Vendor {
        vendor_id: String,
        upstream_tracking: bool,
    },
}

/// Governance model for a fork
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GovernanceModel {
    /// Fork inherits governance from upstream
    Inherited,
    /// Fork has independent governance
    Independent {
        council_size: usize,
        voting_threshold: f64,
    },
    /// Fork has delegated governance
    Delegated {
        delegate_to: ForkId,
    },
    /// Fork uses hybrid governance
    Hybrid {
        local_decisions: Vec<String>,
        upstream_decisions: Vec<String>,
    },
}

/// Policy for reconciling divergent forks
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReconciliationPolicy {
    /// No reconciliation planned
    None,
    /// Automatic reconciliation when conditions met
    Automatic {
        trigger_conditions: Vec<String>,
        merge_strategy: MergeStrategy,
    },
    /// Manual reconciliation with review process
    Manual {
        review_required: bool,
        approval_threshold: usize,
    },
    /// Periodic reconciliation on schedule
    Periodic {
        interval: Duration,
        merge_strategy: MergeStrategy,
    },
}

/// Strategy for merging fork changes
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MergeStrategy {
    /// Upstream changes take precedence
    UpstreamFirst,
    /// Fork changes take precedence
    ForkFirst,
    /// Conflict resolution required for all differences
    RequireResolution,
    /// Three-way merge with common ancestor
    ThreeWay,
    /// Cherry-pick only marked changes
    CherryPick { cherry_pick_labels: Vec<String> },
}

/// State of a fork
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForkState {
    /// Fork proposal submitted, awaiting approval
    Proposed { 
        proposed_at: SystemTime,
        proposer: String,
    },
    /// Fork approved, initialization in progress
    Initializing {
        approved_at: SystemTime,
        progress_pct: u8,
    },
    /// Fork active and operational
    Active {
        activated_at: SystemTime,
        health: ForkHealth,
    },
    /// Fork suspended (governance decision)
    Suspended {
        suspended_at: SystemTime,
        reason: String,
    },
    /// Fork being reconciled with upstream
    Reconciling {
        started_at: SystemTime,
        target: ForkId,
    },
    /// Fork archived (read-only)
    Archived {
        archived_at: SystemTime,
        reason: String,
    },
    /// Fork terminated
    Terminated {
        terminated_at: SystemTime,
        reason: String,
    },
}

/// Health status of a fork
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForkHealth {
    Healthy,
    Degraded { issues: Vec<String> },
    Critical { issues: Vec<String> },
    Unknown,
}

// ============================================================================
// Fork Manifest
// ============================================================================

/// Complete manifest for a fork
#[derive(Debug, Clone)]
pub struct ForkManifest {
    /// Unique fork identifier
    pub id: ForkId,
    /// Human-readable name
    pub name: String,
    /// Fork description
    pub description: String,
    /// Type of fork
    pub fork_type: ForkType,
    /// Current state
    pub state: ForkState,
    /// Parent fork (None for root)
    pub parent: Option<ForkId>,
    /// Version at fork point
    pub fork_point_version: String,
    /// Commit hash at fork point
    pub fork_point_commit: String,
    /// Maintainers
    pub maintainers: Vec<String>,
    /// Creation timestamp
    pub created_at: SystemTime,
    /// Metadata
    pub metadata: HashMap<String, String>,
}

impl ForkManifest {
    pub fn new(
        name: &str,
        description: &str,
        fork_type: ForkType,
        parent: Option<ForkId>,
        fork_point_version: &str,
        fork_point_commit: &str,
    ) -> Self {
        Self {
            id: ForkId::new(name, fork_point_version),
            name: name.to_string(),
            description: description.to_string(),
            fork_type,
            state: ForkState::Proposed {
                proposed_at: SystemTime::now(),
                proposer: String::new(), // Set by caller
            },
            parent,
            fork_point_version: fork_point_version.to_string(),
            fork_point_commit: fork_point_commit.to_string(),
            maintainers: Vec::new(),
            created_at: SystemTime::now(),
            metadata: HashMap::new(),
        }
    }
}

// ============================================================================
// Fork Protocol
// ============================================================================

/// Protocol for creating, managing, and reconciling forks
pub struct ForkProtocol {
    /// Active forks registry
    forks: HashMap<ForkId, ForkManifest>,
    /// Fork rules and policies
    rules: ForkRules,
    /// Event listeners
    listeners: Vec<Box<dyn ForkEventListener>>,
}

/// Rules governing fork creation and management
#[derive(Debug, Clone)]
pub struct ForkRules {
    /// Minimum approval threshold for fork creation
    pub min_approval_threshold: f64,
    /// Required reviewers for fork proposals
    pub required_reviewers: usize,
    /// Maximum active forks per organization
    pub max_active_forks: usize,
    /// Automatic reconciliation interval
    pub auto_reconcile_interval: Duration,
    /// Fork naming conventions
    pub naming_pattern: String,
    /// Allowed fork types
    pub allowed_fork_types: Vec<ForkType>,
    /// Require divergence justification
    pub require_justification: bool,
}

impl Default for ForkRules {
    fn default() -> Self {
        Self {
            min_approval_threshold: 0.66,
            required_reviewers: 3,
            max_active_forks: 10,
            auto_reconcile_interval: Duration::from_secs(86400 * 30), // 30 days
            naming_pattern: r"^[a-z][a-z0-9-]*$".to_string(),
            allowed_fork_types: vec![
                ForkType::Experimental { expiry: None, auto_merge: false },
                ForkType::LongTerm { 
                    governance_model: GovernanceModel::Inherited,
                    reconciliation_policy: ReconciliationPolicy::None,
                },
            ],
            require_justification: true,
        }
    }
}

/// Events emitted by the fork protocol
#[derive(Debug, Clone)]
pub enum ForkEvent {
    ForkProposed { fork_id: ForkId, proposer: String },
    ForkApproved { fork_id: ForkId, approvers: Vec<String> },
    ForkRejected { fork_id: ForkId, reason: String },
    ForkActivated { fork_id: ForkId },
    ForkSuspended { fork_id: ForkId, reason: String },
    ForkReconciliationStarted { fork_id: ForkId, target: ForkId },
    ForkReconciliationCompleted { fork_id: ForkId, target: ForkId },
    ForkReconciliationFailed { fork_id: ForkId, reason: String },
    ForkArchived { fork_id: ForkId },
    ForkTerminated { fork_id: ForkId },
    DivergenceDetected { fork_id: ForkId, divergence_score: f64 },
}

/// Listener for fork events
pub trait ForkEventListener: Send + Sync {
    fn on_event(&self, event: &ForkEvent);
}

impl ForkProtocol {
    pub fn new(rules: ForkRules) -> Self {
        Self {
            forks: HashMap::new(),
            rules,
            listeners: Vec::new(),
        }
    }

    /// Propose a new fork
    pub fn propose_fork(&mut self, manifest: ForkManifest) -> Result<ForkId, ForkError> {
        // Validate naming convention
        if !self.validate_name(&manifest.name) {
            return Err(ForkError::InvalidName(manifest.name.clone()));
        }

        // Check fork limit
        let active_count = self.forks.values()
            .filter(|f| matches!(f.state, ForkState::Active { .. }))
            .count();
        if active_count >= self.rules.max_active_forks {
            return Err(ForkError::ForkLimitReached(self.rules.max_active_forks));
        }

        // Validate justification if required
        if self.rules.require_justification && manifest.description.is_empty() {
            return Err(ForkError::JustificationRequired);
        }

        let fork_id = manifest.id.clone();
        self.forks.insert(fork_id.clone(), manifest);
        
        self.emit_event(ForkEvent::ForkProposed {
            fork_id: fork_id.clone(),
            proposer: "system".to_string(),
        });

        Ok(fork_id)
    }

    /// Approve a fork proposal
    pub fn approve_fork(
        &mut self,
        fork_id: &ForkId,
        approvers: Vec<String>,
    ) -> Result<(), ForkError> {
        let manifest = self.forks.get_mut(fork_id)
            .ok_or_else(|| ForkError::ForkNotFound(fork_id.clone()))?;

        // Verify we have enough approvers
        if approvers.len() < self.rules.required_reviewers {
            return Err(ForkError::InsufficientApprovers {
                required: self.rules.required_reviewers,
                provided: approvers.len(),
            });
        }

        // Transition to initializing state
        manifest.state = ForkState::Initializing {
            approved_at: SystemTime::now(),
            progress_pct: 0,
        };

        self.emit_event(ForkEvent::ForkApproved {
            fork_id: fork_id.clone(),
            approvers,
        });

        Ok(())
    }

    /// Activate a fork after initialization
    pub fn activate_fork(&mut self, fork_id: &ForkId) -> Result<(), ForkError> {
        let manifest = self.forks.get_mut(fork_id)
            .ok_or_else(|| ForkError::ForkNotFound(fork_id.clone()))?;

        manifest.state = ForkState::Active {
            activated_at: SystemTime::now(),
            health: ForkHealth::Healthy,
        };

        self.emit_event(ForkEvent::ForkActivated {
            fork_id: fork_id.clone(),
        });

        Ok(())
    }

    /// Start reconciliation between fork and target
    pub fn start_reconciliation(
        &mut self,
        fork_id: &ForkId,
        target_id: &ForkId,
    ) -> Result<ReconciliationSession, ForkError> {
        let manifest = self.forks.get_mut(fork_id)
            .ok_or_else(|| ForkError::ForkNotFound(fork_id.clone()))?;

        manifest.state = ForkState::Reconciling {
            started_at: SystemTime::now(),
            target: target_id.clone(),
        };

        self.emit_event(ForkEvent::ForkReconciliationStarted {
            fork_id: fork_id.clone(),
            target: target_id.clone(),
        });

        Ok(ReconciliationSession {
            fork_id: fork_id.clone(),
            target_id: target_id.clone(),
            started_at: SystemTime::now(),
            conflicts: Vec::new(),
            resolved_conflicts: Vec::new(),
        })
    }

    /// Archive a fork
    pub fn archive_fork(&mut self, fork_id: &ForkId, reason: &str) -> Result<(), ForkError> {
        let manifest = self.forks.get_mut(fork_id)
            .ok_or_else(|| ForkError::ForkNotFound(fork_id.clone()))?;

        manifest.state = ForkState::Archived {
            archived_at: SystemTime::now(),
            reason: reason.to_string(),
        };

        self.emit_event(ForkEvent::ForkArchived {
            fork_id: fork_id.clone(),
        });

        Ok(())
    }

    /// Calculate divergence between fork and upstream
    pub fn calculate_divergence(&self, fork_id: &ForkId) -> Result<DivergenceReport, ForkError> {
        let manifest = self.forks.get(fork_id)
            .ok_or_else(|| ForkError::ForkNotFound(fork_id.clone()))?;

        // Placeholder divergence calculation
        Ok(DivergenceReport {
            fork_id: fork_id.clone(),
            upstream_id: manifest.parent.clone(),
            commits_behind: 0,
            commits_ahead: 0,
            files_changed: Vec::new(),
            divergence_score: 0.0,
            last_common_commit: manifest.fork_point_commit.clone(),
            analyzed_at: SystemTime::now(),
        })
    }

    fn validate_name(&self, name: &str) -> bool {
        let pattern = regex::Regex::new(&self.rules.naming_pattern)
            .unwrap_or_else(|_| regex::Regex::new(r".*").unwrap());
        pattern.is_match(name)
    }

    fn emit_event(&self, event: ForkEvent) {
        for listener in &self.listeners {
            listener.on_event(&event);
        }
    }

    pub fn add_listener(&mut self, listener: Box<dyn ForkEventListener>) {
        self.listeners.push(listener);
    }
}

// ============================================================================
// Reconciliation
// ============================================================================

/// Session for reconciling a fork with its target
#[derive(Debug)]
pub struct ReconciliationSession {
    pub fork_id: ForkId,
    pub target_id: ForkId,
    pub started_at: SystemTime,
    pub conflicts: Vec<Conflict>,
    pub resolved_conflicts: Vec<ResolvedConflict>,
}

/// A conflict between fork and target
#[derive(Debug, Clone)]
pub struct Conflict {
    pub path: String,
    pub conflict_type: ConflictType,
    pub fork_content: String,
    pub target_content: String,
    pub common_ancestor_content: Option<String>,
}

#[derive(Debug, Clone)]
pub enum ConflictType {
    ContentDiffers,
    AddedInBoth,
    DeletedInFork,
    DeletedInTarget,
    MovedDifferently,
}

/// A resolved conflict
#[derive(Debug, Clone)]
pub struct ResolvedConflict {
    pub conflict: Conflict,
    pub resolution: Resolution,
    pub resolved_by: String,
    pub resolved_at: SystemTime,
}

#[derive(Debug, Clone)]
pub enum Resolution {
    UseFork,
    UseTarget,
    UseCommonAncestor,
    CustomMerge(String),
}

impl ReconciliationSession {
    pub fn add_conflict(&mut self, conflict: Conflict) {
        self.conflicts.push(conflict);
    }

    pub fn resolve_conflict(
        &mut self,
        conflict_index: usize,
        resolution: Resolution,
        resolved_by: &str,
    ) -> Result<(), ForkError> {
        if conflict_index >= self.conflicts.len() {
            return Err(ForkError::ConflictNotFound(conflict_index));
        }

        let conflict = self.conflicts.remove(conflict_index);
        self.resolved_conflicts.push(ResolvedConflict {
            conflict,
            resolution,
            resolved_by: resolved_by.to_string(),
            resolved_at: SystemTime::now(),
        });

        Ok(())
    }

    pub fn is_complete(&self) -> bool {
        self.conflicts.is_empty()
    }

    pub fn pending_conflicts(&self) -> usize {
        self.conflicts.len()
    }
}

// ============================================================================
// Divergence Analysis
// ============================================================================

/// Report on divergence between fork and upstream
#[derive(Debug)]
pub struct DivergenceReport {
    pub fork_id: ForkId,
    pub upstream_id: Option<ForkId>,
    pub commits_behind: usize,
    pub commits_ahead: usize,
    pub files_changed: Vec<FileChange>,
    pub divergence_score: f64,
    pub last_common_commit: String,
    pub analyzed_at: SystemTime,
}

#[derive(Debug)]
pub struct FileChange {
    pub path: String,
    pub change_type: ChangeType,
    pub lines_added: usize,
    pub lines_removed: usize,
}

#[derive(Debug)]
pub enum ChangeType {
    Added,
    Modified,
    Deleted,
    Renamed { from: String },
}

impl DivergenceReport {
    pub fn is_highly_diverged(&self) -> bool {
        self.divergence_score > 0.5
    }

    pub fn needs_reconciliation(&self) -> bool {
        self.commits_behind > 100 || self.divergence_score > 0.3
    }
}

// ============================================================================
// Errors
// ============================================================================

#[derive(Debug, Clone)]
pub enum ForkError {
    ForkNotFound(ForkId),
    InvalidName(String),
    ForkLimitReached(usize),
    JustificationRequired,
    InsufficientApprovers { required: usize, provided: usize },
    ConflictNotFound(usize),
    ReconciliationInProgress,
    InvalidStateTransition { from: String, to: String },
    UpstreamNotFound,
    PermissionDenied(String),
}

impl std::fmt::Display for ForkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ForkError::ForkNotFound(id) => write!(f, "Fork not found: {:?}", id),
            ForkError::InvalidName(name) => write!(f, "Invalid fork name: {}", name),
            ForkError::ForkLimitReached(limit) => write!(f, "Fork limit reached: {}", limit),
            ForkError::JustificationRequired => write!(f, "Fork justification required"),
            ForkError::InsufficientApprovers { required, provided } => {
                write!(f, "Insufficient approvers: {} required, {} provided", required, provided)
            }
            ForkError::ConflictNotFound(idx) => write!(f, "Conflict not found: {}", idx),
            ForkError::ReconciliationInProgress => write!(f, "Reconciliation already in progress"),
            ForkError::InvalidStateTransition { from, to } => {
                write!(f, "Invalid state transition: {} -> {}", from, to)
            }
            ForkError::UpstreamNotFound => write!(f, "Upstream fork not found"),
            ForkError::PermissionDenied(reason) => write!(f, "Permission denied: {}", reason),
        }
    }
}

impl std::error::Error for ForkError {}

// ============================================================================
// Fork Registry
// ============================================================================

/// Global registry of known forks
pub struct ForkRegistry {
    /// Known forks by ID
    forks: HashMap<ForkId, ForkManifest>,
    /// Fork hierarchy (parent -> children)
    hierarchy: HashMap<ForkId, Vec<ForkId>>,
    /// Fork protocol instance
    protocol: ForkProtocol,
}

impl ForkRegistry {
    pub fn new(rules: ForkRules) -> Self {
        Self {
            forks: HashMap::new(),
            hierarchy: HashMap::new(),
            protocol: ForkProtocol::new(rules),
        }
    }

    /// Register a new fork
    pub fn register(&mut self, manifest: ForkManifest) -> Result<ForkId, ForkError> {
        let fork_id = self.protocol.propose_fork(manifest.clone())?;
        
        // Update hierarchy
        if let Some(ref parent_id) = manifest.parent {
            self.hierarchy
                .entry(parent_id.clone())
                .or_insert_with(Vec::new)
                .push(fork_id.clone());
        }

        self.forks.insert(fork_id.clone(), manifest);
        Ok(fork_id)
    }

    /// Get fork by ID
    pub fn get(&self, fork_id: &ForkId) -> Option<&ForkManifest> {
        self.forks.get(fork_id)
    }

    /// Get children of a fork
    pub fn get_children(&self, fork_id: &ForkId) -> Vec<&ForkManifest> {
        self.hierarchy
            .get(fork_id)
            .map(|children| {
                children.iter()
                    .filter_map(|id| self.forks.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get all active forks
    pub fn active_forks(&self) -> Vec<&ForkManifest> {
        self.forks.values()
            .filter(|f| matches!(f.state, ForkState::Active { .. }))
            .collect()
    }

    /// Get fork lineage (ancestor chain)
    pub fn get_lineage(&self, fork_id: &ForkId) -> Vec<&ForkManifest> {
        let mut lineage = Vec::new();
        let mut current = self.forks.get(fork_id);

        while let Some(manifest) = current {
            lineage.push(manifest);
            current = manifest.parent.as_ref().and_then(|p| self.forks.get(p));
        }

        lineage
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fork_creation() {
        let mut protocol = ForkProtocol::new(ForkRules::default());
        
        let manifest = ForkManifest::new(
            "test-fork",
            "Test fork for experimentation",
            ForkType::Experimental { expiry: None, auto_merge: false },
            None,
            "1.0.0",
            "abc123",
        );

        let result = protocol.propose_fork(manifest);
        assert!(result.is_ok());
    }

    #[test]
    fn test_fork_approval() {
        let mut protocol = ForkProtocol::new(ForkRules::default());
        
        let manifest = ForkManifest::new(
            "test-fork",
            "Test fork",
            ForkType::Experimental { expiry: None, auto_merge: false },
            None,
            "1.0.0",
            "abc123",
        );

        let fork_id = protocol.propose_fork(manifest).unwrap();
        
        // Should fail with insufficient approvers
        let result = protocol.approve_fork(&fork_id, vec!["alice".to_string()]);
        assert!(matches!(result, Err(ForkError::InsufficientApprovers { .. })));

        // Should succeed with enough approvers
        let result = protocol.approve_fork(
            &fork_id, 
            vec!["alice".to_string(), "bob".to_string(), "charlie".to_string()]
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_reconciliation_session() {
        let mut session = ReconciliationSession {
            fork_id: ForkId::new("fork", "1.0.0"),
            target_id: ForkId::new("upstream", "1.0.0"),
            started_at: SystemTime::now(),
            conflicts: Vec::new(),
            resolved_conflicts: Vec::new(),
        };

        session.add_conflict(Conflict {
            path: "src/lib.rs".to_string(),
            conflict_type: ConflictType::ContentDiffers,
            fork_content: "fork version".to_string(),
            target_content: "target version".to_string(),
            common_ancestor_content: Some("original".to_string()),
        });

        assert!(!session.is_complete());
        assert_eq!(session.pending_conflicts(), 1);

        session.resolve_conflict(0, Resolution::UseFork, "maintainer").unwrap();

        assert!(session.is_complete());
        assert_eq!(session.pending_conflicts(), 0);
    }
}
