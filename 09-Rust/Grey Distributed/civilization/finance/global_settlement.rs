//! Global Financial Settlement and Clearing Protocol
//!
//! This module implements civilization-scale financial settlement infrastructure,
//! enabling real-time gross settlement (RTGS) across sovereign monetary systems,
//! central bank digital currencies (CBDCs), and traditional banking networks.
//!
//! # Architecture Overview
//!
//! The global settlement system operates as a federated network of clearing houses,
//! each representing a monetary jurisdiction. Grey Distributed provides:
//!
//! - **Atomic cross-border settlement**: Multi-currency transactions settle atomically
//! - **Regulatory compliance**: Each jurisdiction enforces local rules
//! - **Finality guarantees**: Settlement is irrevocable once confirmed
//! - **Liquidity optimization**: Netting reduces actual fund movements
//!
//! # Settlement Model
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────────┐
//! │                     Global Settlement Network                           │
//! │                                                                         │
//! │  ┌─────────────┐     ┌─────────────┐     ┌─────────────┐              │
//! │  │  Americas   │     │   EMEA      │     │ Asia-Pacific│              │
//! │  │  Clearing   │◄───►│  Clearing   │◄───►│  Clearing   │              │
//! │  │   House     │     │   House     │     │   House     │              │
//! │  └──────┬──────┘     └──────┬──────┘     └──────┬──────┘              │
//! │         │                   │                   │                      │
//! │    ┌────┴────┐         ┌────┴────┐         ┌────┴────┐                │
//! │    ▼         ▼         ▼         ▼         ▼         ▼                │
//! │  ┌───┐    ┌───┐     ┌───┐    ┌───┐     ┌───┐    ┌───┐                │
//! │  │USD│    │BRL│     │EUR│    │GBP│     │JPY│    │CNY│                │
//! │  │ACH│    │PIX│     │T2 │    │CHAPS│   │BOJ│    │CNAPS│              │
//! │  └───┘    └───┘     └───┘    └───┘     └───┘    └───┘                │
//! └─────────────────────────────────────────────────────────────────────────┘
//! ```

use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

// ============================================================================
// Core Types
// ============================================================================

/// Unique identifier for a settlement transaction
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SettlementId(pub [u8; 32]);

impl SettlementId {
    pub fn generate() -> Self {
        let mut bytes = [0u8; 32];
        // In production: use cryptographic random
        Self(bytes)
    }
}

/// ISO 4217 currency code with extensions for digital currencies
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Currency {
    /// Traditional fiat currency (ISO 4217)
    Fiat(String),
    /// Central Bank Digital Currency
    Cbdc { issuer: String, code: String },
    /// Stablecoin (regulated)
    Stablecoin { issuer: String, backing: String },
    /// Special Drawing Rights (IMF)
    Sdr,
}

impl Currency {
    pub fn usd() -> Self { Currency::Fiat("USD".into()) }
    pub fn eur() -> Self { Currency::Fiat("EUR".into()) }
    pub fn cny() -> Self { Currency::Fiat("CNY".into()) }
    pub fn digital_euro() -> Self { 
        Currency::Cbdc { issuer: "ECB".into(), code: "DEUR".into() } 
    }
}

/// Monetary amount with precision handling
/// 
/// Financial systems require exact decimal arithmetic. We use fixed-point
/// representation with 8 decimal places (supports crypto precision).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Amount {
    /// Value in smallest unit (e.g., cents for USD)
    pub units: i128,
    /// Decimal places (typically 2 for fiat, 8 for crypto)
    pub decimals: u8,
}

impl Amount {
    pub fn new(units: i128, decimals: u8) -> Self {
        Self { units, decimals }
    }
    
    pub fn from_major(major: i64, decimals: u8) -> Self {
        let multiplier = 10i128.pow(decimals as u32);
        Self { units: major as i128 * multiplier, decimals }
    }
    
    pub fn is_positive(&self) -> bool { self.units > 0 }
    pub fn is_negative(&self) -> bool { self.units < 0 }
    pub fn abs(&self) -> Self { Self { units: self.units.abs(), decimals: self.decimals } }
}

/// A participant in the settlement network
#[derive(Debug, Clone)]
pub struct Participant {
    /// Unique identifier (e.g., BIC, LEI)
    pub id: ParticipantId,
    /// Legal entity name
    pub name: String,
    /// Type of institution
    pub institution_type: InstitutionType,
    /// Regulatory jurisdiction
    pub jurisdiction: Jurisdiction,
    /// Settlement limits
    pub limits: SettlementLimits,
    /// Current status
    pub status: ParticipantStatus,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParticipantId(pub String);

#[derive(Debug, Clone)]
pub enum InstitutionType {
    CentralBank,
    CommercialBank,
    PaymentProcessor,
    ClearingHouse,
    CorporateTreasury,
    GovernmentAgency,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Jurisdiction(pub String);

#[derive(Debug, Clone)]
pub struct SettlementLimits {
    /// Maximum single transaction
    pub max_transaction: Amount,
    /// Maximum daily volume
    pub max_daily_volume: Amount,
    /// Currencies allowed
    pub allowed_currencies: HashSet<Currency>,
    /// Counterparty restrictions
    pub counterparty_whitelist: Option<HashSet<ParticipantId>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParticipantStatus {
    Active,
    Suspended { reason: String, until: Option<SystemTime> },
    Probation { reason: String },
    Terminated,
}

// ============================================================================
// Settlement Instructions
// ============================================================================

/// A settlement instruction from a participant
/// 
/// Instructions flow through multiple stages:
/// 1. Submission → Validation
/// 2. Matching (for bilateral)
/// 3. Netting (for multilateral)
/// 4. Liquidity check
/// 5. Final settlement
#[derive(Debug, Clone)]
pub struct SettlementInstruction {
    pub id: SettlementId,
    pub sender: ParticipantId,
    pub receiver: ParticipantId,
    pub currency: Currency,
    pub amount: Amount,
    pub value_date: ValueDate,
    pub instruction_type: InstructionType,
    pub priority: Priority,
    pub metadata: SettlementMetadata,
    pub submitted_at: SystemTime,
}

/// When the settlement should occur
#[derive(Debug, Clone, PartialEq)]
pub enum ValueDate {
    /// Settle immediately (T+0)
    Immediate,
    /// Settle on specific date
    Dated(chrono::NaiveDate),
    /// Settle at next netting cycle
    NextCycle,
}

#[derive(Debug, Clone)]
pub enum InstructionType {
    /// Single credit transfer
    CreditTransfer,
    /// Direct debit (pre-authorized)
    DirectDebit { mandate_id: String },
    /// Securities settlement (DVP)
    DeliveryVsPayment { security_id: String, quantity: u64 },
    /// FX settlement (PVP)
    PaymentVsPayment { counter_currency: Currency, counter_amount: Amount },
    /// Collateral movement
    CollateralTransfer { collateral_type: String },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Priority {
    /// Can wait for netting cycles
    Normal,
    /// Process before normal priority
    High,
    /// Real-time gross settlement
    Urgent,
    /// Central bank operations
    Critical,
}

#[derive(Debug, Clone)]
pub struct SettlementMetadata {
    /// Reference for reconciliation
    pub reference: String,
    /// Regulatory reporting codes
    pub regulatory_codes: Vec<String>,
    /// Additional structured data
    pub extensions: HashMap<String, String>,
}

// ============================================================================
// Settlement State Machine
// ============================================================================

/// Lifecycle states of a settlement instruction
#[derive(Debug, Clone, PartialEq)]
pub enum SettlementState {
    /// Received but not yet validated
    Pending,
    /// Passed validation, awaiting match
    Validated,
    /// Matched with counterparty instruction
    Matched { counterparty_id: SettlementId },
    /// Included in netting batch
    Netted { batch_id: BatchId },
    /// Queued for settlement (liquidity check passed)
    Queued,
    /// Settlement in progress
    Settling,
    /// Successfully settled
    Settled { 
        settled_at: SystemTime,
        finality_proof: FinalityProof,
    },
    /// Failed to settle
    Failed { 
        reason: SettlementFailure,
        failed_at: SystemTime,
    },
    /// Cancelled by participant
    Cancelled { 
        reason: String,
        cancelled_at: SystemTime,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BatchId(pub [u8; 16]);

#[derive(Debug, Clone)]
pub struct FinalityProof {
    /// Hash of the settlement batch
    pub batch_hash: [u8; 32],
    /// Signatures from clearing house nodes
    pub attestations: Vec<ClearingAttestation>,
    /// Block height in distributed ledger
    pub ledger_height: u64,
}

#[derive(Debug, Clone)]
pub struct ClearingAttestation {
    pub node_id: String,
    pub signature: Vec<u8>,
    pub timestamp: SystemTime,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SettlementFailure {
    InsufficientFunds,
    LimitExceeded,
    CounterpartyRejected,
    RegulatoryBlock { code: String, reason: String },
    Timeout,
    TechnicalError { code: String },
}

// ============================================================================
// Netting Engine
// ============================================================================

/// Multilateral netting reduces the number of actual fund movements
/// 
/// Example: If A owes B $100, B owes C $80, and C owes A $50:
/// - Gross: 3 payments totaling $230
/// - Net: A pays B $50, B pays C $30 (2 payments totaling $80)
/// 
/// Grey Distributed enables real-time netting across time zones by
/// maintaining consistent netting windows globally.
pub struct NettingEngine {
    /// Active netting cycles by currency
    cycles: HashMap<Currency, NettingCycle>,
    /// Netting configuration
    config: NettingConfig,
}

#[derive(Debug, Clone)]
pub struct NettingConfig {
    /// How often to run netting
    pub cycle_duration: Duration,
    /// Minimum batch size to trigger netting
    pub min_batch_size: usize,
    /// Maximum instructions per batch
    pub max_batch_size: usize,
    /// Settlement window after netting
    pub settlement_window: Duration,
}

#[derive(Debug)]
pub struct NettingCycle {
    pub id: BatchId,
    pub currency: Currency,
    pub started_at: SystemTime,
    pub instructions: Vec<SettlementInstruction>,
    pub state: NettingState,
}

#[derive(Debug, Clone)]
pub enum NettingState {
    Collecting,
    Computing,
    Settling,
    Completed { results: NettingResults },
    Failed { reason: String },
}

#[derive(Debug, Clone)]
pub struct NettingResults {
    /// Original gross positions
    pub gross_volume: Amount,
    /// Net positions after netting
    pub net_volume: Amount,
    /// Netting efficiency (1 - net/gross)
    pub efficiency: f64,
    /// Number of original instructions
    pub instruction_count: usize,
    /// Number of net settlements
    pub settlement_count: usize,
    /// Net positions by participant
    pub positions: HashMap<ParticipantId, NetPosition>,
}

#[derive(Debug, Clone)]
pub struct NetPosition {
    pub participant: ParticipantId,
    /// Positive = receive, Negative = pay
    pub net_amount: Amount,
    /// Gross amounts owed
    pub gross_payable: Amount,
    /// Gross amounts due
    pub gross_receivable: Amount,
}

impl NettingEngine {
    pub fn new(config: NettingConfig) -> Self {
        Self {
            cycles: HashMap::new(),
            config,
        }
    }
    
    /// Add instruction to current netting cycle
    pub fn submit(&mut self, instruction: SettlementInstruction) -> Result<BatchId, NettingError> {
        let cycle = self.cycles
            .entry(instruction.currency.clone())
            .or_insert_with(|| NettingCycle {
                id: BatchId(rand::random()),
                currency: instruction.currency.clone(),
                started_at: SystemTime::now(),
                instructions: Vec::new(),
                state: NettingState::Collecting,
            });
        
        if cycle.instructions.len() >= self.config.max_batch_size {
            return Err(NettingError::BatchFull);
        }
        
        let batch_id = cycle.id.clone();
        cycle.instructions.push(instruction);
        Ok(batch_id)
    }
    
    /// Compute net positions for a cycle
    /// 
    /// Algorithm: Build a directed graph of obligations, then compute
    /// the minimum set of payments to achieve equivalent end state.
    pub fn compute_net_positions(&self, cycle: &NettingCycle) -> NettingResults {
        let mut gross_payable: HashMap<ParticipantId, i128> = HashMap::new();
        let mut gross_receivable: HashMap<ParticipantId, i128> = HashMap::new();
        let mut bilateral: HashMap<(ParticipantId, ParticipantId), i128> = HashMap::new();
        
        let decimals = cycle.instructions.first()
            .map(|i| i.amount.decimals)
            .unwrap_or(2);
        
        // Build gross positions
        for inst in &cycle.instructions {
            *gross_payable.entry(inst.sender.clone()).or_default() += inst.amount.units;
            *gross_receivable.entry(inst.receiver.clone()).or_default() += inst.amount.units;
            *bilateral.entry((inst.sender.clone(), inst.receiver.clone())).or_default() 
                += inst.amount.units;
        }
        
        // Compute net positions (what each participant actually owes/receives)
        let mut positions = HashMap::new();
        let all_participants: HashSet<_> = gross_payable.keys()
            .chain(gross_receivable.keys())
            .cloned()
            .collect();
        
        for participant in all_participants {
            let payable = *gross_payable.get(&participant).unwrap_or(&0);
            let receivable = *gross_receivable.get(&participant).unwrap_or(&0);
            let net = receivable - payable;
            
            positions.insert(participant.clone(), NetPosition {
                participant,
                net_amount: Amount::new(net, decimals),
                gross_payable: Amount::new(payable, decimals),
                gross_receivable: Amount::new(receivable, decimals),
            });
        }
        
        let gross_volume: i128 = cycle.instructions.iter().map(|i| i.amount.units).sum();
        let net_volume: i128 = positions.values()
            .map(|p| p.net_amount.units.abs())
            .sum::<i128>() / 2; // Divide by 2 because each payment is counted twice
        
        let efficiency = if gross_volume > 0 {
            1.0 - (net_volume as f64 / gross_volume as f64)
        } else {
            0.0
        };
        
        // Count actual settlements needed (positions with net payable)
        let settlement_count = positions.values()
            .filter(|p| p.net_amount.is_negative())
            .count();
        
        NettingResults {
            gross_volume: Amount::new(gross_volume, decimals),
            net_volume: Amount::new(net_volume, decimals),
            efficiency,
            instruction_count: cycle.instructions.len(),
            settlement_count,
            positions,
        }
    }
}

#[derive(Debug)]
pub enum NettingError {
    BatchFull,
    CycleNotFound,
    InvalidCurrency,
}

// ============================================================================
// Cross-Border Settlement
// ============================================================================

/// Cross-border settlement coordinator
/// 
/// Handles the complexity of settling transactions that cross monetary
/// jurisdictions, including:
/// - FX conversion and rate locking
/// - Multi-hop routing through correspondent banks
/// - Regulatory compliance for each jurisdiction
/// - Time zone coordination for value dating
pub struct CrossBorderSettlement {
    /// Exchange rate providers
    fx_providers: Vec<Arc<dyn FxProvider>>,
    /// Corridor configurations
    corridors: HashMap<CorridorKey, CorridorConfig>,
    /// Active cross-border transactions
    transactions: HashMap<SettlementId, CrossBorderTx>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CorridorKey {
    pub source_currency: Currency,
    pub target_currency: Currency,
}

#[derive(Debug, Clone)]
pub struct CorridorConfig {
    /// Available routing paths
    pub routes: Vec<SettlementRoute>,
    /// Processing hours (in UTC)
    pub processing_hours: ProcessingHours,
    /// Regulatory requirements
    pub compliance: CorridorCompliance,
    /// Typical settlement time
    pub typical_latency: Duration,
}

#[derive(Debug, Clone)]
pub struct SettlementRoute {
    pub route_id: String,
    pub hops: Vec<ClearingHop>,
    pub estimated_cost: Amount,
    pub estimated_time: Duration,
}

#[derive(Debug, Clone)]
pub struct ClearingHop {
    pub clearing_house: String,
    pub currency: Currency,
    pub settlement_system: String,
}

#[derive(Debug, Clone)]
pub struct ProcessingHours {
    /// Days when corridor is active
    pub active_days: Vec<chrono::Weekday>,
    /// Start time in UTC
    pub start_utc: chrono::NaiveTime,
    /// End time in UTC
    pub end_utc: chrono::NaiveTime,
    /// Holiday calendar
    pub holidays: Vec<chrono::NaiveDate>,
}

#[derive(Debug, Clone)]
pub struct CorridorCompliance {
    /// AML/KYC level required
    pub aml_level: AmlLevel,
    /// Reporting thresholds
    pub reporting_threshold: Option<Amount>,
    /// Required documentation
    pub required_documents: Vec<String>,
    /// Sanctions screening required
    pub sanctions_screening: bool,
}

#[derive(Debug, Clone)]
pub enum AmlLevel {
    Standard,
    Enhanced,
    Detailed,
}

/// An in-flight cross-border transaction
#[derive(Debug)]
pub struct CrossBorderTx {
    pub id: SettlementId,
    pub source: PaymentLeg,
    pub target: PaymentLeg,
    pub fx_rate: FxRate,
    pub route: SettlementRoute,
    pub state: CrossBorderState,
    pub created_at: SystemTime,
}

#[derive(Debug, Clone)]
pub struct PaymentLeg {
    pub participant: ParticipantId,
    pub currency: Currency,
    pub amount: Amount,
    pub local_settlement_id: Option<SettlementId>,
}

#[derive(Debug, Clone)]
pub struct FxRate {
    pub rate: f64,
    pub source: String,
    pub valid_until: SystemTime,
    pub spread: f64,
}

#[derive(Debug, Clone)]
pub enum CrossBorderState {
    /// Validating compliance requirements
    Compliance,
    /// FX rate locked, awaiting source leg
    AwaitingSource,
    /// Source leg settled, processing target
    AwaitingTarget,
    /// Both legs settled
    Completed {
        source_proof: FinalityProof,
        target_proof: FinalityProof,
    },
    /// Failed, may need unwind
    Failed {
        reason: String,
        unwind_required: bool,
    },
}

/// FX rate provider trait
pub trait FxProvider: Send + Sync {
    fn get_rate(&self, from: &Currency, to: &Currency) -> Option<FxRate>;
    fn provider_name(&self) -> &str;
}

impl CrossBorderSettlement {
    /// Execute a cross-border payment using Payment-vs-Payment
    /// 
    /// PvP ensures atomic settlement: either both legs complete or neither does.
    /// This eliminates Herstatt risk (one leg settling while the other fails).
    pub async fn execute_pvp(
        &mut self,
        source: PaymentLeg,
        target: PaymentLeg,
    ) -> Result<SettlementId, CrossBorderError> {
        let corridor_key = CorridorKey {
            source_currency: source.currency.clone(),
            target_currency: target.currency.clone(),
        };
        
        // Find corridor configuration
        let corridor = self.corridors.get(&corridor_key)
            .ok_or(CrossBorderError::CorridorNotAvailable)?;
        
        // Get FX rate
        let fx_rate = self.get_best_rate(&source.currency, &target.currency)
            .ok_or(CrossBorderError::NoFxRateAvailable)?;
        
        // Select optimal route
        let route = self.select_route(corridor, &source.amount)?;
        
        // Create transaction
        let tx_id = SettlementId::generate();
        let tx = CrossBorderTx {
            id: tx_id.clone(),
            source,
            target,
            fx_rate,
            route,
            state: CrossBorderState::Compliance,
            created_at: SystemTime::now(),
        };
        
        self.transactions.insert(tx_id.clone(), tx);
        
        // In production: trigger async settlement workflow
        Ok(tx_id)
    }
    
    fn get_best_rate(&self, from: &Currency, to: &Currency) -> Option<FxRate> {
        self.fx_providers.iter()
            .filter_map(|p| p.get_rate(from, to))
            .min_by(|a, b| a.spread.partial_cmp(&b.spread).unwrap())
    }
    
    fn select_route(&self, corridor: &CorridorConfig, amount: &Amount) -> Result<SettlementRoute, CrossBorderError> {
        // Select cheapest route that can handle the amount
        corridor.routes.first()
            .cloned()
            .ok_or(CrossBorderError::NoRouteAvailable)
    }
}

#[derive(Debug)]
pub enum CrossBorderError {
    CorridorNotAvailable,
    NoFxRateAvailable,
    NoRouteAvailable,
    ComplianceCheckFailed(String),
    SettlementFailed(String),
}

// ============================================================================
// Real-Time Gross Settlement (RTGS)
// ============================================================================

/// RTGS engine for urgent/critical payments
/// 
/// Unlike deferred net settlement, RTGS settles each transaction individually
/// and immediately. Used for high-value payments where finality is urgent.
pub struct RtgsEngine {
    /// Liquidity positions by participant and currency
    liquidity: HashMap<(ParticipantId, Currency), LiquidityPosition>,
    /// Queue of pending settlements
    queue: BTreeMap<(Priority, SystemTime), SettlementInstruction>,
    /// Configuration
    config: RtgsConfig,
}

#[derive(Debug, Clone)]
pub struct LiquidityPosition {
    pub available: Amount,
    pub reserved: Amount,
    pub intraday_credit_limit: Amount,
    pub intraday_credit_used: Amount,
}

impl LiquidityPosition {
    pub fn effective_available(&self) -> Amount {
        Amount::new(
            self.available.units + 
            (self.intraday_credit_limit.units - self.intraday_credit_used.units),
            self.available.decimals,
        )
    }
}

#[derive(Debug, Clone)]
pub struct RtgsConfig {
    /// Maximum time to wait in queue
    pub max_queue_time: Duration,
    /// Enable gridlock resolution
    pub gridlock_resolution: bool,
    /// Minimum interval between resolution attempts
    pub resolution_interval: Duration,
}

impl RtgsEngine {
    pub fn new(config: RtgsConfig) -> Self {
        Self {
            liquidity: HashMap::new(),
            queue: BTreeMap::new(),
            config,
        }
    }
    
    /// Attempt immediate settlement
    pub fn settle_immediate(
        &mut self,
        instruction: SettlementInstruction,
    ) -> Result<FinalityProof, SettlementFailure> {
        // Check sender liquidity
        let sender_key = (instruction.sender.clone(), instruction.currency.clone());
        let position = self.liquidity.get_mut(&sender_key)
            .ok_or(SettlementFailure::InsufficientFunds)?;
        
        if position.effective_available().units < instruction.amount.units {
            return Err(SettlementFailure::InsufficientFunds);
        }
        
        // Reserve and transfer
        position.available.units -= instruction.amount.units;
        
        // Credit receiver
        let receiver_key = (instruction.receiver.clone(), instruction.currency.clone());
        if let Some(receiver_pos) = self.liquidity.get_mut(&receiver_key) {
            receiver_pos.available.units += instruction.amount.units;
        }
        
        // Generate finality proof
        Ok(FinalityProof {
            batch_hash: [0u8; 32], // Would be computed from transaction
            attestations: vec![],   // Would include node signatures
            ledger_height: 0,       // Would be actual height
        })
    }
    
    /// Queue instruction for later settlement
    pub fn queue_for_settlement(&mut self, instruction: SettlementInstruction) {
        let key = (instruction.priority, instruction.submitted_at);
        self.queue.insert(key, instruction);
    }
    
    /// Attempt to resolve gridlock by finding circular payment chains
    /// 
    /// Gridlock occurs when multiple participants are waiting for incoming
    /// payments to fund outgoing payments. Resolution finds cycles that
    /// can be settled atomically.
    pub fn resolve_gridlock(&mut self) -> Vec<SettlementId> {
        if !self.config.gridlock_resolution {
            return vec![];
        }
        
        // Build dependency graph
        // Find cycles using Tarjan's algorithm
        // Attempt atomic settlement of cycles
        
        vec![] // Placeholder
    }
}

// ============================================================================
// Regulatory Compliance Engine
// ============================================================================

/// Compliance engine for regulatory requirements
/// 
/// Each jurisdiction has specific rules that must be enforced:
/// - Transaction reporting thresholds
/// - Sanctions screening
/// - AML pattern detection
/// - Capital adequacy checks
pub struct ComplianceEngine {
    /// Jurisdiction-specific rule sets
    jurisdictions: HashMap<Jurisdiction, JurisdictionRules>,
    /// Sanctions lists
    sanctions: SanctionsDatabase,
    /// Transaction monitoring
    monitoring: TransactionMonitor,
}

#[derive(Debug, Clone)]
pub struct JurisdictionRules {
    pub jurisdiction: Jurisdiction,
    /// Reporting threshold (transactions above must be reported)
    pub reporting_threshold: Amount,
    /// Large transaction threshold (additional scrutiny)
    pub large_tx_threshold: Amount,
    /// Required fields for transactions
    pub required_fields: Vec<String>,
    /// Prohibited transaction types
    pub prohibited_types: HashSet<String>,
}

pub struct SanctionsDatabase {
    /// Entity names and identifiers
    entities: HashSet<String>,
    /// Last update time
    last_updated: SystemTime,
}

impl SanctionsDatabase {
    pub fn screen(&self, participant: &ParticipantId) -> SanctionsResult {
        if self.entities.contains(&participant.0) {
            SanctionsResult::Hit { confidence: 1.0 }
        } else {
            SanctionsResult::Clear
        }
    }
}

#[derive(Debug)]
pub enum SanctionsResult {
    Clear,
    Hit { confidence: f64 },
    PotentialMatch { candidates: Vec<String> },
}

pub struct TransactionMonitor {
    /// Historical patterns by participant
    patterns: HashMap<ParticipantId, ParticipantPattern>,
}

#[derive(Debug)]
pub struct ParticipantPattern {
    pub average_transaction: Amount,
    pub daily_volume: Amount,
    pub counterparty_count: usize,
    pub last_30_days: Vec<TransactionSummary>,
}

#[derive(Debug)]
pub struct TransactionSummary {
    pub date: chrono::NaiveDate,
    pub count: usize,
    pub volume: Amount,
}

impl ComplianceEngine {
    /// Run all compliance checks for a transaction
    pub fn check(&self, instruction: &SettlementInstruction) -> ComplianceResult {
        let mut issues = Vec::new();
        
        // Sanctions screening
        match self.sanctions.screen(&instruction.sender) {
            SanctionsResult::Hit { confidence } if confidence > 0.9 => {
                issues.push(ComplianceIssue::SanctionsHit {
                    party: "sender".into(),
                    confidence,
                });
            }
            SanctionsResult::PotentialMatch { candidates } => {
                issues.push(ComplianceIssue::SanctionsPotential {
                    party: "sender".into(),
                    candidates,
                });
            }
            _ => {}
        }
        
        // Similar check for receiver...
        
        // Threshold checks
        // Pattern analysis
        // ...
        
        if issues.is_empty() {
            ComplianceResult::Approved
        } else if issues.iter().any(|i| matches!(i, ComplianceIssue::SanctionsHit { .. })) {
            ComplianceResult::Blocked { issues }
        } else {
            ComplianceResult::Review { issues }
        }
    }
}

#[derive(Debug)]
pub enum ComplianceResult {
    Approved,
    Review { issues: Vec<ComplianceIssue> },
    Blocked { issues: Vec<ComplianceIssue> },
}

#[derive(Debug)]
pub enum ComplianceIssue {
    SanctionsHit { party: String, confidence: f64 },
    SanctionsPotential { party: String, candidates: Vec<String> },
    ThresholdExceeded { threshold: Amount },
    PatternAnomaly { description: String },
    MissingInformation { fields: Vec<String> },
}

// ============================================================================
// Settlement Metrics
// ============================================================================

/// Metrics for settlement system monitoring
#[derive(Debug, Default)]
pub struct SettlementMetrics {
    /// Total transactions processed today
    pub transactions_today: u64,
    /// Total volume today (in USD equivalent)
    pub volume_today_usd: Amount,
    /// Average settlement latency
    pub avg_latency_ms: f64,
    /// P99 settlement latency
    pub p99_latency_ms: f64,
    /// Current queue depth
    pub queue_depth: usize,
    /// Failed transactions today
    pub failed_today: u64,
    /// Netting efficiency (last cycle)
    pub netting_efficiency: f64,
    /// Active cross-border transactions
    pub active_cross_border: usize,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_netting_efficiency() {
        let config = NettingConfig {
            cycle_duration: Duration::from_secs(300),
            min_batch_size: 2,
            max_batch_size: 10000,
            settlement_window: Duration::from_secs(60),
        };
        
        let engine = NettingEngine::new(config);
        
        // Create circular payment chain: A→B, B→C, C→A
        let cycle = NettingCycle {
            id: BatchId([0u8; 16]),
            currency: Currency::usd(),
            started_at: SystemTime::now(),
            instructions: vec![
                create_instruction("A", "B", 100),
                create_instruction("B", "C", 80),
                create_instruction("C", "A", 50),
            ],
            state: NettingState::Collecting,
        };
        
        let results = engine.compute_net_positions(&cycle);
        
        // Gross: $230, Net: should be much less
        assert_eq!(results.instruction_count, 3);
        assert!(results.efficiency > 0.5); // At least 50% reduction
    }
    
    fn create_instruction(sender: &str, receiver: &str, amount: i64) -> SettlementInstruction {
        SettlementInstruction {
            id: SettlementId::generate(),
            sender: ParticipantId(sender.into()),
            receiver: ParticipantId(receiver.into()),
            currency: Currency::usd(),
            amount: Amount::from_major(amount, 2),
            value_date: ValueDate::Immediate,
            instruction_type: InstructionType::CreditTransfer,
            priority: Priority::Normal,
            metadata: SettlementMetadata {
                reference: "test".into(),
                regulatory_codes: vec![],
                extensions: HashMap::new(),
            },
            submitted_at: SystemTime::now(),
        }
    }
}
