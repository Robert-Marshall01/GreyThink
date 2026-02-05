//! Financial Crash Resilience Protocol
//!
//! Handles distributed system operations during global financial crisis scenarios
//! including market crashes, currency collapses, banking system failures, and
//! sovereign debt crises.
//!
//! Key challenges addressed:
//! - Extreme transaction volume spikes
//! - Counterparty failures
//! - Currency instability
//! - Regulatory emergency interventions
//! - Settlement system stress

use std::collections::{HashMap, HashSet, VecDeque};
use std::time::Duration;
use serde::{Deserialize, Serialize};

// =============================================================================
// FINANCIAL CRISIS MODEL
// =============================================================================

/// Financial crisis severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum CrisisSeverity {
    /// Elevated volatility, enhanced monitoring
    Elevated = 0,
    
    /// Significant market stress, circuit breakers active
    Stress = 1,
    
    /// Systemic risk emerging, intervention likely
    Systemic = 2,
    
    /// Full-scale crisis, multiple failures
    Crisis = 3,
    
    /// Existential threat to financial system
    Collapse = 4,
}

/// Type of financial crisis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CrisisType {
    /// Stock market crash
    MarketCrash {
        indices_affected: Vec<String>,
        decline_percent: f64,
    },
    
    /// Currency crisis
    CurrencyCrisis {
        currencies_affected: Vec<CurrencyCode>,
        volatility_index: f64,
    },
    
    /// Banking system failure
    BankingCrisis {
        institutions_at_risk: Vec<InstitutionId>,
        exposure_estimate: f64,
    },
    
    /// Sovereign debt crisis
    SovereignDebt {
        nations_affected: Vec<NationCode>,
        default_probability: f64,
    },
    
    /// Cryptocurrency collapse
    CryptoCollapse {
        protocols_affected: Vec<String>,
        market_cap_loss: f64,
    },
    
    /// Systemic liquidity crisis
    LiquidityCrisis {
        markets_frozen: Vec<MarketId>,
        duration_estimate: Duration,
    },
}

/// Real-time crisis status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrisisStatus {
    pub severity: CrisisSeverity,
    pub crisis_types: Vec<CrisisType>,
    pub started: Timestamp,
    pub last_updated: Timestamp,
    pub markets_halted: Vec<MarketId>,
    pub institutions_failed: Vec<InstitutionId>,
    pub regulatory_interventions: Vec<RegulatoryIntervention>,
    pub estimated_resolution: Option<Duration>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegulatoryIntervention {
    pub authority: String,
    pub intervention_type: InterventionType,
    pub scope: InterventionScope,
    pub effective: Timestamp,
    pub expires: Option<Timestamp>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InterventionType {
    TradingHalt,
    ShortSellingBan,
    CapitalControls,
    EmergencyLending,
    Nationalization,
    DebtMoratorium,
    CurrencyPeg,
}

// =============================================================================
// TRANSACTION SURGE HANDLING
// =============================================================================

/// Manages extreme transaction volume during financial crises
#[derive(Debug, Clone)]
pub struct TransactionSurgeManager {
    /// Normal transaction rate
    baseline_rate: u64,
    
    /// Current transaction rate
    current_rate: u64,
    
    /// Maximum sustainable rate
    max_sustainable_rate: u64,
    
    /// Queue for excess transactions
    overflow_queue: VecDeque<QueuedTransaction>,
    
    /// Priority classification
    priority_classifier: PriorityClassifier,
    
    /// Circuit breaker configuration
    circuit_breakers: CircuitBreakerConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueuedTransaction {
    pub id: TransactionId,
    pub submitted: Timestamp,
    pub priority: TransactionPriority,
    pub value: f64,
    pub parties: Vec<PartyId>,
    pub deadline: Option<Timestamp>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum TransactionPriority {
    /// Can be delayed indefinitely
    Deferrable = 0,
    
    /// Normal priority
    Normal = 1,
    
    /// Time-sensitive
    Elevated = 2,
    
    /// Critical infrastructure
    Critical = 3,
    
    /// System survival
    Emergency = 4,
}

impl TransactionSurgeManager {
    /// Handle incoming transaction during crisis
    pub async fn handle_transaction(&mut self, tx: Transaction) -> TransactionResult {
        let priority = self.priority_classifier.classify(&tx);
        
        // Check circuit breakers
        if self.circuit_breakers.is_tripped() {
            return match priority {
                TransactionPriority::Emergency => {
                    // Emergency transactions bypass circuit breaker
                    self.process_immediately(tx).await
                }
                TransactionPriority::Critical => {
                    // Queue critical transactions
                    self.queue_transaction(tx, priority);
                    TransactionResult::Queued { position: self.overflow_queue.len() }
                }
                _ => {
                    // Reject lower priority
                    TransactionResult::Rejected {
                        reason: "Circuit breaker active".to_string(),
                        retry_after: self.circuit_breakers.estimated_reset(),
                    }
                }
            };
        }
        
        // Check capacity
        if self.current_rate >= self.max_sustainable_rate {
            self.queue_transaction(tx, priority);
            TransactionResult::Queued { position: self.overflow_queue.len() }
        } else {
            self.process_immediately(tx).await
        }
    }
    
    /// Process queued transactions as capacity becomes available
    pub async fn drain_queue(&mut self) -> Vec<TransactionResult> {
        let mut results = Vec::new();
        
        while self.current_rate < self.max_sustainable_rate {
            if let Some(queued) = self.dequeue_highest_priority() {
                let result = self.process_queued(queued).await;
                results.push(result);
            } else {
                break;
            }
        }
        
        results
    }
    
    /// Activate emergency rate limiting
    pub fn activate_emergency_limiting(&mut self, max_rate: u64) {
        self.max_sustainable_rate = max_rate;
        
        // Re-prioritize queue
        self.reprioritize_queue();
        
        // Reject transactions that can't be processed in time
        self.expire_stale_transactions();
    }
    
    fn queue_transaction(&mut self, tx: Transaction, priority: TransactionPriority) {
        self.overflow_queue.push_back(QueuedTransaction {
            id: tx.id,
            submitted: Timestamp::now(),
            priority,
            value: tx.value,
            parties: tx.parties,
            deadline: tx.deadline,
        });
    }
    
    fn dequeue_highest_priority(&mut self) -> Option<QueuedTransaction> {
        // Find highest priority transaction
        let mut best_idx = None;
        let mut best_priority = TransactionPriority::Deferrable;
        
        for (idx, tx) in self.overflow_queue.iter().enumerate() {
            if tx.priority > best_priority {
                best_priority = tx.priority;
                best_idx = Some(idx);
            }
        }
        
        best_idx.and_then(|idx| self.overflow_queue.remove(idx))
    }
    
    async fn process_immediately(&self, _tx: Transaction) -> TransactionResult {
        TransactionResult::Processed { latency_ms: 50 }
    }
    
    async fn process_queued(&self, _tx: QueuedTransaction) -> TransactionResult {
        TransactionResult::Processed { latency_ms: 100 }
    }
    
    fn reprioritize_queue(&mut self) {
        // Re-sort queue by priority
    }
    
    fn expire_stale_transactions(&mut self) {
        let now = Timestamp::now();
        self.overflow_queue.retain(|tx| {
            tx.deadline.map_or(true, |d| d > now)
        });
    }
}

// =============================================================================
// COUNTERPARTY FAILURE HANDLING
// =============================================================================

/// Manages counterparty failures during financial crises
#[derive(Debug, Clone)]
pub struct CounterpartyFailureManager {
    /// Known counterparties and their health
    counterparties: HashMap<PartyId, CounterpartyStatus>,
    
    /// Exposure limits
    exposure_limits: HashMap<PartyId, f64>,
    
    /// Current exposures
    current_exposures: HashMap<PartyId, f64>,
    
    /// Netting arrangements
    netting_sets: HashMap<NettingSetId, NettingSet>,
    
    /// Collateral held
    collateral: HashMap<PartyId, CollateralPosition>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CounterpartyStatus {
    pub id: PartyId,
    pub health: CounterpartyHealth,
    pub credit_rating: Option<String>,
    pub last_payment: Option<Timestamp>,
    pub failed_obligations: u32,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum CounterpartyHealth {
    Healthy,
    Stressed,
    Distressed,
    DefaultImminent,
    Defaulted,
    Failed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NettingSet {
    pub id: NettingSetId,
    pub counterparty: PartyId,
    pub obligations: Vec<Obligation>,
    pub net_position: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollateralPosition {
    pub counterparty: PartyId,
    pub posted: f64,
    pub received: f64,
    pub margin_call_threshold: f64,
    pub last_valuation: Timestamp,
}

impl CounterpartyFailureManager {
    /// Handle counterparty failure
    pub async fn handle_failure(&mut self, party: PartyId) -> FailureHandlingResult {
        let exposure = self.calculate_exposure(&party);
        let collateral = self.collateral.get(&party);
        
        // Close out netting sets
        let closeout_value = self.closeout_netting_sets(&party).await;
        
        // Recover collateral
        let recovered = match collateral {
            Some(pos) => pos.received,
            None => 0.0,
        };
        
        // Calculate loss
        let loss = (exposure - closeout_value - recovered).max(0.0);
        
        // Update exposure limits for similar counterparties
        self.update_exposure_limits_conservatively();
        
        FailureHandlingResult {
            party,
            initial_exposure: exposure,
            closeout_value,
            collateral_recovered: recovered,
            net_loss: loss,
            contagion_risk: self.assess_contagion_risk(loss),
        }
    }
    
    /// Proactive exposure reduction as crisis develops
    pub fn reduce_exposures(&mut self, factor: f64) {
        for limit in self.exposure_limits.values_mut() {
            *limit *= factor;
        }
    }
    
    /// Real-time counterparty health monitoring
    pub fn monitor_counterparties(&self) -> Vec<CounterpartyAlert> {
        let mut alerts = Vec::new();
        
        for (id, status) in &self.counterparties {
            match status.health {
                CounterpartyHealth::Distressed => {
                    alerts.push(CounterpartyAlert {
                        party: id.clone(),
                        severity: AlertSeverity::Warning,
                        message: "Counterparty distressed".to_string(),
                        exposure: self.current_exposures.get(id).copied().unwrap_or(0.0),
                    });
                }
                CounterpartyHealth::DefaultImminent => {
                    alerts.push(CounterpartyAlert {
                        party: id.clone(),
                        severity: AlertSeverity::Critical,
                        message: "Default imminent".to_string(),
                        exposure: self.current_exposures.get(id).copied().unwrap_or(0.0),
                    });
                }
                _ => {}
            }
        }
        
        alerts
    }
    
    fn calculate_exposure(&self, party: &PartyId) -> f64 {
        self.current_exposures.get(party).copied().unwrap_or(0.0)
    }
    
    async fn closeout_netting_sets(&mut self, party: &PartyId) -> f64 {
        let mut total = 0.0;
        for set in self.netting_sets.values() {
            if &set.counterparty == party {
                total += set.net_position;
            }
        }
        total
    }
    
    fn update_exposure_limits_conservatively(&mut self) {
        for limit in self.exposure_limits.values_mut() {
            *limit *= 0.8; // Reduce all limits by 20%
        }
    }
    
    fn assess_contagion_risk(&self, loss: f64) -> ContagionRisk {
        if loss > 1_000_000_000.0 {
            ContagionRisk::High
        } else if loss > 100_000_000.0 {
            ContagionRisk::Medium
        } else {
            ContagionRisk::Low
        }
    }
}

// =============================================================================
// SETTLEMENT SYSTEM RESILIENCE
// =============================================================================

/// Ensures settlement continues during crisis
#[derive(Debug, Clone)]
pub struct SettlementResilience {
    /// Primary settlement systems
    primary_systems: Vec<SettlementSystem>,
    
    /// Backup settlement mechanisms
    backup_systems: Vec<SettlementSystem>,
    
    /// Settlement queues
    pending_settlements: HashMap<SettlementSystem, Vec<Settlement>>,
    
    /// Netting algorithms
    netting_algorithms: Vec<NettingAlgorithm>,
    
    /// Liquidity buffers
    liquidity_buffers: HashMap<CurrencyCode, f64>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct SettlementSystem {
    pub id: String,
    pub name: String,
    pub currencies: Vec<CurrencyCode>,
    pub is_primary: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Settlement {
    pub id: SettlementId,
    pub payer: PartyId,
    pub receiver: PartyId,
    pub amount: f64,
    pub currency: CurrencyCode,
    pub due_date: Timestamp,
    pub priority: SettlementPriority,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SettlementPriority {
    Normal,
    SameDay,
    Urgent,
    Critical,
}

impl SettlementResilience {
    /// Handle settlement system failure
    pub async fn handle_system_failure(&mut self, failed: &SettlementSystem) -> SettlementFailoverResult {
        // Identify affected settlements
        let affected = self.pending_settlements.remove(failed).unwrap_or_default();
        
        // Find backup system
        let backup = self.find_backup_for(failed);
        
        match backup {
            Some(system) => {
                // Migrate settlements to backup
                let migrated = self.migrate_settlements(affected, &system).await;
                
                SettlementFailoverResult::Success {
                    failed_system: failed.clone(),
                    backup_system: system,
                    settlements_migrated: migrated,
                    settlements_delayed: 0,
                }
            }
            None => {
                // No backup available - queue for later
                let count = affected.len();
                self.queue_for_manual_processing(affected);
                
                SettlementFailoverResult::Degraded {
                    failed_system: failed.clone(),
                    settlements_queued: count,
                    estimated_delay: Duration::from_secs(3600),
                }
            }
        }
    }
    
    /// Optimize settlements through netting during crisis
    pub fn crisis_netting(&self, settlements: Vec<Settlement>) -> NettingResult {
        // Multi-lateral netting to reduce settlement volume
        let mut net_positions: HashMap<(PartyId, CurrencyCode), f64> = HashMap::new();
        
        for settlement in &settlements {
            let key = (settlement.payer.clone(), settlement.currency.clone());
            *net_positions.entry(key).or_insert(0.0) -= settlement.amount;
            
            let key = (settlement.receiver.clone(), settlement.currency.clone());
            *net_positions.entry(key).or_insert(0.0) += settlement.amount;
        }
        
        // Generate netted settlements
        let netted: Vec<Settlement> = net_positions.iter()
            .filter(|(_, &amount)| amount.abs() > 0.01)
            .map(|((party, currency), &amount)| {
                Settlement {
                    id: format!("NET-{}-{}", party, currency),
                    payer: if amount < 0.0 { party.clone() } else { "CENTRAL".to_string() },
                    receiver: if amount > 0.0 { party.clone() } else { "CENTRAL".to_string() },
                    amount: amount.abs(),
                    currency: currency.clone(),
                    due_date: Timestamp::now(),
                    priority: SettlementPriority::Urgent,
                }
            })
            .collect();
        
        NettingResult {
            original_count: settlements.len(),
            original_gross: settlements.iter().map(|s| s.amount).sum(),
            netted_count: netted.len(),
            netted_gross: netted.iter().map(|s| s.amount).sum(),
            reduction_ratio: 1.0 - (netted.len() as f64 / settlements.len() as f64),
        }
    }
    
    fn find_backup_for(&self, failed: &SettlementSystem) -> Option<SettlementSystem> {
        self.backup_systems.iter()
            .find(|s| s.currencies.iter().any(|c| failed.currencies.contains(c)))
            .cloned()
    }
    
    async fn migrate_settlements(&mut self, settlements: Vec<Settlement>, target: &SettlementSystem) -> usize {
        let count = settlements.len();
        self.pending_settlements.entry(target.clone())
            .or_insert_with(Vec::new)
            .extend(settlements);
        count
    }
    
    fn queue_for_manual_processing(&mut self, _settlements: Vec<Settlement>) {
        // Queue for manual processing
    }
}

// =============================================================================
// FINANCIAL CRISIS COORDINATOR
// =============================================================================

/// Main coordinator for financial crisis resilience
pub struct FinancialCrashCoordinator {
    /// Current crisis status
    status: CrisisStatus,
    
    /// Transaction surge management
    surge_manager: TransactionSurgeManager,
    
    /// Counterparty management
    counterparty_manager: CounterpartyFailureManager,
    
    /// Settlement resilience
    settlement_resilience: SettlementResilience,
    
    /// Regulatory interface
    regulatory_interface: RegulatoryInterface,
    
    /// Market data feeds
    market_data: MarketDataManager,
}

impl FinancialCrashCoordinator {
    /// Main crisis response loop
    pub async fn respond_to_crisis(&mut self, severity: CrisisSeverity) -> CrisisResponse {
        let mut response = CrisisResponse::new(severity);
        
        // 1. Activate circuit breakers if needed
        if severity >= CrisisSeverity::Stress {
            self.surge_manager.circuit_breakers.activate(severity);
            response.add_action(CrisisAction::CircuitBreakersActivated);
        }
        
        // 2. Reduce counterparty exposures
        let exposure_factor = match severity {
            CrisisSeverity::Elevated => 0.9,
            CrisisSeverity::Stress => 0.7,
            CrisisSeverity::Systemic => 0.5,
            CrisisSeverity::Crisis => 0.3,
            CrisisSeverity::Collapse => 0.1,
        };
        self.counterparty_manager.reduce_exposures(exposure_factor);
        
        // 3. Monitor counterparty health
        let counterparty_alerts = self.counterparty_manager.monitor_counterparties();
        response.counterparty_alerts = counterparty_alerts;
        
        // 4. Prepare settlement system backup
        if severity >= CrisisSeverity::Crisis {
            response.add_action(CrisisAction::SettlementBackupActivated);
        }
        
        // 5. Implement regulatory requirements
        for intervention in &self.status.regulatory_interventions {
            self.implement_intervention(intervention).await;
        }
        
        response
    }
    
    /// Handle specific market event
    pub async fn handle_market_event(&mut self, event: MarketEvent) -> EventResponse {
        match event {
            MarketEvent::MarketHalt { market } => {
                // Queue pending transactions for this market
                EventResponse::Acknowledged {
                    action_taken: format!("Halted processing for {}", market),
                }
            }
            MarketEvent::InstitutionFailure { institution } => {
                // Handle counterparty failure
                let result = self.counterparty_manager.handle_failure(institution).await;
                EventResponse::CounterpartyHandled(result)
            }
            MarketEvent::CurrencyEvent { currency, event_type } => {
                // Adjust processing for currency
                EventResponse::Acknowledged {
                    action_taken: format!("Adjusted for {} {}", currency, event_type),
                }
            }
        }
    }
    
    async fn implement_intervention(&mut self, _intervention: &RegulatoryIntervention) {
        // Implement regulatory intervention
    }
}

// =============================================================================
// TYPE DEFINITIONS
// =============================================================================

pub type CurrencyCode = String;
pub type InstitutionId = String;
pub type NationCode = String;
pub type MarketId = String;
pub type PartyId = String;
pub type TransactionId = String;
pub type NettingSetId = String;
pub type SettlementId = String;
pub type Timestamp = u64;

impl Timestamp {
    pub fn now() -> Self {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InterventionScope {
    Global,
    Regional(Vec<String>),
    Market(MarketId),
    Institution(InstitutionId),
}

#[derive(Debug, Clone)]
pub struct PriorityClassifier {
    rules: Vec<ClassificationRule>,
}

impl PriorityClassifier {
    fn classify(&self, tx: &Transaction) -> TransactionPriority {
        // Classify based on rules
        if tx.value > 1_000_000_000.0 {
            TransactionPriority::Critical
        } else if tx.value > 100_000_000.0 {
            TransactionPriority::Elevated
        } else {
            TransactionPriority::Normal
        }
    }
}

#[derive(Debug, Clone)]
pub struct ClassificationRule {
    pub condition: String,
    pub priority: TransactionPriority,
}

#[derive(Debug, Clone)]
pub struct CircuitBreakerConfig {
    pub thresholds: HashMap<String, f64>,
    pub cooldown: Duration,
    pub tripped: bool,
}

impl CircuitBreakerConfig {
    fn is_tripped(&self) -> bool {
        self.tripped
    }
    
    fn estimated_reset(&self) -> Duration {
        self.cooldown
    }
    
    fn activate(&mut self, _severity: CrisisSeverity) {
        self.tripped = true;
    }
}

#[derive(Debug, Clone)]
pub struct Transaction {
    pub id: TransactionId,
    pub value: f64,
    pub parties: Vec<PartyId>,
    pub deadline: Option<Timestamp>,
}

#[derive(Debug, Clone)]
pub enum TransactionResult {
    Processed { latency_ms: u64 },
    Queued { position: usize },
    Rejected { reason: String, retry_after: Duration },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Obligation {
    pub id: String,
    pub amount: f64,
    pub due: Timestamp,
}

#[derive(Debug, Clone)]
pub struct FailureHandlingResult {
    pub party: PartyId,
    pub initial_exposure: f64,
    pub closeout_value: f64,
    pub collateral_recovered: f64,
    pub net_loss: f64,
    pub contagion_risk: ContagionRisk,
}

#[derive(Debug, Clone, Copy)]
pub enum ContagionRisk {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone)]
pub struct CounterpartyAlert {
    pub party: PartyId,
    pub severity: AlertSeverity,
    pub message: String,
    pub exposure: f64,
}

#[derive(Debug, Clone, Copy)]
pub enum AlertSeverity {
    Info,
    Warning,
    Critical,
}

#[derive(Debug, Clone)]
pub struct NettingAlgorithm {
    pub name: String,
    pub algorithm_type: String,
}

#[derive(Debug, Clone)]
pub enum SettlementFailoverResult {
    Success {
        failed_system: SettlementSystem,
        backup_system: SettlementSystem,
        settlements_migrated: usize,
        settlements_delayed: usize,
    },
    Degraded {
        failed_system: SettlementSystem,
        settlements_queued: usize,
        estimated_delay: Duration,
    },
}

#[derive(Debug, Clone)]
pub struct NettingResult {
    pub original_count: usize,
    pub original_gross: f64,
    pub netted_count: usize,
    pub netted_gross: f64,
    pub reduction_ratio: f64,
}

#[derive(Debug, Clone)]
pub struct RegulatoryInterface {
    pub connections: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct MarketDataManager {
    pub feeds: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct CrisisResponse {
    pub severity: CrisisSeverity,
    pub actions: Vec<CrisisAction>,
    pub counterparty_alerts: Vec<CounterpartyAlert>,
}

impl CrisisResponse {
    pub fn new(severity: CrisisSeverity) -> Self {
        Self {
            severity,
            actions: Vec::new(),
            counterparty_alerts: Vec::new(),
        }
    }
    
    pub fn add_action(&mut self, action: CrisisAction) {
        self.actions.push(action);
    }
}

#[derive(Debug, Clone)]
pub enum CrisisAction {
    CircuitBreakersActivated,
    ExposuresReduced,
    SettlementBackupActivated,
    RegulatoryCompliance(String),
}

#[derive(Debug, Clone)]
pub enum MarketEvent {
    MarketHalt { market: MarketId },
    InstitutionFailure { institution: InstitutionId },
    CurrencyEvent { currency: CurrencyCode, event_type: String },
}

#[derive(Debug, Clone)]
pub enum EventResponse {
    Acknowledged { action_taken: String },
    CounterpartyHandled(FailureHandlingResult),
}
