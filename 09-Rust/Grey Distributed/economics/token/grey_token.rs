//! # Grey Distributed — Grey Token
//!
//! This module implements the Grey Token (GRT) system for internal resource accounting.
//! Tokens are not cryptocurrency — they are internal accounting units for fair resource
//! allocation and tracking across tenants and federated clusters.
//!
//! ## Token Model
//!
//! Grey Tokens serve as:
//! - **Resource credits**: Pre-allocated budget for resource consumption
//! - **Reward units**: Earned through contribution and good behavior
//! - **Accounting ledger**: Tracking usage across distributed clusters
//!
//! ## Token Lifecycle
//!
//! ```text
//! ┌─────────┐  allocation   ┌──────────┐  consumption  ┌───────────┐
//! │  Minted │──────────────►│  Active  │──────────────►│  Consumed │
//! └─────────┘               └──────────┘               └───────────┘
//!                                │
//!                                │ expiration
//!                                ▼
//!                           ┌──────────┐
//!                           │  Expired │
//!                           └──────────┘
//! ```

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::{Duration, SystemTime};

// =============================================================================
// Types and Constants
// =============================================================================

pub type TenantId = String;
pub type ClusterId = String;
pub type TokenAmount = u64;
pub type TransactionId = String;

/// Smallest divisible token unit (1 GRT = 1_000_000 micro-tokens)
pub const MICRO_TOKENS_PER_TOKEN: u64 = 1_000_000;

/// Default token expiration (365 days)
pub const DEFAULT_EXPIRATION_DAYS: u64 = 365;

/// Minimum transfer amount
pub const MIN_TRANSFER_AMOUNT: TokenAmount = 1;

// =============================================================================
// Token Types
// =============================================================================

/// A Grey Token balance
#[derive(Debug, Clone)]
pub struct TokenBalance {
    /// Available tokens
    pub available: TokenAmount,
    
    /// Reserved tokens (committed but not consumed)
    pub reserved: TokenAmount,
    
    /// Consumed tokens (total spent)
    pub consumed: TokenAmount,
    
    /// Expired tokens
    pub expired: TokenAmount,
    
    /// Earned tokens (from rewards)
    pub earned: TokenAmount,
    
    /// Last updated
    pub updated_at: SystemTime,
}

impl TokenBalance {
    pub fn new() -> Self {
        Self {
            available: 0,
            reserved: 0,
            consumed: 0,
            expired: 0,
            earned: 0,
            updated_at: SystemTime::now(),
        }
    }
    
    /// Total tokens ever allocated
    pub fn total_allocated(&self) -> TokenAmount {
        self.available + self.reserved + self.consumed + self.expired
    }
    
    /// Effective balance (available + reserved)
    pub fn effective_balance(&self) -> TokenAmount {
        self.available + self.reserved
    }
}

impl Default for TokenBalance {
    fn default() -> Self {
        Self::new()
    }
}

/// Token allocation with expiration
#[derive(Debug, Clone)]
pub struct TokenAllocation {
    /// Unique allocation ID
    pub allocation_id: String,
    
    /// Tenant receiving tokens
    pub tenant_id: TenantId,
    
    /// Amount allocated
    pub amount: TokenAmount,
    
    /// Amount remaining
    pub remaining: TokenAmount,
    
    /// Allocation source
    pub source: AllocationSource,
    
    /// Created timestamp
    pub created_at: SystemTime,
    
    /// Expiration timestamp
    pub expires_at: SystemTime,
    
    /// Whether allocation is active
    pub active: bool,
}

impl TokenAllocation {
    pub fn is_expired(&self) -> bool {
        SystemTime::now() >= self.expires_at
    }
}

/// Source of token allocation
#[derive(Debug, Clone, PartialEq)]
pub enum AllocationSource {
    /// Initial allocation (subscription)
    Initial,
    /// Top-up purchase
    TopUp,
    /// Earned reward
    Reward,
    /// Transfer from another tenant
    Transfer { from_tenant: TenantId },
    /// Federation grant
    FederationGrant { from_cluster: ClusterId },
    /// Promotional credit
    Promotional,
}

/// Token reservation (hold before consumption)
#[derive(Debug, Clone)]
pub struct TokenReservation {
    /// Unique reservation ID
    pub reservation_id: String,
    
    /// Tenant ID
    pub tenant_id: TenantId,
    
    /// Amount reserved
    pub amount: TokenAmount,
    
    /// Purpose
    pub purpose: ReservationPurpose,
    
    /// Created timestamp
    pub created_at: SystemTime,
    
    /// Expires after (auto-release)
    pub expires_at: SystemTime,
    
    /// Whether reservation is active
    pub active: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReservationPurpose {
    /// Task execution
    TaskExecution { task_id: String },
    /// Resource allocation
    ResourceAllocation { resource_id: String },
    /// Storage provisioning
    StorageProvisioning { storage_id: String },
    /// Network bandwidth
    NetworkBandwidth { session_id: String },
    /// Other purpose
    Other(String),
}

// =============================================================================
// Token Transactions
// =============================================================================

/// A token transaction
#[derive(Debug, Clone)]
pub struct TokenTransaction {
    /// Unique transaction ID
    pub transaction_id: TransactionId,
    
    /// Transaction type
    pub transaction_type: TransactionType,
    
    /// Tenant ID
    pub tenant_id: TenantId,
    
    /// Amount (positive)
    pub amount: TokenAmount,
    
    /// Balance before transaction
    pub balance_before: TokenAmount,
    
    /// Balance after transaction
    pub balance_after: TokenAmount,
    
    /// Timestamp
    pub timestamp: SystemTime,
    
    /// Reference (e.g., task ID, invoice ID)
    pub reference: Option<String>,
    
    /// Description
    pub description: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TransactionType {
    /// Credit (add tokens)
    Credit,
    /// Debit (remove tokens)
    Debit,
    /// Reservation (hold tokens)
    Reserve,
    /// Release reservation
    Release,
    /// Consume (finalize reserved tokens)
    Consume,
    /// Expire (tokens expired)
    Expire,
    /// Transfer to another tenant
    Transfer { to_tenant: TenantId },
}

// =============================================================================
// Token Manager
// =============================================================================

/// Manages Grey Token balances and transactions
pub struct TokenManager {
    /// Tenant balances
    balances: Arc<RwLock<HashMap<TenantId, TokenBalance>>>,
    
    /// Active allocations
    allocations: Arc<RwLock<Vec<TokenAllocation>>>,
    
    /// Active reservations
    reservations: Arc<RwLock<HashMap<String, TokenReservation>>>,
    
    /// Transaction history
    transactions: Arc<RwLock<Vec<TokenTransaction>>>,
    
    /// Configuration
    config: TokenConfig,
}

#[derive(Debug, Clone)]
pub struct TokenConfig {
    /// Default expiration in days
    pub default_expiration_days: u64,
    
    /// Maximum reservation duration
    pub max_reservation_duration: Duration,
    
    /// Allow tenant-to-tenant transfers
    pub allow_transfers: bool,
    
    /// Minimum transfer amount
    pub min_transfer_amount: TokenAmount,
    
    /// Allow overdraft (negative balance)
    pub allow_overdraft: bool,
    
    /// Maximum overdraft amount
    pub max_overdraft: TokenAmount,
}

impl Default for TokenConfig {
    fn default() -> Self {
        Self {
            default_expiration_days: DEFAULT_EXPIRATION_DAYS,
            max_reservation_duration: Duration::from_secs(3600), // 1 hour
            allow_transfers: true,
            min_transfer_amount: MIN_TRANSFER_AMOUNT,
            allow_overdraft: false,
            max_overdraft: 0,
        }
    }
}

impl TokenManager {
    pub fn new(config: TokenConfig) -> Self {
        Self {
            balances: Arc::new(RwLock::new(HashMap::new())),
            allocations: Arc::new(RwLock::new(Vec::new())),
            reservations: Arc::new(RwLock::new(HashMap::new())),
            transactions: Arc::new(RwLock::new(Vec::new())),
            config,
        }
    }
    
    // =========================================================================
    // Balance Operations
    // =========================================================================
    
    /// Get balance for a tenant
    pub fn get_balance(&self, tenant_id: &TenantId) -> TokenBalance {
        self.balances.read().unwrap()
            .get(tenant_id)
            .cloned()
            .unwrap_or_default()
    }
    
    /// Check if tenant has sufficient balance
    pub fn has_sufficient_balance(&self, tenant_id: &TenantId, amount: TokenAmount) -> bool {
        let balance = self.get_balance(tenant_id);
        balance.available >= amount
    }
    
    // =========================================================================
    // Allocation Operations
    // =========================================================================
    
    /// Allocate tokens to a tenant
    ///
    /// ## Token Minting Formula
    ///
    /// ```text
    /// new_balance = current_balance + allocated_amount
    /// allocation_expires = now + expiration_days
    /// ```
    pub fn allocate_tokens(
        &self,
        tenant_id: &TenantId,
        amount: TokenAmount,
        source: AllocationSource,
        expiration_days: Option<u64>,
    ) -> Result<TokenAllocation, TokenError> {
        if amount == 0 {
            return Err(TokenError::InvalidAmount("Amount must be positive".into()));
        }
        
        let expiration = Duration::from_secs(
            expiration_days.unwrap_or(self.config.default_expiration_days) * 86400
        );
        
        let allocation = TokenAllocation {
            allocation_id: generate_id("alloc"),
            tenant_id: tenant_id.clone(),
            amount,
            remaining: amount,
            source: source.clone(),
            created_at: SystemTime::now(),
            expires_at: SystemTime::now() + expiration,
            active: true,
        };
        
        // Update balance
        {
            let mut balances = self.balances.write().unwrap();
            let balance = balances.entry(tenant_id.clone()).or_insert_with(TokenBalance::new);
            balance.available += amount;
            if matches!(source, AllocationSource::Reward) {
                balance.earned += amount;
            }
            balance.updated_at = SystemTime::now();
        }
        
        // Record allocation
        self.allocations.write().unwrap().push(allocation.clone());
        
        // Record transaction
        let balance = self.get_balance(tenant_id);
        self.record_transaction(
            tenant_id.clone(),
            TransactionType::Credit,
            amount,
            balance.available - amount,
            balance.available,
            Some(allocation.allocation_id.clone()),
            format!("Token allocation from {:?}", source),
        );
        
        Ok(allocation)
    }
    
    // =========================================================================
    // Reservation Operations
    // =========================================================================
    
    /// Reserve tokens for a pending operation
    ///
    /// Reservations hold tokens without consuming them, allowing
    /// for atomic operations across distributed systems.
    pub fn reserve_tokens(
        &self,
        tenant_id: &TenantId,
        amount: TokenAmount,
        purpose: ReservationPurpose,
    ) -> Result<TokenReservation, TokenError> {
        if amount == 0 {
            return Err(TokenError::InvalidAmount("Amount must be positive".into()));
        }
        
        // Check available balance
        let balance = self.get_balance(tenant_id);
        if balance.available < amount && !self.config.allow_overdraft {
            return Err(TokenError::InsufficientBalance {
                available: balance.available,
                required: amount,
            });
        }
        
        let reservation = TokenReservation {
            reservation_id: generate_id("rsv"),
            tenant_id: tenant_id.clone(),
            amount,
            purpose,
            created_at: SystemTime::now(),
            expires_at: SystemTime::now() + self.config.max_reservation_duration,
            active: true,
        };
        
        // Update balance
        {
            let mut balances = self.balances.write().unwrap();
            let balance = balances.entry(tenant_id.clone()).or_insert_with(TokenBalance::new);
            balance.available -= amount;
            balance.reserved += amount;
            balance.updated_at = SystemTime::now();
        }
        
        // Record reservation
        self.reservations.write().unwrap().insert(
            reservation.reservation_id.clone(),
            reservation.clone(),
        );
        
        // Record transaction
        let new_balance = self.get_balance(tenant_id);
        self.record_transaction(
            tenant_id.clone(),
            TransactionType::Reserve,
            amount,
            new_balance.available + amount,
            new_balance.available,
            Some(reservation.reservation_id.clone()),
            "Token reservation".into(),
        );
        
        Ok(reservation)
    }
    
    /// Consume reserved tokens (finalize the reservation)
    pub fn consume_reservation(&self, reservation_id: &str) -> Result<TokenAmount, TokenError> {
        let reservation = {
            let mut reservations = self.reservations.write().unwrap();
            let reservation = reservations.get_mut(reservation_id)
                .ok_or(TokenError::ReservationNotFound(reservation_id.to_string()))?;
            
            if !reservation.active {
                return Err(TokenError::ReservationNotActive(reservation_id.to_string()));
            }
            
            reservation.active = false;
            reservation.clone()
        };
        
        // Update balance
        {
            let mut balances = self.balances.write().unwrap();
            let balance = balances.entry(reservation.tenant_id.clone())
                .or_insert_with(TokenBalance::new);
            balance.reserved -= reservation.amount;
            balance.consumed += reservation.amount;
            balance.updated_at = SystemTime::now();
        }
        
        // Record transaction
        let new_balance = self.get_balance(&reservation.tenant_id);
        self.record_transaction(
            reservation.tenant_id.clone(),
            TransactionType::Consume,
            reservation.amount,
            new_balance.consumed - reservation.amount,
            new_balance.consumed,
            Some(reservation_id.to_string()),
            "Reservation consumed".into(),
        );
        
        Ok(reservation.amount)
    }
    
    /// Release reserved tokens (cancel reservation)
    pub fn release_reservation(&self, reservation_id: &str) -> Result<TokenAmount, TokenError> {
        let reservation = {
            let mut reservations = self.reservations.write().unwrap();
            let reservation = reservations.get_mut(reservation_id)
                .ok_or(TokenError::ReservationNotFound(reservation_id.to_string()))?;
            
            if !reservation.active {
                return Err(TokenError::ReservationNotActive(reservation_id.to_string()));
            }
            
            reservation.active = false;
            reservation.clone()
        };
        
        // Update balance
        {
            let mut balances = self.balances.write().unwrap();
            let balance = balances.entry(reservation.tenant_id.clone())
                .or_insert_with(TokenBalance::new);
            balance.reserved -= reservation.amount;
            balance.available += reservation.amount;
            balance.updated_at = SystemTime::now();
        }
        
        // Record transaction
        let new_balance = self.get_balance(&reservation.tenant_id);
        self.record_transaction(
            reservation.tenant_id.clone(),
            TransactionType::Release,
            reservation.amount,
            new_balance.available - reservation.amount,
            new_balance.available,
            Some(reservation_id.to_string()),
            "Reservation released".into(),
        );
        
        Ok(reservation.amount)
    }
    
    // =========================================================================
    // Direct Consumption
    // =========================================================================
    
    /// Consume tokens directly (without reservation)
    pub fn consume_tokens(
        &self,
        tenant_id: &TenantId,
        amount: TokenAmount,
        reference: Option<String>,
        description: String,
    ) -> Result<TokenAmount, TokenError> {
        if amount == 0 {
            return Err(TokenError::InvalidAmount("Amount must be positive".into()));
        }
        
        let balance = self.get_balance(tenant_id);
        let available_with_overdraft = if self.config.allow_overdraft {
            balance.available + self.config.max_overdraft
        } else {
            balance.available
        };
        
        if amount > available_with_overdraft {
            return Err(TokenError::InsufficientBalance {
                available: balance.available,
                required: amount,
            });
        }
        
        // Update balance
        {
            let mut balances = self.balances.write().unwrap();
            let bal = balances.entry(tenant_id.clone()).or_insert_with(TokenBalance::new);
            bal.available = bal.available.saturating_sub(amount);
            bal.consumed += amount;
            bal.updated_at = SystemTime::now();
        }
        
        // Record transaction
        let new_balance = self.get_balance(tenant_id);
        self.record_transaction(
            tenant_id.clone(),
            TransactionType::Debit,
            amount,
            new_balance.available + amount,
            new_balance.available,
            reference,
            description,
        );
        
        Ok(amount)
    }
    
    // =========================================================================
    // Transfers
    // =========================================================================
    
    /// Transfer tokens between tenants
    pub fn transfer_tokens(
        &self,
        from_tenant: &TenantId,
        to_tenant: &TenantId,
        amount: TokenAmount,
    ) -> Result<TransactionId, TokenError> {
        if !self.config.allow_transfers {
            return Err(TokenError::TransfersNotAllowed);
        }
        
        if amount < self.config.min_transfer_amount {
            return Err(TokenError::BelowMinimumTransfer {
                amount,
                minimum: self.config.min_transfer_amount,
            });
        }
        
        let from_balance = self.get_balance(from_tenant);
        if from_balance.available < amount {
            return Err(TokenError::InsufficientBalance {
                available: from_balance.available,
                required: amount,
            });
        }
        
        // Debit from sender
        {
            let mut balances = self.balances.write().unwrap();
            let bal = balances.entry(from_tenant.clone()).or_insert_with(TokenBalance::new);
            bal.available -= amount;
            bal.updated_at = SystemTime::now();
        }
        
        // Credit to receiver
        {
            let mut balances = self.balances.write().unwrap();
            let bal = balances.entry(to_tenant.clone()).or_insert_with(TokenBalance::new);
            bal.available += amount;
            bal.updated_at = SystemTime::now();
        }
        
        // Record transaction
        let tx_id = generate_id("tx");
        let from_new = self.get_balance(from_tenant);
        let to_new = self.get_balance(to_tenant);
        
        self.record_transaction(
            from_tenant.clone(),
            TransactionType::Transfer { to_tenant: to_tenant.clone() },
            amount,
            from_new.available + amount,
            from_new.available,
            Some(tx_id.clone()),
            format!("Transfer to {}", to_tenant),
        );
        
        self.record_transaction(
            to_tenant.clone(),
            TransactionType::Credit,
            amount,
            to_new.available - amount,
            to_new.available,
            Some(tx_id.clone()),
            format!("Transfer from {}", from_tenant),
        );
        
        Ok(tx_id)
    }
    
    // =========================================================================
    // Expiration
    // =========================================================================
    
    /// Expire tokens that have passed their expiration date
    pub fn process_expirations(&self) -> Vec<(TenantId, TokenAmount)> {
        let now = SystemTime::now();
        let mut expired_list = Vec::new();
        
        let mut allocations = self.allocations.write().unwrap();
        for allocation in allocations.iter_mut() {
            if allocation.active && allocation.expires_at <= now && allocation.remaining > 0 {
                let expired_amount = allocation.remaining;
                allocation.remaining = 0;
                allocation.active = false;
                
                // Update balance
                {
                    let mut balances = self.balances.write().unwrap();
                    if let Some(balance) = balances.get_mut(&allocation.tenant_id) {
                        balance.available = balance.available.saturating_sub(expired_amount);
                        balance.expired += expired_amount;
                        balance.updated_at = SystemTime::now();
                    }
                }
                
                expired_list.push((allocation.tenant_id.clone(), expired_amount));
            }
        }
        
        expired_list
    }
    
    // =========================================================================
    // Transaction Recording
    // =========================================================================
    
    fn record_transaction(
        &self,
        tenant_id: TenantId,
        transaction_type: TransactionType,
        amount: TokenAmount,
        balance_before: TokenAmount,
        balance_after: TokenAmount,
        reference: Option<String>,
        description: String,
    ) {
        let tx = TokenTransaction {
            transaction_id: generate_id("tx"),
            transaction_type,
            tenant_id,
            amount,
            balance_before,
            balance_after,
            timestamp: SystemTime::now(),
            reference,
            description,
        };
        
        self.transactions.write().unwrap().push(tx);
    }
    
    /// Get transaction history for a tenant
    pub fn get_transactions(&self, tenant_id: &TenantId) -> Vec<TokenTransaction> {
        self.transactions.read().unwrap()
            .iter()
            .filter(|tx| &tx.tenant_id == tenant_id)
            .cloned()
            .collect()
    }
}

// =============================================================================
// Errors
// =============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum TokenError {
    InvalidAmount(String),
    InsufficientBalance { available: TokenAmount, required: TokenAmount },
    ReservationNotFound(String),
    ReservationNotActive(String),
    TransfersNotAllowed,
    BelowMinimumTransfer { amount: TokenAmount, minimum: TokenAmount },
    AllocationNotFound(String),
}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::InvalidAmount(msg) => write!(f, "Invalid amount: {}", msg),
            TokenError::InsufficientBalance { available, required } => {
                write!(f, "Insufficient balance: {} available, {} required", available, required)
            }
            TokenError::ReservationNotFound(id) => write!(f, "Reservation not found: {}", id),
            TokenError::ReservationNotActive(id) => write!(f, "Reservation not active: {}", id),
            TokenError::TransfersNotAllowed => write!(f, "Token transfers are not allowed"),
            TokenError::BelowMinimumTransfer { amount, minimum } => {
                write!(f, "Transfer amount {} below minimum {}", amount, minimum)
            }
            TokenError::AllocationNotFound(id) => write!(f, "Allocation not found: {}", id),
        }
    }
}

impl std::error::Error for TokenError {}

// =============================================================================
// Utilities
// =============================================================================

fn generate_id(prefix: &str) -> String {
    format!("{}-{}", prefix, uuid_v4())
}

fn uuid_v4() -> String {
    // Simple UUID v4-like generation (in production, use uuid crate)
    use std::time::{SystemTime, UNIX_EPOCH};
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    format!("{:032x}", nanos)
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_allocate_tokens() {
        let manager = TokenManager::new(TokenConfig::default());
        let tenant = "tenant-1".to_string();
        
        let allocation = manager.allocate_tokens(
            &tenant,
            1000,
            AllocationSource::Initial,
            None,
        ).unwrap();
        
        assert_eq!(allocation.amount, 1000);
        assert_eq!(manager.get_balance(&tenant).available, 1000);
    }
    
    #[test]
    fn test_reserve_and_consume() {
        let manager = TokenManager::new(TokenConfig::default());
        let tenant = "tenant-1".to_string();
        
        manager.allocate_tokens(&tenant, 1000, AllocationSource::Initial, None).unwrap();
        
        let reservation = manager.reserve_tokens(
            &tenant,
            300,
            ReservationPurpose::TaskExecution { task_id: "task-1".into() },
        ).unwrap();
        
        let balance = manager.get_balance(&tenant);
        assert_eq!(balance.available, 700);
        assert_eq!(balance.reserved, 300);
        
        manager.consume_reservation(&reservation.reservation_id).unwrap();
        
        let balance = manager.get_balance(&tenant);
        assert_eq!(balance.available, 700);
        assert_eq!(balance.reserved, 0);
        assert_eq!(balance.consumed, 300);
    }
    
    #[test]
    fn test_release_reservation() {
        let manager = TokenManager::new(TokenConfig::default());
        let tenant = "tenant-1".to_string();
        
        manager.allocate_tokens(&tenant, 1000, AllocationSource::Initial, None).unwrap();
        
        let reservation = manager.reserve_tokens(
            &tenant,
            300,
            ReservationPurpose::Other("test".into()),
        ).unwrap();
        
        manager.release_reservation(&reservation.reservation_id).unwrap();
        
        let balance = manager.get_balance(&tenant);
        assert_eq!(balance.available, 1000);
        assert_eq!(balance.reserved, 0);
    }
    
    #[test]
    fn test_insufficient_balance() {
        let manager = TokenManager::new(TokenConfig::default());
        let tenant = "tenant-1".to_string();
        
        manager.allocate_tokens(&tenant, 100, AllocationSource::Initial, None).unwrap();
        
        let result = manager.reserve_tokens(
            &tenant,
            500,
            ReservationPurpose::Other("test".into()),
        );
        
        assert!(matches!(result, Err(TokenError::InsufficientBalance { .. })));
    }
    
    #[test]
    fn test_transfer() {
        let manager = TokenManager::new(TokenConfig::default());
        let tenant_a = "tenant-a".to_string();
        let tenant_b = "tenant-b".to_string();
        
        manager.allocate_tokens(&tenant_a, 1000, AllocationSource::Initial, None).unwrap();
        manager.transfer_tokens(&tenant_a, &tenant_b, 300).unwrap();
        
        assert_eq!(manager.get_balance(&tenant_a).available, 700);
        assert_eq!(manager.get_balance(&tenant_b).available, 300);
    }
}
