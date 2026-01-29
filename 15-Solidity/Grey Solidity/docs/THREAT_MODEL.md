# Grey Protocol Threat Model

## Executive Summary

This document provides a comprehensive threat model for the Grey Protocol ecosystem. It identifies potential attack vectors, assesses their impact and likelihood, and documents the mitigations implemented.

## Threat Categories

### 1. Smart Contract Vulnerabilities

#### 1.1 Reentrancy Attacks

**Threat Level:** HIGH

**Description:** Attacker exploits callback mechanisms to re-enter functions before state updates complete.

**Attack Vectors:**
- Flash loan callbacks
- ERC777 token hooks
- NFT onReceived callbacks
- Cross-contract calls

**Mitigations:**
- ✅ `ReentrancyGuard` applied to all external state-changing functions
- ✅ Checks-Effects-Interactions pattern enforced
- ✅ Pull payment pattern for withdrawals
- ✅ Flash loan guards with state locks

**Affected Contracts:**
- `FlashLoanProvider.sol` - Protected via `FlashLoanGuard`
- `LiquidityPool.sol` - Protected via `ReentrancyGuard`
- `Vault.sol` - Protected via `ReentrancyGuard`
- `NFTMarketplace.sol` - Protected via `ReentrancyGuard`

---

#### 1.2 Integer Overflow/Underflow

**Threat Level:** LOW (Solidity 0.8+)

**Description:** Arithmetic operations wrap around causing unexpected values.

**Attack Vectors:**
- Large multiplication in fee calculations
- Subtractions in balance updates
- Exponential calculations in bonding curves

**Mitigations:**
- ✅ Solidity 0.8.28 built-in overflow checks
- ✅ SafeMath patterns where unchecked is used
- ✅ Bounds checking on user inputs
- ✅ Explicit `unchecked` blocks documented

**Affected Contracts:**
- `BondingCurve.sol` - Uses checked arithmetic
- `StableSwapAMM.sol` - Newton's method iterations protected

---

#### 1.3 Access Control Flaws

**Threat Level:** HIGH

**Description:** Unauthorized users gain privileged access to admin functions.

**Attack Vectors:**
- Missing access modifiers
- Role escalation
- Unprotected initializers
- Proxy storage collisions

**Mitigations:**
- ✅ OpenZeppelin `AccessControl` for role-based permissions
- ✅ Multi-sig requirements for critical operations
- ✅ Timelock on governance actions
- ✅ Initializer guards on upgradeable contracts
- ✅ Storage gaps in upgradeable contracts

**Affected Contracts:**
- All contracts with admin functions
- `GreyGovernor.sol` - Timelock protected
- `EmergencyGovernance.sol` - Multi-guardian approval

---

#### 1.4 Oracle Manipulation

**Threat Level:** CRITICAL

**Description:** Attacker manipulates price feeds to exploit lending, liquidation, or trading logic.

**Attack Vectors:**
- Flash loan price manipulation
- TWAP manipulation via sustained trading
- Stale price exploitation
- Single oracle failure

**Mitigations:**
- ✅ TWAP oracle with configurable window (`TWAPOracle.sol`)
- ✅ Median oracle aggregating multiple sources (`MedianOracle.sol`)
- ✅ Chainlink integration with staleness checks (`ChainlinkAdapter.sol`)
- ✅ Price deviation circuit breakers
- ✅ Minimum liquidity requirements for TWAP

**Affected Contracts:**
- `LendingPool.sol`
- `LiquidationEngine.sol`
- `PerpetualPositions.sol`
- `SyntheticAssetFactory.sol`

---

### 2. Economic Attacks

#### 2.1 Flash Loan Attacks

**Threat Level:** CRITICAL

**Description:** Attacker uses flash loans to manipulate protocol state within a single transaction.

**Attack Vectors:**
- Governance vote manipulation
- Collateral ratio manipulation
- AMM arbitrage extraction
- Liquidation exploitation

**Mitigations:**
- ✅ `FlashLoanGuard` contract for state protection
- ✅ Block-based voting power snapshots
- ✅ Collateral deposit lockup periods
- ✅ MEV protection via commit-reveal schemes

**Affected Contracts:**
- `FlashLoanProvider.sol`
- `AdvancedGovernor.sol`
- `LendingPool.sol`

---

#### 2.2 Front-Running / MEV

**Threat Level:** HIGH

**Description:** Miners/validators or sophisticated actors extract value by ordering transactions.

**Attack Vectors:**
- Sandwich attacks on DEX swaps
- Liquidation front-running
- NFT sniping
- Governance proposal front-running

**Mitigations:**
- ✅ `MEVProtection.sol` with commit-reveal
- ✅ Minimum output amounts on swaps
- ✅ Private mempools integration support
- ✅ Time-weighted operations where possible
- ✅ Slippage protection parameters

**Affected Contracts:**
- `ConstantProductAMM.sol`
- `StableSwapAMM.sol`
- `NFTMarketplace.sol`
- `LimitOrderBook.sol`

---

#### 2.3 Liquidity Drainage

**Threat Level:** HIGH

**Description:** Attacker drains liquidity pools through arbitrage or manipulation.

**Attack Vectors:**
- Imbalanced pool manipulation
- Fee extraction attacks
- LP token inflation attacks

**Mitigations:**
- ✅ Minimum liquidity locks (MINIMUM_LIQUIDITY constant)
- ✅ Fee rate limits
- ✅ Emergency pause mechanisms
- ✅ Circuit breakers on large withdrawals

---

### 3. Governance Attacks

#### 3.1 Vote Buying / Bribery

**Threat Level:** MEDIUM

**Description:** Attacker bribes token holders to influence governance outcomes.

**Attack Vectors:**
- External bribery platforms
- Flash loan voting
- Vote delegation to attackers
- Dark pools for vote trading

**Mitigations:**
- ✅ Vote escrow with time-weighted power (`VotingEscrow.sol`)
- ✅ Conviction voting for sustained support (`ConvictionVoting.sol`)
- ✅ Quadratic voting to reduce whale influence (`QuadraticVoting.sol`)
- ✅ Snapshot-based voting power
- ✅ Minimum proposal thresholds

---

#### 3.2 Governance Takeover

**Threat Level:** CRITICAL

**Description:** Malicious actor gains sufficient voting power to control protocol.

**Attack Vectors:**
- 51% token accumulation
- Flash loan governance attacks
- Delegation exploitation
- Emergency mechanism abuse

**Mitigations:**
- ✅ Timelock delays on all governance actions
- ✅ Multi-sig guardian system
- ✅ Emergency governance override
- ✅ Proposal threshold requirements
- ✅ Quorum requirements

---

### 4. Cross-Chain Attacks

#### 4.1 Bridge Exploits

**Threat Level:** CRITICAL

**Description:** Attacker exploits bridge vulnerabilities to mint unbacked tokens.

**Attack Vectors:**
- Message replay attacks
- Fake proof submission
- Validator collusion
- Race conditions in message processing

**Mitigations:**
- ✅ Message nonces to prevent replay (`MessageRouter.sol`)
- ✅ Merkle proof verification
- ✅ Multi-validator consensus (`BFTConsensus.sol`)
- ✅ Fraud proofs with challenge periods (`OptimisticRollupBridge.sol`)
- ✅ Rate limiting on minting

**Affected Contracts:**
- `CrossChainBridge.sol`
- `MessageRouter.sol`
- `OptimisticRollupBridge.sol`
- `WrappedToken.sol`

---

#### 4.2 Optimistic Rollup Fraud

**Threat Level:** HIGH

**Description:** Fraudulent state roots submitted and not challenged.

**Attack Vectors:**
- Challenge period bypass
- Challenger griefing
- Invalid state transitions

**Mitigations:**
- ✅ 7-day challenge period
- ✅ Fraud proof verification system
- ✅ Staked challengers with incentives
- ✅ State commitment verification

---

### 5. Infrastructure Attacks

#### 5.1 Key Compromise

**Threat Level:** CRITICAL

**Description:** Private keys for admin/owner accounts are compromised.

**Attack Vectors:**
- Phishing attacks
- Social engineering
- Infrastructure breaches
- Insider threats

**Mitigations:**
- ✅ Multi-signature wallets for admin operations
- ✅ Hardware wallet requirements
- ✅ Timelock on administrative changes
- ✅ Guardian recovery system
- ✅ Role separation (no single point of failure)

---

#### 5.2 Denial of Service

**Threat Level:** MEDIUM

**Description:** Attacker prevents normal protocol operation.

**Attack Vectors:**
- Block stuffing
- Gas price manipulation
- Contract state bloating
- Unbounded loops

**Mitigations:**
- ✅ Gas-efficient implementations
- ✅ Bounded iterations in all loops
- ✅ Pagination for bulk operations
- ✅ Rate limiting (`RateLimiter.sol`)
- ✅ Circuit breakers (`CircuitBreaker.sol`)

---

## Risk Matrix

| Threat | Likelihood | Impact | Risk Level | Status |
|--------|------------|--------|------------|--------|
| Reentrancy | Medium | Critical | HIGH | Mitigated |
| Integer Overflow | Low | Critical | LOW | Mitigated |
| Access Control | Medium | Critical | HIGH | Mitigated |
| Oracle Manipulation | High | Critical | CRITICAL | Mitigated |
| Flash Loan Attacks | High | Critical | CRITICAL | Mitigated |
| MEV/Front-Running | High | High | HIGH | Partially Mitigated |
| Liquidity Drainage | Medium | High | HIGH | Mitigated |
| Vote Buying | Medium | Medium | MEDIUM | Partially Mitigated |
| Governance Takeover | Low | Critical | HIGH | Mitigated |
| Bridge Exploits | Medium | Critical | CRITICAL | Mitigated |
| Key Compromise | Low | Critical | HIGH | Mitigated |
| DoS | Medium | Medium | MEDIUM | Mitigated |

---

## Security Measures by Contract Category

### DeFi Primitives
- Flash loan guards
- Slippage protection
- Oracle validation
- Liquidation health checks

### Governance
- Timelock enforcement
- Quorum requirements
- Vote snapshots
- Guardian oversight

### Cross-Chain
- Message authentication
- Nonce tracking
- Challenge periods
- Rate limiting

### Access Control
- Role-based permissions
- Multi-sig requirements
- Emergency pause
- Recovery procedures

---

## Incident Response Plan

### Level 1: Suspicious Activity
1. Monitor alerts triggered
2. Security team notified
3. Investigation initiated
4. No protocol changes

### Level 2: Confirmed Vulnerability
1. Emergency governance convened
2. Affected modules paused
3. Fix developed and tested
4. Coordinated disclosure

### Level 3: Active Exploit
1. Global pause activated
2. All guardians notified
3. Bridge operations halted
4. User communication issued
5. Post-mortem conducted

### Level 4: Critical Breach
1. Full protocol lockdown
2. Recovery mode initiated
3. Fund recovery prioritized
4. External audit engaged
5. Legal team engaged

---

## Audit History

| Date | Auditor | Scope | Status |
|------|---------|-------|--------|
| TBD | TBD | Core DeFi | Pending |
| TBD | TBD | Governance | Pending |
| TBD | TBD | Cross-Chain | Pending |

---

## Bug Bounty Program

### Rewards
- Critical: Up to $500,000
- High: Up to $50,000
- Medium: Up to $10,000
- Low: Up to $1,000

### Scope
- All deployed smart contracts
- Bridge infrastructure
- Governance mechanisms

### Out of Scope
- Already known issues
- Theoretical attacks without PoC
- Social engineering

---

## Continuous Security

### Monitoring
- Real-time transaction monitoring
- Anomaly detection
- Oracle price tracking
- Bridge balance verification

### Testing
- Invariant fuzzing (Foundry)
- Symbolic execution (Mythril)
- Static analysis (Slither)
- Manual review cycles

### Updates
- This threat model reviewed quarterly
- New threats assessed continuously
- Mitigations updated as needed

---

*Last Updated: [Current Date]*
*Version: 1.0*
