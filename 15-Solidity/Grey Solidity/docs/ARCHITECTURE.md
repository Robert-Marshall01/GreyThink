# Grey Ecosystem Architecture

## Executive Summary

The Grey Ecosystem is a production-grade blockchain infrastructure built on Solidity 0.8.28, demonstrating comprehensive expertise across consensus mechanisms, cryptography, DeFi protocols, governance systems, cross-chain interoperability, and Layer 2 scaling solutions.

**Key Metrics:**
- **110 Smart Contracts**
- **61,000+ Lines of Solidity**
- **21 Comprehensive Test Suites**
- **Full Documentation Suite**

---

## System Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           GREY ECOSYSTEM                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐             │
│  │   Governance    │  │     DeFi        │  │   Cross-Chain   │             │
│  │                 │  │                 │  │                 │             │
│  │ • Governor      │  │ • LiquidityPool │  │ • Bridge        │             │
│  │ • Timelock      │  │ • LendingPool   │  │ • MessageRouter │             │
│  │ • Treasury      │  │ • Vault         │  │ • StateCommit   │             │
│  │ • VotingEscrow  │  │ • YieldAggr     │  │ • FraudProof    │             │
│  │ • Conviction    │  │ • Liquidation   │  │ │               │             │
│  │ • Quadratic     │  │ • Synthetics    │  │ └───────────────┘             │
│  └────────┬────────┘  └────────┬────────┘                                  │
│           │                    │                                            │
│           └────────────────────┴────────────────────┐                       │
│                                                     │                       │
│  ┌─────────────────┐  ┌─────────────────┐          │                       │
│  │    Tokens       │  │    Oracles      │          │                       │
│  │                 │  │                 │          │                       │
│  │ • ERC20         │  │ • MedianOracle  │          │                       │
│  │ • ERC721        │  │ • TWAPOracle    │          │                       │
│  │ • ERC1155       │  │ • Chainlink     │          │                       │
│  │ • Soulbound     │  │                 │          │                       │
│  └────────┬────────┘  └────────┬────────┘          │                       │
│           │                    │                    │                       │
│           └────────────────────┴────────────────────┘                       │
│                                │                                            │
│  ┌─────────────────────────────┴───────────────────────────────┐           │
│  │                      Core Infrastructure                     │           │
│  │                                                              │           │
│  │  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐        │           │
│  │  │   Security   │ │   Identity   │ │   Consensus  │        │           │
│  │  │              │ │              │ │              │        │           │
│  │  │ • MultiSig   │ │ • Registry   │ │ • BFT        │        │           │
│  │  │ • RateLimiter│ │ • Credentials│ │ • Validators │        │           │
│  │  │ • Circuit    │ │ • Attestation│ │ • Slashing   │        │           │
│  │  │   Breaker    │ │ • Soulbound  │ │ • LightClient│        │           │
│  │  └──────────────┘ └──────────────┘ └──────────────┘        │           │
│  │                                                              │           │
│  │  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐        │           │
│  │  │  Libraries   │ │  Factories   │ │ Upgradeable  │        │           │
│  │  │              │ │              │ │              │        │           │
│  │  │ • MathUtils  │ │ • TokenFact  │ │ • UUPS       │        │           │
│  │  │ • ArrayUtils │ │ • ProxyFact  │ │ • Transparent│        │           │
│  │  │ • Merkle     │ │              │ │ • Beacon     │        │           │
│  │  │ • Signature  │ │              │ │              │        │           │
│  │  └──────────────┘ └──────────────┘ └──────────────┘        │           │
│  └──────────────────────────────────────────────────────────────┘           │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Module Deep Dives

### 1. Consensus & Validator Systems

#### BFT Consensus (`contracts/consensus/BFTConsensus.sol`)

Implements Byzantine Fault Tolerant consensus for validator coordination:

```
Consensus Flow:
┌──────────┐    ┌──────────┐    ┌──────────┐    ┌──────────┐
│ Pre-Prep │───▶│  Prepare │───▶│  Commit  │───▶│ Execute  │
└──────────┘    └──────────┘    └──────────┘    └──────────┘
     │               │               │               │
     │          2f+1 votes      2f+1 votes          │
     │               │               │               │
     └───────────────┴───────────────┴───────────────┘
                     View Change on Timeout
```

**Security Model:**
- Tolerates up to f Byzantine validators where n ≥ 3f+1
- View change mechanism for leader failures
- Cryptographic vote verification

#### Validator Registry (`contracts/consensus/ValidatorRegistry.sol`)

Manages validator lifecycle:
- Self-stake requirement (32 ETH minimum)
- Delegation with voting power calculation
- Epoch-based rewards distribution
- Jailing and unjailing mechanics

#### Slashing Manager (`contracts/consensus/SlashingManager.sol`)

Enforces protocol rules through economic penalties:
- Double-signing detection
- Liveness violations
- Configurable slash percentages
- Appeal mechanism with timelock

---

### 2. DeFi Protocol Architecture

#### Liquidity Pool (`contracts/defi/LiquidityPool.sol`)

Implements constant-product AMM (x * y = k):

```
Price Impact Calculation:
┌─────────────────────────────────────────────────────────┐
│                                                         │
│   ΔY = (Y * ΔX) / (X + ΔX) - fee                       │
│                                                         │
│   Where:                                                │
│   - X, Y = Current reserves                             │
│   - ΔX = Input amount                                   │
│   - ΔY = Output amount                                  │
│   - fee = 0.3% (configurable)                           │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

**Key Features:**
- LP token minting proportional to liquidity share
- Flash loan support with mandatory repayment
- Price oracle with TWAP calculation
- MEV-resistant minimum output requirements

#### Liquidation Engine (`contracts/defi/LiquidationEngine.sol`)

Dutch auction liquidation system:

```
Auction Price Decay:

Price
  │
  │████████
  │         ████████
  │                  ████████
  │                           ████████
  │                                    ████████
  └──────────────────────────────────────────────── Time
  Start Price                           End Price
  (Premium)                             (Discount)
```

**Liquidation Flow:**
1. Health check: `collateralValue * LTV / debtValue`
2. Auction initiation with premium
3. Linear price decay over duration
4. Execution at market-clearing price
5. Penalty distribution to protocol

#### Synthetic Asset Factory (`contracts/defi/SyntheticAssetFactory.sol`)

CDP-based synthetic minting (MakerDAO-style):

```
CDP Mechanics:
┌─────────────────────────────────────────────────────────┐
│                                                         │
│   Collateralization Ratio = Collateral Value / Debt    │
│                                                         │
│   Minimum CR: 150% (configurable per asset)             │
│   Liquidation: Triggered when CR < MCR                  │
│                                                         │
│   Interest Accrual:                                     │
│   newDebt = debt * (1 + rate) ^ timeElapsed            │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

---

### 3. Cross-Chain & Layer 2

#### Optimistic Rollup Bridge (`contracts/crosschain/OptimisticRollupBridge.sol`)

L1 contract for optimistic rollup with fraud proofs:

```
State Commitment Lifecycle:
┌─────────────────────────────────────────────────────────────────────┐
│                                                                     │
│   ┌───────────┐    ┌───────────────┐    ┌────────────────┐         │
│   │  Submit   │───▶│  Challenge    │───▶│  Finalize or   │         │
│   │  Batch    │    │  Period (7d)  │    │  Invalidate    │         │
│   └───────────┘    └───────────────┘    └────────────────┘         │
│        │                   │                    │                   │
│        │                   │                    │                   │
│        ▼                   ▼                    ▼                   │
│   Sequencer posts     Anyone can           Batch becomes           │
│   state root          challenge            canonical               │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

**Security Guarantees:**
- 7-day challenge period for withdrawals
- Bond requirements for sequencers (10 ETH)
- Fraud proof verification slashes malicious actors
- Emergency exit mechanism when bridge compromised

#### State Commitment Chain (`contracts/crosschain/StateCommitmentChain.sol`)

Interactive fraud proof with bisection game:

```
Bisection Dispute Resolution:
┌─────────────────────────────────────────────────────────────────────┐
│                                                                     │
│   Block 0                                             Block 1000   │
│   ├────────────────────────────────────────────────────────┤       │
│                              ▼                                      │
│   Block 0                  Block 500            Block 1000         │
│   ├─────────────────────────┤─────────────────────┤                │
│                              ▼                                      │
│   Block 500               Block 750             Block 1000         │
│   ├────────────────────────┤────────────────────┤                  │
│                              ▼                                      │
│                    ... (log₂ steps) ...                            │
│                              ▼                                      │
│   Block N                Block N+1              Single Step        │
│   ├──────────────────────┤──────────────────────▶ Execution       │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘

Complexity: O(log n) rounds to identify fraudulent step
```

---

### 4. Governance Systems

#### Voting Power Flow

```
Token Holder
     │
     ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Delegate or    │───▶│  VotingEscrow   │───▶│   Governor      │
│  Lock Tokens    │    │  (ve Tokens)    │    │   Contract      │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                              │                       │
                              │                       │
                        ┌─────┴─────┐           ┌─────┴─────┐
                        │ Time Lock │           │ Proposal  │
                        │ Multiplier│           │ Execution │
                        └───────────┘           └───────────┘

Voting Power = Balance × Time_Multiplier × Participation_Bonus
```

#### Governance Variants

| System | Mechanism | Use Case |
|--------|-----------|----------|
| Standard | 1 token = 1 vote | Simple decisions |
| Quadratic | √(tokens) votes | Democratic equality |
| Conviction | Time-weighted | Anti-flash loan |
| VotingEscrow | Lock duration boost | Long-term alignment |

---

### 5. Security Architecture

#### Defense in Depth

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Layer 1: Access Control                     │
│                                                                     │
│   ┌─────────────────────────────────────────────────────────────┐  │
│   │                    Layer 2: Rate Limiting                    │  │
│   │                                                              │  │
│   │   ┌──────────────────────────────────────────────────────┐  │  │
│   │   │                Layer 3: Timelocks                     │  │  │
│   │   │                                                       │  │  │
│   │   │   ┌───────────────────────────────────────────────┐  │  │  │
│   │   │   │           Layer 4: Circuit Breakers           │  │  │  │
│   │   │   │                                               │  │  │  │
│   │   │   │   ┌───────────────────────────────────────┐  │  │  │  │
│   │   │   │   │    Layer 5: Reentrancy Guards        │  │  │  │  │
│   │   │   │   │                                       │  │  │  │  │
│   │   │   │   │   ┌───────────────────────────────┐  │  │  │  │  │
│   │   │   │   │   │  Core Business Logic          │  │  │  │  │  │
│   │   │   │   │   └───────────────────────────────┘  │  │  │  │  │
│   │   │   │   └───────────────────────────────────────┘  │  │  │  │
│   │   │   └───────────────────────────────────────────────┘  │  │  │
│   │   └──────────────────────────────────────────────────────┘  │  │
│   └─────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

#### Security Components

| Component | Purpose | Configuration |
|-----------|---------|---------------|
| MultiSigWallet | Multi-party authorization | N-of-M signatures |
| RateLimiter | Prevent abuse | Per-address limits |
| CircuitBreaker | Emergency pause | Threshold triggers |
| FlashLoanGuard | MEV protection | Same-block checks |
| TimelockGuard | Change delay | 24-48h timelock |

---

### 6. Identity & Attestations

#### W3C DID-Compatible Identity

```
DID Document Structure:
┌─────────────────────────────────────────────────────────────────────┐
│                                                                     │
│  did:grey:0x1234...5678                                            │
│                                                                     │
│  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐          │
│  │ Authentication│  │ Key Agreement │  │ Assertion     │          │
│  │ Keys          │  │ Keys          │  │ Methods       │          │
│  └───────────────┘  └───────────────┘  └───────────────┘          │
│                                                                     │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │                     Service Endpoints                         │ │
│  │  • messaging: https://comm.grey.io/123                        │ │
│  │  • storage: ipfs://Qm...                                       │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                                                     │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │                      Attestations                             │ │
│  │  • KYC verified by Issuer A (expires: 2025-01-01)             │ │
│  │  • Credit score attested by Issuer B                          │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

#### Soulbound Credentials

ERC-5192 compliant non-transferable tokens:
- Consent-based issuance
- Revocation with reason tracking
- Batch operations
- Expiration handling

---

## Threat Model

### External Threats

| Threat | Impact | Mitigation |
|--------|--------|------------|
| Flash Loan Attacks | Governance manipulation | Checkpoint-based voting power |
| Oracle Manipulation | Price-dependent liquidations | Multi-source median oracle |
| Reentrancy | Fund theft | ReentrancyGuard + CEI pattern |
| Front-running | Value extraction | Commit-reveal, slippage protection |
| Griefing | DoS | Gas limits, rate limiting |

### Internal Threats

| Threat | Impact | Mitigation |
|--------|--------|------------|
| Admin Key Compromise | Total loss | MultiSig + Timelock |
| Upgradeability Abuse | Logic replacement | Governance-controlled upgrades |
| Oracle Collusion | False prices | Staking + Slashing |

---

## Economic Model

### Token Flow

```
                    ┌────────────────────┐
                    │    Grey Token      │
                    │    (GREY)          │
                    └─────────┬──────────┘
                              │
          ┌───────────────────┼───────────────────┐
          │                   │                   │
          ▼                   ▼                   ▼
    ┌───────────┐      ┌───────────┐      ┌───────────┐
    │  Staking  │      │ Governance│      │   Fees    │
    │  Rewards  │      │  Power    │      │  Payment  │
    └─────┬─────┘      └─────┬─────┘      └─────┬─────┘
          │                  │                  │
          │                  │                  │
          ▼                  ▼                  ▼
    ┌───────────┐      ┌───────────┐      ┌───────────┐
    │ veGREY    │      │ Proposal  │      │ Protocol  │
    │ (locked)  │◀────▶│ Voting    │      │ Treasury  │
    └───────────┘      └───────────┘      └───────────┘
```

### Fee Distribution

| Source | Distribution |
|--------|-------------|
| Swap Fees (0.3%) | 80% LPs, 20% Treasury |
| Lending Interest | 90% Lenders, 10% Treasury |
| Liquidation Penalty | 60% Liquidator, 40% Protocol |
| Bridge Fees | 100% Treasury |

---

## Deployment Architecture

### Network Configuration

```javascript
// hardhat.config.js
networks: {
  mainnet: { chainId: 1, url: process.env.MAINNET_RPC },
  sepolia: { chainId: 11155111, url: process.env.SEPOLIA_RPC },
  arbitrum: { chainId: 42161, url: process.env.ARBITRUM_RPC },
  optimism: { chainId: 10, url: process.env.OPTIMISM_RPC },
}
```

### Deployment Order

1. **Core Infrastructure**
   - Libraries (MathUtils, ArrayUtils, etc.)
   - AccessControlManager
   - Registry

2. **Token Layer**
   - GreyToken
   - TokenFactory

3. **DeFi Protocols**
   - Oracle system
   - LiquidityPool
   - LendingPool
   - Vault

4. **Governance**
   - GreyTimelock
   - GreyGovernor
   - Treasury

5. **Cross-Chain**
   - OptimisticRollupBridge
   - MessageRouter

6. **Role Configuration**
   - Grant roles to governance contracts
   - Revoke deployer admin roles

---

## Testing Strategy

### Test Categories

| Category | Purpose | Coverage Target |
|----------|---------|-----------------|
| Unit | Individual function logic | 100% |
| Integration | Multi-contract flows | 90% |
| Fuzz | Random input handling | Edge cases |
| Invariant | System properties | Critical paths |
| Adversarial | Attack simulation | All vectors |

### Running Tests

```bash
# Full test suite
npx hardhat test

# Specific category
npx hardhat test test/Security.comprehensive.test.js

# With coverage
npx hardhat coverage

# Gas reporting
REPORT_GAS=true npx hardhat test
```

---

## Audit Checklist

### Pre-Deployment

- [ ] All tests passing
- [ ] 100% statement coverage
- [ ] Slither analysis clean
- [ ] Echidna invariant tests
- [ ] Gas optimization review
- [ ] Role permissions verified
- [ ] Timelock delays configured
- [ ] Emergency procedures documented

### Post-Deployment

- [ ] Verify all contracts on Etherscan
- [ ] Monitor initial transactions
- [ ] Bug bounty program live
- [ ] Incident response team ready

---

## Appendix

### Contract Size Summary

| Module | Contract Count | Lines of Code |
|--------|---------------|---------------|
| Tokens | 4 | ~4,000 |
| DeFi | 21 | ~15,000 |
| Governance | 8 | ~6,000 |
| Security | 8 | ~4,500 |
| Cross-Chain | 5 | ~5,000 |
| Consensus | 4 | ~4,000 |
| Identity | 3 | ~3,000 |
| Marketplace | 5 | ~4,500 |
| Cryptography | 4 | ~3,500 |
| Tokenomics | 4 | ~3,000 |
| Upgradeable | 4 | ~2,500 |
| Libraries | 9 | ~3,000 |
| Factories | 3 | ~1,500 |
| Interfaces | 16 | ~2,000 |
| Mocks & Test | 9 | ~2,000 |
| **Total** | **110** | **61,000+** |

### External Dependencies

- OpenZeppelin Contracts v5.0.0
- OpenZeppelin Contracts Upgradeable v5.0.0
- Hardhat Toolbox v4.0.0
- Hardhat Upgrades v3.0.0
