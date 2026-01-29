# Grey Protocol Economic Design

## Overview

This document outlines the economic rationale, tokenomics, and incentive mechanisms of the Grey Protocol ecosystem. It provides the foundation for understanding value flows, stakeholder incentives, and economic sustainability.

---

## 1. Token Economics (Tokenomics)

### 1.1 GREY Token

The GREY token serves as the primary utility and governance token for the ecosystem.

#### Token Properties
- **Standard:** ERC-20
- **Total Supply:** 1,000,000,000 GREY (1 billion)
- **Decimal Places:** 18
- **Network:** Ethereum Mainnet (with cross-chain bridges)

#### Distribution

| Allocation | Percentage | Amount | Vesting |
|------------|------------|--------|---------|
| Community Treasury | 40% | 400,000,000 | Governed by DAO |
| Team & Advisors | 20% | 200,000,000 | 4-year vest, 1-year cliff |
| Investors | 15% | 150,000,000 | 2-year vest, 6-month cliff |
| Ecosystem Incentives | 15% | 150,000,000 | Released via rewards |
| Liquidity Provision | 10% | 100,000,000 | Initial liquidity |

### 1.2 Token Utility

1. **Governance Voting**
   - Proposal creation and voting
   - Parameter adjustments
   - Treasury allocation
   - Protocol upgrades

2. **Staking & Security**
   - Validator staking
   - Slashing collateral
   - Insurance pool participation

3. **Fee Discounts**
   - Reduced trading fees
   - Prioritized liquidation
   - Premium feature access

4. **Protocol Revenue Share**
   - Fee distribution to stakers
   - Buyback-and-burn mechanism
   - Treasury accrual

---

## 2. Fee Structure

### 2.1 Trading Fees

#### Constant Product AMM (ConstantProductAMM)
```
Swap Fee: 0.30% (30 basis points)
├── LP Rewards: 0.25% (goes to liquidity providers)
└── Protocol Fee: 0.05% (goes to treasury)
```

#### StableSwap AMM (StableSwapAMM)
```
Swap Fee: 0.04% (4 basis points)
├── LP Rewards: 0.02% (low due to lower risk)
└── Protocol Fee: 0.02%
```

### 2.2 Lending/Borrowing Fees

```
Borrow Interest: Variable rate based on utilization
├── Base Rate: 2% APR
├── Utilization Multiplier: Up to 20% APR at 80% utilization
└── Kink Rate: 100%+ APR above 90% utilization

Origination Fee: 0.1% of borrowed amount
Liquidation Penalty: 5-15% (varies by collateral type)
```

### 2.3 NFT Marketplace Fees

```
Listing Fee: Free
Sale Fee: 2.5% of sale price
├── Platform: 2.0%
└── Curator: 0.5%

Auction Settlement: 0.5% additional
Royalties: 0-10% (creator-defined)
```

### 2.4 Cross-Chain Bridge Fees

```
Bridge Fee: 0.1% of transferred amount
├── Relayer Compensation: 0.05%
├── Validator Rewards: 0.03%
└── Protocol: 0.02%

Minimum Fee: 0.001 ETH equivalent
Gas Subsidy: Up to 50% for small transfers
```

---

## 3. Incentive Mechanisms

### 3.1 Liquidity Mining

**Objective:** Bootstrap and maintain deep liquidity

**Mechanism:** Reward LPs with GREY tokens proportional to:
- Liquidity provided (L)
- Time deposited (t)
- Pool priority weight (w)

```
Reward = Base_Emission × (L / Total_L) × t × w
```

**Emission Schedule:**
| Year | Daily Emission | Annual Total |
|------|----------------|--------------|
| 1 | 500,000 GREY | 182,500,000 |
| 2 | 250,000 GREY | 91,250,000 |
| 3 | 125,000 GREY | 45,625,000 |
| 4+ | Governance-determined | Variable |

### 3.2 Staking Rewards ($veGREY)

**Objective:** Encourage long-term alignment and reduce circulating supply

**Mechanism:** Vote-Escrowed GREY (veGREY)
- Lock GREY for 1 week to 4 years
- Voting power proportional to lock duration
- Reward boost for longer locks

```
veGREY = GREY_amount × (lock_duration / MAX_LOCK_DURATION)
```

**Boost Multiplier:**
| Lock Duration | veGREY Ratio | Reward Boost |
|---------------|--------------|--------------|
| 1 week | 0.005x | 1.0x |
| 1 month | 0.02x | 1.2x |
| 6 months | 0.125x | 1.5x |
| 1 year | 0.25x | 2.0x |
| 4 years | 1.0x | 2.5x |

### 3.3 Conviction Voting

**Objective:** Reward sustained support for proposals

**Mechanism:**
- Voting power accumulates over time
- Early voting has more conviction
- Prevents last-minute vote swings

```
Conviction = (tokens × time_voted) / halflife
where halflife = 3 days
```

### 3.4 Reputation System

**Objective:** Reward positive ecosystem participation

**Metrics Tracked:**
- Successful trades
- Governance participation
- Dispute resolution accuracy
- Validator uptime

**Benefits:**
- Priority access to new features
- Reduced collateral requirements
- Enhanced yield opportunities

---

## 4. Economic Flows

### 4.1 Value Accrual

```
┌─────────────────────────────────────────────────────────────────┐
│                        PROTOCOL REVENUE                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  Trading Fees ────┐                                              │
│  Lending Spread ──┼────► Treasury ────┬──► Buyback & Burn (30%) │
│  Bridge Fees ─────┤                   ├──► veGREY Stakers (50%)  │
│  NFT Fees ────────┘                   └──► Development (20%)     │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

### 4.2 Inflation vs. Deflation Balance

**Inflationary Pressures:**
- Liquidity mining emissions
- Staking rewards
- Ecosystem grants

**Deflationary Pressures:**
- Fee buyback and burn
- Slashing penalties burned
- Unused treasury burns (governance-approved)

**Target:** Net neutral to slightly deflationary after Year 3

### 4.3 Treasury Management

**Diversification Strategy:**
- 40% GREY tokens
- 30% Stablecoins (USDC, DAI)
- 20% Blue-chip assets (ETH, BTC)
- 10% Strategic investments

**Spending Categories:**
- Protocol development
- Security audits
- Marketing and growth
- Community grants
- Emergency reserves

---

## 5. Bonding Curve Mechanics

### 5.1 Advanced Bonding Curve (AdvancedBondingCurve.sol)

**Formula:** Polynomial bonding curve

```
Price = Base_Price × (Supply / Scale)^Exponent

Where:
- Base_Price = Initial price floor
- Supply = Current circulating supply
- Scale = Scaling factor for smoothness
- Exponent = Curve steepness (typically 1-3)
```

**Use Cases:**
- Token launches
- Continuous fundraising
- Dynamic pricing for limited resources

### 5.2 Fee Distribution in Bonding

```
Buy Tax: 1-5% (configurable)
├── LP Provision: 30%
├── Treasury: 30%
├── Burn: 20%
└── Reflection: 20%

Sell Tax: 2-10% (configurable)
├── LP Provision: 40%
├── Treasury: 30%
└── Burn: 30%
```

---

## 6. Liquidation Economics

### 6.1 Liquidation Incentives

**Liquidator Reward:**
```
Liquidation Bonus = Collateral × (1 + Liquidation_Incentive)
Where Liquidation_Incentive = 5-15% based on:
├── Collateral type volatility
├── Time since undercollateralization
└── Protocol risk parameters
```

### 6.2 Insurance Pool (InsurancePool.sol)

**Purpose:** Cover bad debt from failed liquidations

**Funding:**
- 10% of liquidation penalties
- Protocol fee allocation
- Community staking (with yield)

**Coverage:**
- Maximum 100% of bad debt
- Claims require governance approval
- Tiered payout based on severity

---

## 7. Cross-Chain Economic Model

### 7.1 Bridge Economics

**Locked vs. Minted Model:**
```
Source Chain: Lock native tokens
              ├── Lock in bridge contract
              └── Generate proof/message

Destination: Mint wrapped tokens
             ├── Verify proof
             └── Mint 1:1 wrapped token

Invariant: Locked_Tokens >= Minted_Wrapped_Tokens
```

### 7.2 Validator Economics

**Consensus Participation:**
- Stake: Minimum 10,000 GREY
- Reward: Share of bridge fees
- Slashing: Up to 100% for malicious behavior

**Slashing Conditions:**
- Signing invalid state roots: 100%
- Downtime > 24 hours: 1-10%
- Double signing: 50%

---

## 8. Governance Economics

### 8.1 Proposal Costs

**Proposal Threshold:**
- Create proposal: 100,000 veGREY (0.01% of supply)
- Quorum requirement: 4% of veGREY voting
- Approval threshold: 51% simple majority

**Economic Spam Prevention:**
- Proposal deposit: 1,000 GREY (refunded if passed)
- Cooldown between user proposals: 7 days

### 8.2 Delegation Economics

**Delegate Incentives:**
- Delegates receive 10% of governance rewards
- Active voting bonus: 1.2x multiplier
- Delegation is trustless and revocable

---

## 9. Risk Parameters

### 9.1 Collateral Ratios

| Asset Type | LTV | Liquidation Threshold | Liquidation Penalty |
|------------|-----|----------------------|---------------------|
| ETH | 80% | 85% | 5% |
| WBTC | 75% | 80% | 7% |
| Stablecoins | 90% | 95% | 3% |
| GREY | 50% | 60% | 15% |
| NFTs | 30-50% | 40-60% | 10-20% |

### 9.2 Interest Rate Model

**Utilization-Based Rate:**
```
if utilization <= optimal (80%):
    rate = base_rate + (utilization / optimal) × slope1
else:
    rate = base_rate + slope1 + ((utilization - optimal) / (1 - optimal)) × slope2

Where:
- base_rate = 2%
- slope1 = 10%
- slope2 = 100%
```

---

## 10. Sustainability Analysis

### 10.1 Revenue Projections

| Metric | Year 1 | Year 2 | Year 3 |
|--------|--------|--------|--------|
| TVL Target | $100M | $500M | $1B |
| Trading Volume | $2B | $10B | $25B |
| Fee Revenue | $6M | $30M | $75M |
| Net Protocol Revenue | $1.2M | $6M | $15M |

### 10.2 Break-Even Analysis

**Fixed Costs:**
- Development team: $2M/year
- Security audits: $500K/year
- Infrastructure: $200K/year
- Legal/Compliance: $300K/year

**Required TVL for Sustainability:**
- Minimum: $50M (break-even)
- Target: $200M (profitable with reserves)
- Goal: $1B (full feature development)

---

## 11. Economic Governance

All economic parameters are adjustable via governance:

**Adjustable Parameters:**
- Fee rates (within bounds)
- Emission schedules
- Collateral ratios
- Liquidation parameters
- Treasury allocations

**Parameter Bounds:**
- Fees: 0.01% - 5%
- Emissions: 0 - 1,000,000 GREY/day
- LTV: 10% - 90%
- Quorum: 1% - 20%

---

## Appendix A: Mathematical Formulas

### A.1 AMM Pricing
```
Constant Product: x × y = k
StableSwap: A × n^n × Σx + D = A × D × n^n + D^(n+1) / (n^n × Πx)
```

### A.2 Yield Calculations
```
APY = (1 + APR/n)^n - 1
Where n = compounding frequency
```

### A.3 Impermanent Loss
```
IL = 2×√(price_ratio) / (1+price_ratio) - 1
```

---

*This document is subject to change via governance proposals.*

*Last Updated: [Current Date]*
*Version: 1.0*
