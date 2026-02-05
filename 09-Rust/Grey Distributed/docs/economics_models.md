# Grey Distributed — Economic Models

This document describes the economic models underlying Grey Distributed's resource pricing, token economics, and cost optimization strategies.

## Table of Contents

1. [Overview](#overview)
2. [Resource Pricing Models](#resource-pricing-models)
3. [Token Economics](#token-economics)
4. [Cost Optimization](#cost-optimization)
5. [Market Dynamics](#market-dynamics)
6. [Economic Sustainability](#economic-sustainability)

---

## Overview

Grey Distributed employs a token-based internal economy to:

- **Allocate resources fairly** across competing tenants
- **Incentivize efficient usage** through dynamic pricing
- **Enable federation** with transparent cross-cluster accounting
- **Prevent abuse** through penalties and quotas

### Design Principles

| Principle | Description |
|-----------|-------------|
| **Transparency** | All pricing formulas are deterministic and auditable |
| **Fairness** | No tenant can monopolize resources at others' expense |
| **Efficiency** | Prices reflect true resource scarcity |
| **Predictability** | Tenants can estimate costs before committing |
| **Sustainability** | Token supply and rewards remain balanced |

---

## Resource Pricing Models

### Compute Pricing (CPU & Memory)

#### Base Formula

```
price = base_price × demand_multiplier × tier_discount × time_factor

where:
  base_price = configured base rate per resource-unit-hour
  demand_multiplier = f(current_utilization, queue_depth)
  tier_discount = discount based on tenant tier (1.0, 0.9, 0.8, 0.7)
  time_factor = adjustment for time-of-day or spot pricing
```

#### Demand Multiplier

The demand multiplier adjusts prices based on current system load:

```
demand_multiplier = 1 + α × (utilization - threshold)² × sign(utilization - threshold)

where:
  α = price sensitivity coefficient (default: 2.0)
  utilization = current resource utilization (0-1)
  threshold = target utilization (default: 0.7)
```

**Behavior:**

| Utilization | Multiplier | Effect |
|-------------|------------|--------|
| 0% - 50% | 0.8 - 0.95 | Slight discount (encourage usage) |
| 50% - 70% | 0.95 - 1.0 | Near base price |
| 70% - 85% | 1.0 - 1.5 | Moderate premium |
| 85% - 95% | 1.5 - 3.0 | High premium (discourage non-urgent) |
| 95% - 100% | 3.0 - 5.0 | Peak pricing |

#### Tier Discounts

| Tier | Discount | Commitment |
|------|----------|------------|
| Pay-as-you-go | 0% | None |
| Standard | 10% | 1-month minimum |
| Professional | 20% | 6-month minimum |
| Enterprise | 30% | 12-month minimum |

### Network Pricing

#### Bandwidth Pricing

```
price = base_rate × volume × direction_multiplier × congestion_factor

where:
  base_rate = tokens per GB
  volume = data transferred in GB
  direction_multiplier:
    - ingress: 0.5 (cheaper to receive)
    - egress: 1.0 (standard)
    - cross_region: 2.0 (more expensive)
    - cross_federation: 3.0 (most expensive)
  congestion_factor = 1 + β × max(0, link_utilization - 0.8)
```

#### Request Pricing

```
price = base_request_rate × request_count × complexity_factor

where:
  complexity_factor:
    - simple GET: 1.0
    - complex query: 2.0
    - write operation: 1.5
    - transaction: 3.0
```

### Storage Pricing

#### Capacity Pricing

```
monthly_cost = capacity_gb × storage_class_rate × redundancy_factor

where:
  storage_class_rate:
    - archive: $0.004/GB
    - cold: $0.01/GB
    - standard: $0.023/GB
    - ssd: $0.10/GB
    - nvme: $0.20/GB
  redundancy_factor:
    - single: 1.0
    - replicated_2: 1.8
    - replicated_3: 2.5
    - erasure_coded: 1.4
```

#### IOPS Pricing

```
iops_cost = provisioned_iops × iops_rate + burst_iops × burst_rate

where:
  iops_rate = base rate per 1000 IOPS/month
  burst_rate = 3 × iops_rate (penalty for exceeding provisioned)
```

---

## Token Economics

### Token Model

Grey Tokens (GRT) are internal accounting units with the following properties:

| Property | Value |
|----------|-------|
| Divisibility | 1 GRT = 1,000,000 micro-tokens |
| Expiration | 365 days from allocation |
| Transferability | Between tenants (optional) |
| Backing | Not backed by external currency |

### Token Lifecycle

```
┌─────────────┐     ┌────────────┐     ┌────────────┐     ┌──────────┐
│   Minted    │────►│  Allocated │────►│  Reserved  │────►│ Consumed │
│ (Creation)  │     │  (Credit)  │     │   (Hold)   │     │ (Spent)  │
└─────────────┘     └────────────┘     └────────────┘     └──────────┘
                          │                  │
                          │ expiration       │ release
                          ▼                  ▼
                    ┌────────────┐     ┌────────────┐
                    │  Expired   │     │  Released  │
                    └────────────┘     └────────────┘
```

### Token Supply Management

#### Inflation Control

```
daily_mint_limit = min(
  absolute_cap,
  base_mint × (1 + growth_factor × tenant_growth_rate),
  total_supply × max_inflation_rate
)

where:
  absolute_cap = 10,000,000 tokens/day
  base_mint = 1,000,000 tokens/day
  growth_factor = 0.5
  max_inflation_rate = 0.001 (0.1% per day)
```

#### Token Velocity

Token velocity measures how quickly tokens circulate:

```
velocity = transaction_volume / average_token_supply

Target velocity: 0.3 - 0.5 per day
```

| Velocity | Interpretation | Action |
|----------|---------------|--------|
| < 0.2 | Tokens hoarded | Increase rewards, reduce prices |
| 0.3 - 0.5 | Healthy | Maintain current policy |
| > 0.7 | Tokens churning fast | May indicate speculation |

---

## Cost Optimization

### Tenant Cost Optimization Strategies

#### 1. Tier Selection

Expected savings from tier commitment:

```
savings = monthly_usage × tier_discount × commitment_months
risk = monthly_usage × (1 - actual_usage_ratio) × commitment_months
net_benefit = savings - risk
```

**Recommendation:** Commit to higher tiers when usage variance < 20%.

#### 2. Resource Right-Sizing

```
optimal_allocation = observed_peak × (1 + safety_margin)
waste = allocated - optimal_allocation
cost_savings = waste × unit_price

Recommended safety_margin: 15% for production, 5% for dev/test
```

#### 3. Spot/Preemptible Usage

```
spot_savings = (on_demand_price - spot_price) × spot_hours
spot_risk = interruption_rate × restart_cost × spot_hours

Use spot when: spot_savings > spot_risk × risk_tolerance
```

#### 4. Storage Tiering

```
tiering_savings = sum(
  data_in_tier × (current_tier_price - optimal_tier_price)
)

Optimal tier selection based on access frequency:
- Last accessed > 90 days → archive
- Last accessed > 30 days → cold  
- Last accessed > 7 days → standard
- Frequently accessed → ssd/nvme
```

### System Cost Optimization

#### Capacity Planning

```
optimal_capacity = projected_demand × (1 + capacity_buffer)
                 × (1 + growth_reserve)

where:
  capacity_buffer = 0.15 (15% for demand spikes)
  growth_reserve = projected_growth_rate × planning_horizon
```

#### Resource Bin Packing

Maximize resource utilization through efficient placement:

```
utilization_score = sum(resource_used / resource_capacity) / num_resources
fragmentation_score = 1 - (smallest_fitting_request / largest_free_block)

Optimization target: maximize utilization_score while fragmentation_score < 0.3
```

---

## Market Dynamics

### Price Discovery

Grey Distributed uses a **modified Dutch auction** for price discovery during high demand:

```
current_price = base_price × demand_multiplier

demand_multiplier updates:
- Every 5 minutes based on queue depth
- Immediately on utilization threshold breach
- Smoothed with exponential moving average (α = 0.3)
```

### Supply and Demand Equilibrium

```
equilibrium_price = marginal_cost × (1 + target_margin)
                  × supply_demand_ratio

where:
  marginal_cost = variable cost of providing one more unit
  target_margin = 0.2 (20% margin)
  supply_demand_ratio = available_capacity / requested_capacity
```

### Cross-Cluster Arbitrage

Federation enables resource arbitrage between clusters:

```
arbitrage_opportunity = price_cluster_A - price_cluster_B
                      - transfer_cost
                      - federation_fee

Execute arbitrage when: arbitrage_opportunity > min_profit_threshold
```

---

## Economic Sustainability

### Revenue Model

```
total_revenue = resource_consumption_revenue
              + premium_tier_revenue
              + federation_fees
              - rewards_distributed
              - operational_costs

Sustainability constraint: total_revenue ≥ operational_costs × (1 + reserve_rate)
```

### Incentive Budget

```
daily_incentive_budget = min(
  revenue × incentive_percentage,
  absolute_incentive_cap
)

where:
  incentive_percentage = 0.05 - 0.10 (5-10% of revenue)
  absolute_incentive_cap = prevents runaway rewards
```

### Long-term Token Value

To maintain token value stability:

1. **Burn Mechanism**: Penalties permanently remove tokens
2. **Expiration**: Unused tokens expire after 365 days
3. **Reward Caps**: Maximum rewards per tenant per day
4. **Inflation Targets**: Keep below 3% annually

### Economic Health Metrics

| Metric | Healthy Range | Action if Outside |
|--------|--------------|-------------------|
| Token Velocity | 0.3 - 0.5 | Adjust pricing/rewards |
| Reward/Revenue Ratio | 5% - 10% | Adjust incentive budget |
| Price Variance | < 50%/day | Smooth pricing algorithm |
| Tenant Balance Gini | < 0.6 | Review fairness policies |
| Utilization | 60% - 85% | Add/remove capacity |

---

## Appendix: Economic Formulas Reference

### Quick Reference

| Formula | Purpose |
|---------|---------|
| `price = base × demand × tier × time` | Resource pricing |
| `reward = base × timeliness × quality × reputation` | Task rewards |
| `penalty = base × escalation × repeat × duration` | Violations |
| `fairness = (Σxᵢ)² / (n × Σxᵢ²)` | Jain's Index |
| `velocity = transactions / supply` | Token health |

### Symbol Glossary

| Symbol | Meaning | Typical Value |
|--------|---------|---------------|
| α | Price sensitivity | 2.0 |
| β | Congestion factor | 1.5 |
| γ | Reward decay rate | 0.8 |
| δ | Penalty escalation | 1.5 |
| θ | Utilization threshold | 0.70 |

---

*Last updated: 2024*
