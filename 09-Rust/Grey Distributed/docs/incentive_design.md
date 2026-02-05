# Grey Distributed — Incentive Design

This document describes the incentive mechanisms that encourage good behavior, fair resource usage, and cooperation across Grey Distributed's federated infrastructure.

## Table of Contents

1. [Incentive Philosophy](#incentive-philosophy)
2. [Reward Mechanisms](#reward-mechanisms)
3. [Penalty Mechanisms](#penalty-mechanisms)
4. [Fairness Guarantees](#fairness-guarantees)
5. [Federation Incentives](#federation-incentives)
6. [Game-Theoretic Analysis](#game-theoretic-analysis)
7. [Sustainability Analysis](#sustainability-analysis)

---

## Incentive Philosophy

### Core Principles

Grey Distributed's incentive system is designed around four principles:

1. **Aligned Incentives**: Individual optimization leads to system optimization
2. **Fairness First**: No strategy should disadvantage honest participants
3. **Gradual Enforcement**: Penalties escalate; first-time issues get warnings
4. **Transparency**: All rules and outcomes are predictable and auditable

### Incentive Goals

| Goal | Mechanism | Metric |
|------|-----------|--------|
| Efficient resource use | Dynamic pricing | Utilization rate |
| Timely task completion | Completion rewards | Deadline adherence |
| Fair sharing | DRF allocation + penalties | Jain's Fairness Index |
| Federation participation | Sharing rewards | Cross-cluster transactions |
| System reliability | Attestation rewards | Verification rate |

---

## Reward Mechanisms

### Task Completion Rewards

Tenants earn rewards for completing tasks efficiently.

#### Reward Formula

```
reward = base_reward × timeliness_factor × quality_factor × reputation_factor

where:
  base_reward = task_complexity × base_reward_rate
  timeliness_factor = max(0, 1 - α × (actual_time / deadline - 1)²)
  quality_factor = 1 + β × (quality_score - quality_threshold)
  reputation_factor = tenant_reputation_score
```

#### Timeliness Curve

```
                    │
    timeliness     1│━━━━━━━━━━╲
    factor          │           ╲
                 0.5│            ╲
                    │             ╲
                  0 │──────────────╲────────
                    0     1.0    1.5    2.0
                        actual_time / deadline
```

**Interpretation:**

| Completion Time | Timeliness Factor | Outcome |
|-----------------|-------------------|---------|
| ≤ Deadline | 1.0 | Full reward |
| 1.0 - 1.25× | 0.5 - 1.0 | Partial reward |
| > 1.5× | 0 | No reward |

#### Quality Multiplier

```
quality_factor = {
  1.5  if quality_score ≥ 0.95    (Exceptional)
  1.2  if quality_score ≥ 0.90    (Excellent)
  1.0  if quality_score ≥ 0.80    (Good)
  0.8  if quality_score ≥ 0.70    (Acceptable)
  0.0  if quality_score < 0.70    (Rejected)
}
```

### Fairness Bonus Rewards

Tenants that stay within their fair share receive bonuses.

#### Formula

```
fairness_bonus = base_bonus × compliance_factor × duration_factor

where:
  compliance_factor = min(1.0, quota_remaining / (quota × threshold))
  duration_factor = log2(1 + consecutive_compliant_periods)
  threshold = 0.9 (10% buffer before losing bonus)
```

**Example:**

| Quota Used | Compliance Factor | Bonus |
|------------|-------------------|-------|
| 80% | 1.0 | Full bonus |
| 90% | 1.0 | Full bonus |
| 95% | 0.5 | Half bonus |
| 100%+ | 0 | No bonus |

### Reputation Staking

Tenants can stake tokens to boost their reputation multiplier:

```
reputation_factor = base_reputation + stake_bonus

stake_bonus = min(
  max_stake_bonus,
  sqrt(staked_tokens / stake_threshold)
)

max_stake_bonus = 0.5 (50% maximum boost)
stake_threshold = 10,000 tokens
```

**Risk:** Staked tokens are partially forfeited on violations:

```
forfeiture = min(stake, penalty × stake_forfeiture_rate)
stake_forfeiture_rate = 0.1 (10% of penalty from stake)
```

---

## Penalty Mechanisms

### Progressive Penalty Model

Penalties escalate with severity and frequency.

#### Base Penalty Formula

```
penalty = base_penalty × escalation_factor × repeat_multiplier × duration_factor

where:
  base_penalty = resource_specific_base_rate × violation_magnitude
  escalation_factor = 1.5 ^ violation_severity_level
  repeat_multiplier = 1 + 0.5 × violations_in_lookback_window
  duration_factor = 1 + log2(violation_duration_minutes / 10)
```

#### Violation Severity Levels

| Level | Description | Base Penalty | Escalation |
|-------|-------------|--------------|------------|
| 1 | Minor (5-10% over quota) | 10 tokens | 1.0× |
| 2 | Moderate (10-25% over) | 50 tokens | 1.5× |
| 3 | Significant (25-50% over) | 200 tokens | 2.25× |
| 4 | Major (50-100% over) | 500 tokens | 3.4× |
| 5 | Severe (>100% over) | 1000 tokens | 5.0× |

### Grace Period

First-time violations receive a grace period:

```
grace_period = {
  60 seconds  for CPU/Memory
  30 seconds  for Network rate
  300 seconds for Storage
}

During grace period:
  - Warning issued (no penalty)
  - Violation logged for tracking
  - Tenant notified immediately
```

### Repeat Offense Tracking

```
repeat_multiplier = 1 + 0.5 × count(violations in last 24 hours)

Maximum multiplier: 5.0×

Example:
  1st violation today: 1.0×
  2nd violation today: 1.5×
  3rd violation today: 2.0×
  5th violation today: 3.0×
  9th violation today: 5.0× (capped)
```

### Cooling-Off Decay

Good behavior reduces accumulated penalty history:

```
violation_weight decay:
  - After 24 hours clean: 50% reduction
  - After 48 hours clean: 75% reduction
  - After 72 hours clean: 90% reduction
  - After 7 days clean: Full reset
```

### Suspension Thresholds

```
suspension_score = sum(weighted_penalties) over rolling_window

Thresholds:
  suspension_score > 500:  1-hour soft suspension (reduced priority)
  suspension_score > 1000: 4-hour full suspension
  suspension_score > 2000: 24-hour suspension + manual review
  suspension_score > 5000: Account termination pending review
```

---

## Fairness Guarantees

### Dominant Resource Fairness (DRF)

Grey Distributed implements DRF to ensure fair multi-resource allocation.

#### DRF Algorithm

```
For each tenant t:
  dominant_share[t] = max(
    allocated[t][r] / total_capacity[r]
    for all resources r
  )

Allocation rule:
  Priority to tenant with minimum dominant_share
  
  if request fits AND new_dominant_share < fair_share:
    grant request
  else:
    queue request OR reject
```

#### Example

```
System: 100 CPUs, 400 GB RAM
Tenant A: needs 3 CPU, 1 GB per task
Tenant B: needs 1 CPU, 4 GB per task

Fair allocation:
  Tenant A: 25 tasks → uses 75 CPU (75%), 25 GB (6.25%)
  Tenant B: 25 tasks → uses 25 CPU (25%), 100 GB (25%)
  
Both have 75% dominant share (CPU for A, neither for B)
```

### Jain's Fairness Index

```
J(x₁, x₂, ..., xₙ) = (Σxᵢ)² / (n × Σxᵢ²)

where:
  xᵢ = resource allocation for tenant i
  n = number of tenants

Interpretation:
  J = 1.0: Perfect fairness (all equal)
  J = 0.5: Two tenants have all, rest have none
  J = 1/n: One tenant has all
  
Target: J ≥ 0.9
```

### Fairness Enforcement

```
if jain_index < fairness_threshold:
  for each tenant t:
    if allocation[t] > fair_share × 1.1:
      apply_penalty(t, UNFAIR_USAGE)
      reduce_allocation(t, target=fair_share)
```

---

## Federation Incentives

### Resource Sharing Rewards

Clusters earn rewards for sharing resources with federation partners.

#### Sharing Reward Formula

```
sharing_reward = (
  cpu_hours × cpu_rate +
  memory_gb_hours × memory_rate +
  storage_gb_days × storage_rate +
  bandwidth_gb × bandwidth_rate
) × urgency_factor × qos_factor × cooperation_multiplier

where:
  urgency_factor = 1.5 if on-demand request, else 1.0
  qos_factor = measured_quality (0.5 to 1.5)
  cooperation_multiplier = cluster's cooperation score (0.5 to 3.0)
```

### Attestation Participation

#### Reward Distribution

| Role | Reward |
|------|--------|
| Attestation Leader | verification_reward + leader_bonus (35 tokens) |
| Verifier | verification_reward (10 tokens) |
| Attested Cluster (verified) | challenge_response_reward (15 tokens) |
| Attested Cluster (failed) | penalty (-100 tokens) |

### Cooperation Score

```
cooperation_score evolution:
  
  On successful cooperation:
    score = min(3.0, score + 0.05)
  
  On failed cooperation:
    score = max(0.5, score - 0.1)
  
  Daily decay (no activity):
    score = max(0.5, score - 0.01 × days_inactive)
```

### Network Contribution

```
network_reward = latency_bonus + throughput_bonus + uptime_bonus - stability_penalty

where:
  latency_bonus = 30 tokens if avg_latency < 50ms
  throughput_bonus = throughput_gbps × 20 tokens
  uptime_bonus = per_tenth_percent_above_99 × 10 tokens (max 500)
  stability_penalty = (1 - stability_score) × 50 tokens
```

---

## Game-Theoretic Analysis

### Nash Equilibrium Analysis

#### Honest Strategy Payoff

```
E[honest] = base_reward × completion_rate
          + fairness_bonus × compliance_rate
          - rare_accident_penalties
          
≈ 0.85 × base_reward + 0.1 × base_reward
≈ 0.95 × base_reward
```

#### Gaming Strategy Payoff

```
E[gaming] = (base_reward + unfair_gain) × (1 - detection_rate)
          - penalty × detection_rate
          - reputation_loss × future_discount

Detection rate ≈ 0.9 (high observability)
Penalty ≈ 2-5× unfair_gain

E[gaming] ≈ 1.1 × base_reward × 0.1 - 3 × base_reward × 0.9
          ≈ -2.6 × base_reward

Conclusion: Gaming is dominated by honest strategy
```

### Collusion Resistance

#### Anti-Collusion Measures

1. **Random Witness Selection**: Witnesses selected randomly for attestation
2. **Reputation Impact**: Colluding parties lose reputation faster
3. **Cross-Cluster Verification**: Multiple clusters verify claims
4. **Temporal Separation**: Related transactions spread across time

#### Sybil Resistance

```
new_account_restrictions:
  - Reduced quotas for first 7 days
  - No reputation staking for 30 days
  - Higher penalty rates initially
  - Proof-of-work for account creation (optional)
```

### Mechanism Design Properties

| Property | Status | Mechanism |
|----------|--------|-----------|
| Incentive Compatibility | ✓ | Honest strategy dominates |
| Individual Rationality | ✓ | Participation > non-participation |
| Budget Balance | ✓ | Rewards ≤ consumption + penalties |
| Sybil Resistance | ✓ | New account restrictions |
| Collusion Resistance | ✓ | Random selection + verification |

---

## Sustainability Analysis

### Incentive Budget Model

```
daily_budget = min(
  revenue × 0.08,      # 8% of revenue
  1,000,000 tokens,    # Absolute cap
  token_pool × 0.01    # 1% of remaining pool
)

Budget allocation:
  - Task rewards: 50%
  - Fairness bonuses: 20%
  - Federation rewards: 25%
  - Reserve: 5%
```

### Long-term Sustainability

#### Token Flow Analysis

```
Inflows (to users):
  - New allocations (purchases, grants)
  - Rewards distributed
  
Outflows (from users):
  - Resource consumption
  - Penalties assessed
  - Token expiration

Equilibrium condition:
  consumption + penalties + expiration ≈ allocations + rewards
```

#### Sustainability Metrics

| Metric | Target | Action if Violated |
|--------|--------|-------------------|
| Reward/Revenue Ratio | 5-10% | Adjust reward rates |
| Penalty/Reward Ratio | <20% | Review quota settings |
| Token Velocity | 0.3-0.5 | Adjust pricing |
| Fairness Index | >0.9 | Increase fairness weight |
| Surplus/Deficit | ±5% | Adjust token supply |

### Incentive Adjustment Mechanisms

```
Quarterly review process:
  1. Analyze reward/penalty distributions
  2. Compute sustainability metrics
  3. Identify behavioral patterns
  4. Adjust parameters:
     - Base reward rates (±10% max)
     - Penalty escalation factors (±20% max)
     - Fairness thresholds (±5% max)
  5. Announce changes 30 days in advance
```

---

## Appendix: Incentive Parameters Reference

### Reward Parameters

| Parameter | Default | Range | Description |
|-----------|---------|-------|-------------|
| `base_reward_rate` | 10 tokens | 5-20 | Per-task base reward |
| `timeliness_decay_alpha` | 0.8 | 0.5-1.0 | Timeliness penalty curve |
| `quality_threshold` | 0.8 | 0.7-0.9 | Minimum quality for reward |
| `max_reputation_bonus` | 0.5 | 0.3-1.0 | Maximum reputation boost |
| `stake_threshold` | 10,000 | 5k-50k | Tokens to stake for max bonus |

### Penalty Parameters

| Parameter | Default | Range | Description |
|-----------|---------|-------|-------------|
| `base_penalty_cpu` | 50 tokens | 20-100 | CPU violation base penalty |
| `base_penalty_memory` | 50 tokens | 20-100 | Memory violation base |
| `grace_period_seconds` | 60 | 30-120 | First-violation grace |
| `escalation_factor` | 1.5 | 1.2-2.0 | Severity escalation |
| `repeat_multiplier_max` | 5.0 | 3.0-10.0 | Maximum repeat penalty |
| `lookback_window_hours` | 24 | 12-48 | Period for repeat counting |
| `cooling_decay_rate` | 0.5 | 0.3-0.7 | Daily decay for good behavior |

### Fairness Parameters

| Parameter | Default | Range | Description |
|-----------|---------|-------|-------------|
| `fairness_threshold` | 0.9 | 0.8-0.95 | Minimum acceptable fairness |
| `drf_enforcement` | true | - | Enable DRF allocation |
| `fairness_check_interval` | 60s | 30-300s | Fairness check frequency |
| `fairness_bonus_base` | 100 tokens | 50-200 | Daily fairness bonus |

### Federation Parameters

| Parameter | Default | Range | Description |
|-----------|---------|-------|-------------|
| `sharing_cpu_rate` | 50 tokens/h | 30-80 | Per CPU-hour sharing reward |
| `attestation_reward` | 10 tokens | 5-20 | Per-verification reward |
| `leader_bonus` | 25 tokens | 15-40 | Attestation leader bonus |
| `cooperation_increment` | 0.05 | 0.02-0.1 | Score increase per success |
| `cooperation_max` | 3.0 | 2.0-5.0 | Maximum cooperation score |

---

*Last updated: 2024*
