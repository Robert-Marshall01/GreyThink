# Grey Distributed — Governance Economics Guide

This document covers the economic and governance models for operating Grey Distributed clusters, including quota management, resource allocation, cost optimization, and performance economics.

---

## Table of Contents

1. [Overview](#overview)
2. [Quota Management](#quota-management)
3. [Resource Allocation Models](#resource-allocation-models)
4. [Cost Dashboards](#cost-dashboards)
5. [Performance Economics](#performance-economics)
6. [Tenant Billing](#tenant-billing)
7. [Capacity Planning](#capacity-planning)
8. [Optimization Strategies](#optimization-strategies)

---

## Overview

### Governance Principles

Grey Distributed implements **governance-aware scheduling** that balances:

1. **Fairness**: Equitable resource distribution among tenants
2. **Efficiency**: Maximum utilization of cluster resources
3. **Isolation**: Strong guarantees between tenant workloads
4. **Economics**: Cost-effective operation with clear billing

### Key Metrics

| Metric | Description | Target |
|--------|-------------|--------|
| Cluster Utilization | Active CPU/Memory vs. provisioned | 70-85% |
| Tenant Fairness Index | Jain's fairness index across tenants | > 0.9 |
| Cost per Task | Infrastructure cost / tasks completed | Minimize |
| Quota Compliance | % of requests within quota | > 99% |

---

## Quota Management

### Quota Types

Grey supports multiple quota dimensions:

```yaml
quotas:
  # Compute quotas
  cpu_cores: 100          # Maximum concurrent CPU cores
  memory_gb: 256          # Maximum concurrent memory
  
  # Task quotas
  max_concurrent_tasks: 1000    # Tasks running simultaneously
  max_queue_depth: 10000        # Tasks waiting in queue
  max_task_duration: 3600s      # Maximum single task duration
  
  # Rate quotas
  submit_rate: 100/s      # Tasks submitted per second
  api_rate: 1000/s        # API calls per second
  
  # Storage quotas
  state_size_gb: 100      # State storage
  log_retention_days: 30  # Log retention
```

### Viewing Current Quotas

```bash
# List all tenant quotas
curl -s "http://localhost:8080/v1/governance/quotas" | jq

# Get specific tenant quota
curl -s "http://localhost:8080/v1/governance/quotas/tenant-acme" | jq

# Check current usage vs. quota
curl -s "http://localhost:8080/v1/governance/usage/tenant-acme" | jq
```

**Sample output:**

```json
{
  "tenant": "tenant-acme",
  "quotas": {
    "cpu_cores": {"limit": 100, "used": 45, "available": 55},
    "memory_gb": {"limit": 256, "used": 128, "available": 128},
    "max_concurrent_tasks": {"limit": 1000, "used": 234, "available": 766}
  },
  "quota_utilization": 0.45,
  "throttled": false
}
```

### Adjusting Quotas

#### Immediate Quota Adjustment

```bash
# Increase CPU quota
curl -X PATCH "http://localhost:8080/v1/governance/quotas/tenant-acme" \
    -H "Content-Type: application/json" \
    -d '{
      "cpu_cores": 200,
      "reason": "Quarterly capacity increase",
      "expires_at": "2024-04-01T00:00:00Z"
    }'

# Decrease quota (with grace period for existing workloads)
curl -X PATCH "http://localhost:8080/v1/governance/quotas/tenant-acme" \
    -H "Content-Type: application/json" \
    -d '{
      "cpu_cores": 50,
      "grace_period": "1h",
      "reason": "Cost optimization"
    }'
```

#### Scheduled Quota Changes

```bash
# Schedule quota increase during expected load
curl -X POST "http://localhost:8080/v1/governance/quotas/scheduled" \
    -d '{
      "tenant": "tenant-acme",
      "schedule": "0 8 * * 1-5",
      "timezone": "America/New_York",
      "duration": "10h",
      "quotas": {
        "cpu_cores": 200,
        "max_concurrent_tasks": 2000
      }
    }'
```

### Quota Policies

#### Soft vs. Hard Limits

```yaml
# Governance configuration
quota_policy:
  # Soft limits: Warn but allow (for burst capacity)
  soft_limits:
    enabled: true
    overage_allowed: 20%    # Allow 20% over limit temporarily
    overage_duration: 15m   # Maximum burst duration
    
  # Hard limits: Reject when exceeded
  hard_limits:
    enabled: true
    reject_behavior: queue  # queue | reject | throttle
```

#### Quota Borrowing (Multi-tenant)

Unused quota can be borrowed by other tenants:

```yaml
quota_sharing:
  enabled: true
  
  # How much quota can be borrowed
  borrow_limit: 50%       # Max 50% of own quota
  
  # Priority when reclaiming borrowed resources
  reclaim_priority:
    - owner_tenant
    - high_priority_borrowers
    - low_priority_borrowers
    
  # Grace period before forceful reclaim
  reclaim_grace_period: 5m
```

---

## Resource Allocation Models

### Allocation Strategies

Grey supports multiple resource allocation strategies:

#### 1. Fair Share

Divides resources equally among active tenants, with unused portions redistributed.

```yaml
allocation:
  strategy: fair_share
  
  # Each tenant gets at least this share
  min_share: 5%
  
  # Maximum share any tenant can get
  max_share: 50%
  
  # Weight multipliers for premium tenants
  weights:
    tenant-enterprise: 2.0
    tenant-standard: 1.0
```

**Calculation:**

```
TenantShare = (TenantWeight / SumOfWeights) * ClusterCapacity
```

#### 2. Priority-Based

Higher priority tasks preempt lower priority ones.

```yaml
allocation:
  strategy: priority
  
  preemption:
    enabled: true
    grace_period: 30s     # Time before preemption
    checkpoint: true      # Save state before preemption
    
  priority_levels:
    critical: 10
    high: 7
    normal: 5
    low: 3
    background: 1
```

#### 3. Reservation-Based

Guaranteed capacity with opportunistic overflow.

```yaml
allocation:
  strategy: reservation
  
  # Guaranteed resources (always available)
  reservations:
    tenant-enterprise:
      cpu_cores: 50
      memory_gb: 128
      
  # Opportunistic pool (first-come, first-served)
  opportunistic_pool:
    cpu_cores: 200
    memory_gb: 512
    
  # Can reserved resources be borrowed when idle?
  allow_reservation_borrowing: true
```

### Viewing Allocations

```bash
# Current allocation state
curl -s "http://localhost:8080/v1/governance/allocations" | jq

# Historical allocation efficiency
curl -s "http://localhost:8080/v1/governance/allocations/history?period=24h" | jq
```

---

## Cost Dashboards

### Infrastructure Cost Breakdown

Grey tracks costs across multiple dimensions:

#### Cost by Resource Type

```
┌─────────────────────────────────────────────────────────────────┐
│ Resource Type          │ Hourly Cost │ Daily Cost │ % of Total │
├─────────────────────────────────────────────────────────────────┤
│ Compute (Workers)      │    $45.00   │   $1080    │    65%     │
│ Compute (Coordinators) │    $12.00   │    $288    │    17%     │
│ Storage (EBS/NVMe)     │     $8.00   │    $192    │    12%     │
│ Network (Cross-AZ)     │     $3.50   │     $84    │     5%     │
│ TEE Premium            │     $1.50   │     $36    │     1%     │
├─────────────────────────────────────────────────────────────────┤
│ TOTAL                  │    $70.00   │   $1680    │   100%     │
└─────────────────────────────────────────────────────────────────┘
```

#### Cost by Tenant

```bash
# Get tenant cost attribution
curl -s "http://localhost:8080/v1/governance/costs?period=30d" | jq

# Example output
{
  "period": "2024-01-01/2024-01-31",
  "total_cost": 50400.00,
  "currency": "USD",
  "by_tenant": [
    {"tenant": "enterprise-a", "cost": 15120.00, "share": 0.30},
    {"tenant": "enterprise-b", "cost": 12600.00, "share": 0.25},
    {"tenant": "startup-c", "cost": 7560.00, "share": 0.15},
    {"tenant": "OTHER", "cost": 15120.00, "share": 0.30}
  ]
}
```

### Grafana Cost Dashboard Panels

```json
{
  "panels": [
    {
      "title": "Hourly Cluster Cost",
      "expr": "sum(grey_cost_hourly_dollars)"
    },
    {
      "title": "Cost per 1000 Tasks",
      "expr": "sum(rate(grey_cost_hourly_dollars[1h])) / (sum(rate(grey_scheduler_tasks_completed_total[1h])) / 1000)"
    },
    {
      "title": "Cost by Tenant (24h)",
      "expr": "sum by (tenant) (increase(grey_cost_hourly_dollars{job='grey'}[24h]))"
    },
    {
      "title": "Compute Cost Breakdown",
      "expr": "sum by (component) (grey_cost_hourly_dollars{type='compute'})"
    }
  ]
}
```

### Cost Metrics

Key Prometheus metrics for cost tracking:

```promql
# Total hourly cost
grey_cost_hourly_dollars

# Cost by resource type
grey_cost_hourly_dollars{type="compute|storage|network|tee"}

# Cost by tenant (attributed)
grey_cost_tenant_hourly_dollars{tenant="..."}

# Cost efficiency
grey_cost_per_task_dollar
grey_cost_per_cpu_hour_dollar

# Waste metrics
grey_cost_idle_resources_dollars
grey_cost_overprovisioned_dollars
```

### Setting Cost Alerts

```yaml
# Prometheus alerting rules
groups:
  - name: grey-cost-alerts
    rules:
      - alert: GreyCostSpike
        expr: |
          sum(grey_cost_hourly_dollars) > 100
        for: 1h
        labels:
          severity: warning
        annotations:
          summary: "Hourly cost exceeds $100"
          
      - alert: GreyTenantCostExceeded
        expr: |
          sum by (tenant) (increase(grey_cost_tenant_hourly_dollars[24h])) > 1000
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Tenant {{$labels.tenant}} daily cost exceeds $1000"
          
      - alert: GreyWasteHigh
        expr: |
          sum(grey_cost_idle_resources_dollars) / sum(grey_cost_hourly_dollars) > 0.2
        for: 2h
        labels:
          severity: info
        annotations:
          summary: "Cluster waste exceeds 20%"
```

---

## Performance Economics

### Latency vs. Cost Tradeoffs

Understanding the cost of performance improvements:

```
┌────────────────────────────────────────────────────────────────────────┐
│ Configuration            │ p50 Latency │ p99 Latency │ Monthly Cost   │
├────────────────────────────────────────────────────────────────────────┤
│ Baseline (3 workers)     │    50ms     │    200ms    │    $2,000      │
│ +2 workers               │    40ms     │    150ms    │    $3,300      │
│ +5 workers               │    30ms     │    100ms    │    $5,000      │
│ +NVMe storage            │    25ms     │     80ms    │    $6,500      │
│ +Dedicated coordinators  │    20ms     │     60ms    │    $8,000      │
└────────────────────────────────────────────────────────────────────────┘
```

### Cost per Latency Percentile

```bash
# Calculate marginal cost of latency improvement
curl -s "http://localhost:8080/v1/governance/economics/latency-cost" | jq

# Example output
{
  "current_p99_ms": 150,
  "marginal_costs": [
    {"target_p99_ms": 100, "additional_cost_monthly": 1200, "required_workers": 2},
    {"target_p99_ms": 75, "additional_cost_monthly": 2800, "required_workers": 5},
    {"target_p99_ms": 50, "additional_cost_monthly": 5500, "required_workers": 10}
  ]
}
```

### Throughput Economics

```
Cost per 1M tasks processed:

┌─────────────────────────────────────────────────────────┐
│ Load Level    │ Cost/1M Tasks │ Efficiency │ Notes     │
├─────────────────────────────────────────────────────────┤
│ Low (10%)     │    $125       │   Low      │ Wasteful  │
│ Moderate (50%)│     $45       │   Good     │ Baseline  │
│ High (75%)    │     $35       │   Best     │ Optimal   │
│ Peak (90%)    │     $38       │   Good     │ Near limit│
│ Overload(100%)│     $55       │   Poor     │ Degraded  │
└─────────────────────────────────────────────────────────┘
```

### Performance SLO Costing

Define SLOs and understand their cost:

```yaml
slo_tiers:
  platinum:
    latency_p99_ms: 50
    availability: 99.99%
    throughput_min: 10000/s
    estimated_cost_multiplier: 3.0
    
  gold:
    latency_p99_ms: 100
    availability: 99.9%
    throughput_min: 5000/s
    estimated_cost_multiplier: 1.5
    
  silver:
    latency_p99_ms: 250
    availability: 99.0%
    throughput_min: 1000/s
    estimated_cost_multiplier: 1.0
```

---

## Tenant Billing

### Billing Models

#### 1. Reserved Capacity

Fixed monthly fee for guaranteed resources:

```yaml
billing:
  model: reserved
  
  plans:
    small:
      cpu_cores: 10
      memory_gb: 32
      monthly_price: 500
    medium:
      cpu_cores: 50
      memory_gb: 128
      monthly_price: 2000
    large:
      cpu_cores: 200
      memory_gb: 512
      monthly_price: 6500
      
  overage:
    enabled: true
    rate_per_cpu_hour: 0.05
    rate_per_gb_hour: 0.01
```

#### 2. Pay-per-Use

Charged based on actual consumption:

```yaml
billing:
  model: usage_based
  
  rates:
    cpu_core_hour: 0.04
    memory_gb_hour: 0.006
    storage_gb_month: 0.10
    network_gb: 0.02
    task_execution: 0.0001
    
  minimums:
    monthly_minimum: 100
```

#### 3. Tiered Pricing

Decreasing rates at higher volumes:

```yaml
billing:
  model: tiered
  
  tiers:
    - range: [0, 1000000]       # 0-1M tasks
      rate_per_task: 0.0002
    - range: [1000000, 10000000] # 1M-10M tasks
      rate_per_task: 0.00015
    - range: [10000000, null]    # 10M+ tasks
      rate_per_task: 0.0001
```

### Generating Billing Reports

```bash
# Monthly billing summary
curl -s "http://localhost:8080/v1/billing/summary?tenant=tenant-acme&month=2024-01" | jq

# Example output
{
  "tenant": "tenant-acme",
  "period": "2024-01",
  "line_items": [
    {"description": "Reserved Capacity (Medium)", "quantity": 1, "unit_price": 2000, "total": 2000},
    {"description": "CPU Overage (hours)", "quantity": 450, "unit_price": 0.05, "total": 22.50},
    {"description": "Storage (GB-months)", "quantity": 250, "unit_price": 0.10, "total": 25.00}
  ],
  "subtotal": 2047.50,
  "credits": -100.00,
  "total": 1947.50,
  "currency": "USD"
}

# Detailed usage breakdown
curl -s "http://localhost:8080/v1/billing/usage?tenant=tenant-acme&start=2024-01-01&end=2024-01-31" > usage.json
```

### Chargeback to Internal Teams

For internal deployments, allocate costs to teams:

```bash
# Generate chargeback report
curl -s "http://localhost:8080/v1/billing/chargeback?month=2024-01" | jq

# Output by cost center
{
  "month": "2024-01",
  "total_infrastructure_cost": 50000,
  "allocations": [
    {"cost_center": "CC-ENGINEERING", "cost": 20000, "share": 0.40},
    {"cost_center": "CC-DATASCIENCE", "cost": 15000, "share": 0.30},
    {"cost_center": "CC-ANALYTICS", "cost": 10000, "share": 0.20},
    {"cost_center": "CC-SHARED", "cost": 5000, "share": 0.10}
  ]
}
```

---

## Capacity Planning

### Current Capacity Analysis

```bash
# Get capacity report
curl -s "http://localhost:8080/v1/governance/capacity" | jq

# Output
{
  "current": {
    "cpu_cores": {"total": 400, "allocated": 320, "available": 80},
    "memory_gb": {"total": 1024, "allocated": 768, "available": 256},
    "workers": {"total": 50, "active": 45, "draining": 2, "idle": 3}
  },
  "utilization": {
    "cpu": 0.80,
    "memory": 0.75,
    "overall": 0.77
  },
  "headroom": {
    "burst_capacity_percent": 20,
    "estimated_burst_duration_minutes": 15
  }
}
```

### Forecasting Future Needs

```bash
# Get capacity forecast
curl -s "http://localhost:8080/v1/governance/capacity/forecast?horizon=90d" | jq

# Output
{
  "forecast_horizon": "90d",
  "methodology": "linear_regression_with_seasonality",
  "predictions": [
    {"date": "2024-02-01", "predicted_cpu_demand": 420, "confidence": 0.85},
    {"date": "2024-03-01", "predicted_cpu_demand": 480, "confidence": 0.75},
    {"date": "2024-04-01", "predicted_cpu_demand": 550, "confidence": 0.65}
  ],
  "recommendations": [
    {
      "action": "scale_up",
      "by_date": "2024-02-15",
      "add_workers": 10,
      "reason": "Predicted demand exceeds current capacity by March"
    }
  ]
}
```

### Capacity Thresholds

```yaml
capacity_alerts:
  # Warning thresholds
  warning:
    cpu_utilization: 0.75
    memory_utilization: 0.80
    queue_depth: 5000
    
  # Critical thresholds
  critical:
    cpu_utilization: 0.90
    memory_utilization: 0.92
    queue_depth: 20000
    
  # Auto-scaling triggers
  auto_scale:
    enabled: true
    scale_up_threshold: 0.80
    scale_down_threshold: 0.40
    cooldown_minutes: 10
```

---

## Optimization Strategies

### Right-sizing Recommendations

```bash
# Get right-sizing recommendations
curl -s "http://localhost:8080/v1/governance/optimize/rightsizing" | jq

# Output
{
  "recommendations": [
    {
      "component": "grey-worker",
      "current": {"cpu_request": "2", "memory_request": "4Gi"},
      "recommended": {"cpu_request": "1", "memory_request": "2Gi"},
      "rationale": "Average utilization is 35% CPU, 40% memory",
      "estimated_savings_monthly": 1200
    }
  ],
  "total_estimated_savings": 1200
}
```

### Spot Instance Optimization

```yaml
spot_optimization:
  enabled: true
  
  # Components eligible for spot
  eligible_components:
    - workers
    - batch_processors
    
  # Spot configuration
  spot_mix:
    target_spot_percent: 70
    fallback_to_ondemand: true
    
  # Interruption handling
  interruption_handler:
    drain_timeout: 60s
    checkpoint_on_interrupt: true
```

### Reserved Instance Planning

```bash
# Analyze reserved instance opportunities
curl -s "http://localhost:8080/v1/governance/optimize/reservations" | jq

# Output
{
  "analysis_period": "90d",
  "recommendations": [
    {
      "instance_type": "m6i.2xlarge",
      "quantity": 10,
      "term": "1yr",
      "estimated_savings_percent": 35,
      "estimated_savings_annual": 18000,
      "break_even_months": 4
    }
  ]
}
```

### Idle Resource Detection

```bash
# Find idle resources
curl -s "http://localhost:8080/v1/governance/optimize/idle" | jq

# Output
{
  "idle_resources": [
    {
      "resource": "grey-worker-47",
      "idle_since": "2024-01-10T00:00:00Z",
      "idle_hours": 168,
      "wasted_cost": 84.00,
      "recommendation": "Scale down or terminate"
    }
  ],
  "total_waste_last_7d": 588.00
}
```

### Schedule-Based Optimization

Reduce capacity during off-hours:

```yaml
scheduled_scaling:
  enabled: true
  timezone: "America/New_York"
  
  schedules:
    # Business hours: Full capacity
    - name: business_hours
      cron: "0 8 * * 1-5"
      min_replicas: 30
      
    # Evening: Reduced capacity
    - name: evening
      cron: "0 18 * * 1-5"
      min_replicas: 15
      
    # Weekend: Minimal capacity
    - name: weekend
      cron: "0 0 * * 6-7"
      min_replicas: 5
```

---

## Summary: Key Economic Indicators

### Daily Operations Checklist

| Metric | Healthy Range | Action if Outside |
|--------|---------------|-------------------|
| Cluster Utilization | 65-85% | Scale up/down |
| Cost per Task | < $0.001 | Investigate inefficiency |
| Quota Violations | < 1% | Adjust quotas |
| Idle Resources | < 10% | Scale down |
| Tenant Fairness Index | > 0.9 | Rebalance allocations |

### Monthly Review Items

1. ☐ Review tenant usage vs. quotas
2. ☐ Analyze cost trends and anomalies
3. ☐ Check right-sizing recommendations
4. ☐ Evaluate reserved instance opportunities
5. ☐ Update capacity forecasts
6. ☐ Adjust billing rates if needed
7. ☐ Generate chargeback reports

---

*Last Updated: 2024*  
*Owner: Grey Platform Team*  
*Review Cycle: Monthly*
