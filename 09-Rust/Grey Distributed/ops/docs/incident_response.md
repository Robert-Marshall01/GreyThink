# Grey Distributed — Incident Response Runbook

This document provides structured incident response procedures for Grey Distributed clusters. Each workflow includes detection, triage, mitigation, and post-incident steps.

---

## Table of Contents

1. [Incident Severity Levels](#incident-severity-levels)
2. [General Response Framework](#general-response-framework)
3. [Failure Detection & Escalation](#failure-detection--escalation)
4. [Consensus Reconfiguration](#consensus-reconfiguration)
5. [Isolation Breach Response](#isolation-breach-response)
6. [Data Corruption Recovery](#data-corruption-recovery)
7. [Performance Degradation](#performance-degradation)
8. [Security Incident Response](#security-incident-response)
9. [Chaos Experiment Recovery](#chaos-experiment-recovery)
10. [Communication Templates](#communication-templates)

---

## Incident Severity Levels

| Severity | Definition | Response Time | Escalation |
|----------|------------|---------------|------------|
| **P1 - Critical** | Complete cluster outage, data loss risk, security breach | ≤ 15 min | Immediate on-call + leadership |
| **P2 - High** | Partial outage, degraded performance >50% | ≤ 30 min | On-call + team lead |
| **P3 - Medium** | Single component failure, minor degradation | ≤ 2 hours | On-call |
| **P4 - Low** | Non-urgent issues, warnings | ≤ 24 hours | Business hours |

---

## General Response Framework

### OODA Loop for Incidents

1. **Observe**: Gather data from monitoring, logs, and alerts
2. **Orient**: Understand scope and impact
3. **Decide**: Choose mitigation strategy
4. **Act**: Execute response, verify results

### Initial Response Checklist

```
□ Acknowledge incident in alerting system
□ Join incident communication channel
□ Open incident tracking document
□ Assess severity and scope
□ Notify stakeholders per severity
□ Begin investigation
□ Document all actions with timestamps
```

---

## Failure Detection & Escalation

### Symptoms

- `GreyNodeDown` alert firing
- Node health checks failing
- Increased error rates
- Leader election instability

### Detection Sources

1. **Prometheus Alerts**: `grey_node_up == 0`
2. **Kubernetes Events**: `kubectl get events -n grey-system`
3. **Grey API**: `GET /v1/health`
4. **Log Patterns**: `grep -i "unreachable\|timeout\|failed" /var/log/grey/*.log`

### Triage Steps

```bash
# 1. Check cluster overview
./ops/scripts/scale_cluster.sh status

# 2. Identify affected nodes
kubectl get pods -n grey-system -o wide | grep -v Running

# 3. Check node health details
kubectl describe pod <pod-name> -n grey-system

# 4. Review recent events
kubectl get events -n grey-system --sort-by='.lastTimestamp' | tail -20

# 5. Check Prometheus metrics
curl -s "http://prometheus:9090/api/v1/query?query=grey_node_up" | jq
```

### Mitigation Workflows

#### Single Node Failure (Worker)

**Impact**: Reduced capacity, tasks may be requeued  
**Time to Mitigate**: 5-10 minutes

```bash
# Step 1: Verify node is truly down (not just slow health check)
kubectl logs <pod-name> -n grey-system --tail=100

# Step 2: If node is unrecoverable, delete and let scheduler recreate
kubectl delete pod <pod-name> -n grey-system

# Step 3: If underlying node is problematic, cordon it
kubectl cordon <node-name>

# Step 4: Scale up to compensate if needed
./ops/scripts/scale_cluster.sh up workers 1 --wait

# Step 5: Verify cluster health
./ops/scripts/scale_cluster.sh status
```

#### Single Node Failure (Coordinator)

**Impact**: Potential leader election, brief unavailability  
**Time to Mitigate**: 10-15 minutes

```bash
# Step 1: Check consensus status
curl -s "http://localhost:8080/v1/consensus/status" | jq

# Step 2: If quorum is maintained, let Raft handle recovery
# Monitor for 2 minutes for automatic recovery

# Step 3: If node needs replacement
kubectl delete pod <coordinator-pod> -n grey-system

# Step 4: Verify new pod joins Raft
watch "curl -s 'http://localhost:8080/v1/consensus/members' | jq"

# Step 5: Confirm quorum restored
curl -s "http://localhost:8080/v1/consensus/status" | jq '.quorum_satisfied'
```

#### Multiple Node Failure

**Impact**: Potential quorum loss, service disruption  
**Time to Mitigate**: 15-30 minutes

```bash
# Step 1: Assess scope immediately
kubectl get pods -n grey-system -o wide

# Step 2: Check if this is infrastructure-wide (e.g., AZ failure)
kubectl get nodes -o wide

# Step 3: If quorum is lost, see "Consensus Reconfiguration" below

# Step 4: For worker failures, scale up immediately
./ops/scripts/scale_cluster.sh up workers 10 --wait

# Step 5: Investigate root cause (networking, resource exhaustion, etc.)
```

### Escalation Matrix

| Condition | Action |
|-----------|--------|
| >2 workers down | Page on-call |
| Any coordinator down | Page on-call |
| Quorum lost | Page on-call + team lead |
| Error rate >20% | Page on-call |
| Latency p99 >1s for >5min | Page on-call |

---

## Consensus Reconfiguration

### Symptoms

- `GreyQuorumLost` alert firing
- `grey_consensus_quorum_satisfied` = 0
- No leader elected
- Writes failing across cluster

### Understanding Quorum

- **3 nodes**: Need 2 for quorum (can lose 1)
- **5 nodes**: Need 3 for quorum (can lose 2)
- **7 nodes**: Need 4 for quorum (can lose 3)

### Recovery Workflows

#### Quorum Loss with Recoverable Nodes

**When**: Nodes are down but can be brought back (temporary network issue)

```bash
# Step 1: Identify down nodes
curl -s "http://localhost:8080/v1/consensus/members" | jq '.[] | select(.state != "follower" and .state != "leader")'

# Step 2: Attempt to bring nodes back online
kubectl delete pod <down-pod> -n grey-system

# Step 3: Wait for recovery (up to 5 minutes)
for i in {1..30}; do
    quorum=$(curl -s "http://localhost:8080/v1/consensus/status" | jq -r '.quorum_satisfied')
    echo "Attempt $i: quorum=$quorum"
    [[ "$quorum" == "true" ]] && break
    sleep 10
done

# Step 4: If still no quorum, proceed to manual reconfiguration
```

#### Quorum Loss with Unrecoverable Nodes

**When**: Nodes are permanently lost (disk failure, terminated instances)

⚠️ **DANGER**: This can result in data loss. Document everything.

```bash
# Step 1: Document current state
kubectl get pods -n grey-system -o yaml > /tmp/pre-recovery-state.yaml
curl -s "http://localhost:8080/v1/consensus/members" > /tmp/pre-recovery-members.json

# Step 2: Identify surviving nodes
SURVIVING=$(kubectl get pods -n grey-system -l app=grey-coordinator --field-selector=status.phase=Running -o jsonpath='{.items[*].metadata.name}')

# Step 3: CRITICAL - Force new cluster with surviving nodes
# This requires direct access to coordinator data directory

# On first surviving node:
kubectl exec -it $SURVIVING_NODE -n grey-system -- \
    grey-admin raft force-new-cluster --data-dir /data/raft

# Step 4: Verify new cluster
curl -s "http://localhost:8080/v1/consensus/status" | jq

# Step 5: Scale up coordinators to restore redundancy
./ops/scripts/scale_cluster.sh up coordinators 2 --wait

# Step 6: Verify data integrity
curl -s "http://localhost:8080/v1/health/data-integrity" | jq
```

#### Adding New Coordinator to Existing Cluster

```bash
# Step 1: Scale up StatefulSet
kubectl scale statefulset grey-coordinator -n grey-system --replicas=<NEW_COUNT>

# Step 2: Wait for pod to be ready
kubectl wait --for=condition=Ready pod/grey-coordinator-<N> -n grey-system --timeout=120s

# Step 3: Verify Raft membership
curl -s "http://localhost:8080/v1/consensus/members" | jq

# Step 4: Check replication lag
curl -s "http://localhost:8080/v1/consensus/status" | jq '.members[].match_index'
```

### Post-Recovery Verification

```bash
# Verify consensus health
curl -s "http://localhost:8080/v1/consensus/status" | jq '
{
    leader: .leader,
    term: .term,
    quorum: .quorum_satisfied,
    member_count: (.members | length),
    commit_index: .commit_index
}'

# Run consistency check
curl -X POST "http://localhost:8080/v1/admin/consistency-check" | jq
```

---

## Isolation Breach Response

### Symptoms

- `GreyIsolationViolation` alert firing
- TEE attestation failures
- Unexpected data access patterns
- Cross-tenant data visibility

### Severity: **P1 - Critical**

### Immediate Actions (< 5 minutes)

```bash
# Step 1: Quarantine affected node immediately
./ops/scripts/quarantine_node.sh quarantine <NODE_ID> \
    --reason="Isolation breach - $(date)" \
    --immediate

# Step 2: Preserve evidence
kubectl logs <pod-name> -n grey-system > /secure/evidence/logs-$(date +%s).txt
kubectl describe pod <pod-name> -n grey-system > /secure/evidence/describe-$(date +%s).txt

# Step 3: Check attestation status for all nodes
curl -s "http://localhost:8080/v1/security/attestation/status" | jq

# Step 4: Notify security team
# (Use your organization's security incident notification process)
```

### Investigation Steps

```bash
# 1. Identify affected tenants
curl -s "http://localhost:8080/v1/audit/access-log?node=$NODE_ID" | jq

# 2. Check for data exfiltration
curl -s "http://localhost:8080/v1/audit/data-access?node=$NODE_ID&since=1h" | jq

# 3. Review network traffic
kubectl exec -it <pod> -n grey-system -- tcpdump -i any -w /tmp/capture.pcap &
sleep 60
kubectl cp grey-system/<pod>:/tmp/capture.pcap /secure/evidence/network-$(date +%s).pcap

# 4. Check TEE attestation report
curl -s "http://localhost:8080/v1/security/attestation/report?node=$NODE_ID" | jq
```

### Containment

```bash
# Block all traffic to/from affected node
./ops/scripts/quarantine_node.sh isolate <NODE_ID> --network-only

# Rotate keys that may have been exposed
./ops/scripts/rotate_keys.sh attestation --node=<NODE_ID> --force

# If tenant data compromised, rotate tenant keys
./ops/scripts/rotate_keys.sh encryption --tenant=<TENANT_ID> --force
```

### Post-Incident

1. Complete security incident report
2. Conduct root cause analysis
3. Review and update isolation mechanisms
4. Notify affected parties per compliance requirements
5. File CVE if applicable

---

## Data Corruption Recovery

### Symptoms

- Checksum verification failures
- Unexpected data values
- `grey_state_corruption_detected` metric > 0

### Detection

```bash
# Check corruption metrics
curl -s "http://localhost:8080/v1/metrics" | grep corruption

# Run integrity verification
curl -X POST "http://localhost:8080/v1/admin/verify-integrity" | jq
```

### Recovery Steps

#### Using State Snapshots

```bash
# Step 1: List available snapshots
curl -s "http://localhost:8080/v1/state/snapshots" | jq

# Step 2: Identify last known good snapshot
# (Check creation time vs. when corruption was detected)

# Step 3: Restore from snapshot (causes brief disruption)
curl -X POST "http://localhost:8080/v1/admin/restore" \
    -d '{"snapshot_id": "<SNAPSHOT_ID>"}' | jq

# Step 4: Replay events from snapshot point
curl -X POST "http://localhost:8080/v1/admin/replay-from" \
    -d '{"from_index": <SNAPSHOT_INDEX>, "verify": true}' | jq

# Step 5: Verify data integrity
curl -X POST "http://localhost:8080/v1/admin/verify-integrity" | jq
```

#### Point-in-Time Recovery

```bash
# Step 1: Identify corruption timeline
curl -s "http://localhost:8080/v1/audit/events?type=data_write&since=24h" | jq

# Step 2: Calculate target recovery point
# (Last known good state before corruption)

# Step 3: Restore to point in time
curl -X POST "http://localhost:8080/v1/admin/pit-recovery" \
    -d '{"target_time": "2024-01-15T10:00:00Z"}' | jq

# Step 4: Review recovered state
# Step 5: Reprocess events that occurred after recovery point
```

---

## Performance Degradation

### Symptoms

- `GreyLatencyHigh` alert firing
- Task queue depth increasing
- CPU/memory pressure
- Increased p99 latency

### Rapid Diagnosis

```bash
# 1. Check overall latency distribution
curl -s "http://localhost:8080/v1/metrics" | grep latency_bucket

# 2. Identify hotspots
curl -s "http://localhost:8080/v1/scheduler/hotspots" | jq

# 3. Check resource utilization
kubectl top pods -n grey-system

# 4. Check for specific tenant causing load
curl -s "http://localhost:8080/v1/governance/tenant-usage" | jq | sort -k2 -rn
```

### Mitigation by Root Cause

#### CPU Saturation

```bash
# Scale up workers
./ops/scripts/scale_cluster.sh up workers 5 --wait

# Or enable emergency scaling
curl -X POST "http://localhost:8080/v1/admin/emergency-scale" \
    -d '{"target_replicas": 50}'
```

#### Memory Pressure

```bash
# Identify memory-heavy tasks
curl -s "http://localhost:8080/v1/scheduler/tasks?sort=memory_usage" | jq

# Throttle memory-intensive workloads
curl -X POST "http://localhost:8080/v1/governance/throttle" \
    -d '{"reason": "memory_pressure", "factor": 0.5}'
```

#### Queue Bloat

```bash
# Check queue statistics
curl -s "http://localhost:8080/v1/scheduler/queue/stats" | jq

# If specific tenant is causing bloat, apply rate limit
curl -X POST "http://localhost:8080/v1/governance/rate-limit" \
    -d '{"tenant": "<TENANT_ID>", "rate": 100}'

# Scale workers for queue processing
./ops/scripts/scale_cluster.sh up workers 10 --wait
```

#### Network Saturation

```bash
# Check cross-node traffic
kubectl exec -it <pod> -n grey-system -- ss -tnp

# Check for network policy issues
kubectl get networkpolicies -n grey-system

# Consider enabling request shedding
curl -X POST "http://localhost:8080/v1/admin/request-shedding" \
    -d '{"percentage": 10}'
```

---

## Security Incident Response

### For detailed security incidents, see also: Isolation Breach Response

### General Security Response

```bash
# Step 1: Assess and contain
# - What data/systems are affected?
# - Is the attacker still active?
# - What access did they gain?

# Step 2: Preserve evidence before remediation
kubectl logs -l app.kubernetes.io/name=grey -n grey-system --since=24h > evidence.log
curl -s "http://localhost:8080/v1/audit/events?since=24h" > audit.json

# Step 3: Rotate compromised credentials
./ops/scripts/rotate_keys.sh all --force --backup

# Step 4: Review access logs
curl -s "http://localhost:8080/v1/audit/access?since=24h" | jq

# Step 5: Follow organization security incident procedure
```

### Certificate Expiry Emergency

```bash
# Check certificate status
./ops/scripts/rotate_keys.sh status

# Emergency rotation
./ops/scripts/rotate_keys.sh identity --immediate --force
```

---

## Chaos Experiment Recovery

### For planned chaos experiments that require recovery

### Common Recovery Scenarios

#### Stop Chaos Experiment

```bash
# Terminate chaos experiment immediately
curl -X DELETE "http://localhost:8080/v1/chaos/experiments/current"

# Remove chaos-induced network partitions
kubectl delete networkpolicies -l grey.io/chaos=true -n grey-system

# Release quarantined nodes
./ops/scripts/quarantine_node.sh list
./ops/scripts/quarantine_node.sh release <NODE_ID>
```

#### Restore Normal Operations

```bash
# Verify all nodes healthy
./ops/scripts/scale_cluster.sh status

# Check for orphaned tasks
curl -s "http://localhost:8080/v1/scheduler/orphaned-tasks" | jq

# Replay affected tasks
./ops/scripts/replay_task.sh batch --status=failed --since=1h

# Clear rate limits
curl -X DELETE "http://localhost:8080/v1/governance/throttle"
```

---

## Communication Templates

### Incident Declaration

```
Subject: [P{1-4}] Grey Distributed Incident - {Brief Description}

Status: INVESTIGATING | IDENTIFIED | MONITORING | RESOLVED

Impact: {Description of user/system impact}

Timeline:
- HH:MM UTC: Issue detected
- HH:MM UTC: Investigation started
- HH:MM UTC: {Current status}

Actions:
- {What we're doing}

Next Update: HH:MM UTC
```

### Resolution Notification

```
Subject: [RESOLVED] Grey Distributed Incident - {Brief Description}

Incident Duration: {Start} to {End} (X hours Y minutes)

Root Cause: {Brief explanation}

Resolution: {What fixed it}

Customer Impact: {Summary of impact}

Follow-up Actions:
- [ ] Complete incident postmortem by {date}
- [ ] Implement {preventive measure}

Contact: {on-call engineer} for questions
```

---

## Appendix: Quick Reference

### Common Commands

```bash
# Cluster status
./ops/scripts/scale_cluster.sh status

# Node quarantine
./ops/scripts/quarantine_node.sh quarantine <NODE> --reason="..."
./ops/scripts/quarantine_node.sh release <NODE>

# Task replay
./ops/scripts/replay_task.sh replay <TASK_ID>
./ops/scripts/replay_task.sh batch --status=failed

# Key rotation
./ops/scripts/rotate_keys.sh status
./ops/scripts/rotate_keys.sh identity
```

### Important Endpoints

| Endpoint | Purpose |
|----------|---------|
| `/v1/health` | Overall cluster health |
| `/v1/consensus/status` | Raft consensus state |
| `/v1/consensus/members` | Cluster membership |
| `/v1/scheduler/queue/stats` | Queue statistics |
| `/v1/audit/events` | Audit log |
| `/v1/security/attestation/status` | TEE attestation |

### Alert Runbook References

| Alert | Section |
|-------|---------|
| GreyNodeDown | Failure Detection |
| GreyQuorumLost | Consensus Reconfiguration |
| GreyIsolationViolation | Isolation Breach Response |
| GreyLatencyHigh | Performance Degradation |
| GreyCertExpiringSoon | Security Incident - Certificate |

---

*Last Updated: 2024*  
*Owner: Grey Platform Team*  
*Review Cycle: Quarterly*
