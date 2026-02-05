# Grey Distributed — Crisis Framework

> Framework for detecting, responding to, and recovering from active crises affecting Grey Distributed operations.

## Overview

The Crisis Framework provides structured processes for managing active crises, from initial detection through resolution and post-incident improvement. It complements the Resilience Framework (preventive) by providing response protocols when prevention fails.

## Crisis Classification

### Severity Levels

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         CRISIS SEVERITY LEVELS                              │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  Level 5: EXISTENTIAL                                                       │
│  ├── Definition: Threatens continued existence of Grey Distributed         │
│  ├── Examples: Complete infrastructure destruction, total compromise       │
│  ├── Response: All-hands, external coordination, preservation mode         │
│  └── Authority: Foundation Council + external partners                     │
│                                                                             │
│  Level 4: CRITICAL                                                          │
│  ├── Definition: Major system-wide impact, significant data at risk        │
│  ├── Examples: Multi-region outage, major security breach                  │
│  ├── Response: Senior leadership, 24/7 response, public communication      │
│  └── Authority: Project Stewards                                           │
│                                                                             │
│  Level 3: HIGH                                                              │
│  ├── Definition: Significant impact, affecting multiple sectors/regions    │
│  ├── Examples: Regional outage, sector degradation, significant breach     │
│  ├── Response: Domain experts, extended hours, stakeholder notification    │
│  └── Authority: Technical Stewards                                         │
│                                                                             │
│  Level 2: MEDIUM                                                            │
│  ├── Definition: Noticeable impact, limited scope                          │
│  ├── Examples: Single facility issue, minor security incident              │
│  ├── Response: On-call team, business hours escalation                     │
│  └── Authority: Core Maintainers                                           │
│                                                                             │
│  Level 1: LOW                                                               │
│  ├── Definition: Minor impact, routine handling                            │
│  ├── Examples: Component failure with failover, minor bug                  │
│  ├── Response: Automated + on-call acknowledgment                          │
│  └── Authority: Maintainers                                                │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Crisis Categories

| Category | Description | Primary Concern |
|----------|-------------|-----------------|
| **Infrastructure** | Physical/virtual infrastructure failure | Service availability |
| **Security** | Breach, attack, vulnerability | Data protection |
| **Operational** | Process/human failure | Service quality |
| **External** | Third-party/dependency failure | Continuity |
| **Governance** | Decision-making breakdown | Coordination |
| **Reputational** | Trust/perception crisis | Stakeholder confidence |

## Crisis Response Structure

### Response Teams

```yaml
crisis_response_teams:
  incident_commander:
    role: Single point of authority for crisis
    authority: Full operational authority for duration
    selection: Rotating schedule + escalation path
    
  technical_lead:
    role: Technical decision-making
    authority: System changes, resource allocation
    selection: Domain expertise + availability
    
  communications_lead:
    role: Internal and external communication
    authority: Message approval, stakeholder contact
    selection: Designated communications team
    
  operations_lead:
    role: Coordinate operational response
    authority: Staffing, tools, logistics
    selection: Operations team rotation
    
  subject_matter_experts:
    role: Domain-specific expertise
    authority: Advisory (recommendations to IC)
    selection: As needed per crisis type
```

### Command Structure

```
                    ┌───────────────────────┐
                    │  Incident Commander   │
                    │   (Single authority)  │
                    └───────────┬───────────┘
                                │
        ┌───────────────────────┼───────────────────────┐
        │                       │                       │
        ▼                       ▼                       ▼
┌───────────────┐      ┌───────────────┐      ┌───────────────┐
│ Technical Lead│      │ Comms Lead    │      │ Operations    │
│               │      │               │      │ Lead          │
└───────┬───────┘      └───────┬───────┘      └───────┬───────┘
        │                      │                      │
        ▼                      ▼                      ▼
┌───────────────┐      ┌───────────────┐      ┌───────────────┐
│ Engineering   │      │ Stakeholder   │      │ Resource      │
│ Teams         │      │ Communication │      │ Coordination  │
└───────────────┘      └───────────────┘      └───────────────┘
```

## Crisis Lifecycle

### Phase 1: Detection

```rust
/// Crisis detection mechanisms
pub struct CrisisDetection {
    /// Automated monitoring alerts
    automated_detection: Vec<MonitoringAlert>,
    
    /// Human observation reports
    human_reports: Vec<IncidentReport>,
    
    /// External notifications
    external_signals: Vec<ExternalNotification>,
    
    /// Correlation engine
    correlator: EventCorrelator,
}

impl CrisisDetection {
    pub async fn evaluate(&self, signal: Signal) -> Option<PotentialCrisis> {
        // Correlate with existing signals
        let correlated = self.correlator.correlate(signal).await;
        
        // Assess severity
        let severity = self.assess_severity(&correlated);
        
        // Determine if crisis threshold met
        if severity >= SeverityThreshold::Crisis {
            Some(PotentialCrisis {
                signals: correlated,
                severity,
                recommended_response: self.recommend_response(severity),
            })
        } else {
            None
        }
    }
}
```

**Detection Sources:**
- Automated monitoring (infrastructure, security, performance)
- Human observation (operators, users, partners)
- External feeds (threat intelligence, partner notifications)
- Social signals (reputation monitoring, community channels)

### Phase 2: Triage

```
Signal Received
      │
      ▼
┌─────────────────┐
│ Initial Triage  │  ← On-call engineer (5 min SLA)
│                 │
│ • Validate      │
│ • Classify      │
│ • Scope         │
└────────┬────────┘
         │
    ┌────┴────────────────────────────┐
    │         │         │             │
    ▼         ▼         ▼             ▼
  Level 1   Level 2   Level 3-4    Level 5
    │         │         │             │
    ▼         ▼         ▼             ▼
 Handle    Escalate  Declare        Emergency
 locally   to team   Crisis         Activation
```

### Phase 3: Mobilization

**For Level 3+ crises:**

```yaml
mobilization_checklist:
  immediate_0_5_min:
    - Designate incident commander
    - Establish command channel (voice + chat)
    - Send initial notification to response team
    - Begin incident log
    
  short_term_5_15_min:
    - Assemble core response team
    - Briefing on current situation
    - Assign initial roles
    - Establish communication rhythm
    
  stabilization_15_60_min:
    - Full team assembled
    - Situation assessment complete
    - Initial response actions underway
    - External stakeholder notification (if needed)
```

### Phase 4: Response

```rust
/// Crisis response coordination
pub struct CrisisResponse {
    /// Current crisis state
    state: CrisisState,
    
    /// Active response actions
    actions: Vec<ResponseAction>,
    
    /// Resource allocation
    resources: ResourceAllocation,
    
    /// Communication status
    communications: CommunicationStatus,
}

impl CrisisResponse {
    /// Execute response action with tracking
    pub async fn execute_action(&mut self, action: ResponseAction) -> ActionResult {
        // Log action start
        self.log_action_start(&action);
        
        // Execute with timeout
        let result = timeout(action.timeout, action.execute()).await;
        
        // Log result
        self.log_action_result(&action, &result);
        
        // Update state
        self.update_state(&result);
        
        result
    }
    
    /// Escalate crisis if needed
    pub async fn evaluate_escalation(&self) -> Option<Escalation> {
        if self.state.duration > self.state.escalation_threshold {
            return Some(Escalation::Timeout);
        }
        if self.state.severity_increasing() {
            return Some(Escalation::Worsening);
        }
        if self.resources.exhausted() {
            return Some(Escalation::ResourceExhaustion);
        }
        None
    }
}
```

### Phase 5: Stabilization

Criteria for declaring stabilization:
- Immediate threat contained
- No ongoing data loss or security breach
- Service at acceptable degradation level
- Response resources sustainable
- Communication channels stable

### Phase 6: Recovery

```
Stabilization Declared
         │
         ▼
┌───────────────────┐
│ Recovery Planning │
│                   │
│ • Prioritize      │
│ • Sequence        │
│ • Resources       │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│ Staged Recovery   │
│                   │
│ • Critical first  │
│ • Validate each   │
│ • Monitor closely │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│ Full Restoration  │
│                   │
│ • All services    │
│ • Full capacity   │
│ • Normal ops      │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│ Crisis Closure    │
│                   │
│ • Stand down      │
│ • Documentation   │
│ • Initial lessons │
└───────────────────┘
```

### Phase 7: Post-Incident

```yaml
post_incident_process:
  immediate_24_48_hours:
    - Initial incident report
    - Key stakeholder debriefs
    - Preliminary timeline
    
  short_term_1_2_weeks:
    - Detailed post-mortem
    - Root cause analysis
    - Contributing factor identification
    - Action items defined
    
  medium_term_1_3_months:
    - Action items implemented
    - Effectiveness validated
    - Documentation updated
    - Training updated
    
  long_term_ongoing:
    - Trend analysis
    - Pattern identification
    - Framework updates
    - Drill scenario updates
```

## Communication Protocols

### Internal Communication

| Channel | Purpose | Update Frequency |
|---------|---------|------------------|
| War room (voice) | Real-time coordination | Continuous |
| Incident chat | Detailed logs, links | Continuous |
| Leadership bridge | Executive updates | Every 30 min |
| All-hands updates | Broad team awareness | As needed |

### External Communication

| Stakeholder | Channel | Timing | Content |
|-------------|---------|--------|---------|
| Affected users | Status page | Immediate | Impact, ETA |
| Partners | Direct notification | 15 min | Impact, actions |
| Regulators | Formal channels | Per regulation | Required details |
| Media | Press team | As needed | Approved statements |
| Public | Social media | As needed | Status updates |

### Communication Templates

```yaml
templates:
  initial_notification:
    subject: "[SEVERITY] CRISIS: [Brief Description]"
    content:
      - Current status
      - Known impact
      - Response team activated
      - Next update time
      
  status_update:
    subject: "[SEVERITY] UPDATE: [Brief Description]"
    content:
      - Current status (changed from last)
      - Actions taken
      - Current focus
      - Next update time
      
  resolution_notice:
    subject: "RESOLVED: [Brief Description]"
    content:
      - Resolution summary
      - Root cause (if known)
      - Preventive measures planned
      - Point of contact for questions
```

## Decision Framework

### Emergency Decisions

```rust
/// Emergency decision authority
pub enum EmergencyDecision {
    /// Can be made by incident commander alone
    OperationalImmediate {
        examples: vec![
            "Failover to backup system",
            "Isolate compromised component",
            "Scale resources",
        ],
    },
    
    /// Requires technical lead approval
    TechnicalSignificant {
        examples: vec![
            "Deploy untested patch",
            "Modify security controls",
            "Change data handling",
        ],
    },
    
    /// Requires leadership approval
    BusinessImpact {
        examples: vec![
            "Extend downtime window",
            "Customer data disclosure",
            "Significant cost authorization",
        ],
    },
    
    /// Requires council authorization
    Existential {
        examples: vec![
            "System shutdown",
            "Public disclosure of breach",
            "Regulatory notification",
        ],
    },
}
```

### Decision Logging

All significant decisions logged with:
- Decision made
- Decision maker
- Rationale
- Alternatives considered
- Outcome expected
- Actual outcome (post-hoc)

## Resource Management

### Resource Allocation

```rust
/// Crisis resource allocation
pub struct CrisisResources {
    /// Personnel assigned
    personnel: Vec<PersonnelAssignment>,
    
    /// Infrastructure allocated
    infrastructure: InfrastructureAllocation,
    
    /// Budget authorized
    budget: BudgetAuthorization,
    
    /// External resources
    external: Vec<ExternalResource>,
}

impl CrisisResources {
    pub fn request_personnel(&mut self, request: PersonnelRequest) -> Result<Assignment, Denial> {
        // Check availability
        // Check priority vs current assignments
        // Authorize or escalate
    }
    
    pub fn request_infrastructure(&mut self, request: InfraRequest) -> Result<Allocation, Denial> {
        // Check capacity
        // Check authorization level
        // Provision or escalate
    }
}
```

### Shift Management

For extended crises:
- 8-12 hour maximum shifts
- Mandatory handoff briefings
- Written shift logs
- Fresh eyes on long-running issues
- Rest requirements before return

## Metrics and Reporting

### Crisis Metrics

| Metric | Description | Target |
|--------|-------------|--------|
| Detection time | Signal to acknowledgment | < 5 min |
| Triage time | Acknowledgment to classification | < 10 min |
| Mobilization time | Classification to team assembled | < 30 min |
| Stabilization time | By severity level | Per SLA |
| Recovery time | Stabilization to full service | Per SLA |
| Post-mortem completion | Closure to report | < 2 weeks |

### Crisis Reporting

```yaml
crisis_report_structure:
  executive_summary:
    - One paragraph overview
    - Impact summary
    - Key actions taken
    - Lessons learned
    
  timeline:
    - Chronological event list
    - Key decision points
    - State transitions
    
  impact_analysis:
    - Service impact
    - User impact
    - Data impact
    - Financial impact
    
  response_analysis:
    - What worked well
    - What could improve
    - Resource utilization
    
  root_cause:
    - Technical root cause
    - Contributing factors
    - Systemic issues
    
  action_items:
    - Immediate fixes
    - Short-term improvements
    - Long-term changes
    - Owners and deadlines
```

## Integration Points

### With Resilience Framework

- Crisis triggers resilience mode changes
- Degradation levels inform crisis severity
- Recovery procedures from resilience framework
- Drill results inform crisis planning

### With Governance Framework

- Crisis authority delegation
- Emergency decision processes
- Post-crisis governance review
- Accountability structures

### With Evolution Framework

- Crisis lessons inform evolution
- Post-crisis improvements tracked
- Long-term trend analysis
- Framework updates

## References

- [Resilience Framework](resilience_framework.md)
- [Governance Framework](governance_framework.md)
- [Crisis Dashboard](../dashboards/crisis_dashboard.json)
- [Emergency Response Protocols](../ops/docs/emergency_response.md)

---

*This framework is reviewed after every Level 3+ crisis and updated annually.*
