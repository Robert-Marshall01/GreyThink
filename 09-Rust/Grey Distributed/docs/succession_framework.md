# Grey Distributed — Succession Framework

> A comprehensive framework for leadership continuity, knowledge transfer, and community sustainability across decades of project evolution.

## Overview

The Succession Framework ensures Grey Distributed can thrive through inevitable changes in leadership, maintainership, and community composition. It establishes clear pathways for advancement, knowledge preservation, and graceful transitions.

## Philosophy

### Core Beliefs

1. **No Indispensable People**: The project must survive any individual's departure
2. **Knowledge Is Shared**: Information exists in documentation, not just minds
3. **Leadership Is Temporary**: All roles have terms; succession is expected
4. **Community Is Primary**: The project serves the community, not vice versa

### Anti-Patterns We Avoid

| Anti-Pattern | Risk | Mitigation |
|--------------|------|------------|
| **BDFL Syndrome** | Single point of failure | Term limits, rotation |
| **Knowledge Hoarding** | Loss on departure | Documentation requirements |
| **Burnout Spiral** | Reduced bus factor | Mandatory rest, backup roles |
| **Closed Cabal** | Community alienation | Transparent advancement |
| **Organizational Capture** | Loss of independence | Diversity requirements |

## Role Hierarchy

### Leadership Structure

```
┌─────────────────────────────────────────────────────────────────┐
│                     Foundation Council                          │
│                      (5-7 members)                              │
│                     Term: 5 years                               │
│           Strategic direction, foundation governance            │
└──────────────────────────┬──────────────────────────────────────┘
                           │
           ┌───────────────┼───────────────┐
           ▼               ▼               ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│ Project Steward │ │ Project Steward │ │ Project Steward │
│   Term: 3 yrs   │ │   Term: 3 yrs   │ │   Term: 3 yrs   │
│ Cross-cutting   │ │ External focus  │ │ Internal focus  │
└────────┬────────┘ └────────┬────────┘ └────────┬────────┘
         │                   │                   │
         └───────────────────┼───────────────────┘
                             │
┌────────────────────────────┼────────────────────────────────────┐
│       ┌────────────────────┼────────────────────────┐           │
│       ▼                    ▼                        ▼           │
│ ┌───────────┐        ┌───────────┐           ┌───────────┐      │
│ │ Technical │        │ Technical │           │ Technical │      │
│ │ Steward   │        │ Steward   │           │ Steward   │      │
│ │ Core      │        │ Storage   │           │ Security  │      │
│ │ 2yr term  │        │ 2yr term  │           │ 2yr term  │      │
│ └─────┬─────┘        └─────┬─────┘           └─────┬─────┘      │
│       │                    │                       │            │
│       ▼                    ▼                       ▼            │
│ ┌───────────┐        ┌───────────┐           ┌───────────┐      │
│ │  3 Core   │        │  3 Core   │           │  3 Core   │      │
│ │Maintainers│        │Maintainers│           │Maintainers│      │
│ │ 2yr term  │        │ 2yr term  │           │ 2yr term  │      │
│ └───────────┘        └───────────┘           └───────────┘      │
└─────────────────────────────────────────────────────────────────┘

            │
            ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Maintainer Pool                              │
│                     (30-50 members)                             │
│              No term limit, activity-based                      │
└─────────────────────────────────────────────────────────────────┘
            │
            ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Contributor Ladder                           │
│         Newcomer → Contributor → Reviewer → Maintainer          │
└─────────────────────────────────────────────────────────────────┘
```

### Role Definitions

| Role | Responsibilities | Selection | Term |
|------|------------------|-----------|------|
| **Foundation Council** | Strategic direction, legal entity | Election | 5 years |
| **Project Steward** | Vision, roadmap, cross-team | Council nomination | 3 years |
| **Technical Steward** | Domain architecture, standards | Steward nomination | 2 years |
| **Core Maintainer** | Subsystem ownership, reviews | Technical Steward nom | 2 years |
| **Maintainer** | Component ownership, mentoring | Peer nomination | Activity-based |
| **Reviewer** | Code review, triage | Maintainer nomination | Activity-based |
| **Contributor** | Regular PRs, issues | Self-identified | Activity-based |
| **Newcomer** | First contributions | Automatic | Until promotion |

## Transition Protocol

### Standard Transition (Planned)

```
Phase 1: Announcement (T-6 months)
├── Public announcement of intent to transition
├── Call for successor nominations
├── Hand-off planning begins
└── Documentation audit initiated

Phase 2: Selection (T-4 months)
├── Nomination period closes
├── Candidate evaluation
├── Community feedback period
└── Selection committee decision

Phase 3: Knowledge Transfer (T-3 months)
├── Shadow period begins
├── Access and permissions staged
├── Decision-making gradual handoff
└── Documentation updates

Phase 4: Handoff (T-1 month)
├── Primary responsibilities transfer
├── Announcement of succession
├── Final documentation review
└── Outgoing leader advisory role

Phase 5: Completion (T+3 months)
├── Full autonomy for successor
├── Advisory period ends
├── Transition retrospective
└── Process improvements documented
```

### Emergency Transition (Unplanned)

When a leader becomes unavailable unexpectedly:

1. **Immediate (0-24 hours)**
   - Pre-designated backup assumes temporary authority
   - Critical access transferred
   - Community notification

2. **Short-term (1-7 days)**
   - Interim leadership confirmed
   - Responsibilities distributed
   - Accelerated selection process initiated

3. **Medium-term (1-4 weeks)**
   - Permanent successor identified
   - Compressed knowledge transfer
   - Full transition completed

### Pre-Designated Successors

All leadership roles must maintain:
- **Primary Successor**: Ready to assume role immediately
- **Secondary Successor**: Backup if primary unavailable
- **Documentation**: Sufficient for cold-start assumption

## Contributor Ladder

### Advancement Criteria

```
Newcomer → Contributor
├── Requirement: 5+ merged PRs
├── Time: 2+ months active
└── Outcome: Recognized community member

Contributor → Reviewer
├── Requirement: 20+ merged PRs, quality reviews
├── Time: 6+ months as contributor
├── Sponsorship: 2 maintainer endorsements
└── Outcome: Can approve non-critical changes

Reviewer → Maintainer
├── Requirement: Demonstrated expertise in domain
├── Time: 12+ months as reviewer
├── Sponsorship: 1 core maintainer endorsement
└── Outcome: Full merge authority for domain

Maintainer → Core Maintainer
├── Requirement: Cross-domain contributions
├── Nomination: Technical Steward
├── Approval: Existing core maintainers (⅔)
└── Outcome: Subsystem ownership

Core Maintainer → Technical Steward
├── Requirement: Vision for domain
├── Nomination: Project Steward
├── Approval: Foundation Council
└── Outcome: Domain authority

Technical Steward → Project Steward
├── Requirement: Project-wide vision
├── Nomination: Foundation Council
├── Approval: Community vote
└── Outcome: Project leadership
```

### Demotion and Removal

**Inactivity:**
- 6 months inactive → gentle check-in
- 12 months inactive → emeritus status offered
- Emeritus: no voting rights, recognition preserved

**Conduct Issues:**
- Minor: private warning
- Moderate: temporary suspension
- Severe: permanent removal

**Performance Issues:**
- Addressed through mentorship first
- Role change if improvement insufficient
- Removal only as last resort

## Knowledge Transfer

### Documentation Requirements

All leaders must maintain:

1. **Role Runbook**: Day-to-day responsibilities
2. **Decision Log**: Key decisions and rationale
3. **Relationship Map**: Key contacts and stakeholders
4. **Access Inventory**: Systems and credentials needed
5. **Succession Notes**: Context for successor

### Institutional Memory

```yaml
knowledge_preservation:
  explicit:
    - Design documents
    - Architecture Decision Records
    - Meeting minutes
    - RFC archive
    
  tacit:
    - Recorded video walkthroughs
    - Paired working sessions
    - Q&A documentation
    - War stories archive
    
  relational:
    - Stakeholder introductions
    - Community relationship transfer
    - Partner handoffs
    - Conflict history briefing
```

### Shadow Program

Successors participate in a shadow period:
- Attend all meetings (initially silent)
- CC'd on all communications
- Paired on all decisions
- Gradual responsibility assumption

## Mentorship Program

### Program Structure

```
Mentorship Lifecycle:
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│  Matching   │───▶│  Active     │───▶│  Completion │
│  (2 weeks)  │    │  (4 months) │    │  (ongoing)  │
└─────────────┘    └─────────────┘    └─────────────┘
      │                  │                  │
      ▼                  ▼                  ▼
   Skills and        Regular 1:1s       Advancement
   goal alignment    Goal tracking      Alumni network
   Compatibility     Skill building     Peer mentoring
```

### Mentor Responsibilities

| Responsibility | Frequency | Time |
|----------------|-----------|------|
| 1:1 meetings | Weekly | 30-60 min |
| Code review | As needed | varies |
| Career guidance | Monthly | 30 min |
| Introduction to community | Ongoing | varies |
| Advocacy for advancement | As appropriate | varies |

### Mentee Expectations

- Prepare for meetings
- Complete agreed-upon tasks
- Seek help when stuck
- Provide feedback on program
- Pay it forward when ready

## Community Health

### Bus Factor Monitoring

```python
def calculate_bus_factor(domain):
    """
    Bus factor = minimum people who could leave
    before the domain becomes unmaintainable
    """
    contributors = get_active_contributors(domain)
    
    # Sort by contribution weight
    sorted_contributors = sort_by_impact(contributors)
    
    # Find minimum set needed for coverage
    coverage = 0
    bus_factor = 0
    
    for contributor in sorted_contributors:
        coverage += contributor.knowledge_coverage
        bus_factor += 1
        if coverage >= 0.8:  # 80% coverage threshold
            break
    
    return bus_factor

# Targets:
# Critical domains: bus_factor >= 3
# Standard domains: bus_factor >= 2
# All domains: bus_factor >= 1
```

### Diversity Requirements

| Level | Organization Limit | Geography | Timezone |
|-------|-------------------|-----------|----------|
| Foundation Council | Max 2 from same org | 3+ countries | 3+ zones |
| Project Stewards | Max 1 from same org | 2+ countries | 2+ zones |
| Technical Stewards | Max 50% from same org | — | — |
| Core Maintainers | Max 60% from same org | — | — |

### Burnout Prevention

1. **Mandatory Rest**: Leadership must take 2+ weeks off annually
2. **Load Balancing**: No single person on more than 2 critical paths
3. **Backup Coverage**: All responsibilities have designated backups
4. **Workload Monitoring**: Observable metrics for contribution patterns
5. **Proactive Support**: Check-ins when burnout indicators appear

## Sustainability Mechanisms

### Pipeline Health Metrics

| Metric | Target | Alert Threshold |
|--------|--------|-----------------|
| New contributors (monthly) | 10+ | <5 |
| Newcomer → Contributor rate | 30% | <15% |
| Contributor → Reviewer rate | 20% | <10% |
| Reviewer → Maintainer rate | 15% | <7% |
| Average time to first merge | <7 days | >14 days |
| Mentor availability | 10+ | <5 |

### Succession Readiness Score

```json
{
  "readiness_calculation": {
    "components": [
      { "name": "successor_identified", "weight": 0.30 },
      { "name": "documentation_current", "weight": 0.25 },
      { "name": "shadow_active", "weight": 0.20 },
      { "name": "knowledge_distributed", "weight": 0.15 },
      { "name": "backup_capable", "weight": 0.10 }
    ],
    "thresholds": {
      "healthy": 0.80,
      "warning": 0.60,
      "critical": 0.40
    }
  }
}
```

### Long-Term Resilience

```
Year 1-2: Establish
├── Core team forms
├── Governance documented
├── Contributor pipeline started
└── First succession exercises

Year 3-5: Mature
├── First leadership transitions
├── Mentorship program scales
├── Knowledge base comprehensive
└── Bus factor healthy

Year 5-10: Sustain
├── Second generation leaders
├── Self-sustaining pipeline
├── Institutional memory preserved
└── Regular succession smooth

Year 10+: Evolve
├── Multi-generational community
├── Framework updates regular
├── External recognition
└── Model for other projects
```

## Governance Integration

### Decision Rights by Role

| Decision Type | Decides | Consults | Informs |
|---------------|---------|----------|---------|
| Strategic direction | Council | Stewards | All |
| Major architecture | Tech Steward | Core | Maintainers |
| Feature approval | Steward | Tech Stewards | Community |
| Code merge | Maintainer | Reviewer | — |
| Contributor promotion | Maintainer | Peers | Steward |
| Leadership succession | Council | Stewards | All |

### Escalation Path

```
Contributor conflict
    ↓
Maintainer mediation
    ↓
Core Maintainer review
    ↓
Technical Steward decision
    ↓
Project Steward appeal
    ↓
Foundation Council final
```

## References

- [Stewardship Roles](../succession/roles/stewardship_roles.md)
- [Transition Protocol](../succession/roles/transition_protocol.md)
- [Community Succession](../succession/roles/community_succession.md)
- [Succession Dashboard](../dashboards/succession_dashboard.json)
- [Governance Framework](governance_framework.md)

---

*This framework is reviewed biennially and updated based on community feedback and lessons learned from transitions.*
