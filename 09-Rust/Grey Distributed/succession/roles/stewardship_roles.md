# Grey Distributed — Stewardship Roles

This document defines the roles responsible for long-term stewardship of Grey Distributed across generations of maintainers.

## Table of Contents

1. [Stewardship Philosophy](#stewardship-philosophy)
2. [Role Hierarchy](#role-hierarchy)
3. [Core Steward Roles](#core-steward-roles)
4. [Support Roles](#support-roles)
5. [Role Lifecycle](#role-lifecycle)
6. [Responsibilities Matrix](#responsibilities-matrix)

---

## Stewardship Philosophy

### Guiding Principles

```
Stewardship Principles:
━━━━━━━━━━━━━━━━━━━━━━━

  1. CONTINUITY
     └─ The project outlives any individual contributor

  2. DIVERSITY
     └─ Stewardship distributed across organizations and regions

  3. MENTORSHIP
     └─ Current stewards actively develop successors

  4. TRANSPARENCY
     └─ All stewardship decisions are public

  5. MERITOCRACY
     └─ Advancement based on contribution and capability

  6. INSTITUTIONAL MEMORY
     └─ Document decisions, rationale, and lessons learned
```

### Time Horizons

| Horizon | Focus | Steward Level |
|---------|-------|---------------|
| Immediate (0-1 year) | Day-to-day operations | Maintainers |
| Short-term (1-3 years) | Feature roadmap, releases | Core Maintainers |
| Medium-term (3-5 years) | Architecture evolution | Technical Stewards |
| Long-term (5-10 years) | Project sustainability | Project Stewards |
| Generational (10+ years) | Legacy and succession | Foundation Council |

---

## Role Hierarchy

```
Stewardship Hierarchy:
═══════════════════════════════════════════════════════════════════

                    ┌─────────────────────┐
                    │  Foundation Council │
                    │   (3-7 members)     │
                    │   Term: 5 years     │
                    └──────────┬──────────┘
                               │
           ┌───────────────────┼───────────────────┐
           │                   │                   │
  ┌────────▼────────┐ ┌───────▼────────┐ ┌───────▼────────┐
  │ Project Steward │ │ Project Steward│ │ Project Steward│
  │  (Technical)    │ │  (Community)   │ │  (Operations)  │
  │  Term: 3 years  │ │  Term: 3 years │ │  Term: 3 years │
  └────────┬────────┘ └───────┬────────┘ └───────┬────────┘
           │                  │                   │
           └──────────────────┼───────────────────┘
                              │
              ┌───────────────┼───────────────┐
              │               │               │
     ┌────────▼────────┐ ┌────▼────────┐ ┌────▼────────┐
     │Technical Steward│ │Core Maintainer│ │Core Maintainer│
     │ (Architecture)  │ │ (Consensus)   │ │ (Federation) │
     │  Term: 2 years  │ │  Term: 2 years │ │  Term: 2 years│
     └────────┬────────┘ └───────┬────────┘ └───────┬──────┘
              │                  │                   │
              └──────────────────┼───────────────────┘
                                 │
                    ┌────────────┼────────────┐
                    │            │            │
              ┌─────▼─────┐ ┌────▼────┐ ┌─────▼─────┐
              │ Maintainer│ │Maintainer│ │ Maintainer│
              │ (No term) │ │ (No term)│ │ (No term) │
              └─────┬─────┘ └────┬────┘ └─────┬─────┘
                    │            │            │
                    └────────────┼────────────┘
                                 │
                         ┌───────▼───────┐
                         │  Contributors │
                         └───────────────┘
```

---

## Core Steward Roles

### Foundation Council Member

**Purpose**: Ensure long-term sustainability and project direction across decades.

```yaml
role: foundation_council_member
term: 5 years (renewable once)
count: 3-7 (odd number, minimum 3)
selection: Elected by existing council + project stewards

qualifications:
  required:
    - 5+ years as project steward or equivalent
    - Demonstrated commitment to project values
    - Organizational independence (max 2 from same org)
    - No conflicts of interest
  preferred:
    - Prior open-source foundation experience
    - Legal/financial background useful
    - Geographic diversity

responsibilities:
  governance:
    - Amend project charter
    - Approve major governance changes
    - Resolve escalated disputes
    - Manage foundation assets
  
  succession:
    - Elect new council members
    - Approve project steward appointments
    - Maintain succession documentation
    - Conduct annual succession review
  
  strategy:
    - Set 10-year vision
    - Approve major partnerships
    - Manage trademark/branding
    - Oversee foundation finances

time_commitment: 5-10 hours/month
compensation: Travel expenses, no salary
```

### Project Steward

**Purpose**: Guide project direction within their domain (technical, community, or operations).

```yaml
role: project_steward
term: 3 years (renewable twice)
count: 3 (one per domain)
selection: Elected by core maintainers, approved by foundation council

domains:
  technical:
    focus: Architecture, technical direction, quality
    background: Senior engineer, architect
    
  community:
    focus: Growth, inclusivity, contributor experience
    background: Developer advocate, community manager
    
  operations:
    focus: Infrastructure, releases, security
    background: DevOps, SRE, security engineer

qualifications:
  required:
    - 3+ years as core maintainer
    - Domain expertise
    - Leadership experience
    - Time availability (10+ hours/week)
  preferred:
    - Conflict resolution training
    - Public speaking ability
    - Mentorship track record

responsibilities:
  domain_leadership:
    - Set domain strategy
    - Mentor core maintainers
    - Represent domain in council
    - Manage domain resources
  
  cross_functional:
    - Participate in steward meetings (weekly)
    - Vote on project-wide decisions
    - Handle escalations from maintainers
    - Conduct annual domain review
  
  succession:
    - Identify potential successors
    - Maintain 2+ succession candidates
    - Develop successor skills
    - Document domain knowledge

time_commitment: 10-20 hours/week
compensation: Foundation stipend (optional)
```

### Technical Steward

**Purpose**: Maintain architectural integrity and technical excellence in specialized areas.

```yaml
role: technical_steward
term: 2 years (renewable)
count: 3-5 (one per major subsystem)
selection: Nominated by project steward (technical), approved by maintainers

subsystems:
  - consensus: Consensus protocol steward
  - federation: Federation protocol steward
  - security: Security and cryptography steward
  - storage: State and storage steward
  - api: API and SDK steward

qualifications:
  required:
    - 2+ years as maintainer in subsystem
    - Deep technical expertise
    - Code review excellence
    - Documentation skills
  preferred:
    - Conference speaking
    - Published research/writing
    - Industry recognition

responsibilities:
  technical:
    - Own subsystem architecture decisions
    - Review all major subsystem changes
    - Maintain technical documentation
    - Guide performance and reliability
  
  mentorship:
    - Mentor subsystem maintainers
    - Review maintainer applications
    - Develop technical standards
    - Knowledge transfer sessions
  
  coordination:
    - Cross-subsystem API design
    - Breaking change coordination
    - Migration planning
    - Ecosystem compatibility

time_commitment: 5-10 hours/week
compensation: Recognition, swag, conference sponsorship
```

### Core Maintainer

**Purpose**: Day-to-day technical leadership within assigned areas.

```yaml
role: core_maintainer
term: 2 years (renewable)
count: 9-15
selection: Elected by existing maintainers, approved by project steward

areas:
  - consensus, federation, governance
  - scheduler, network, storage
  - security, observability, sdk
  - docs, testing, release

qualifications:
  required:
    - 1+ year as maintainer
    - Consistent contribution (weekly)
    - Review quality >= 90%
    - Mentorship of contributors
  preferred:
    - Full subsystem knowledge
    - Community leadership
    - Release management experience

responsibilities:
  technical:
    - Merge authority for area
    - RFC authorship and review
    - Breaking change authority
    - Performance ownership
  
  community:
    - Contributor mentorship
    - Issue triage
    - Community support
    - Documentation review
  
  governance:
    - Vote on maintainer elections
    - Vote on governance RFCs
    - Participate in monthly sync
    - Annual retrospective

time_commitment: 5-15 hours/week
compensation: Company sponsorship encouraged
```

### Maintainer

**Purpose**: Review and merge contributions, support contributors.

```yaml
role: maintainer
term: Indefinite (annual reconfirmation)
count: 30-50
selection: Nominated by core maintainer, approved by peers

qualifications:
  required:
    - 6+ months as contributor
    - 20+ merged PRs
    - Good review feedback (>80% approval)
    - Code of conduct adherence
    - Passed maintainer quiz
  preferred:
    - Expertise in specific area
    - Prior open-source experience
    - Responsive communication

responsibilities:
  code:
    - Review and merge PRs (area-scoped)
    - Triage issues
    - Bug fixes and improvements
    - Test coverage maintenance
  
  community:
    - Mentor new contributors
    - Answer questions
    - Participate in discussions
    - Represent project

time_commitment: 2-5 hours/week
compensation: Recognition, contributor programs
```

---

## Support Roles

### Security Response Team

```yaml
role: security_response_team
count: 5-7
term: 1 year (renewable)
selection: Appointed by project steward (operations)

responsibilities:
  - Receive vulnerability reports
  - Coordinate disclosure timeline
  - Develop patches
  - Publish security advisories
  
requirements:
  - Security expertise
  - Rapid response availability
  - Discretion (embargo handling)
  - Core maintainer status

rotation: On-call rotation (weekly)
```

### Release Managers

```yaml
role: release_manager
count: 3-5
term: Per-release or 6 months
selection: Volunteer from core maintainers

responsibilities:
  - Coordinate release schedule
  - Manage release branches
  - Quality gate enforcement
  - Release documentation
  
requirements:
  - Core maintainer status
  - Release experience
  - Communication skills
  - Availability during release window
```

### Documentation Steward

```yaml
role: documentation_steward
count: 1-2
term: 1 year (renewable)
selection: Nominated by community steward

responsibilities:
  - Documentation quality
  - Style guide maintenance
  - Tutorial development
  - Translation coordination
  
requirements:
  - Technical writing skill
  - Project understanding
  - Contributor status (at minimum)
```

---

## Role Lifecycle

### Appointment Process

```
Role Appointment Flow:
═══════════════════════════════════════════════════════════════════

1. NOMINATION
   ├─ Self-nomination or peer nomination
   ├─ Nomination statement (goals, qualifications)
   └─ Endorsements collected

2. ELIGIBILITY CHECK
   ├─ Qualifications verified
   ├─ Time commitment confirmed
   └─ No conflicts of interest

3. DISCUSSION PERIOD
   ├─ 2 weeks for feedback
   ├─ Q&A with candidate
   └─ Concerns addressed

4. VOTING
   ├─ Eligible voters cast ballot
   ├─ Threshold varies by role
   └─ Results announced

5. ONBOARDING
   ├─ Access granted
   ├─ Mentorship assigned
   ├─ Training completed
   └─ Role activated
```

### Voting Thresholds

| Role | Votes From | Threshold | Quorum |
|------|-----------|-----------|--------|
| Foundation Council | Council + Stewards | 75% | 80% |
| Project Steward | Core Maintainers | 66% | 66% |
| Technical Steward | Maintainers | 51% | 50% |
| Core Maintainer | Maintainers | 51% | 50% |
| Maintainer | Peers in area | 66% | 66% |

### Term Management

```yaml
term_management:
  term_limits:
    foundation_council: 2 consecutive terms (10 years max)
    project_steward: 3 consecutive terms (9 years max)
    technical_steward: No limit
    core_maintainer: No limit
    maintainer: No limit (annual reconfirmation)
  
  reconfirmation:
    frequency: Annual
    criteria:
      - Activity level maintained
      - No Code of Conduct violations
      - Peer support
    process: Lightweight vote (approval voting)
  
  emeritus:
    trigger: Voluntary step-down or term limit
    benefits:
      - Emeritus title
      - Advisory input welcome
      - Community access retained
      - Recognition in CONTRIBUTORS
```

### Removal Process

```
Removal Grounds:
━━━━━━━━━━━━━━━━

  Automatic Removal:
  ├─ Inactivity (90 days without explanation)
  ├─ Term limit reached
  └─ Organization capacity limit exceeded

  Vote-Based Removal:
  ├─ Code of Conduct violation (severe)
  ├─ Trust violation (security, confidentiality)
  ├─ Repeated governance violation
  └─ Vote of no confidence (66% threshold)

  Removal Process:
  1. Issue raised (privately for sensitive matters)
  2. Investigation by appropriate body
  3. Opportunity to respond
  4. Vote if warranted
  5. Decision communicated
  6. Transition support if applicable
```

---

## Responsibilities Matrix

### Decision Authority

| Decision Type | Maintainer | Core | Tech Steward | Proj Steward | Council |
|---------------|------------|------|--------------|--------------|---------|
| Merge PR | ✅ | ✅ | ✅ | ✅ | — |
| RFC approval | — | ✅ | ✅ | ✅ | — |
| Breaking change | — | ✅ | ✅ | ✅ | — |
| Major release | — | — | ✅ | ✅ | — |
| Governance change | — | — | — | ✅ | ✅ |
| Charter amendment | — | — | — | — | ✅ |
| Trademark decision | — | — | — | — | ✅ |

### Communication Responsibilities

| Channel | Maintainer | Core | Steward |
|---------|------------|------|---------|
| Issue response | Required | Required | Optional |
| PR review | Required | Required | Optional |
| Mailing list | Optional | Required | Required |
| Blog posts | Optional | Encouraged | Required |
| Conference talks | Optional | Encouraged | Required |
| Media interviews | — | Optional | Required |

---

## Related Documents

- [Transition Protocol](transition_protocol.md)
- [Community Succession](community_succession.md)
- [Governance Framework](/docs/governance_framework.md)

---

*Last updated: February 2026*
