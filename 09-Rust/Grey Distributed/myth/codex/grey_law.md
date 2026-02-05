# Grey Distributed — Legal Codification

> Legal rules, procedures, and enforcement mechanisms governing Grey Distributed as permanent civilization infrastructure.

---

## Title I: General Provisions

### Chapter 1: Scope and Application

#### Section 1.1: Jurisdiction

This Legal Codification applies to:

1. **Operations**: All activities conducted by Grey Distributed infrastructure.
2. **Governance**: All decisions made by Grey Distributed governing bodies.
3. **Participants**: All individuals and entities participating in Grey Distributed.
4. **Services**: All services provided through Grey Distributed.
5. **Assets**: All assets owned or controlled by Grey Distributed.

#### Section 1.2: Supremacy

```
Constitutional provisions supersede all other law.
Ethical framework guides interpretation of all law.
This Legal Codification implements constitutional and ethical requirements.
```

#### Section 1.3: Interpretation

When interpreting this Codification:

1. **Purpose**: Interpret to advance the purpose of Grey Distributed.
2. **Rights**: Construe provisions to maximize protected rights.
3. **Ambiguity**: Resolve ambiguity in favor of individuals over institutions.
4. **Evolution**: Allow interpretive evolution while preserving core meaning.

---

## Title II: Membership and Participation

### Chapter 2: Participant Rights

#### Section 2.1: Right to Access

```rust
/// Access rights hierarchy
pub enum AccessRight {
    /// Basic access for all humans
    Universal {
        scope: vec!["read", "basic_compute", "basic_storage"],
        conditions: None,
        cost: Free,
    },
    
    /// Standard access for contributors
    Contributor {
        scope: vec!["universal", "write", "governance_participation"],
        conditions: Some(ContributionRequirement),
        cost: Contribution,
    },
    
    /// Enhanced access for substantive contributors
    Substantive {
        scope: vec!["contributor", "elevated_resources", "technical_governance"],
        conditions: Some(SubstantiveContributionRequirement),
        cost: SubstantiveContribution,
    },
    
    /// Administrative access for stewards
    Steward {
        scope: vec!["substantive", "administrative", "emergency"],
        conditions: Some(StewardshipRequirement),
        cost: Stewardship,
    },
}
```

#### Section 2.2: Right to Privacy

All participants have the right to:

| Right | Description | Limitations |
|-------|-------------|-------------|
| Data minimization | Only necessary data collected | Operational necessity |
| Purpose limitation | Data used only for stated purpose | Legal requirements |
| Access | View all personal data held | Security exceptions |
| Rectification | Correct inaccurate data | Auditability requirements |
| Deletion | Remove data upon request | Retention obligations |
| Portability | Export data in open formats | Technical feasibility |

#### Section 2.3: Right to Due Process

Prior to any adverse action, participants shall receive:

1. **Notice**: Written notice of proposed action.
2. **Explanation**: Reason for proposed action.
3. **Evidence**: Access to evidence supporting action.
4. **Response**: Opportunity to respond before final action.
5. **Review**: Review by impartial decision-maker.
6. **Appeal**: Right to appeal adverse decision.

### Chapter 3: Participant Obligations

#### Section 3.1: Acceptable Use

Participants shall not:

```yaml
prohibited_conduct:
  absolute_prohibitions:
    - Use services for illegal purposes
    - Interfere with service operation
    - Violate others' rights through service
    - Attempt unauthorized access
    - Distribute malware or harmful content
    - Engage in spam or abuse
    
  conditional_prohibitions:
    - Exceed resource allocations without authorization
    - Operate competing services without disclosure
    - Resell services without agreement
    - Represent affiliation without authorization
```

#### Section 3.2: Contribution Obligations

| Participant Level | Contribution Requirement |
|-------------------|-------------------------|
| Universal | None (free tier) |
| Contributor | Meaningful contribution annually |
| Substantive | Ongoing significant contribution |
| Steward | Substantial ongoing commitment |

---

## Title III: Governance Law

### Chapter 4: Legislative Process

#### Section 4.1: Types of Enactments

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         ENACTMENT TYPES                                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  Constitutional Amendment                                                   │
│  ├── Authority: All governing bodies + community ratification              │
│  ├── Process: 180-day review + supermajority + ratification                │
│  └── Effect: Modifies fundamental law                                       │
│                                                                             │
│  Statute                                                                    │
│  ├── Authority: Community Parliament                                        │
│  ├── Process: Committee + floor vote + review                               │
│  └── Effect: Establishes policy and rules                                   │
│                                                                             │
│  Technical Standard                                                         │
│  ├── Authority: Technical Assembly                                          │
│  ├── Process: RFC + review + adoption                                       │
│  └── Effect: Establishes technical requirements                             │
│                                                                             │
│  Regulation                                                                 │
│  ├── Authority: Executive bodies                                            │
│  ├── Process: Notice + comment + adoption                                   │
│  └── Effect: Implements statutes                                            │
│                                                                             │
│  Guidance                                                                   │
│  ├── Authority: Appropriate body                                            │
│  ├── Process: Publication                                                   │
│  └── Effect: Informational, non-binding                                     │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

#### Section 4.2: Legislative Procedure

Standard legislative procedure:

1. **Introduction**: Proposal submitted by authorized party.
2. **Committee Review**: Assigned committee reviews and amends.
3. **Public Comment**: 30-day public comment period.
4. **Floor Consideration**: Full body debate and vote.
5. **Second Body**: Second body review if required.
6. **Executive Review**: Executive review for consistency.
7. **Publication**: Published in official register.
8. **Effective Date**: Takes effect after publication period.

#### Section 4.3: Emergency Legislation

In emergencies, the Foundation Council may:

1. Enact temporary measures with immediate effect.
2. Duration: Maximum 90 days without regular process.
3. Scope: Limited to addressing emergency.
4. Review: Must be ratified or expires automatically.

### Chapter 5: Judicial Process

#### Section 5.1: Dispute Resolution System

```rust
/// Dispute resolution hierarchy
pub enum DisputeResolution {
    /// Level 1: Automated/informal
    Automated {
        scope: "Clear-cut violations, minor disputes",
        process: AutomatedResolution,
        appeal: Some(CommunityMediation),
    },
    
    /// Level 2: Community mediation
    CommunityMediation {
        scope: "Participant disputes, interpretation questions",
        process: MediatorAssisted,
        appeal: Some(AdjudicationPanel),
    },
    
    /// Level 3: Adjudication panel
    AdjudicationPanel {
        scope: "Significant disputes, policy interpretation",
        process: FormalHearing,
        appeal: Some(ConstitutionalCourt),
    },
    
    /// Level 4: Constitutional court
    ConstitutionalCourt {
        scope: "Constitutional questions, final appeals",
        process: FullJudicialProcess,
        appeal: None, // Final
    },
}
```

#### Section 5.2: Adjudication Standards

| Standard | Application |
|----------|-------------|
| Beyond reasonable doubt | Criminal-equivalent sanctions |
| Clear and convincing | Serious sanctions |
| Preponderance of evidence | General disputes |
| Substantial evidence | Review of decisions |

#### Section 5.3: Remedies

Available remedies:

```yaml
remedies:
  injunctive:
    - Cease and desist order
    - Mandatory action requirement
    - Temporary restraining order
    
  compensatory:
    - Restoration of access
    - Data correction
    - Resource allocation adjustment
    - Monetary compensation (where applicable)
    
  declaratory:
    - Rights determination
    - Policy interpretation
    - Constitutional guidance
    
  disciplinary:
    - Warning
    - Probation
    - Suspension
    - Expulsion (extreme cases only)
```

---

## Title IV: Administrative Law

### Chapter 6: Administrative Bodies

#### Section 6.1: Executive Functions

Executive functions are distributed across:

| Function | Responsible Body | Oversight |
|----------|------------------|-----------|
| Operations | Operations Council | Technical Assembly |
| Security | Security Council | Foundation Council |
| Finance | Finance Council | Community Parliament |
| Community | Community Council | Community Parliament |
| Technical | Technical Leads | Technical Assembly |

#### Section 6.2: Administrative Procedure

All administrative decisions must:

1. **Authority**: Cite legal authority for action.
2. **Notice**: Provide advance notice to affected parties.
3. **Opportunity**: Allow response before action.
4. **Reasoning**: Provide written reasoning for decision.
5. **Record**: Maintain complete decision record.
6. **Appeal**: Allow appeal to independent body.

#### Section 6.3: Rulemaking

Administrative rules require:

1. **Notice**: Published notice of proposed rule.
2. **Comment Period**: Minimum 30-day comment period.
3. **Response**: Response to significant comments.
4. **Publication**: Published in official register.
5. **Delay**: Minimum 30-day delay before effect.

### Chapter 7: Enforcement

#### Section 7.1: Enforcement Powers

```rust
/// Enforcement powers by severity
pub enum EnforcementAction {
    /// Informational/warning
    Warning {
        effect: "Notice of potential violation",
        due_process: PublicNotice,
        appeal: SelfReview,
    },
    
    /// Corrective
    Correction {
        effect: "Required change without penalty",
        due_process: NoticeAndOpportunity,
        appeal: AdministrativeReview,
    },
    
    /// Restrictive
    Restriction {
        effect: "Temporary limitation of access/rights",
        due_process: Hearing,
        appeal: AdjudicationPanel,
    },
    
    /// Suspensive
    Suspension {
        effect: "Temporary removal of privileges",
        due_process: FormalHearing,
        appeal: ConstitutionalCourt,
    },
    
    /// Expulsive
    Expulsion {
        effect: "Permanent removal from governance",
        due_process: FullJudicialProcess,
        appeal: ConstitutionalCourt,
    },
}
```

#### Section 7.2: Proportionality

All enforcement must be:

1. **Proportional**: Sanction proportional to violation.
2. **Necessary**: Least restrictive effective measure.
3. **Consistent**: Similar violations treated similarly.
4. **Corrective**: Aimed at correction, not punishment.

#### Section 7.3: Statute of Limitations

| Violation Type | Limitation Period |
|----------------|-------------------|
| Minor procedural | 6 months |
| Moderate violations | 2 years |
| Serious violations | 5 years |
| Constitutional violations | None |
| Fraud or concealment | From discovery |

---

## Title V: Property and Resources

### Chapter 8: Resource Governance

#### Section 8.1: Common Resources

Grey Distributed resources are held in common trust:

```yaml
common_resources:
  infrastructure:
    governance: Technical Assembly allocation
    access: Proportional to contribution
    priority: Critical services first
    
  treasury:
    governance: Finance Council + Parliament
    access: Authorized expenditures only
    priority: Operations > Development > Reserves
    
  intellectual_property:
    governance: Foundation Council
    access: Open source license terms
    priority: Preservation > Distribution
    
  data:
    governance: Participant ownership + platform stewardship
    access: Participant consent + operational necessity
    priority: Privacy > Utility
```

#### Section 8.2: Contribution Recognition

Contributions are recognized through:

| Contribution Type | Recognition | Governance Weight |
|-------------------|-------------|-------------------|
| Code | Commit credit | Based on impact |
| Documentation | Attribution | Based on scope |
| Operations | Role recognition | Based on commitment |
| Financial | Contribution tier | Based on amount |
| Community | Community standing | Based on engagement |

#### Section 8.3: Resource Allocation

Resource allocation follows:

1. **Operational needs**: Minimum for continued operation.
2. **Security requirements**: Protection of systems and data.
3. **Development priorities**: Approved by Technical Assembly.
4. **Community requests**: Based on available capacity.
5. **Reserve maintenance**: 24-month operating reserve.

---

## Title VI: International and External Relations

### Chapter 9: External Legal Relations

#### Section 9.1: Legal Entity Structure

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    LEGAL ENTITY STRUCTURE                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  Grey Distributed Foundation (Swiss)                                        │
│  ├── Role: Primary governance vehicle                                       │
│  ├── Jurisdiction: Switzerland                                              │
│  └── Purpose: Neutral, stable legal home                                    │
│                                                                             │
│  Regional Operating Entities                                                │
│  ├── Europe: Grey Distributed Europe (Netherlands/Ireland)                  │
│  ├── Americas: Grey Distributed Americas (Delaware)                         │
│  ├── Asia-Pacific: Grey Distributed Asia (Singapore)                        │
│  └── Other: As needed for local operation                                   │
│                                                                             │
│  Relationships                                                              │
│  ├── Foundation controls policy                                             │
│  ├── Regional entities operate locally                                      │
│  └── Coordination through Federation Agreement                              │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

#### Section 9.2: Compliance Framework

Compliance with external law:

```rust
/// Compliance decision framework
pub fn handle_legal_requirement(requirement: LegalRequirement) -> Response {
    // Step 1: Analyze requirement
    if requirement.violates_constitution() {
        return Response::ResistWithinLaw;
    }
    
    // Step 2: Check for conflict with ethics
    if requirement.violates_ethics() {
        return Response::SeekExemptionOrChallenge;
    }
    
    // Step 3: Assess scope
    match requirement.scope {
        Scope::Local => Response::ComplyLocally,
        Scope::Global => Response::GlobalPolicy,
    }
}
```

#### Section 9.3: Jurisdictional Conflicts

When laws conflict:

1. **Priority**: Constitution > Ethics > Internal Law > External Law
2. **Resolution**: Comply with highest protecting rights.
3. **Transparency**: Document conflicts and reasoning.
4. **Advocacy**: Work to change conflicting external laws.

---

## Title VII: Amendments and Transition

### Chapter 10: Legal Evolution

#### Section 10.1: Amendment Process

| Provision Type | Amendment Process |
|----------------|-------------------|
| Constitutional | Per Constitution Article IV |
| Statutory | Parliament majority + review |
| Regulatory | Administrative procedure |
| Procedural | Appropriate body authority |

#### Section 10.2: Transition Rules

When laws change:

```yaml
transition_rules:
  notice_period:
    minor: 30 days
    moderate: 90 days
    major: 180 days
    
  grandfathering:
    existing_rights: Protected for transition period
    pending_matters: Governed by prior law
    contracts: Honored through term
    
  retroactivity:
    beneficial_changes: May be retroactive
    burdensome_changes: Prospective only
    rights_reductions: Never retroactive
```

#### Section 10.3: Sunset Provisions

All non-constitutional law shall include:

1. **Review date**: Scheduled review within 10 years.
2. **Sunset**: Automatic expiration unless renewed.
3. **Assessment**: Effectiveness assessment before renewal.

---

## Appendix A: Legal Definitions

```yaml
definitions:
  participant: "Any individual or entity using Grey Distributed services"
  contributor: "Participant making recognized contributions"
  steward: "Participant with governance responsibilities"
  governing_body: "Authorized decision-making entity"
  adverse_action: "Any action negatively affecting participant rights"
  due_process: "Fair procedures before adverse action"
  appeal: "Review of decision by independent body"
  supermajority: "Two-thirds or more of voting members"
  quorum: "Minimum participation for valid action"
```

## Appendix B: Legal Procedures Quick Reference

```
Participant Rights Violation:
  1. File complaint with Ombudsperson
  2. Investigation (14 days)
  3. Resolution attempt (30 days)
  4. Escalation to Adjudication Panel if unresolved

Governance Dispute:
  1. Raise with appropriate body
  2. Body resolution attempt
  3. Escalation to Foundation Council
  4. Constitutional Court for constitutional issues

Appeal Process:
  1. File notice of appeal (30 days from decision)
  2. Record preparation (30 days)
  3. Briefing (30 days each side)
  4. Hearing (if granted)
  5. Decision (60 days from hearing)
```

---

*This Legal Codification implements the Grey Constitution and Ethical Framework, providing operational rules for permanent, fair governance.*
