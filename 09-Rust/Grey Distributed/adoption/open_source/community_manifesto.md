# Grey Distributed — Community Manifesto

## Our Vision

Grey Distributed exists to democratize access to planet-scale distributed computing. We believe that the power to coordinate massive computational workloads should not be locked behind proprietary systems or gated by corporate interests. Through open collaboration, we are building infrastructure that is:

- **Open**: Source code, design decisions, and governance are transparent
- **Fair**: Resource allocation respects all participants equally
- **Resilient**: No single point of failure, no single point of control
- **Sustainable**: Economic models that ensure long-term viability

---

## Core Principles

### 1. Openness by Default

We develop in the open. Our code, our discussions, our roadmaps, and our decision-making processes are visible to anyone who wishes to observe or participate.

**What this means in practice:**
- All source code is licensed under Apache 2.0 with Commons Clause
- Design documents (RFCs) are published before implementation begins
- Meeting notes from core team discussions are public
- Roadmap decisions are made through community RFC process

**What this does NOT mean:**
- Security vulnerabilities are discussed publicly (responsible disclosure applies)
- Personal information of contributors is shared
- Every decision requires a public vote (maintainers exercise judgment)

### 2. Meritocratic Contribution

Influence in the Grey project is earned through contribution, not purchased through funding or claimed through affiliation. We recognize contributions in many forms:

| Contribution Type | Recognition Path |
|-------------------|------------------|
| Code contributions | Committer → Maintainer → Core |
| Documentation | Docs Lead → Steering Committee |
| Community support | Community Champion recognition |
| Testing/QA | Release team membership |
| Security research | Security team membership |
| Advocacy | Ambassador program |

### 3. Federation is Freedom

We reject the model where a single entity controls the network. Grey is designed for federation from the ground up—independent operators can run their own clusters and choose whether and how to interconnect.

**Federation guarantees:**
- No cluster is required to federate with any other
- Federation agreements are bilateral and revocable
- Resource sharing respects declared policies
- Data sovereignty is maintained across federation boundaries

### 4. Sustainable Economics

Open source projects fail when they aren't economically sustainable. We embrace transparent economic mechanisms that align incentives:

- Operators earn Grey tokens for providing reliable resources
- Resource consumers pay fair market rates
- Core development is funded by the Grey Foundation
- Commercial entities can build on Grey with clear licensing terms

### 5. Progressive Decentralization

We are honest about where we are in the decentralization journey. Today, the Grey Foundation holds significant influence. Our explicit goal is to progressively transfer control to the community:

```
Phase 1 (Current): Foundation-led development
  └─ Core team makes most technical decisions
  └─ Foundation controls reference implementation
  └─ Community provides input via RFCs

Phase 2 (2026): Shared governance
  └─ Technical Steering Committee elected by contributors
  └─ Multiple independent implementations
  └─ Foundation becomes one voice among many

Phase 3 (2027+): Community-led
  └─ Foundation transitions to infrastructure support only
  └─ Governance fully decentralized
  └─ Token-based voting on protocol changes
```

---

## Community Rights

As a participant in the Grey community, you have the following rights:

### Right to Fork

You may fork the Grey codebase at any time for any reason. This is not just permitted—it is encouraged as the ultimate check on project governance. A healthy project should never fear forks.

### Right to Voice

Every community member may propose changes through the RFC process. Your proposal will be considered fairly, regardless of your history with the project. Rejection must include substantive technical reasoning.

### Right to Privacy

Your participation in Grey does not require you to reveal your identity. Pseudonymous contribution is fully supported. We will never require identity verification for community participation.

### Right to Exit

You may leave the Grey community at any time without penalty. Your contributions remain under the terms of their original license. We will not retaliate against departing community members.

### Right to Fair Treatment

The Grey Code of Conduct applies to all community spaces. Harassment, discrimination, and abuse are not tolerated. Reports are handled confidentially by an independent committee.

---

## Community Responsibilities

With rights come responsibilities. We ask all community members to:

### Act in Good Faith

Assume positive intent. Disagree constructively. Remember that text lacks tone—give others the benefit of the doubt.

### Contribute Upstream

When you improve Grey for your use case, consider whether your improvement benefits others. Upstream contributions keep the ecosystem healthy.

### Report Issues

Security vulnerabilities, Code of Conduct violations, and governance concerns should be reported through appropriate channels. Speaking up protects the community.

### Mentor Newcomers

We were all beginners once. Take time to help new contributors find their footing. A welcoming community grows faster than an exclusive one.

### Respect Decisions

Not every proposal is accepted. Not every feature makes the roadmap. Maintainers must balance many concerns. Accept decisions gracefully, even when you disagree.

---

## Governance Structure

### Grey Foundation

The non-profit entity that:
- Holds the Grey trademark
- Manages core infrastructure (main git repository, build systems, website)
- Employs some full-time core developers
- Administers the Grey token treasury

### Technical Steering Committee (TSC)

The seven-member body that:
- Approves major architectural changes
- Resolves technical disputes
- Manages the RFC process
- Maintains the technical roadmap

*Election process:* TSC members are elected annually by contributors with 50+ merged commits or equivalent contribution.

### Maintainers

Individuals responsible for specific subsystems:

| Subsystem | Scope |
|-----------|-------|
| Consensus | Raft implementation, leader election, log replication |
| Scheduler | Task placement, resource allocation, fairness |
| Network | Cluster communication, federation protocol |
| Storage | Distributed storage, replication, erasure coding |
| Security | Authentication, authorization, encryption |
| Observability | Metrics, logging, tracing |
| SDK | Client libraries (Rust, Python, JavaScript) |
| Docs | Documentation, tutorials, API references |

### Special Interest Groups (SIGs)

Cross-cutting working groups:

- **SIG-Adoption**: Enterprise and government adoption
- **SIG-Federation**: Multi-cluster federation protocols
- **SIG-Economics**: Token economics and incentive design
- **SIG-Security**: Security audits and vulnerability response
- **SIG-Performance**: Benchmarking and optimization

### Community Members

Everyone who participates in the Grey ecosystem:
- Users who run Grey clusters
- Developers who build applications on Grey
- Contributors who improve Grey code or docs
- Advocates who spread awareness of Grey

---

## Decision Making

### RFC Process

Significant changes require a Request for Comments (RFC):

```
1. Draft RFC
   └─ Author creates RFC document from template
   └─ Initial discussion in community forum

2. Review Period (2 weeks minimum)
   └─ Community provides feedback
   └─ Author iterates on proposal

3. TSC Review
   └─ TSC schedules discussion
   └─ Author presents proposal
   └─ TSC votes: Accept / Request Changes / Reject

4. Implementation
   └─ Accepted RFCs enter implementation
   └─ PR references RFC number
   └─ RFC status updated on merge
```

### Lazy Consensus

For smaller changes, we use lazy consensus:
- Proposal is made via PR or issue
- If no objections within 72 hours, proposal is assumed approved
- Objections must be substantive (not blanket vetoes)
- Maintainer has final call on unresolved disagreements

### Voting

When consensus cannot be reached:
- TSC votes on technical matters
- Foundation board votes on organizational matters
- Vote results are public, individual votes may be public or private

---

## Economic Sustainability

### How Grey is Funded

```
Funding Sources
═══════════════

Foundation Treasury (50%)
├─ Initial token allocation
├─ Gradual release over 10 years
└─ Funds core development, infrastructure

Commercial Licensing (25%)
├─ Enterprises requiring non-OSS terms
├─ Priority support contracts
└─ Professional services

Grants (15%)
├─ Academic research grants
├─ Government innovation funding
└─ Philanthropic foundations

Donations (10%)
├─ Individual contributors
├─ Corporate sponsors
└─ Community fundraising
```

### Token Distribution

The Grey token enables resource coordination:

```
Token Allocation
════════════════

Community (40%)
├─ Mining rewards for operators
├─ Contribution bounties
└─ Ecosystem grants

Foundation (25%)
├─ Core development
├─ Infrastructure
└─ Security audits

Team (15%)
├─ Core contributors
├─ 4-year vesting
└─ 1-year cliff

Early Supporters (10%)
├─ Initial funding round
├─ 2-year vesting
└─ Infrastructure alignment

Reserve (10%)
├─ Emergency fund
├─ Future development
└─ Partnership opportunities
```

### Commercial Use

Grey embraces commercial adoption while protecting the commons:

**Permitted:**
- Running Grey clusters for commercial purposes
- Building commercial applications on Grey
- Offering Grey-based managed services
- Creating derivative works under Apache 2.0

**Requires License:**
- Using "Grey" trademark in product names
- Removing attribution requirements
- Offering Grey as a competing managed service

---

## Code of Conduct

### Our Pledge

We pledge to make participation in our community a harassment-free experience for everyone, regardless of age, body size, visible or invisible disability, ethnicity, sex characteristics, gender identity and expression, level of experience, education, socio-economic status, nationality, personal appearance, race, religion, or sexual identity and orientation.

### Our Standards

**Positive behaviors:**
- Using welcoming and inclusive language
- Being respectful of differing viewpoints
- Gracefully accepting constructive criticism
- Focusing on what is best for the community
- Showing empathy towards other community members

**Unacceptable behaviors:**
- Sexualized language or imagery
- Trolling, insulting, or derogatory comments
- Personal or political attacks
- Public or private harassment
- Publishing others' private information
- Other conduct which could reasonably be considered inappropriate

### Enforcement

Violations should be reported to: conduct@grey-distributed.io

Reports are handled by the Code of Conduct Committee, which is independent from the Foundation and TSC. All reports are confidential. The committee may:

1. Issue warnings
2. Impose temporary bans from community spaces
3. Impose permanent bans from all project spaces
4. Remove offending content

Appeals are handled by an independent arbitrator selected from a pre-approved panel.

---

## How to Get Involved

### For Users

1. **Deploy Grey**: Run a cluster, provide feedback
2. **Join Discussions**: Participate in forums and chat
3. **Report Issues**: Help us identify bugs and improvements
4. **Spread the Word**: Write about your Grey experience

### For Developers

1. **Good First Issues**: Start with [issues tagged "good first issue"](https://github.com/grey-distributed/grey/labels/good%20first%20issue)
2. **Read the Guide**: Follow the [Contribution Guide](CONTRIBUTING.md)
3. **Join SIGs**: Participate in Special Interest Groups
4. **Propose RFCs**: Shape the project's direction

### For Organizations

1. **Evaluate Grey**: Run a proof-of-concept
2. **Join the Foundation**: Corporate membership levels available
3. **Sponsor Development**: Fund specific features or improvements
4. **Contribute Resources**: Provide infrastructure for testing

---

## Contact

- **Website**: https://grey-distributed.io
- **GitHub**: https://github.com/grey-distributed
- **Forum**: https://discuss.grey-distributed.io
- **Chat**: https://chat.grey-distributed.io
- **Security**: security@grey-distributed.io
- **Conduct**: conduct@grey-distributed.io
- **Press**: press@grey-distributed.io

---

## In Conclusion

Grey Distributed is more than software—it is a community committed to building infrastructure for the common good. We believe that the future of distributed computing should be open, fair, and owned by no one and everyone.

Join us.

---

*This manifesto is a living document. Changes follow the standard RFC process.*
*Last updated: 2026-02-04*
*Version: 1.0*
