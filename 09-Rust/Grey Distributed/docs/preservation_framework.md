# Grey Distributed — Preservation Framework

> A comprehensive framework for artifact archival, historical preservation, and long-term accessibility of system knowledge across decades.

## Overview

The Preservation Framework ensures that Grey Distributed's artifacts, decisions, proofs, and institutional knowledge remain accessible, verifiable, and useful for decades. It addresses both technical preservation (data formats, storage) and institutional preservation (context, meaning, relationships).

## Philosophy

### Preservation Principles

1. **Nothing Important Is Lost**: Critical artifacts survive technical and organizational changes
2. **Context Survives**: Artifacts retain meaning without original creators
3. **Verification Remains Possible**: Proofs and tests can be re-executed
4. **History Informs Future**: Past decisions guide future ones
5. **Open Access**: Preserved artifacts are publicly accessible

### What We Preserve

| Category | Examples | Retention | Priority |
|----------|----------|-----------|----------|
| **Proofs** | Formal verification, TLA+ specs | Perpetual | Critical |
| **Source** | All releases, significant commits | Perpetual | Critical |
| **Decisions** | ADRs, RFCs, meeting minutes | Perpetual | High |
| **Governance** | Votes, resolutions, policies | Perpetual | High |
| **Security** | Advisories, patches, audits | Perpetual | Critical |
| **Benchmarks** | Performance baselines | 20 years | Medium |
| **Documentation** | Guides, references | 10 years | Medium |
| **Discussions** | Mailing lists, forums | 10 years | Low |

## Tiered Preservation Model

### Tier Structure

```
┌─────────────────────────────────────────────────────────────────┐
│                    Tier 1: Perpetual                            │
│                    (Forever guarantee)                          │
│                                                                 │
│  Formal proofs, consensus specifications, security advisories   │
│                                                                 │
│  Storage: Multiple cold archives, geographically distributed    │
│  Format: Plain text, PDF/A, standardized schemas                │
│  Verification: Annual integrity check, biennial readability     │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Tier 2: Long-Term                            │
│                    (20-year guarantee)                          │
│                                                                 │
│  Release artifacts, benchmark baselines, major governance       │
│                                                                 │
│  Storage: Cold archive + warm backup                            │
│  Format: Open formats, migration on deprecation                 │
│  Verification: Quarterly integrity, annual readability          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Tier 3: Standard                             │
│                    (10-year guarantee)                          │
│                                                                 │
│  Documentation, community discussions, operational logs         │
│                                                                 │
│  Storage: Standard object storage with redundancy               │
│  Format: Current formats acceptable                             │
│  Verification: Annual integrity                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Tier 4: Operational                          │
│                    (5-year guarantee)                           │
│                                                                 │
│  Development artifacts, CI logs, transient documentation        │
│                                                                 │
│  Storage: Standard storage                                      │
│  Format: Native formats                                         │
│  Verification: Continuous availability                          │
└─────────────────────────────────────────────────────────────────┘
```

### Tier Assignments

```yaml
tier_1_perpetual:
  formal_verification:
    - TLA+ specifications
    - Coq/Lean proofs
    - Model checking results
    - Proof narratives
    
  consensus_artifacts:
    - Protocol specifications
    - Safety/liveness proofs
    - Reconfiguration proofs
    
  security_critical:
    - Security advisories (CVEs)
    - Audit reports
    - Cryptographic specifications
    - Incident post-mortems
    
  governance_core:
    - Foundation charter
    - Governance amendments
    - Election results (leadership)

tier_2_long_term:
  releases:
    - Source tarballs (all versions)
    - Binary distributions (major versions)
    - Release notes
    - Changelogs
    
  benchmarks:
    - Performance baselines
    - Regression test results
    - Scalability measurements
    
  architecture:
    - Architecture Decision Records
    - Design documents
    - API specifications

tier_3_standard:
  documentation:
    - User guides
    - API references
    - Tutorials
    
  community:
    - Mailing list archives
    - Forum discussions
    - Meeting recordings
    
  development:
    - Git history
    - Issue tracker
    - PR discussions

tier_4_operational:
  transient:
    - CI/CD logs
    - Build artifacts (non-release)
    - Development snapshots
```

## Storage Strategy

### Multi-Location Redundancy

```
Primary Storage (Hot)
├── Cloud Provider A (US-East)
│   └── All tiers, immediate access
│
├── Cloud Provider B (EU-West)
│   └── All tiers, immediate access
│
└── Self-Hosted (Foundation DC)
    └── Tier 1-2, immediate access

Cold Storage (Archive)
├── AWS Glacier Deep Archive
│   └── Tier 1-2, 12-hour retrieval
│
├── Azure Archive Storage
│   └── Tier 1-2, 12-hour retrieval
│
├── Software Heritage
│   └── Source code, perpetual
│
└── Internet Archive
    └── Documentation, perpetual
```

### Format Standards

| Content Type | Primary Format | Fallback Format | Migration Trigger |
|--------------|----------------|-----------------|-------------------|
| Text documents | Markdown | PDF/A | Format obsolescence |
| Specifications | Plain text + diagrams | PDF/A | — |
| Proofs | Native + PDF export | Plain text narrative | Tool EOL |
| Source code | Git (bare) | tarball | — |
| Binaries | Original + checksum | — | 10 years |
| Data | JSON/YAML | CSV | Schema change |
| Diagrams | SVG + source | PNG | Tool EOL |
| Video | MP4 (H.264) | — | Codec obsolescence |

### Integrity Verification

```python
class IntegrityVerification:
    """Artifact integrity verification system."""
    
    def __init__(self):
        self.hash_algorithms = ['SHA-256', 'BLAKE3']
        self.signature_algorithm = 'Ed25519'
    
    def create_manifest(self, artifact_path):
        """Create verification manifest for artifact."""
        return {
            'path': artifact_path,
            'size': get_file_size(artifact_path),
            'hashes': {
                alg: compute_hash(artifact_path, alg)
                for alg in self.hash_algorithms
            },
            'signature': sign_artifact(artifact_path),
            'created': timestamp_now(),
            'verified': []
        }
    
    def verify_integrity(self, artifact_path, manifest):
        """Verify artifact matches manifest."""
        checks = []
        
        # Size check
        checks.append(('size', 
            get_file_size(artifact_path) == manifest['size']))
        
        # Hash checks
        for alg, expected in manifest['hashes'].items():
            actual = compute_hash(artifact_path, alg)
            checks.append((f'hash_{alg}', actual == expected))
        
        # Signature check
        checks.append(('signature',
            verify_signature(artifact_path, manifest['signature'])))
        
        return VerificationResult(
            passed=all(c[1] for c in checks),
            checks=checks,
            timestamp=timestamp_now()
        )
```

## Artifact Lifecycle

### Ingestion

```
Artifact Created
      │
      ▼
┌─────────────────┐
│  Classification │  ← Determine tier and retention
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Validation     │  ← Format check, completeness
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Enrichment     │  ← Add metadata, context links
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Checksumming   │  ← Generate integrity hashes
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Replication    │  ← Copy to all required locations
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Indexing       │  ← Add to search, catalog
└─────────────────┘
```

### Maintenance

| Activity | Frequency | Scope |
|----------|-----------|-------|
| Integrity verification | Daily | Sample (1%) |
| Integrity verification | Monthly | Tier 1 (100%) |
| Integrity verification | Quarterly | All tiers (100%) |
| Readability test | Annually | Random sample |
| Format migration check | Annually | All formats |
| Storage health audit | Quarterly | All locations |
| Disaster recovery test | Annually | Full recovery drill |

### Retirement

```
Retention Period Ends
         │
         ▼
┌─────────────────┐
│  Review         │  ← Confirm no extended retention needed
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Notice         │  ← 90-day public notice of retirement
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Archive Copy   │  ← Final copy to long-term archive
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Deletion       │  ← Remove from active storage
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Catalog Update │  ← Update indexes, add redirect
└─────────────────┘
```

## Context Preservation

### Artifact Metadata

Every preserved artifact includes:

```yaml
artifact_metadata:
  core:
    id: "uuid"
    title: "Human-readable title"
    description: "What this artifact is"
    created: "2024-01-15T10:30:00Z"
    creator: "person or system"
    
  classification:
    tier: 1  # 1-4
    category: "formal_verification"
    subcategory: "consensus_proofs"
    retention: "perpetual"
    
  context:
    purpose: "Why this artifact was created"
    related_artifacts: ["uuid1", "uuid2"]
    supersedes: "uuid-of-old"
    superseded_by: null
    references:
      - type: "ADR"
        id: "ADR-042"
      - type: "RFC"
        id: "RFC-2024-003"
        
  provenance:
    origin: "Description of how artifact was created"
    tools: ["TLA+ Toolbox v1.7", "Apalache v0.30"]
    environment: "Ubuntu 22.04, 64GB RAM"
    reproducibility: "See repro/specs/consensus/"
    
  access:
    license: "Apache-2.0"
    public: true
    restrictions: null
```

### Narrative Documentation

Critical artifacts include narrative context:

```markdown
# Artifact Narrative: Raft Consensus Safety Proof

## Background

This proof was developed in Q2 2024 as part of the v2.0 release
certification. It addresses the core safety property of our Raft
implementation.

## What It Proves

The specification proves that no two nodes can have different values
committed at the same log index. This is the fundamental safety
property required for consensus.

## How To Use This Proof

1. The TLA+ specification is in `raft_safety.tla`
2. Run with TLC using config `raft_safety.cfg`
3. Expected runtime: 4-6 hours on 64-core machine
4. See `raft_safety_results.txt` for expected output

## Historical Context

This proof supersedes the earlier v1.0 proof which did not account
for dynamic membership changes. The key insight from ADR-042 is
encoded in the `MembershipChange` action.

## Known Limitations

- Does not model network partitions > 30 seconds
- Assumes monotonic clocks
- Does not cover Byzantine failures

## Related Work

- Original Raft paper (Ongaro 2014)
- Our v1.0 proof (superseded)
- Linearizability proof (see sibling artifact)
```

## Disaster Recovery

### Scenarios and Response

| Scenario | RTO | RPO | Recovery Process |
|----------|-----|-----|------------------|
| Single location failure | 1 hour | 0 | Automatic failover |
| Cloud provider failure | 4 hours | 0 | Cross-cloud recovery |
| Data corruption (detected) | 24 hours | 0 | Restore from backup |
| Data corruption (undetected) | 48 hours | varies | Forensic recovery |
| Ransomware attack | 72 hours | 24 hours | Cold storage recovery |
| Complete foundation dissolution | 30 days | 0 | Third-party archive |

### Third-Party Safeguards

```yaml
external_archives:
  software_heritage:
    url: "https://www.softwareheritage.org/"
    coverage: "All source code"
    frequency: "Continuous"
    relationship: "Automatic via GitHub"
    
  internet_archive:
    url: "https://archive.org/"
    coverage: "Public documentation"
    frequency: "Monthly crawl"
    relationship: "Partnership agreement"
    
  zenodo:
    url: "https://zenodo.org/"
    coverage: "Formal proofs, benchmark data"
    frequency: "Per release"
    relationship: "Manual deposit"
    
  university_libraries:
    partners:
      - "MIT Libraries"
      - "Stanford Digital Repository"
    coverage: "Complete archive snapshots"
    frequency: "Annual"
    relationship: "Formal MOU"
```

### Recovery Testing

Annual disaster recovery exercises:

1. **Tabletop Exercise**: Walk through scenarios, validate runbooks
2. **Partial Recovery**: Recover random sample from cold storage
3. **Full Recovery**: Complete system restoration from archives
4. **Accessibility Test**: Verify artifacts readable without special tools

## Accessibility

### Format Migration Strategy

```
Current Format                    Migration Trigger
      │                                  │
      ├──── Usage drops below 10% ───────┤
      ├──── Major tool EOL announced ────┤
      ├──── Security vulnerability ──────┤
      └──── 10 years without updates ────┘
                     │
                     ▼
           ┌─────────────────┐
           │ Migration Plan  │
           └────────┬────────┘
                    │
    ┌───────────────┼───────────────┐
    ▼               ▼               ▼
Convert to      Maintain         Export to
new format     parallel        open format
                copies
```

### Open Format Requirements

All preserved artifacts must be:

1. **Readable with open-source tools**: No proprietary lock-in
2. **Self-describing**: Format identifiable without external reference
3. **Documented**: Format specification publicly available
4. **Migratable**: Path to successor format when deprecated

### Long-Term Readability

For perpetual artifacts:

- Store in at least two independent formats
- Include format specification with artifact
- Test readability with new tools annually
- Maintain rendering examples

## Governance

### Preservation Committee

```yaml
preservation_committee:
  membership:
    - Archive Steward (chair)
    - Technical Steward (rotating)
    - External archivist (advisory)
    
  responsibilities:
    - Approve tier assignments
    - Review retention decisions
    - Authorize destruction
    - Approve format migrations
    - Conduct annual audit
    
  meeting_frequency: quarterly
  
  decisions_require:
    - Quorum: 2 of 3 members
    - Tier 1 changes: Unanimous
    - Tier 2+ changes: Majority
```

### Policies

| Policy | Review Cycle | Authority |
|--------|--------------|-----------|
| Retention Schedule | Annual | Committee |
| Format Standards | Biennial | Committee + Tech Steward |
| Storage Locations | Annual | Committee |
| Access Controls | Annual | Security Steward |
| Destruction Approval | As needed | Committee (unanimous) |

## Metrics and Monitoring

### Preservation Health

| Metric | Target | Alert |
|--------|--------|-------|
| Integrity check pass rate | 100% | <99.9% |
| Storage redundancy | 3+ copies | <3 |
| Format freshness | All current | Any deprecated |
| Catalog completeness | 100% indexed | <95% |
| Recovery drill success | 100% | Any failure |

### Dashboard Indicators

```json
{
  "preservation_dashboard": {
    "summary": {
      "total_artifacts": "count by tier",
      "storage_used": "size by tier",
      "last_verification": "timestamp",
      "health_score": "0-100"
    },
    "alerts": [
      "overdue_verification",
      "storage_approaching_limit",
      "format_deprecation_pending",
      "integrity_failure"
    ]
  }
}
```

## References

- [Artifact Archive](../legacy/archival/artifact_archive.yaml)
- [Preservation Policy](../legacy/archival/preservation_policy.md)
- [Historical Case Study](../legacy/archival/historical_case_study.md)
- [Legacy Dashboard](../dashboards/legacy_dashboard.json)

---

*This framework is reviewed annually and updated to reflect evolving preservation best practices and storage technology changes.*
