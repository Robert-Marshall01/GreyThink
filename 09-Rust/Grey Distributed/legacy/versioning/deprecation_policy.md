# Grey Distributed — Deprecation Policy

This document defines the policy for deprecating features, APIs, and protocols safely across the Grey Distributed ecosystem.

## Table of Contents

1. [Deprecation Philosophy](#deprecation-philosophy)
2. [Deprecation Lifecycle](#deprecation-lifecycle)
3. [Deprecation Categories](#deprecation-categories)
4. [Communication Requirements](#communication-requirements)
5. [Migration Support](#migration-support)
6. [Exception Process](#exception-process)
7. [Current Deprecations](#current-deprecations)

---

## Deprecation Philosophy

### Core Principles

```
Deprecation Principles:
━━━━━━━━━━━━━━━━━━━━━━

  1. TRANSPARENCY
     └─ Users should never be surprised by deprecations

  2. ADEQUATE NOTICE
     └─ Sufficient time to migrate before removal

  3. MIGRATION PATH
     └─ Always provide a clear alternative

  4. TOOLING SUPPORT
     └─ Automated tools to assist migration

  5. BACKWARD COMPATIBILITY
     └─ Deprecated features continue to work
```

### Impact Assessment

Every deprecation must include an impact assessment:

| Impact Level | Criteria | Minimum Notice |
|--------------|----------|----------------|
| **Critical** | Core functionality, widely used | 24 months |
| **High** | Important feature, moderate usage | 18 months |
| **Medium** | Secondary feature, limited usage | 12 months |
| **Low** | Rarely used, easy alternatives | 6 months |
| **Trivial** | Internal API, no external users | 3 months |

---

## Deprecation Lifecycle

### Standard Lifecycle

```
Deprecation Timeline:
═══════════════════════════════════════════════════════════════════

Phase 1: ANNOUNCEMENT (T+0)
├─ Deprecation notice published
├─ Alternative documented
├─ Migration guide created
└─ Deprecation warning added (opt-in)

Phase 2: WARNING (T+3 months)
├─ Deprecation warning enabled by default
├─ CLI shows deprecation notices
├─ Telemetry tracks deprecation usage
└─ Migration tooling released

Phase 3: ERROR (T+6 months to T-3 months before removal)
├─ Deprecation becomes error by default
├─ Can be overridden with flag
├─ Active outreach to remaining users
└─ Migration support intensified

Phase 4: REMOVAL (T+minimum notice period)
├─ Feature removed from codebase
├─ Documentation archived
├─ Breaking change documented
└─ Post-removal support period (3 months)

```

### Lifecycle Example

```
Example: Deprecating Legacy Authentication API
═══════════════════════════════════════════════

2025-01: ANNOUNCEMENT
  ├─ Blog post: "Migrating to OAuth 2.0"
  ├─ Deprecation notice in /api/v1/auth endpoint
  └─ Migration guide published

2025-04: WARNING
  ├─ API returns: X-Deprecation-Warning header
  ├─ greyctl auth shows: "Warning: Legacy auth deprecated"
  └─ Dashboard banner for affected users

2025-10: ERROR
  ├─ API returns: 299 Miscellaneous Warning
  ├─ Requires: --allow-deprecated flag to use
  └─ Weekly emails to remaining users

2026-01: REMOVAL
  ├─ /api/v1/auth removed
  ├─ Returns: 410 Gone with migration link
  └─ 3-month grace period for urgent cases
```

---

## Deprecation Categories

### API Deprecations

| API Type | Minimum Notice | Process |
|----------|----------------|---------|
| REST endpoints | 12 months | Standard lifecycle |
| gRPC services | 12 months | Standard lifecycle |
| SDK methods | 12 months | Standard lifecycle |
| CLI commands | 6 months | Standard lifecycle |
| Configuration options | 6 months | Standard lifecycle |

**API Deprecation Markers**:

```yaml
# REST API: X-Deprecated header
X-Deprecated: true
X-Deprecated-Since: 2025-01-15
X-Deprecated-Removal: 2026-01-15
X-Deprecated-Alternative: /api/v2/auth/oauth
```

```protobuf
// gRPC: Deprecated option
service AuthService {
  rpc LegacyAuth(LegacyAuthRequest) returns (AuthResponse) {
    option deprecated = true;
    option (grey.deprecation) = {
      since: "2025-01-15"
      removal: "2026-01-15"
      alternative: "OAuth2Auth"
    };
  }
}
```

```rust
// SDK: #[deprecated] attribute
#[deprecated(
    since = "1.2.0",
    note = "use `oauth2_authenticate` instead"
)]
pub fn legacy_authenticate(token: &str) -> Result<Session, AuthError> {
    // ...
}
```

### Protocol Deprecations

| Protocol Type | Minimum Notice | Process |
|---------------|----------------|---------|
| Consensus protocol | 24 months | Extended lifecycle |
| Federation protocol | 18 months | Extended lifecycle |
| Wire format | 18 months | Extended lifecycle |
| Encryption algorithm | 12 months | Security exception possible |

### Feature Deprecations

| Feature Type | Minimum Notice | Process |
|--------------|----------------|---------|
| Core feature | 18 months | Extended lifecycle + LTS bridge |
| Optional feature | 12 months | Standard lifecycle |
| Experimental feature | 1 month | Accelerated lifecycle |
| Preview feature | 3 months | Accelerated lifecycle |

---

## Communication Requirements

### Announcement Channels

| Channel | Required For | Timing |
|---------|--------------|--------|
| Blog post | Medium+ impact | At announcement |
| Release notes | All deprecations | At announcement |
| CLI warnings | All deprecations | At warning phase |
| API headers | API deprecations | At warning phase |
| Email notification | High+ impact | At announcement + error phase |
| Dashboard banner | User-affecting | Throughout lifecycle |

### Deprecation Notice Template

```markdown
## Deprecation Notice: [Feature Name]

**Status**: Deprecated
**Since**: [Version]
**Removal Target**: [Version/Date]
**Impact Level**: [Critical/High/Medium/Low/Trivial]

### Summary
[Brief description of what is being deprecated]

### Reason
[Why this deprecation is necessary]

### Migration Path
[Step-by-step migration instructions]

### Alternative
[What to use instead]

### Timeline
| Phase | Date | Action Required |
|-------|------|-----------------|
| Announcement | [Date] | Review migration path |
| Warning | [Date] | Begin migration |
| Error | [Date] | Complete migration |
| Removal | [Date] | Feature removed |

### Resources
- Migration guide: [Link]
- Migration tool: [Command]
- Support contact: [Email/Slack]

### Questions?
Open an issue with the `deprecation` label or contact [team].
```

### Telemetry and Tracking

```
Deprecation Tracking Metrics:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  Metric                          Purpose
  ─────────────────────────────   ──────────────────────────────
  deprecation_usage_total         Total usage of deprecated feature
  deprecation_users_unique        Unique users of deprecated feature
  deprecation_migration_started   Users who began migration
  deprecation_migration_complete  Users who completed migration
  deprecation_override_enabled    Users using override flag
```

---

## Migration Support

### Migration Tooling Requirements

Every deprecation must include:

1. **Automated migration tool** (when feasible)
2. **Migration guide** with examples
3. **Compatibility shim** for gradual migration
4. **Validation tool** to verify successful migration

### Migration Tool Standards

```bash
# Standard migration tool interface
grey-migrate <feature> \
  --from <old-version> \
  --to <new-version> \
  [--dry-run] \
  [--validate] \
  [--rollback]

# Example: Migrate legacy auth to OAuth
grey-migrate auth \
  --from legacy \
  --to oauth2 \
  --dry-run

# Output:
# Migration Plan:
#   - Convert 15 API keys to OAuth clients
#   - Update 3 service accounts
#   - Migrate 127 user sessions
#
# Estimated duration: 5 minutes
# Rollback available: Yes
#
# Run without --dry-run to execute
```

### Compatibility Shims

```rust
// Compatibility shim pattern
mod auth {
    // New implementation
    pub mod oauth2 {
        pub fn authenticate(client_id: &str, secret: &str) -> Result<Token, Error> {
            // New OAuth2 implementation
        }
    }
    
    // Compatibility shim (deprecated)
    #[deprecated(since = "1.2.0", note = "use `oauth2::authenticate` instead")]
    pub mod legacy {
        use super::oauth2;
        
        pub fn authenticate(api_key: &str) -> Result<Token, Error> {
            // Log deprecation warning
            tracing::warn!(
                target: "deprecation",
                feature = "legacy_auth",
                "Legacy authentication is deprecated. Migrate to OAuth2."
            );
            
            // Translate legacy call to new implementation
            let (client_id, secret) = translate_api_key(api_key)?;
            oauth2::authenticate(&client_id, &secret)
        }
    }
}
```

### Support Levels During Deprecation

| Phase | Bug Fixes | Security Fixes | Performance | New Features |
|-------|-----------|----------------|-------------|--------------|
| Active | ✅ | ✅ | ✅ | ✅ |
| Deprecated | ✅ | ✅ | ❌ | ❌ |
| Error | ⚠️ Critical only | ✅ | ❌ | ❌ |
| Removed | ❌ | ❌ | ❌ | ❌ |

---

## Exception Process

### Emergency Deprecation

For security vulnerabilities or critical bugs:

```
Emergency Deprecation Process:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  1. Security team approval
  2. Minimum 30-day notice (can be shorter for critical CVEs)
  3. Immediate availability of alternative
  4. Extended support for migration
  5. Post-incident review
```

### Deprecation Extension

Users can request deprecation extensions:

| Criteria | Extension Available | Maximum Extension |
|----------|---------------------|-------------------|
| Enterprise customer | ✅ | 12 months |
| Government customer | ✅ | 18 months |
| Open-source project | ✅ | 6 months |
| Individual user | ⚠️ Case-by-case | 3 months |

**Extension Request Process**:
1. File issue with `deprecation-extension` label
2. Provide justification and migration timeline
3. Review by deprecation committee (3-5 business days)
4. Decision communicated via issue

### Deprecation Reversal

In rare cases, deprecations may be reversed:

```
Reversal Criteria:
━━━━━━━━━━━━━━━━━━

  ✓ Community feedback overwhelming against deprecation
  ✓ Alternative proves inadequate
  ✓ Business case for continued support
  ✓ Technical feasibility of maintenance
  ✓ Deprecation committee approval

Process:
  1. Proposal with justification
  2. 2-week RFC period
  3. Committee vote
  4. Announcement if approved
```

---

## Current Deprecations

### Active Deprecations

| Feature | Since | Removal | Impact | Alternative |
|---------|-------|---------|--------|-------------|
| Legacy Auth API | v1.2.0 | v3.0.0 | High | OAuth 2.0 |
| XML Config Format | v1.1.0 | v2.1.0 | Low | YAML/JSON |
| gRPC v1 Streaming | v2.0.0 | v3.0.0 | Medium | gRPC v2 Streaming |
| Manual Federation | v2.0.0 | v3.0.0 | Medium | Auto-discovery |

### Upcoming Deprecations

| Feature | Planned | Removal | Impact | Status |
|---------|---------|---------|--------|--------|
| Raft Consensus | v2.1.0 | v4.0.0 | Critical | RFC in progress |
| REST API v1 | v2.1.0 | v3.0.0 | High | Planning |
| SQLite Backend | v2.0.0 | v2.2.0 | Low | Announced |

### Completed Deprecations

| Feature | Deprecated | Removed | Migration Success |
|---------|------------|---------|-------------------|
| HTTP API (non-TLS) | v1.0.0 | v1.1.0 | 100% |
| Config v1 Schema | v1.0.0 | v1.2.0 | 99.8% |
| Legacy CLI Syntax | v1.1.0 | v2.0.0 | 99.5% |

---

## Deprecation Committee

### Membership

| Role | Responsibility |
|------|----------------|
| API Lead | API deprecation decisions |
| Platform Lead | Protocol deprecation decisions |
| Community Lead | Community impact assessment |
| Security Lead | Security-related deprecations |
| Product Lead | Business impact assessment |

### Meeting Schedule

- Regular: Monthly review of deprecation status
- Ad-hoc: Emergency deprecations as needed
- Quarterly: Deprecation roadmap planning

---

## Related Documents

- [Version Roadmap](roadmap.md)
- [Compatibility Matrix](compatibility_matrix.md)
- [Migration Guides](/docs/migrations/)

---

*Last updated: February 2026*
