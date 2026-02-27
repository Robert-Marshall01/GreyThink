# Grey DB — Security Model

> RLS, tenant isolation, RBAC, and protection against dangerous operations.

## Threat Model

Grey DB protects against four categories of threat:

| Threat | Example | Mitigation |
|--------|---------|-----------|
| **Cross-tenant data access** | Tenant A reads Tenant B's orders | RLS policies enforce `tenant_id = current_setting('grey.tenant_id')` |
| **Privilege escalation** | Readonly user runs DROP TABLE | RBAC with scoped API keys; NL→SQL blocks DDL |
| **Destructive migrations** | Developer accidentally drops a column in production | Safety checks, `requiresConfirmation`, advisory locks |
| **SQL injection via AI** | NL→SQL generates `'; DROP TABLE users; --` | Pattern matching for multi-statement injection, system catalog access, COPY, GRANT |

## Row-Level Security (RLS)

### How It Works

```sql
-- Enable RLS on a table
ALTER TABLE "public"."orders" ENABLE ROW LEVEL SECURITY;

-- Create a policy that filters by tenant_id
CREATE POLICY tenant_isolation ON "public"."orders"
  FOR ALL
  USING (tenant_id = current_setting('grey.tenant_id')::uuid)
  WITH CHECK (tenant_id = current_setting('grey.tenant_id')::uuid);
```

`USING` filters reads. `WITH CHECK` filters writes. Together, they ensure a tenant can only see and modify their own data.

### Session Variable Scoping

```typescript
await client.query("BEGIN");
await client.query("SET LOCAL grey.tenant_id = '<uuid>'");
// All queries here are tenant-scoped
await client.query("COMMIT");
// Setting is gone — cannot leak to next request
```

`SET LOCAL` is transaction-scoped. When the transaction ends, the setting is automatically discarded. This prevents tenant ID leakage on pooled connections.

**Why not `SET` (without LOCAL)?** Because `SET` persists for the entire session. If a connection returns to the pool and is reused by another request, the previous tenant's ID would still be set — a catastrophic security vulnerability.

### Bypass for Superuser Operations

The table owner (typically the connection user) bypasses RLS by default. Grey DB's application queries run through RLS; administrative operations (migrations, backups) use the pool directly.

To force RLS even for the table owner:

```sql
ALTER TABLE orders FORCE ROW LEVEL SECURITY;
```

We don't enable this by default because migrations need to operate on all rows.

## RBAC (Role-Based Access Control)

### Role Hierarchy

```
admin ────────→ all permissions
developer ────→ schema:read, schema:write, data:read, data:write, migration:run
readonly ─────→ schema:read, data:read
```

### API Key Security

1. **Keys are never stored** — We store only the SHA-256 hash. The raw key (`grey_<64 hex chars>`) is shown once at creation.
2. **Prefix for identification** — The first 12 characters are stored separately for display (`grey_a1b2c3d4...`).
3. **Expiration** — Optional expiry date. Expired keys are rejected at validation time.
4. **Tenant scoping** — An API key can be scoped to a specific tenant, limiting access to that tenant's data only.
5. **Last-used tracking** — Every validation updates `last_used_at`, enabling stale key detection.

### Permission Checks

```typescript
// Middleware: validate API key and check permission
const { user, apiKey } = await db.rbac.validateAPIKey(rawKey);
if (!db.rbac.hasPermission(apiKey, "data:write")) {
  return res.status(403).json({ error: "Insufficient permissions" });
}
```

`admin:full` is a superuser permission — it bypasses all other permission checks.

## Dangerous Migration Protection

### What Counts as Dangerous

| Operation | Severity | Reason |
|-----------|----------|--------|
| DROP TABLE | danger | Irreversible data loss |
| DROP COLUMN | danger | Irreversible data loss |
| Column type change | danger | May fail, may lose precision |
| TRUNCATE | danger | Deletes all rows without logging |
| DISABLE RLS | warning | Removes tenant isolation |
| DROP INDEX | info | Recoverable, but may degrade performance |

### How Destructive Operations Are Blocked

```typescript
const diff = diffSchemas(currentSchema, desiredSchema);

if (diff.requiresConfirmation) {
  console.log("⚠ Destructive changes detected:");
  for (const action of diff.destructiveActions) {
    console.log(`  - ${action}`);
  }
  // CLI: prompt for confirmation
  // API: return 409 Conflict with destructive actions list
  // UI: show confirmation dialog
}
```

The `requiresConfirmation` flag is computed from the presence of `danger`-severity warnings. The `destructiveActions` array contains human-readable descriptions.

### Dry-Run Mode

```typescript
await applyMigration(pool, migration, { dryRun: true });
// Runs the SQL inside a transaction, then ROLLBACK
// Validates syntax and constraint satisfaction without side effects
```

### Rollback Strategy

Rollbacks apply the `down_sql` within a transaction with an advisory lock:

```
BEGIN
  pg_advisory_xact_lock(2147483647)  -- prevent concurrent rollbacks
  SELECT latest migration
  Execute down_sql
  Mark migration as rolled_back = true
COMMIT
```

Limitations:
- `DROP TABLE` down_sql is `-- Manual restoration required` because we can't regenerate data
- `DROP COLUMN` same — data is gone
- Type changes can be reversed if the types are compatible

This is by design. Irreversible operations produce irreversible down_sql. We don't pretend otherwise.

## Backup Integration with Schema Changes

### Pre-Migration Backup

We recommend (but don't enforce) creating a backup before destructive migrations:

```typescript
// In your deployment pipeline:
await db.backups.createFullBackup();
const diff = diffSchemas(current, desired);
if (diff.requiresConfirmation) {
  // Backup exists — proceed with confirmation
}
await applyMigration(db.pool, migration);
```

### Per-Tenant Backup Before Schema Change

For tenant-scoped operations:
```typescript
await db.backups.createTenantBackup(tenantId);
// Then run tenant-specific schema changes
```

### Restore Flow

```typescript
const result = await db.backups.restore(backupPath, true); // dry-run first
console.log("Would restore:", result.tables, result.rowsRestored);

await db.backups.restore(backupPath); // actual restore
```

Restores use `ON CONFLICT DO NOTHING` to avoid duplicating data that already exists with the same primary key.

## NL→SQL Safety

### Blocked Patterns

| Pattern | What It Catches |
|---------|----------------|
| `DROP TABLE/DATABASE/SCHEMA/INDEX` | Direct destructive DDL |
| `TRUNCATE` | Mass data deletion |
| `; DROP/DELETE/ALTER/TRUNCATE` | Multi-statement injection |
| `pg_shadow` / `pg_authid` | System auth catalog access |
| `COPY TO/FROM` | File system access |
| `GRANT` / `REVOKE` | Privilege modification |
| `SET ROLE` | Role switching |
| `DELETE/UPDATE` without `WHERE` | Unscoped write operations |

### Defense in Depth

1. **NL→SQL generates a prompt, not SQL directly** — The actual SQL generation happens in an LLM. Grey DB validates the output.
2. **Safety check before execution** — Every generated SQL passes through `checkSafety()` before reaching the database.
3. **API key permissions** — Even if SQL passes safety checks, the user must have `data:write` permission for write operations.
4. **RLS enforcement** — Even if a write operation executes, RLS ensures it only affects the current tenant's data.

Four independent layers. An attacker must defeat all four.

## Audit Trail

Every significant operation is logged to `grey_audit_log`:

```sql
CREATE TABLE grey_audit_log (
  id          SERIAL PRIMARY KEY,
  user_id     UUID,
  action      TEXT NOT NULL,       -- 'migration.apply', 'data.delete', etc.
  resource    TEXT,                 -- 'table:orders', 'tenant:abc-123'
  details     JSONB DEFAULT '{}',  -- operation-specific context
  ip_address  TEXT,
  tenant_id   UUID,
  created_at  TIMESTAMPTZ DEFAULT NOW()
);
```

Indexed by `user_id`, `tenant_id`, and `created_at DESC` for efficient querying.

The audit log is append-only — there is no DELETE endpoint. Administrative cleanup should happen through database-level retention policies.
