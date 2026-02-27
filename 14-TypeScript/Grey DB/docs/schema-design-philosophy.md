# Grey DB — Schema Design Philosophy

> Normalization boundaries, denormalization trade-offs, relationships, soft deletes, and audit trails.

## Normalization: Where to Draw the Line

### Third Normal Form (3NF) as Default

Grey DB schemas should start at 3NF:

- Every column depends on the primary key (1NF)
- Every column depends on the *whole* primary key (2NF)
- Every column depends on *nothing but* the primary key (3NF)

This is where most application tables should live. 3NF eliminates update anomalies — when you change a customer's name, you change it in one place.

### When to Break 3NF

| Signal | Example | Denormalization |
|--------|---------|----------------|
| Read-heavy, write-light | Dashboard aggregations | Materialized view or summary table |
| Query joins >3 tables | Order → Items → Product → Category → Vendor | Store `product_name` directly on `order_items` |
| Time-series data | Event logs, metrics | Pre-aggregated hourly/daily rollups |
| Full-text search | User profiles | Denormalized `search_vector` column (tsvector) |

### The Test

Before denormalizing, ask:

1. **How stale can this data be?** If "never" — don't denormalize. If "a few seconds" — use materialized views. If "a few minutes" — use background sync.
2. **Who owns the source of truth?** Denormalized data must always have a canonical source. Never let the denormalized copy become the primary.
3. **What's the write amplification?** If updating a single field requires updating it in 5 places, the denormalization cost exceeds the read benefit.

## Relationship Modeling

### Foreign Keys Always

```typescript
const Order = greyTable("orders", {
  id: uuid().primaryKey().defaultUUID(),
  userId: uuid().notNull().references("users", "id", { onDelete: "CASCADE" }),
  // ...
});
```

Foreign keys serve three purposes:
1. **Referential integrity** — The database rejects orphaned records
2. **Documentation** — The schema self-documents its relationships
3. **Query planner hints** — Postgres uses FK metadata for join optimization

### `ON DELETE` Strategy

| Strategy | When to Use |
|----------|-------------|
| `CASCADE` | Child records have no independent meaning (order items when order is deleted) |
| `SET NULL` | Child records should survive parent deletion (user's posts when user is deleted — show as "deleted user") |
| `RESTRICT` | Deletion should be blocked if children exist (can't delete a department with employees) |
| (none / `NO ACTION`) | Same as RESTRICT but deferred to end of transaction |

### Junction Tables for Many-to-Many

```typescript
const ProjectMembers = greyTable("project_members", {
  projectId: uuid().references("projects", "id", { onDelete: "CASCADE" }),
  userId: uuid().references("users", "id", { onDelete: "CASCADE" }),
  role: text().default("member"),
  joinedAt: timestamptz().defaultNow(),
}).index("pk_project_members", ["projectId", "userId"], { unique: true });
```

Always add a composite unique index on the two FK columns. Always include metadata (`role`, `joined_at`) — the relationship itself often has properties.

## Tenant-Scoped Tables

Every tenant-scoped table follows the same pattern:

```typescript
const Tasks = greyTable("tasks", {
  id: uuid().primaryKey().defaultUUID(),
  tenantId: uuid().notNull().indexed(),  // Always indexed for RLS scans
  title: text().notNull(),
  // ...
}).withTenantScope();
```

Rules:
1. `tenantId` is **never nullable** — every row belongs to exactly one tenant
2. `tenantId` is **always indexed** — RLS policies filter on it for every query
3. `.withTenantScope()` auto-enables RLS and generates the isolation policy
4. Unique constraints should include `tenantId` — e.g., email uniqueness is per-tenant

```typescript
// ✅ Correct: unique per tenant
.index("uq_users_email_tenant", ["tenantId", "email"], { unique: true })

// ❌ Wrong: globally unique email prevents two tenants from having the same email
.unique()  // on email column alone
```

## Soft Deletes

### When to Soft-Delete

Use soft deletes when:
- Legal/compliance requires data retention (GDPR right-to-erasure is separate — soft delete is not GDPR compliant)
- Users expect an "undo" or "trash" feature
- Audit trails need to reference deleted records
- Foreign keys would break with hard deletes

### Implementation Pattern

```typescript
const Projects = greyTable("projects", {
  id: uuid().primaryKey().defaultUUID(),
  tenantId: uuid().notNull().indexed(),
  name: text().notNull(),
  deletedAt: timestamptz().nullable(),      // NULL = active, timestamp = deleted
  deletedBy: uuid().nullable().references("users", "id"),
  // ...
}).index("idx_projects_active", ["tenantId"], {
  where: "deleted_at IS NULL",             // Partial index: only active records
});
```

Key decisions:
1. **`deletedAt` over `isDeleted`** — A timestamp tells you *when* it was deleted and lets you implement retention policies ("purge soft-deleted records older than 90 days").
2. **`deletedBy`** — Audit trail for who deleted it.
3. **Partial index** — Most queries want active records. A partial index on `WHERE deleted_at IS NULL` keeps the index small and fast.
4. **Application-level filtering** — Grey DB doesn't auto-filter soft-deleted records. Your application's query layer should add `WHERE deleted_at IS NULL` to read queries.

### When to Hard-Delete

- Ephemeral data (sessions, tokens, temporary uploads)
- GDPR erasure requests (after soft-delete period)
- Log rotation (delete rows older than retention period)

## Audit Trails

### Application-Level Audit

For business-significant events (who changed what, when):

```typescript
const AuditTrail = greyTable("audit_trail", {
  id: bigserial().primaryKey(),
  tenantId: uuid().notNull().indexed(),
  userId: uuid().references("users", "id"),
  action: text().notNull(),               // 'project.create', 'task.update'
  entityType: text().notNull(),           // 'project', 'task'
  entityId: uuid().notNull(),
  changes: jsonb(),                       // { field: { from: "old", to: "new" } }
  metadata: jsonb(),                      // IP, user agent, etc.
  createdAt: timestamptz().defaultNow(),
})
  .index("idx_audit_entity", ["tenantId", "entityType", "entityId"])
  .index("idx_audit_time", ["tenantId", "createdAt"])
  .withTenantScope();
```

### System-Level Audit

Grey DB's built-in `grey_audit_log` tracks system operations (migrations, user management, API key creation). Application-level audit is separate because:

1. Different retention requirements (system audit: 1 year; business audit: 7 years)
2. Different query patterns (system: by user + time; business: by entity + time)
3. Different access controls (system: admin only; business: visible to tenant admins)

### What to Audit

| Always | Sometimes | Never |
|--------|-----------|-------|
| Create/Update/Delete of business entities | Read access to sensitive data | Health check pings |
| Permission changes | Login/logout events | Cache hits |
| Migration execution | Configuration changes | Background job heartbeats |
| Backup/restore operations | API key usage | Metrics collection |

## Timestamp Conventions

Every table should include:

```typescript
createdAt: timestamptz().defaultNow(),
updatedAt: timestamptz().defaultNow(),
```

Rules:
1. **Always `timestamptz`** — Never `timestamp` (without timezone). `timestamp` stores the literal value without timezone context. If your server moves from US-East to EU-West, all your timestamps are wrong.
2. **Default `NOW()`** — at the database level, not application level. Eliminates clock skew between application servers.
3. **`updatedAt` via trigger** — Postgres doesn't auto-update this. Use a trigger:

```sql
CREATE OR REPLACE FUNCTION set_updated_at()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_updated_at
  BEFORE UPDATE ON your_table
  FOR EACH ROW EXECUTE FUNCTION set_updated_at();
```

## ID Strategy

Grey DB defaults to UUIDv4 (`gen_random_uuid()`) for primary keys.

| Strategy | Pros | Cons |
|----------|------|------|
| Serial/BIGSERIAL | Compact, ordered, fast indexes | Exposes row count, conflicts in distributed systems |
| UUIDv4 | Globally unique, no coordination | 16 bytes (larger indexes), random distribution (page splits) |
| UUIDv7 | Globally unique + time-ordered | Newer standard, less library support |
| ULID | Time-ordered, string-sortable | Non-standard, 26 chars |

We chose UUIDv4 because:
- Multi-tenant systems need IDs that don't collide across tenants
- `gen_random_uuid()` is built into Postgres — no extension required
- The random distribution cost is manageable with proper index maintenance (regular REINDEX for heavily-written tables)

For tables with millions of writes per day, consider UUIDv7 (time-ordered) to reduce B-tree page splits.
