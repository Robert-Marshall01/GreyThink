# Grey DB — Architecture

> A Postgres-backed data platform for multi-tenant SaaS applications.

## Why Postgres?

We chose PostgreSQL over other databases for compounding technical reasons:

1. **RLS (Row-Level Security)** — Postgres is one of the few databases that provides native, kernel-enforced row-level security policies. For multi-tenant SaaS, this is not optional — it's the difference between "we filter by tenant_id in application code" (a single bug away from leaking data) and "the database itself enforces tenant boundaries even if application code has a bug." We chose the latter.

2. **Transactional DDL** — Postgres wraps DDL statements inside transactions. This means `ALTER TABLE` can be rolled back if something fails mid-migration. MySQL, by contrast, auto-commits DDL — a failed migration leaves you in a half-applied state. For a migration engine, transactional DDL is foundational.

3. **`pg_advisory_xact_lock`** — We use advisory locks to serialize migration execution across multiple application instances. Without this, two pods deploying simultaneously could both try to apply the same migration. Advisory locks are lightweight, conflict-free with row locks, and scoped to the transaction.

4. **`pgvector`** — Native vector similarity search enables our semantic search layer without requiring a separate vector database (Pinecone, Weaviate, etc.). One fewer service to operate. The `ivfflat` and `hnsw` index types give us production-grade ANN search.

5. **JSONB** — Feature flags, tenant config, plugin metadata — all stored as JSONB with GIN-indexed query support. This gives us document-store flexibility inside a relational engine.

6. **Ecosystem maturity** — 25+ years of production hardening, `pg_stat_statements`, `EXPLAIN ANALYZE`, `pg_dump`, logical replication. We stand on proven infrastructure.

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    @grey-db/ui  (React + Vite)              │
│       Admin dashboard, query console, schema browser        │
└────────────────────────┬────────────────────────────────────┘
                         │  REST API
┌────────────────────────▼────────────────────────────────────┐
│                 @grey-db/server  (Express)                   │
│       Routes: schema, tenants, query, ai, ops               │
│       Middleware: auth (API key → RBAC), rate limiting       │
└────────────────────────┬────────────────────────────────────┘
                         │
┌────────────────────────▼────────────────────────────────────┐
│                   @grey-db/core                              │
│  ┌───────────┐ ┌──────────┐ ┌───────────┐ ┌─────────────┐  │
│  │  Schema   │ │  Tenant  │ │   Query   │ │     AI      │  │
│  │  Engine   │ │  Engine  │ │   Engine  │ │    Layer    │  │
│  │           │ │          │ │           │ │             │  │
│  │ • DSL     │ │ • RLS    │ │ • Builder │ │ • NL→SQL    │  │
│  │ • Differ  │ │ • Tenant │ │ • Explain │ │ • Auto-Doc  │  │
│  │ • Migrate │ │   Context│ │ • Indexes │ │ • Semantic  │  │
│  │ • Registry│ │ • Schema │ │ • Slow Qry│ │   Search    │  │
│  │           │ │   per    │ │           │ │             │  │
│  │           │ │   Tenant │ │           │ │             │  │
│  └───────────┘ └──────────┘ └───────────┘ └─────────────┘  │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │               Ops Layer                                 │ │
│  │  Backup · RBAC · Observability · Audit Log              │ │
│  └─────────────────────────────────────────────────────────┘ │
└────────────────────────┬────────────────────────────────────┘
                         │
                   ┌─────▼─────┐
                   │ PostgreSQL │
                   │  + pgvector│
                   └───────────┘
```

## Why a DSL Instead of Raw SQL?

The `greyTable()` DSL serves three purposes that raw SQL cannot:

1. **Diffable schema** — We compare two TypeScript schema snapshots (`Table[]`) and derive the exact set of DDL changes. Raw SQL files are strings — you can't structurally diff two SQL files to know what changed.

2. **Code-first metadata** — `.withTenantScope()` marks a table for automatic RLS policy generation. `.withRLS()` enables row-level security. These are declarative intents, not SQL boilerplate.

3. **Static analysis** — TypeScript catches typos in column names, type mismatches, and invalid constraints at compile time. SQL catches them at deploy time, after you've already shipped.

The DSL generates standard PostgreSQL DDL — there is no proprietary runtime. You can always `console.log(generateCreateTable(myTable))` to see the exact SQL.

## Multi-Tenant Strategy

We support three isolation strategies, chosen per-tenant:

| Strategy | Isolation | Performance | Complexity |
|----------|-----------|-------------|------------|
| **Shared** | RLS policies on `tenant_id` column | Best (shared indexes, shared cache) | Lowest |
| **Schema** | Separate Postgres schema per tenant | Medium (separate tables, shared instance) | Medium |
| **Hybrid** | Shared by default, schema for large/regulated tenants | Best of both | Highest |

### Why Shared-First?

Most SaaS applications have a power-law distribution: 90% of tenants are small, 10% generate most of the data. Giving every tenant their own schema creates operational overhead (thousands of migration targets) for negligible isolation benefit.

We default to shared tables with RLS because:
- Postgres enforces isolation at the kernel level (not application code)
- Indexes are shared, keeping query plans warm
- Connection pool is shared, avoiding per-tenant pool exhaustion
- Migrations run once, not once-per-tenant

Large tenants that need dedicated resources move to schema isolation. This is the same strategy used by Salesforce, Notion, and Linear.

### How `withTenant()` Works

```typescript
await db.tenants.withTenant(tenantId, async (client) => {
  // Inside this closure:
  // 1. A transaction is started (BEGIN)
  // 2. SET LOCAL grey.tenant_id = '<uuid>' scopes all RLS policies
  // 3. All queries through `client` are tenant-isolated
  // 4. Transaction commits on success, rolls back on error
  const rows = await client.query("SELECT * FROM orders");
  // ↑ this ONLY returns orders for the specified tenant
});
```

`SET LOCAL` is critical — it scopes the setting to the current transaction only, so it doesn't leak to the next query on a pooled connection.

## Package Boundaries

| Package | Responsibility | Dependencies |
|---------|---------------|--------------|
| `@grey-db/core` | All database logic, no HTTP | `pg`, `uuid` |
| `@grey-db/server` | REST API over core | `@grey-db/core`, `express` |
| `@grey-db/ui` | Admin dashboard | `react`, `vite` (no core dep) |

Core has **zero HTTP dependencies**. This means it can be embedded directly into any Node.js application (CLI, serverless function, test harness) without dragging in Express.

The server is a thin adapter: it exposes core's classes as REST endpoints with API key authentication.

The UI communicates only through the REST API — it never imports `@grey-db/core`. This ensures the same API surface that external consumers use.

## Connection Pool Architecture

```
App Instance 1 ──┐
App Instance 2 ──┼── Pool (max 20) ── PostgreSQL
App Instance 3 ──┘
```

We use `pg.Pool` with:
- **max 20** connections (configurable) — Postgres handles ~100 active connections well; 20 per instance × 5 instances = 100.
- **idle timeout 30s** — connections sitting idle for 30s are released.
- **connection timeout 5s** — if the pool is exhausted, new requests fail fast rather than queueing indefinitely.

Every query goes through `GreyDB.query()`, which automatically records execution time, row count, and errors into the observability layer. This is non-blocking — metric recording failures are silently caught so they never break the primary query path.
