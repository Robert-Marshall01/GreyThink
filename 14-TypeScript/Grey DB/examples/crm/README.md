# Grey DB — Reference CRM Application

A multi-tenant CRM demonstrating every Grey DB subsystem in a real SaaS context.

## What This Demonstrates

| Grey DB Feature | CRM Usage |
|---|---|
| **Schema DSL** | 6 tables with FKs, constraints, indexes, CHECK constraints |
| **Migrations** | `migrate.ts` — diff-based migration from DSL to Postgres |
| **Multi-tenancy** | RLS-based isolation via `X-Tenant-ID` header |
| **Query Builder** | All CRUD endpoints use `QueryBuilder`, `InsertBuilder`, etc. |
| **AI Layer** | `/ai/query` — NL→SQL with schema context, `/ai/check` — safety gate |
| **EXPLAIN Analysis** | `/explain` — query plan analysis with warnings |
| **Tenant Engine** | `withTenant()` for RLS enforcement on every scoped query |

## Quick Start

```bash
# 1. Set up Postgres
cp ../../.env.example .env
# Edit .env with your Postgres credentials

# 2. Run migrations
npx ts-node src/migrate.ts

# 3. Seed demo data
npx ts-node src/seed.ts

# 4. Start the server
npx ts-node src/server.ts
```

## API Usage

All tenant-scoped routes require `X-Tenant-ID` header.

```bash
# List contacts for a tenant
curl -H "X-Tenant-ID: <acme-id>" http://localhost:3001/contacts

# Create a contact
curl -X POST http://localhost:3001/contacts \
  -H "X-Tenant-ID: <acme-id>" \
  -H "Content-Type: application/json" \
  -d '{"firstName":"Test","lastName":"User","email":"test@example.com"}'

# Pipeline summary
curl -H "X-Tenant-ID: <acme-id>" http://localhost:3001/deals/pipeline

# NL → SQL
curl -X POST http://localhost:3001/ai/query \
  -H "X-Tenant-ID: <acme-id>" \
  -H "Content-Type: application/json" \
  -d '{"question":"Show me all deals worth more than $50k in negotiation stage"}'

# SQL safety check
curl -X POST http://localhost:3001/ai/check \
  -H "Content-Type: application/json" \
  -d '{"sql":"DROP TABLE contacts CASCADE"}'
```

## Schema

```
organizations ─┬─ crm_users (tenant-scoped)
               ├─ contacts (tenant-scoped) ── deals (tenant-scoped)
               │        └── activities (tenant-scoped)
               └─ crm_audit_log (tenant-scoped)
```

Every tenant-scoped table has Row Level Security enforced via `withTenant()`.
Tenant A cannot see Tenant B's data — even through raw SQL, RLS blocks it.

## Architecture Signals

- **No ORM** — Grey DB's query builder generates parameterized SQL directly
- **RLS by default** — tenant isolation is a property of the schema, not the application
- **AI without lock-in** — the `/ai/query` endpoint prepares prompts but doesn't call any LLM; you choose the provider
- **Migrations are checksummed** — every schema change gets a SHA-256 checksum and is tracked in `grey_migrations`
