# Grey DB

A Postgres-backed data platform that unifies **schema design**, **migrations**, **multi-tenancy**, **query ergonomics**, **indexing**, **observability**, **data quality**, and **AI-assisted querying** into a single coherent system.

May be unstable. Verify stability before deploying in a production environment.
---

## Getting Started

### Prerequisites

| Tool | Minimum version | Check with |
|------|----------------|------------|
| **Node.js** | 20+ | `node -v` |
| **npm** | 9+ | `npm -v` |
| **Docker Desktop** | Any recent | `docker --version` |
| **Git** | Any | `git --version` |

> **PostgreSQL is provided by Docker** — you do not need to install it separately.

---

### 1. Clone & Install

```bash
git clone <your-repo-url> "Grey DB"
cd "Grey DB"
npm install
```

This installs dependencies for all workspace packages (`core`, `server`, `ui`, `cli`).

---

### 2. Set Up Environment Variables

```bash
# macOS / Linux
cp .env.example .env

# Windows (PowerShell)
Copy-Item .env.example .env
```

The defaults work out of the box — no edits needed unless you changed ports:

```env
GREY_DB_HOST=localhost
GREY_DB_PORT=5432
GREY_DB_DATABASE=greydb
GREY_DB_USER=greydb
GREY_DB_PASSWORD=greydb_secret
PORT=4000
VITE_API_URL=http://localhost:4000
```

---

### 3. Start PostgreSQL

Open **Docker Desktop** first (the daemon must be running), then:

```bash
docker compose up -d postgres
```

Wait for the health check to pass (about 5 seconds):

```bash
docker compose ps
```

You should see `greydb-postgres` with status **healthy**.

> **Troubleshooting:** If `docker compose up` fails with a pipe/connection error,
> Docker Desktop is not running. Open it from your Start Menu / Applications folder
> and wait for the whale icon to stop animating before retrying.

---

### 4. Run Tests (no database needed)

The 196 unit tests run entirely in-memory — no Postgres connection required:

```bash
npm test
```

Expected output:

```
Test Suites: 9 passed, 9 total
Tests:       196 passed, 196 total
```

Integration tests that need a live database will gracefully skip with a console warning when Postgres is unavailable.

---

### 5. Start the Full Stack

**Terminal 1 — API server:**

```bash
npm run dev:server
```

Starts Express on http://localhost:4000. On first start it creates all Grey DB system tables automatically.

**Terminal 2 — Admin UI:**

```bash
npm run dev:ui
```

Starts the React dashboard on http://localhost:3000.

**Or run both at once:**

```bash
npm run dev
```

This uses `concurrently` to start both the server and UI in a single terminal.

| Service | URL |
|---------|-----|
| Admin UI | http://localhost:3000 |
| API Server | http://localhost:4000 |
| PostgreSQL | `localhost:5432` (user: `greydb`, password: `greydb_secret`) |

---

### 6. (Optional) Build Everything

If you want compiled output (e.g., for deployment or the CLI):

```bash
npm run build
```

This compiles `core` → `server` → `ui` → `cli` in workspace order.

---

### 7. (Optional) Run the Example CRM App

A complete reference application lives in `examples/crm/`:

```bash
# Create the CRM tables
npm run crm:migrate

# Seed sample data
npm run crm:seed

# Start the CRM API
npm run crm:dev
```

---

### Docker-only Mode (everything containerized)

If you prefer not to install Node.js locally:

```bash
docker compose up -d
```

This builds and starts **all three services** (Postgres + API + UI) in containers.

| Service | URL |
|---------|-----|
| Admin UI | http://localhost:3000 |
| API Server | http://localhost:4000 |
| PostgreSQL | `localhost:5432` |

To stop everything:

```bash
docker compose down
```

To also remove the database volume (fresh start):

```bash
docker compose down -v
```

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        Admin UI (React)                     │
│  Dashboard · Schema · Migrations · Tenants · Query · AI     │
└────────────────────────────┬────────────────────────────────┘
                             │ REST API
┌────────────────────────────▼────────────────────────────────┐
│                     API Server (Express)                     │
│  Auth Middleware · Routes · Error Handling                    │
└────────────────────────────┬────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────┐
│                      Core Library                            │
│  ┌────────┐ ┌────────┐ ┌───────┐ ┌────┐ ┌─────┐           │
│  │ Schema │ │ Tenant │ │ Query │ │ AI │ │ Ops │           │
│  └────────┘ └────────┘ └───────┘ └────┘ └─────┘           │
│  ┌───────────┐ ┌────────────┐ ┌───────────┐ ┌───────────┐ │
│  │ Modeling  │ │ Validation │ │ Lifecycle │ │ Ingestion │ │
│  └───────────┘ └────────────┘ └───────────┘ └───────────┘ │
└────────────────────────────┬────────────────────────────────┘
                             │
                    ┌────────▼────────┐
                    │   PostgreSQL    │
                    │  + pgvector     │
                    └─────────────────┘
```

## Subsystems

| Subsystem | Capabilities |
|-----------|-------------|
| **Schema Engine** | TypeScript DSL for table definitions, SQL generation, schema diffing, migration engine, versioned schema registry |
| **Tenant Engine** | Multi-tenant isolation (shared/schema/hybrid), RLS policy generation, per-tenant config, usage tracking |
| **Query Engine** | Typed query builder (SELECT/INSERT/UPDATE/DELETE), EXPLAIN analyzer, index manager, slow query logging |
| **AI Layer** | Natural language → SQL prompt generation (LLM-agnostic), auto-documentation, ER diagrams, semantic search via pgvector |
| **Ops Layer** | JSON backups (full & per-tenant), RBAC with API keys, audit logging, query metrics, dashboards, health checks |
| **Modeling** | SCD Type 2 versioning, soft/hard deletes, event tables, immutable audit trail, reference tables & domain enumerations |
| **Validation** | Column-level rules (email, range, pattern, etc.), cross-table consistency checks, data profiling, rule suggestion |
| **Lifecycle** | Retention policies with archive/purge, time-range/list/hash partitioning DDL, compaction jobs, table health checks |
| **Ingestion** | CSV/JSON/JSONL parser, schema validation, transform hooks (trim, rename, compute, filter), batch INSERT, dead-letter queue |

## Project Structure

```
Grey DB/
├── packages/
│   ├── core/                  # Core library (@grey-db/core)
│   │   ├── src/
│   │   │   ├── schema/        # DSL, SQL gen, differ, migrations, registry
│   │   │   ├── tenant/        # Multi-tenant engine
│   │   │   ├── query/         # Query builder, EXPLAIN, indexes, slow queries
│   │   │   ├── ai/            # NL→SQL, auto-docs, semantic search
│   │   │   ├── ops/           # Backups, RBAC, observability
│   │   │   ├── modeling/      # SCD, soft-delete, events, audit, reference tables
│   │   │   ├── validation/    # Rules engine, cross-table checks, profiling
│   │   │   ├── lifecycle/     # Retention, partitioning, compaction
│   │   │   ├── ingestion/     # CSV/JSON pipeline, DLQ, transforms
│   │   │   ├── connection.ts  # Pool management
│   │   │   └── index.ts       # GreyDB unified class
│   │   ├── tests/             # 196 tests (unit + integration)
│   │   └── benchmarks/        # Performance benchmarks
│   ├── server/                # Express API server (@grey-db/server)
│   ├── ui/                    # React admin dashboard (@grey-db/ui)
│   └── cli/                   # CLI tool (@grey-db/cli)
├── examples/
│   └── crm/                   # Reference CRM application
├── docs/                      # Architecture & pattern docs
├── docker-compose.yml
├── Dockerfile
└── init.sql                   # Enables uuid-ossp, pgvector, pg_stat_statements
```

## API Overview

All endpoints are under `/api`. Authentication via `x-api-key` header.

| Group | Endpoints |
|-------|-----------|
| **Schema** | `GET /api/schema/tables`, `GET /api/schema/tables/:name`, `POST /api/schema/ddl`, `GET/POST /api/schema/migrations/*` |
| **Tenants** | `GET/POST /api/tenants`, `GET/PUT/DELETE /api/tenants/:id`, `POST /api/tenants/:id/rls` |
| **Query** | `POST /api/query/execute`, `POST /api/query/explain`, `GET/POST/DELETE /api/query/indexes/*` |
| **AI** | `POST /api/ai/nl-to-sql`, `POST /api/ai/check-safety`, `GET /api/ai/docs/*`, `POST /api/ai/semantic-search/*` |
| **Ops** | `POST /api/ops/backup`, `GET /api/ops/backups`, `POST /api/ops/users`, `GET /api/ops/dashboard` |

## Code Examples

### Schema DSL

```typescript
import { greyTable, uuid, text, varchar, timestamptz } from "@grey-db/core";

const users = greyTable("users", {
  id:        uuid().primaryKey().defaultUUID(),
  email:     varchar(255).unique().notNull(),
  name:      text().notNull(),
  role:      varchar(50).default("user"),
  tenantId:  uuid().notNull(),
  createdAt: timestamptz().defaultNow(),
})
  .withTenantScope()
  .withRLS()
  .index("idx_users_email", ["email"], { unique: true })
  .index("idx_users_tenant", ["tenantId", "createdAt"]);
```

### Query Builder

```typescript
import { QueryBuilder } from "@grey-db/core";

const query = new QueryBuilder("orders")
  .select("customer_id", "SUM(total) as revenue")
  .join("customers", "customers.id = orders.customer_id")
  .where("orders.status = $1", "completed")
  .where("orders.created_at > $2", "2024-01-01")
  .groupBy("customer_id")
  .having("SUM(total) > $3", 1000)
  .orderBy("revenue", "DESC")
  .limit(20)
  .toSQL();
```

### Validation

```typescript
import { ValidationEngine, rules } from "@grey-db/core";

const engine = new ValidationEngine();
engine.register("users", [
  rules.email("email"),
  rules.notEmpty("name"),
  rules.range("age", 0, 150),
]);

const result = engine.validateRow("users", { email: "bad", name: "", age: -1 });
// result.valid === false, result.errors has 3 violations
```

### Data Ingestion

```typescript
import { IngestionPipeline, transforms } from "@grey-db/core";

const pipeline = new IngestionPipeline(pool, {
  table: usersTable,
  format: "csv",
  batchSize: 1000,
  transforms: [
    transforms.trimStrings(),
    transforms.lowercase("email"),
  ],
  enableDLQ: true,
});

const result = await pipeline.ingest(csvText);
// result: { insertedCount, rejectedCount, deadLetterCount }
```

## Further Reading

- [Architecture overview](docs/architecture.md)
- [Schema design philosophy](docs/schema-design-philosophy.md)
- [Data engineering patterns](docs/data-engineering-patterns.md)
- [Security model](docs/security-model.md)
- [Performance model](docs/performance-model.md)
- [Schema evolution](docs/schema-evolution.md)

## License

MIT
