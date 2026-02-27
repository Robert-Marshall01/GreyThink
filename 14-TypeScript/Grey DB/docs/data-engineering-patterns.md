# Grey DB — Data Engineering Patterns

## 1. Data Modeling Depth

### SCD Type 2 (Slowly Changing Dimensions)

Tracks historical changes to dimension attributes using temporal validity ranges.

```ts
import { generateSCDHistory } from "@grey-db/core";

const scd = generateSCDHistory({
  tableName: "customers",
  trackedColumns: ["name", "email", "tier"],
});

// Apply the history table and trigger
await pool.query(scd.historyTableSQL);
await pool.query(scd.triggerSQL);

// Query current state
await pool.query(scd.queryCurrentSQL, [customerId]);

// Query state at a specific point in time
await pool.query(scd.queryAsOfSQL, [customerId, "2024-06-01"]);
```

**Design:** `valid_from` / `valid_to` range with `is_current` boolean. Partial unique index ensures exactly one current row per entity. AFTER UPDATE trigger captures old version automatically.

### Soft Delete Pattern

```ts
import { softDeleteColumns, softDeleteHelpers } from "@grey-db/core";

const users = greyTable("users", {
  id: uuid().primaryKey().defaultUUID(),
  email: text().unique(),
  ...softDeleteColumns(),  // adds deletedAt, deletedBy
});

const helpers = softDeleteHelpers("users");
// helpers.activeIndex     — partial index for active rows
// helpers.activeUnique    — unique constraint on active rows only
// helpers.softDeleteSQL   — parameterized soft-delete
// helpers.restoreSQL      — restore a soft-deleted row
// helpers.purgeSQL(90)    — hard-delete rows deleted > 90 days ago
```

**Philosophy:** Soft delete keeps audit trail intact, enables undo, avoids FK cascade surprises. Partial indexes keep active queries fast. Periodic purge cleans up old soft-deleted rows.

### Event Tables (Append-Only)

```ts
import { generateEventTable, projectState } from "@grey-db/core";

const { table, createSQL, appendSQL } = generateEventTable({
  tableName: "domain_events",
  category: "domain",
  tenantScoped: true,
});

// Events are immutable — UPDATE and DELETE are blocked by PostgreSQL RULEs.
// State is derived by replaying events through a reducer.

const accountState = await projectState(pool, "domain_events", {
  entityType: "account",
  initialState: { balance: 0 },
  reducer: (state, event) => {
    if (event.eventType === "deposit") return { balance: state.balance + event.payload.amount };
    if (event.eventType === "withdraw") return { balance: state.balance - event.payload.amount };
    return state;
  },
}, accountId);
```

### Immutable Audit Trail

```ts
const db = GreyDB.create(config);
await db.init();

// Record an audit entry
await db.audit.record({
  action: "update",
  entityType: "customer",
  entityId: customerId,
  actorId: userId,
  ...AuditTrail.computeDiff(oldValues, newValues),
});

// Query history
const history = await db.audit.getEntityHistory("customer", customerId);
const tenantActivity = await db.audit.getTenantActivity(tenantId, since);
```

### Reference Tables & Domain Enumerations

```ts
await db.references.seedDomain({
  domain: "order_status",
  values: [
    { code: "pending", label: "Pending", sortOrder: 1 },
    { code: "shipped", label: "Shipped", sortOrder: 2 },
    { code: "delivered", label: "Delivered", sortOrder: 3 },
  ],
});

const statuses = await db.references.getDomain("order_status");
const isValid = await db.references.validate("order_status", "shipped");
```

**Why reference tables over PG enums?** Enums can't be renamed, removed, or carry metadata. Reference tables support full CRUD, audit trail, and internationalization without migrations.

---

## 2. Data Quality & Validation

### Constraint Enforcement Philosophy

Grey DB uses three layers:

| Layer | What | How | When |
|-------|------|-----|------|
| **Database constraints** | Invariants (NOT NULL, CHECK, FK, UNIQUE) | PostgreSQL | Every write |
| **Application validation** | Business rules | ValidationEngine | Pre-insert / pre-update |
| **Cross-table checks** | Referential consistency, aggregates | Batch SQL | Periodic job |

### Column-Level Validation Rules

```ts
import { ValidationEngine, rules } from "@grey-db/core";

const engine = new ValidationEngine();
engine.register("users", [
  rules.email("email"),
  rules.notEmpty("name"),
  rules.range("age", 0, 150),
  rules.oneOf("status", ["active", "inactive"]),
  rules.pattern("phone", "^\\d{10}$", "US phone"),
  rules.length("bio", 0, 500),
]);

const result = engine.validateRow("users", {
  email: "invalid",
  name: "",
  age: -1,
});
// result.valid === false, result.errors has 3 violations
```

### Cross-Table Validation

```ts
import { crossTableChecks, runCrossTableChecks } from "@grey-db/core";

const results = await runCrossTableChecks(pool, [
  crossTableChecks.orphanedRecords("orders", "customerId", "customers"),
  crossTableChecks.aggregateMismatch("orders", "total", "order_lines", "amount", "orderId"),
  crossTableChecks.duplicates("users", "email"),
]);
// results.summary: { total: 3, passed: 2, failed: 1 }
```

### Data Profiling

```ts
import { profileTable, suggestRules } from "@grey-db/core";

const profile = await profileTable(pool, "users");
// profile.columns[0] → { nullPercent: 0.05, distinctCount: 9900, ... }

const suggested = suggestRules(profile);
// Automatically suggests rules based on observed patterns
```

---

## 3. Data Lifecycle & Retention

### Retention Policies

```ts
await db.retention.registerPolicy({
  tableName: "events",
  ageColumn: "occurred_at",
  archiveAfterDays: 90,    // Move to events_archive after 90 days
  purgeAfterDays: 365,     // Hard-delete from archive after 1 year
  enabled: true,
});

// Run all policies
const results = await db.retention.runAll();
// results: [{ tableName, archivedCount, purgedCount, durationMs }]
```

### Partitioning Strategies

```ts
import { generatePartitionDDL } from "@grey-db/core";

const { parentDDL, childDDL, detachSQL } = generatePartitionDDL({
  tableName: "events",
  partitionType: "range",
  partitionKey: "created_at",
  rangeInterval: "monthly",
});

// parentDDL: ALTER → CREATE ... PARTITION BY RANGE
// childDDL: 12 monthly partition CREATE TABLE ... FOR VALUES FROM ... TO ...
// detachSQL("events_202401"): detach old partitions for archival
```

### Compaction & Health Checks

```ts
import { checkTableHealth, generateCompactionSQL } from "@grey-db/core";

const health = await checkTableHealth(pool, "orders");
// health: { deadTuplePercent, tableSize, recommendations }

const sqls = generateCompactionSQL({
  tableName: "orders",
  operations: [
    { type: "vacuum", analyze: true },
    { type: "reindex", concurrently: true },
  ],
});
```

---

## 4. Data Ingestion & Transformation

### CSV / JSON Ingestion with Schema Validation

```ts
import { IngestionPipeline, transforms } from "@grey-db/core";

const pipeline = new IngestionPipeline(pool, {
  table: usersTable,
  format: "csv",
  batchSize: 1000,
  validator: validationEngine,
  transforms: [
    transforms.trimStrings(),
    transforms.lowercase("email"),
    transforms.defaults({ role: "user" }),
    transforms.computed("fullName", (r) => `${r.first} ${r.last}`),
    transforms.dropColumns("password_hash"),
  ],
  enableDLQ: true,
  onConflict: "skip",
  conflictColumns: ["email"],
});

await pipeline.init();
const result = await pipeline.ingest(csvText, "users-import.csv");
// result: { insertedCount, rejectedCount, deadLetterCount, batchesProcessed }
```

### Pipeline Stages

```
Raw text → Parse → Transform hooks → Schema validate → Business validate → Batch INSERT → DLQ
```

Each stage can fail independently. Rejected records flow to the dead-letter queue for investigation and retry.

### Dead-Letter Queue

```ts
// Failed records are automatically captured with context
const unresolved = await db.dlq.getUnresolved("users");
const summary = await db.dlq.getSummary();
// summary: [{ targetTable: "users", unresolved: 5, total: 12 }]

// After fixing the issue, resolve entries
await db.dlq.resolve(entryId);
```

### Batch vs. Streaming Philosophy

Grey DB is SQL-first and defaults to **batch** processing:
- Parse entire file → validate → transform → bulk INSERT
- Atomic: entire batch succeeds or fails (with DLQ fallback)
- Efficient: multi-row INSERT (10-100x faster than row-by-row)
- Predictable: resource usage bounded by configurable batch size

The ingestion API accepts streaming-compatible interfaces (row-at-a-time transforms), but the default orchestration is batch-oriented.
