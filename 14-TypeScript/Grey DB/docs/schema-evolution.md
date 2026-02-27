# Grey DB — Schema Evolution Philosophy

> How we think about migrations, backfills, and zero-downtime schema changes.

## Core Principle: Schemas Evolve, They Don't Deploy

A schema change is not a deploy — it's a transition. The database serves live traffic before, during, and after the migration. Every decision in Grey DB's migration engine reflects this.

## The Migration Lifecycle

```
1. Define desired schema (DSL)
2. Diff against current schema
3. Generate migration (SQL + metadata)
4. Safety check (destructive ops flagged)
5. Dry-run (optional, inside transaction)
6. Apply (advisory-locked, transactional)
7. Record (version, checksum, timestamp)
```

### Step-by-Step Guarantees

| Step | What Happens | What Can Go Wrong | How We Handle It |
|------|-------------|-------------------|-----------------|
| Diff | Structural comparison of `Table[]` | Schema drift vs. live DB | Registry stores versioned snapshots |
| Generate | SQL from diff actions | Generated SQL may be unsafe | `safe` flag + `warnings[]` array |
| Safety Check | Pattern match for DROP, type changes | False negatives | `requiresConfirmation` + `destructiveActions[]` |
| Dry Run | Run SQL inside transaction, then ROLLBACK | — | Validates SQL syntax without side effects |
| Apply | `pg_advisory_xact_lock` → SQL → record | Concurrent deploys | Advisory lock serializes execution |
| Record | INSERT into `grey_migrations` | Duplicate version | Unique constraint + pre-check SELECT |
| Rollback | Execute `down_sql` in reverse | Down SQL may not be exact inverse | Checksums detect drift |

## Online Migration Patterns

### Adding a Column (Safe)

```sql
ALTER TABLE "public"."users" ADD COLUMN "display_name" TEXT;
```

This is always safe in Postgres. `ADD COLUMN` with a nullable column and no default is a metadata-only operation — it does not rewrite the table.

**Grey DB behavior:** Flagged as `severity: "info"`, `safe: true`.

### Adding a Column with NOT NULL Default (Postgres 11+)

```sql
ALTER TABLE "public"."users" ADD COLUMN "status" TEXT NOT NULL DEFAULT 'active';
```

Since Postgres 11, this is also metadata-only. The default value is stored in `pg_attrdef` and applied at read time. No table rewrite.

**Grey DB behavior:** Flagged as `severity: "info"`, `safe: true`.

### Dropping a Column (Dangerous)

```sql
ALTER TABLE "public"."users" DROP COLUMN "legacy_field";
```

**Grey DB behavior:** Flagged as `severity: "danger"`, `requiresConfirmation: true`. The `destructiveActions` array contains the human-readable warning.

### Recommended pattern: **Soft-drop, then hard-drop.**

1. Stop writing to the column in application code
2. Deploy
3. Wait one release cycle (ensure no reads reference it)
4. Run the DROP migration

### Type Changes (Dangerous)

```sql
ALTER TABLE "public"."orders" ALTER COLUMN "amount" TYPE NUMERIC(12,2);
```

Postgres must rewrite the entire table if the type change requires a cast. For a 100M-row table, this locks the table for minutes.

**Grey DB behavior:** Type changes are flagged as `severity: "danger"` with the message: *"Column type change: integer → numeric. This may fail or lose data."*

### Recommended pattern: **Expand-contract migration.**

1. Add new column with new type (`amount_v2 NUMERIC(12,2)`)
2. Backfill: `UPDATE orders SET amount_v2 = amount::NUMERIC(12,2)`
3. Switch application reads to `amount_v2`
4. Drop old column
5. Rename: `ALTER TABLE orders RENAME COLUMN amount_v2 TO amount`

Grey DB's differ generates steps 1 and 4. The backfill (step 2) is application code — we don't automate unbounded `UPDATE`s because they need to be batched to avoid long-running transactions.

## Backfill Strategy

Grey DB does not auto-run backfills. Here's why:

| Approach | Risk |
|----------|------|
| Single UPDATE | Locks the table for the entire duration |
| Background job | Can fall behind if write rate exceeds backfill rate |
| Trigger-based | Adds write amplification to every INSERT/UPDATE |

We recommend **batched backfills** outside the migration engine:

```sql
-- Run in batches of 10,000
UPDATE orders SET status = 'active'
WHERE status IS NULL
AND id IN (SELECT id FROM orders WHERE status IS NULL LIMIT 10000);
```

The migration engine handles DDL. Backfills are application-level concerns that require rate limiting, progress tracking, and error handling that differ per use case.

## Dual-Write Pattern

When migrating between data models (e.g., denormalizing), Grey DB supports a phased approach:

```
Phase 1: Add new column/table (migration)
Phase 2: Write to both old and new (application code)
Phase 3: Backfill old data into new structure
Phase 4: Switch reads to new structure (application code)
Phase 5: Stop writing to old structure
Phase 6: Drop old column/table (migration)
```

Grey DB's role is phases 1 and 6. Phases 2–5 are application logic because they require understanding of business constraints (can data be stale? how fast must it converge?).

## Checksum Integrity

Every migration records a SHA-256 checksum of its `up_sql`. This serves two purposes:

1. **Drift detection** — If someone manually edits a migration file after it was applied, the checksum won't match. This prevents "it worked on my machine" situations.

2. **Idempotency verification** — If the same migration is applied twice (e.g., via a retry), the checksum + version guard ensures it's a no-op rather than a double-apply.

## Advisory Lock Concurrency

```
Pod A: BEGIN → pg_advisory_xact_lock(2147483647) → [applies migration] → COMMIT
Pod B: BEGIN → pg_advisory_xact_lock(2147483647) → [blocks until Pod A commits]
Pod B: [wakes up, sees version already exists] → skips migration → COMMIT
```

The lock scope is the transaction — it's released on COMMIT or ROLLBACK. There is no risk of a dangling lock.

We use a fixed lock ID (`2147483647`, the max int32) to avoid collisions with application-level advisory locks that typically use smaller numbers.
