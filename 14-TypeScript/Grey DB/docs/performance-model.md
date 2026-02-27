# Grey DB — Performance Model

> Indexing strategy, query planner expectations, and scaling characteristics.

## Indexing Philosophy

### Indexes We Create Automatically

| Constraint | Index Type | Why |
|-----------|-----------|-----|
| `PRIMARY KEY` | B-tree (unique) | Postgres creates this implicitly |
| `.unique()` | B-tree (unique) | Postgres creates this implicitly |
| `.indexed()` | B-tree | Explicit request via DSL |
| `tenant_id` (on tenant-scoped tables) | B-tree | RLS performance — every query filters by tenant_id |
| `vector(N)` | IVFFlat or HNSW | Similarity search performance |

### Indexes We Suggest

The `IndexManager` analyzes `pg_stat_user_tables` and `pg_stat_user_indexes` to identify:

1. **Tables with high sequential scan ratios** — If `seq_scan >> idx_scan`, the table is being scanned end-to-end. We suggest a B-tree index on the most commonly filtered columns.

2. **Unused indexes** — Indexes with `idx_scan = 0` after significant uptime. These waste disk space and slow writes.

3. **Duplicate indexes** — Multiple indexes covering the same column set. Keep the most-used one, drop the rest.

### Index Sizing Rule of Thumb

```
Index size ≈ 30-40 bytes × row count (for a single-column B-tree)
```

A 10M-row table with a single B-tree index on a UUID column ≈ 300-400 MB. Plan disk accordingly.

## Query Planner Expectations

### When Postgres Uses an Index

Postgres's cost-based optimizer chooses between sequential scan and index scan based on:

1. **Selectivity** — If a WHERE clause matches <5-15% of rows, Postgres usually prefers an index scan. Above that threshold, a sequential scan is often faster because it avoids random I/O.

2. **Index correlation** — If the physical row order matches the index order (correlation ≈ 1.0), index scans are very efficient. If correlation ≈ 0.0, each index lookup requires a random disk seek.

3. **Table size** — For small tables (<1000 rows), Postgres almost always chooses a sequential scan. The overhead of the index lookup exceeds the cost of scanning a few pages.

### EXPLAIN Signals We Flag

Grey DB's `ExplainAnalyzer` parses `EXPLAIN ANALYZE` output and flags:

| Pattern | Severity | Meaning |
|---------|----------|---------|
| `Seq Scan` on a table with >10K rows | warning | Missing index or low selectivity |
| Nested Loop with inner Seq Scan | high | O(n²) join — needs an index on the join column |
| `Sort` with `external disk merge` | high | Not enough `work_mem` — sort spilled to disk |
| `Rows Removed by Filter` >> `Rows` | warning | Index is returning too many candidate rows |
| Actual rows >> estimated rows | warning | Statistics are stale — run `ANALYZE` |

### work_mem Tuning

Default `work_mem` is 4MB. For analytical queries with large sorts or hash joins, this causes disk spills. We recommend:

```sql
-- Per-session for heavy queries
SET work_mem = '256MB';

-- Globally (careful — multiplied by max_connections × sort operations)
ALTER SYSTEM SET work_mem = '64MB';
```

Grey DB doesn't change `work_mem` automatically. It flags the symptom (disk sort) and leaves the tuning decision to the operator.

## Multi-Tenant Performance Characteristics

### Shared Table Performance

With RLS enabled and a B-tree index on `tenant_id`:

| Tenant Data Size | Query Latency (indexed) | Seq Scan Latency |
|-----------------|------------------------|------------------|
| 1K rows | <1ms | <1ms |
| 100K rows | 1-5ms | 10-50ms |
| 1M rows | 5-20ms | 100-500ms |
| 10M rows | 10-50ms | 1-5s |

The index transforms O(n) scans into O(log n) lookups. For multi-tenant tables, the `tenant_id` index is the single most important performance factor.

### Schema-per-Tenant Performance

When a tenant graduates to its own schema:
- Table statistics are per-schema, so the planner has better estimates
- No index bloat from other tenants' data
- Vacuum runs per-table, not across all tenants
- But: connection pool is shared, so one tenant's heavy query can starve others

### Connection Pool Saturation

```
max_connections (Postgres) = 100
pool.max (per app instance) = 20
app instances = 5
Total connections = 100 (fully utilized)
```

If a query holds a connection for 500ms, each instance can serve 40 queries/second per connection × 20 connections = 800 QPS per instance.

Grey DB monitors connection pool utilization via the health check endpoint:
```json
{
  "connectionPool": { "total": 20, "idle": 15, "waiting": 0 }
}
```

If `waiting > 0`, the pool is saturated. Either increase `max`, reduce query latency, or scale horizontally.

## Semantic Search Performance

### pgvector Index Types

| Index | Build Time | Query Latency | Recall | Memory |
|-------|-----------|---------------|--------|--------|
| None (exact) | 0 | O(n) — linear scan | 100% | Low |
| IVFFlat | Minutes | O(√n) | 95-99% | Medium |
| HNSW | Longer | O(log n) | 98-99.9% | High |

We default to **HNSW** for semantic search because:
- Query latency is logarithmic in dataset size
- Recall is near-perfect (>99%)
- No need to periodically rebuild (unlike IVFFlat which needs rebuilding as data changes)

### Vector Dimensions Impact

```
Embedding model    | Dimensions | Storage per row | Index size (1M rows)
OpenAI text-3-small| 1536       | 6.1 KB          | ~6 GB
Cohere embed-v3    | 1024       | 4.1 KB          | ~4 GB
MiniLM-L6          | 384        | 1.5 KB          | ~1.5 GB
```

Choose the smallest dimension that meets your accuracy requirements. 384-dimension models are often sufficient for search-within-a-product-schema.

## Observability Metrics We Track

| Metric | Where | Why |
|--------|-------|-----|
| Query duration (avg, p95) | `grey_query_metrics` | Detect latency regressions |
| Queries per minute | Computed from metrics | Capacity planning |
| Slow queries (>100ms) | `grey_query_metrics` | Performance debugging |
| Error count | `grey_error_log` | Reliability monitoring |
| Connection pool state | `pg_stat_activity` | Saturation detection |
| Database size | `pg_database_size()` | Storage planning |
| Index usage ratios | `pg_stat_user_indexes` | Index optimization |
| Vacuum/analyze stats | `pg_stat_user_tables` | Maintenance monitoring |

All metrics are queryable through the REST API and visible in the admin dashboard.
