// ─────────────────────────────────────────────────────────────
// Grey DB — Data Lifecycle & Retention Engine
// Retention policies, archival, partitioning, compaction
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 1. Retention Policies
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Retention policy defines how long data lives and what happens when it ages out.
 *
 * Design decisions:
 * - Policies are per-table, optionally per-tenant
 * - Aging is based on a configurable timestamp column (usually created_at)
 * - Three lifecycle stages: active → archived → purged
 * - Archive doesn't delete — it moves to a companion `_archive` table
 * - Purge is a hard delete with optional pre-purge hook (export, notify)
 *
 * Why not just DELETE old rows?
 * - Compliance: some data must be kept for N years even if inactive
 * - Analytics: archived data can be queried separately (slower storage)
 * - Safety: archive is reversible; delete is not
 * - Performance: active table stays small → queries stay fast
 */
export interface RetentionPolicy {
    /** Table this policy applies to */
    tableName: string;
    schema?: string;
    /** Column used to determine row age (must be timestamptz) */
    ageColumn: string;
    /** Move to archive table after this many days */
    archiveAfterDays?: number;
    /** Hard-delete (from archive or main table) after this many days */
    purgeAfterDays?: number;
    /** Only apply to rows matching this SQL WHERE clause */
    filter?: string;
    /** Per-tenant override (tenantId → override policy) */
    tenantOverrides?: Map<string, { archiveAfterDays?: number; purgeAfterDays?: number }>;
    /** Enable/disable the policy without removing it */
    enabled: boolean;
}

/**
 * Results from executing a retention sweep.
 */
export interface RetentionResult {
    tableName: string;
    archivedCount: number;
    purgedCount: number;
    errors: string[];
    durationMs: number;
}

/**
 * Retention engine: manages policies and executes lifecycle transitions.
 */
export class RetentionEngine {
    private policies: Map<string, RetentionPolicy> = new Map();

    constructor(private pool: Pool) { }

    async init(): Promise<void> {
        await this.pool.query(`
      CREATE TABLE IF NOT EXISTS "grey_retention_policies" (
        id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        table_name  TEXT NOT NULL UNIQUE,
        schema_name TEXT NOT NULL DEFAULT 'public',
        age_column  TEXT NOT NULL DEFAULT 'created_at',
        archive_after_days INTEGER,
        purge_after_days   INTEGER,
        filter      TEXT,
        enabled     BOOLEAN DEFAULT true,
        created_at  TIMESTAMPTZ DEFAULT NOW(),
        updated_at  TIMESTAMPTZ DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS "grey_retention_log" (
        id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        table_name    TEXT NOT NULL,
        action        TEXT NOT NULL,
        rows_affected INTEGER DEFAULT 0,
        duration_ms   INTEGER,
        error         TEXT,
        executed_at   TIMESTAMPTZ DEFAULT NOW()
      );
    `);
    }

    /** Register a retention policy (in memory + persistent) */
    async registerPolicy(policy: RetentionPolicy): Promise<void> {
        this.policies.set(policy.tableName, policy);

        await this.pool.query(
            `INSERT INTO "grey_retention_policies"
         (table_name, schema_name, age_column, archive_after_days, purge_after_days, filter, enabled)
       VALUES ($1, $2, $3, $4, $5, $6, $7)
       ON CONFLICT (table_name) DO UPDATE SET
         schema_name = EXCLUDED.schema_name,
         age_column = EXCLUDED.age_column,
         archive_after_days = EXCLUDED.archive_after_days,
         purge_after_days = EXCLUDED.purge_after_days,
         filter = EXCLUDED.filter,
         enabled = EXCLUDED.enabled,
         updated_at = NOW()`,
            [
                policy.tableName,
                policy.schema || "public",
                policy.ageColumn,
                policy.archiveAfterDays || null,
                policy.purgeAfterDays || null,
                policy.filter || null,
                policy.enabled,
            ]
        );
    }

    /** Load policies from the database (e.g., after restart) */
    async loadPolicies(): Promise<void> {
        const result = await this.pool.query(
            `SELECT * FROM "grey_retention_policies" WHERE enabled = true`
        );
        for (const row of result.rows) {
            this.policies.set(row.table_name, {
                tableName: row.table_name,
                schema: row.schema_name,
                ageColumn: row.age_column,
                archiveAfterDays: row.archive_after_days,
                purgeAfterDays: row.purge_after_days,
                filter: row.filter,
                enabled: row.enabled,
            });
        }
    }

    /**
     * Execute archival for a table:
     * 1. Create archive table if it doesn't exist (same structure + archived_at)
     * 2. Move aged rows from main → archive
     */
    async archiveTable(tableName: string): Promise<RetentionResult> {
        const start = Date.now();
        const policy = this.policies.get(tableName);
        if (!policy || !policy.archiveAfterDays) {
            return { tableName, archivedCount: 0, purgedCount: 0, errors: ["No archive policy"], durationMs: 0 };
        }

        const schema = policy.schema || "public";
        const src = `"${schema}"."${tableName}"`;
        const archiveTable = `"${schema}"."${tableName}_archive"`;
        const filter = policy.filter ? ` AND (${policy.filter})` : "";
        const errors: string[] = [];
        let archivedCount = 0;

        try {
            // Create archive table mirroring source + archived_at column
            await this.pool.query(`
        CREATE TABLE IF NOT EXISTS ${archiveTable} (LIKE ${src} INCLUDING ALL);
        DO $$ BEGIN
          ALTER TABLE ${archiveTable}
            ADD COLUMN IF NOT EXISTS archived_at TIMESTAMPTZ DEFAULT NOW();
        EXCEPTION WHEN duplicate_column THEN NULL;
        END $$;
      `);

            // Move aged rows in a transaction
            const client = await this.pool.connect();
            try {
                await client.query("BEGIN");

                const insertResult = await client.query(`
          WITH moved AS (
            DELETE FROM ${src}
            WHERE "${policy.ageColumn}" < NOW() - INTERVAL '${policy.archiveAfterDays} days'${filter}
            RETURNING *
          )
          INSERT INTO ${archiveTable}
          SELECT *, NOW() AS archived_at FROM moved
        `);

                archivedCount = insertResult.rowCount ?? 0;
                await client.query("COMMIT");
            } catch (err: any) {
                await client.query("ROLLBACK");
                errors.push(err.message);
            } finally {
                client.release();
            }
        } catch (err: any) {
            errors.push(err.message);
        }

        const durationMs = Date.now() - start;

        // Log the result
        await this.pool.query(
            `INSERT INTO "grey_retention_log" (table_name, action, rows_affected, duration_ms, error)
       VALUES ($1, 'archive', $2, $3, $4)`,
            [tableName, archivedCount, durationMs, errors.length > 0 ? errors.join("; ") : null]
        ).catch(() => { });

        return { tableName, archivedCount, purgedCount: 0, errors, durationMs };
    }

    /**
     * Hard-delete (purge) rows past the purge threshold.
     * Works on both archive tables and main tables.
     */
    async purgeTable(tableName: string, fromArchive = true): Promise<RetentionResult> {
        const start = Date.now();
        const policy = this.policies.get(tableName);
        if (!policy || !policy.purgeAfterDays) {
            return { tableName, archivedCount: 0, purgedCount: 0, errors: ["No purge policy"], durationMs: 0 };
        }

        const schema = policy.schema || "public";
        const targetTable = fromArchive
            ? `"${schema}"."${tableName}_archive"`
            : `"${schema}"."${tableName}"`;

        const ageCol = fromArchive ? "archived_at" : policy.ageColumn;
        const errors: string[] = [];
        let purgedCount = 0;

        try {
            const result = await this.pool.query(`
        DELETE FROM ${targetTable}
        WHERE "${ageCol}" < NOW() - INTERVAL '${policy.purgeAfterDays} days'
      `);
            purgedCount = result.rowCount ?? 0;
        } catch (err: any) {
            errors.push(err.message);
        }

        const durationMs = Date.now() - start;

        await this.pool.query(
            `INSERT INTO "grey_retention_log" (table_name, action, rows_affected, duration_ms, error)
       VALUES ($1, 'purge', $2, $3, $4)`,
            [tableName, purgedCount, durationMs, errors.length > 0 ? errors.join("; ") : null]
        ).catch(() => { });

        return { tableName, archivedCount: 0, purgedCount, errors, durationMs };
    }

    /** Run all enabled retention policies (archive + purge) */
    async runAll(): Promise<RetentionResult[]> {
        const results: RetentionResult[] = [];

        for (const [tableName, policy] of this.policies) {
            if (!policy.enabled) continue;

            if (policy.archiveAfterDays) {
                results.push(await this.archiveTable(tableName));
            }
            if (policy.purgeAfterDays) {
                results.push(await this.purgeTable(tableName));
            }
        }

        return results;
    }

    /** Get retention execution history */
    async getLog(limit = 100): Promise<any[]> {
        const result = await this.pool.query(
            `SELECT * FROM "grey_retention_log" ORDER BY executed_at DESC LIMIT $1`,
            [limit]
        );
        return result.rows;
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 2. Partitioning Strategies
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * PostgreSQL native partitioning DDL generation.
 *
 * When to partition:
 * - Tables > ~10M rows
 * - Time-series data (events, logs, metrics)
 * - Multi-tenant data where tenants have very different sizes
 * - Data with clear lifecycle stages (active → archived)
 *
 * Partition types:
 * - RANGE: time-based (most common), numeric ranges
 * - LIST: discrete values (tenant_id, region, status)
 * - HASH: even distribution when no natural range/list key
 */
export type PartitionType = "range" | "list" | "hash";

export interface PartitionConfig {
    tableName: string;
    schema?: string;
    partitionType: PartitionType;
    partitionKey: string;
    /** For RANGE: generate monthly/weekly/daily partitions */
    rangeInterval?: "daily" | "weekly" | "monthly" | "yearly";
    /** For LIST: explicit partition values */
    listValues?: string[][];
    /** For HASH: number of hash partitions */
    hashModulus?: number;
}

/**
 * Generate CREATE TABLE ... PARTITION BY DDL + child partitions.
 */
export function generatePartitionDDL(config: PartitionConfig): {
    parentDDL: string;
    childDDL: string[];
    detachSQL: (partitionName: string) => string;
} {
    const { tableName, schema = "public", partitionType, partitionKey } = config;
    const fq = `"${schema}"."${tableName}"`;

    // Parent table: PARTITION BY clause (columns defined elsewhere)
    const parentDDL = `-- Convert to partitioned table (requires recreating the table)
-- The original CREATE TABLE DDL should include:
ALTER TABLE ${fq} RENAME TO "${tableName}_old";

CREATE TABLE ${fq} (LIKE "${schema}"."${tableName}_old" INCLUDING ALL)
PARTITION BY ${partitionType.toUpperCase()} ("${partitionKey}");

-- After creating partitions, migrate data:
-- INSERT INTO ${fq} SELECT * FROM "${schema}"."${tableName}_old";
-- DROP TABLE "${schema}"."${tableName}_old";
`;

    const childDDL: string[] = [];

    if (partitionType === "range" && config.rangeInterval) {
        // Generate time-based range partitions for the next 12 periods
        const intervals: { start: string; end: string; suffix: string }[] = [];
        const now = new Date();

        for (let i = 0; i < 12; i++) {
            const d = new Date(now);
            let suffix: string;
            let start: string;
            let end: string;

            switch (config.rangeInterval) {
                case "daily":
                    d.setDate(d.getDate() + i);
                    suffix = d.toISOString().split("T")[0].replace(/-/g, "");
                    start = d.toISOString().split("T")[0];
                    d.setDate(d.getDate() + 1);
                    end = d.toISOString().split("T")[0];
                    break;
                case "weekly":
                    d.setDate(d.getDate() + i * 7);
                    suffix = `w${d.toISOString().split("T")[0].replace(/-/g, "")}`;
                    start = d.toISOString().split("T")[0];
                    d.setDate(d.getDate() + 7);
                    end = d.toISOString().split("T")[0];
                    break;
                case "monthly":
                    d.setMonth(d.getMonth() + i);
                    suffix = `${d.getFullYear()}${String(d.getMonth() + 1).padStart(2, "0")}`;
                    start = `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-01`;
                    d.setMonth(d.getMonth() + 1);
                    end = `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-01`;
                    break;
                case "yearly":
                    d.setFullYear(d.getFullYear() + i);
                    suffix = `${d.getFullYear()}`;
                    start = `${d.getFullYear()}-01-01`;
                    end = `${d.getFullYear() + 1}-01-01`;
                    break;
            }

            intervals.push({ start, end, suffix });
        }

        for (const { start, end, suffix } of intervals) {
            childDDL.push(
                `CREATE TABLE IF NOT EXISTS "${schema}"."${tableName}_${suffix}"
  PARTITION OF ${fq}
  FOR VALUES FROM ('${start}') TO ('${end}');`
            );
        }
    }

    if (partitionType === "list" && config.listValues) {
        for (let i = 0; i < config.listValues.length; i++) {
            const values = config.listValues[i].map((v) => `'${v}'`).join(", ");
            const suffix = config.listValues[i][0].toLowerCase().replace(/[^a-z0-9]/g, "_");
            childDDL.push(
                `CREATE TABLE IF NOT EXISTS "${schema}"."${tableName}_${suffix}"
  PARTITION OF ${fq}
  FOR VALUES IN (${values});`
            );
        }
    }

    if (partitionType === "hash" && config.hashModulus) {
        for (let i = 0; i < config.hashModulus; i++) {
            childDDL.push(
                `CREATE TABLE IF NOT EXISTS "${schema}"."${tableName}_p${i}"
  PARTITION OF ${fq}
  FOR VALUES WITH (MODULUS ${config.hashModulus}, REMAINDER ${i});`
            );
        }
    }

    const detachSQL = (partitionName: string) =>
        `ALTER TABLE ${fq} DETACH PARTITION "${schema}"."${partitionName}";`;

    return { parentDDL, childDDL, detachSQL };
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 3. Compaction & Cleanup
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Compaction is the process of reclaiming space and maintaining performance:
 *
 * 1. VACUUM: reclaim dead tuple space (Postgres autovacuum usually handles this)
 * 2. REINDEX: rebuild bloated indexes
 * 3. CLUSTER: physically reorder table rows by an index (expensive but powerful)
 * 4. Partition maintenance: detach old partitions, attach new ones
 *
 * These operations should run during maintenance windows because they
 * can hold locks or consume significant I/O.
 */
export interface CompactionJob {
    tableName: string;
    schema?: string;
    operations: CompactionOperation[];
}

export type CompactionOperation =
    | { type: "vacuum"; analyze?: boolean; full?: boolean }
    | { type: "reindex"; concurrently?: boolean }
    | { type: "cluster"; indexName: string }
    | { type: "analyze" };

export function generateCompactionSQL(job: CompactionJob): string[] {
    const schema = job.schema || "public";
    const fq = `"${schema}"."${job.tableName}"`;
    const sqls: string[] = [];

    for (const op of job.operations) {
        switch (op.type) {
            case "vacuum": {
                const parts = ["VACUUM"];
                if (op.full) parts.push("FULL");
                if (op.analyze ?? true) parts.push("ANALYZE");
                parts.push(fq);
                sqls.push(parts.join(" ") + ";");
                break;
            }
            case "reindex": {
                if (op.concurrently) {
                    sqls.push(`REINDEX TABLE CONCURRENTLY ${fq};`);
                } else {
                    sqls.push(`REINDEX TABLE ${fq};`);
                }
                break;
            }
            case "cluster": {
                sqls.push(`CLUSTER ${fq} USING "${op.indexName}";`);
                break;
            }
            case "analyze": {
                sqls.push(`ANALYZE ${fq};`);
                break;
            }
        }
    }

    return sqls;
}

/**
 * Table health check: detects bloat and recommends compaction operations.
 */
export async function checkTableHealth(
    pool: Pool,
    tableName: string,
    schema = "public"
): Promise<{
    rowEstimate: number;
    deadTuples: number;
    deadTuplePercent: number;
    tableSize: string;
    indexSize: string;
    lastVacuum: Date | null;
    lastAnalyze: Date | null;
    recommendations: string[];
}> {
    const result = await pool.query(
        `SELECT
       n_live_tup::int AS live_tuples,
       n_dead_tup::int AS dead_tuples,
       pg_size_pretty(pg_total_relation_size('"${schema}"."${tableName}"')) AS table_size,
       pg_size_pretty(pg_indexes_size('"${schema}"."${tableName}"')) AS index_size,
       last_vacuum,
       last_autovacuum,
       last_analyze,
       last_autoanalyze
     FROM pg_stat_user_tables
     WHERE schemaname = $1 AND relname = $2`,
        [schema, tableName]
    );

    if (result.rows.length === 0) {
        return {
            rowEstimate: 0,
            deadTuples: 0,
            deadTuplePercent: 0,
            tableSize: "0 bytes",
            indexSize: "0 bytes",
            lastVacuum: null,
            lastAnalyze: null,
            recommendations: ["Table not found or no stats available"],
        };
    }

    const row = result.rows[0];
    const liveTuples = row.live_tuples || 0;
    const deadTuples = row.dead_tuples || 0;
    const total = liveTuples + deadTuples;
    const deadPercent = total > 0 ? (deadTuples / total) * 100 : 0;
    const lastVac = row.last_vacuum || row.last_autovacuum;
    const lastAna = row.last_analyze || row.last_autoanalyze;

    const recommendations: string[] = [];

    if (deadPercent > 20) {
        recommendations.push("High dead tuple ratio — run VACUUM ANALYZE");
    }
    if (deadPercent > 50) {
        recommendations.push("Very high bloat — consider VACUUM FULL (requires exclusive lock)");
    }
    if (!lastVac) {
        recommendations.push("Table has never been vacuumed");
    }
    if (!lastAna) {
        recommendations.push("Table has never been analyzed — query planner may use poor plans");
    }

    return {
        rowEstimate: liveTuples,
        deadTuples,
        deadTuplePercent: deadPercent,
        tableSize: row.table_size,
        indexSize: row.index_size,
        lastVacuum: lastVac,
        lastAnalyze: lastAna,
        recommendations,
    };
}
