// ─────────────────────────────────────────────────────────────
// Grey DB — Slow Query Analyzer
// Log, analyze, and suggest optimizations for slow queries
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";

export interface SlowQuery {
    id: number;
    query: string;
    executionTimeMs: number;
    tenantId?: string;
    timestamp: Date;
    params?: any[];
    explainSummary?: string;
}

export interface QueryPattern {
    pattern: string;
    count: number;
    avgDurationMs: number;
    maxDurationMs: number;
    minDurationMs: number;
    examples: string[];
}

export class SlowQueryAnalyzer {
    private thresholdMs: number;

    constructor(private pool: Pool, thresholdMs = 100) {
        this.thresholdMs = thresholdMs;
    }

    async init(): Promise<void> {
        await this.pool.query(`
      CREATE TABLE IF NOT EXISTS grey_slow_queries (
        id              SERIAL PRIMARY KEY,
        query_text      TEXT NOT NULL,
        execution_ms    NUMERIC NOT NULL,
        tenant_id       UUID,
        params          JSONB,
        explain_summary TEXT,
        logged_at       TIMESTAMPTZ DEFAULT NOW()
      );

      CREATE INDEX IF NOT EXISTS idx_slow_queries_time ON grey_slow_queries (execution_ms DESC);
      CREATE INDEX IF NOT EXISTS idx_slow_queries_tenant ON grey_slow_queries (tenant_id);
      CREATE INDEX IF NOT EXISTS idx_slow_queries_logged ON grey_slow_queries (logged_at DESC);
    `);
    }

    /** Log a slow query */
    async log(query: string, executionMs: number, tenantId?: string, params?: any[]): Promise<void> {
        if (executionMs < this.thresholdMs) return;
        await this.pool.query(
            `INSERT INTO grey_slow_queries (query_text, execution_ms, tenant_id, params) VALUES ($1, $2, $3, $4)`,
            [query, executionMs, tenantId, params ? JSON.stringify(params) : null]
        );
    }

    /** Get recent slow queries */
    async getRecent(limit = 50): Promise<SlowQuery[]> {
        const result = await this.pool.query(
            `SELECT * FROM grey_slow_queries ORDER BY logged_at DESC LIMIT $1`,
            [limit]
        );
        return result.rows.map(this.rowToSlowQuery);
    }

    /** Get slow queries by tenant */
    async getByTenant(tenantId: string, limit = 50): Promise<SlowQuery[]> {
        const result = await this.pool.query(
            `SELECT * FROM grey_slow_queries WHERE tenant_id = $1 ORDER BY logged_at DESC LIMIT $2`,
            [tenantId, limit]
        );
        return result.rows.map(this.rowToSlowQuery);
    }

    /** Get top slow queries by duration */
    async getTopSlowest(limit = 20): Promise<SlowQuery[]> {
        const result = await this.pool.query(
            `SELECT * FROM grey_slow_queries ORDER BY execution_ms DESC LIMIT $1`,
            [limit]
        );
        return result.rows.map(this.rowToSlowQuery);
    }

    /** Analyze query patterns — group similar queries */
    async getPatterns(limit = 20): Promise<QueryPattern[]> {
        // Normalize queries by replacing literal values
        const result = await this.pool.query(`
      SELECT
        regexp_replace(
          regexp_replace(query_text, '''[^'']*''', '''?''', 'g'),
          '\\d+', '?', 'g'
        ) AS pattern,
        COUNT(*) AS cnt,
        ROUND(AVG(execution_ms)::numeric, 2) AS avg_ms,
        MAX(execution_ms) AS max_ms,
        MIN(execution_ms) AS min_ms,
        array_agg(DISTINCT LEFT(query_text, 200)) AS examples
      FROM grey_slow_queries
      GROUP BY pattern
      ORDER BY cnt DESC
      LIMIT $1
    `, [limit]);

        return result.rows.map((row) => ({
            pattern: row.pattern,
            count: parseInt(row.cnt),
            avgDurationMs: parseFloat(row.avg_ms),
            maxDurationMs: parseFloat(row.max_ms),
            minDurationMs: parseFloat(row.min_ms),
            examples: row.examples.slice(0, 3),
        }));
    }

    /** Get statistics summary */
    async getStats(): Promise<{
        totalSlowQueries: number;
        avgDurationMs: number;
        p95DurationMs: number;
        p99DurationMs: number;
        queriesLast24h: number;
    }> {
        const result = await this.pool.query(`
      SELECT
        COUNT(*) AS total,
        ROUND(AVG(execution_ms)::numeric, 2) AS avg_ms,
        ROUND(PERCENTILE_CONT(0.95) WITHIN GROUP (ORDER BY execution_ms)::numeric, 2) AS p95,
        ROUND(PERCENTILE_CONT(0.99) WITHIN GROUP (ORDER BY execution_ms)::numeric, 2) AS p99,
        COUNT(*) FILTER (WHERE logged_at > NOW() - INTERVAL '24 hours') AS last_24h
      FROM grey_slow_queries
    `);

        const row = result.rows[0];
        return {
            totalSlowQueries: parseInt(row.total),
            avgDurationMs: parseFloat(row.avg_ms) || 0,
            p95DurationMs: parseFloat(row.p95) || 0,
            p99DurationMs: parseFloat(row.p99) || 0,
            queriesLast24h: parseInt(row.last_24h),
        };
    }

    setThreshold(ms: number): void {
        this.thresholdMs = ms;
    }

    private rowToSlowQuery(row: any): SlowQuery {
        return {
            id: row.id,
            query: row.query_text,
            executionTimeMs: parseFloat(row.execution_ms),
            tenantId: row.tenant_id,
            timestamp: row.logged_at,
            params: row.params,
            explainSummary: row.explain_summary,
        };
    }
}
