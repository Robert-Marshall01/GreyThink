// ─────────────────────────────────────────────────────────────
// Grey DB — Observability Engine
// Structured logs, query metrics, dashboards, analytics
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";

export interface QueryMetric {
    query: string;
    durationMs: number;
    tenantId?: string;
    userId?: string;
    timestamp: Date;
    rowsAffected: number;
    cached: boolean;
}

export interface DashboardData {
    totalQueries: number;
    avgDurationMs: number;
    p95DurationMs: number;
    queriesPerMinute: number;
    topTenants: Array<{ tenantId: string; queryCount: number }>;
    slowQueries: number;
    errorCount: number;
    activeConnections: number;
    databaseSize: string;
    queryTrend: Array<{ timestamp: string; count: number; avgMs: number }>;
}

export interface HealthCheck {
    status: "healthy" | "degraded" | "unhealthy";
    database: boolean;
    connectionPool: { total: number; idle: number; waiting: number };
    databaseSize: string;
    uptime: string;
    version: string;
    latencyMs: number;
}

export class ObservabilityEngine {
    constructor(private pool: Pool) { }

    async init(): Promise<void> {
        await this.pool.query(`
      CREATE TABLE IF NOT EXISTS grey_query_metrics (
        id            SERIAL PRIMARY KEY,
        query_text    TEXT NOT NULL,
        duration_ms   NUMERIC NOT NULL,
        tenant_id     UUID,
        user_id       UUID,
        rows_affected INTEGER DEFAULT 0,
        cached        BOOLEAN DEFAULT false,
        created_at    TIMESTAMPTZ DEFAULT NOW()
      );

      CREATE INDEX IF NOT EXISTS idx_metrics_time ON grey_query_metrics (created_at DESC);
      CREATE INDEX IF NOT EXISTS idx_metrics_tenant ON grey_query_metrics (tenant_id);
      CREATE INDEX IF NOT EXISTS idx_metrics_duration ON grey_query_metrics (duration_ms DESC);

      CREATE TABLE IF NOT EXISTS grey_error_log (
        id          SERIAL PRIMARY KEY,
        error_type  TEXT NOT NULL,
        message     TEXT NOT NULL,
        stack       TEXT,
        context     JSONB DEFAULT '{}',
        tenant_id   UUID,
        created_at  TIMESTAMPTZ DEFAULT NOW()
      );
    `);
    }

    /** Record a query metric */
    async recordQuery(metric: Omit<QueryMetric, "timestamp">): Promise<void> {
        await this.pool.query(
            `INSERT INTO grey_query_metrics (query_text, duration_ms, tenant_id, user_id, rows_affected, cached)
       VALUES ($1, $2, $3, $4, $5, $6)`,
            [metric.query, metric.durationMs, metric.tenantId, metric.userId, metric.rowsAffected, metric.cached]
        );
    }

    /** Log an error */
    async logError(type: string, message: string, stack?: string, context?: Record<string, any>, tenantId?: string): Promise<void> {
        await this.pool.query(
            `INSERT INTO grey_error_log (error_type, message, stack, context, tenant_id) VALUES ($1, $2, $3, $4, $5)`,
            [type, message, stack, JSON.stringify(context || {}), tenantId]
        );
    }

    /** Get dashboard data */
    async getDashboard(hours = 24): Promise<DashboardData> {
        const interval = `${hours} hours`;

        const [statsResult, tenantsResult, trendResult, connResult, sizeResult] = await Promise.all([
            this.pool.query(`
        SELECT
          COUNT(*) AS total,
          ROUND(AVG(duration_ms)::numeric, 2) AS avg_ms,
          ROUND(PERCENTILE_CONT(0.95) WITHIN GROUP (ORDER BY duration_ms)::numeric, 2) AS p95_ms,
          COUNT(*) FILTER (WHERE duration_ms > 100) AS slow_count,
          ROUND(COUNT(*)::numeric / GREATEST(EXTRACT(EPOCH FROM (NOW() - MIN(created_at))) / 60, 1), 2) AS qpm
        FROM grey_query_metrics
        WHERE created_at > NOW() - INTERVAL '${interval}'
      `),
            this.pool.query(`
        SELECT tenant_id, COUNT(*) AS cnt
        FROM grey_query_metrics
        WHERE created_at > NOW() - INTERVAL '${interval}' AND tenant_id IS NOT NULL
        GROUP BY tenant_id
        ORDER BY cnt DESC
        LIMIT 10
      `),
            this.pool.query(`
        SELECT
          date_trunc('hour', created_at) AS ts,
          COUNT(*) AS cnt,
          ROUND(AVG(duration_ms)::numeric, 2) AS avg_ms
        FROM grey_query_metrics
        WHERE created_at > NOW() - INTERVAL '${interval}'
        GROUP BY ts
        ORDER BY ts ASC
      `),
            this.pool.query(`SELECT count(*) AS total FROM pg_stat_activity WHERE datname = current_database()`),
            this.pool.query(`SELECT pg_size_pretty(pg_database_size(current_database())) AS size`),
        ]);

        const errorCount = await this.pool.query(
            `SELECT COUNT(*) AS cnt FROM grey_error_log WHERE created_at > NOW() - INTERVAL '${interval}'`
        );

        const stats = statsResult.rows[0];

        return {
            totalQueries: parseInt(stats.total) || 0,
            avgDurationMs: parseFloat(stats.avg_ms) || 0,
            p95DurationMs: parseFloat(stats.p95_ms) || 0,
            queriesPerMinute: parseFloat(stats.qpm) || 0,
            topTenants: tenantsResult.rows.map((r) => ({ tenantId: r.tenant_id, queryCount: parseInt(r.cnt) })),
            slowQueries: parseInt(stats.slow_count) || 0,
            errorCount: parseInt(errorCount.rows[0].cnt) || 0,
            activeConnections: parseInt(connResult.rows[0].total) || 0,
            databaseSize: sizeResult.rows[0].size,
            queryTrend: trendResult.rows.map((r) => ({
                timestamp: r.ts.toISOString(),
                count: parseInt(r.cnt),
                avgMs: parseFloat(r.avg_ms),
            })),
        };
    }

    /** Get per-tenant usage stats */
    async getTenantUsage(tenantId: string, hours = 24): Promise<{
        queryCount: number;
        avgDurationMs: number;
        errorCount: number;
        topQueries: Array<{ query: string; count: number; avgMs: number }>;
    }> {
        const interval = `${hours} hours`;

        const [statsResult, errorResult, topResult] = await Promise.all([
            this.pool.query(`
        SELECT COUNT(*) AS cnt, ROUND(AVG(duration_ms)::numeric, 2) AS avg_ms
        FROM grey_query_metrics
        WHERE tenant_id = $1 AND created_at > NOW() - INTERVAL '${interval}'
      `, [tenantId]),
            this.pool.query(`
        SELECT COUNT(*) AS cnt FROM grey_error_log
        WHERE tenant_id = $1 AND created_at > NOW() - INTERVAL '${interval}'
      `, [tenantId]),
            this.pool.query(`
        SELECT
          LEFT(query_text, 200) AS query,
          COUNT(*) AS cnt,
          ROUND(AVG(duration_ms)::numeric, 2) AS avg_ms
        FROM grey_query_metrics
        WHERE tenant_id = $1 AND created_at > NOW() - INTERVAL '${interval}'
        GROUP BY LEFT(query_text, 200)
        ORDER BY cnt DESC
        LIMIT 10
      `, [tenantId]),
        ]);

        return {
            queryCount: parseInt(statsResult.rows[0].cnt) || 0,
            avgDurationMs: parseFloat(statsResult.rows[0].avg_ms) || 0,
            errorCount: parseInt(errorResult.rows[0].cnt) || 0,
            topQueries: topResult.rows.map((r) => ({
                query: r.query,
                count: parseInt(r.cnt),
                avgMs: parseFloat(r.avg_ms),
            })),
        };
    }

    /** Health check */
    async healthCheck(): Promise<HealthCheck> {
        const start = Date.now();
        try {
            const [versionResult, sizeResult] = await Promise.all([
                this.pool.query(`SELECT version()`),
                this.pool.query(`SELECT pg_size_pretty(pg_database_size(current_database())) AS size`),
            ]);
            const latencyMs = Date.now() - start;

            const uptimeResult = await this.pool.query(
                `SELECT NOW() - pg_postmaster_start_time() AS uptime`
            );

            return {
                status: latencyMs < 100 ? "healthy" : latencyMs < 500 ? "degraded" : "unhealthy",
                database: true,
                connectionPool: {
                    total: (this.pool as any).totalCount ?? 0,
                    idle: (this.pool as any).idleCount ?? 0,
                    waiting: (this.pool as any).waitingCount ?? 0,
                },
                databaseSize: sizeResult.rows[0].size,
                uptime: uptimeResult.rows[0].uptime,
                version: versionResult.rows[0].version,
                latencyMs,
            };
        } catch (err) {
            return {
                status: "unhealthy",
                database: false,
                connectionPool: { total: 0, idle: 0, waiting: 0 },
                databaseSize: "unknown",
                uptime: "unknown",
                version: "unknown",
                latencyMs: Date.now() - start,
            };
        }
    }
}
