// ─────────────────────────────────────────────────────────────
// Grey DB — Query Routes
// Query console, explain plans, index management
// ─────────────────────────────────────────────────────────────

import { Router, Request, Response } from "express";
import { GreyDB, QueryBuilder, InsertBuilder, explainQuery, explainQuerySafe, formatExplainResult } from "@grey-db/core";
import { requirePermission } from "../middleware/auth";

export function queryRoutes(): Router {
    const router = Router();

    /** Execute a raw SQL query */
    router.post("/execute", requirePermission("data:read"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { sql, params = [], tenantId } = req.body;
            if (!sql) {
                res.status(400).json({ error: "sql is required" });
                return;
            }

            // Safety check — block DDL for non-admins
            const permissions: string[] = (req as any).permissions || [];
            const isDDL = /\b(CREATE|ALTER|DROP|TRUNCATE)\b/i.test(sql);
            if (isDDL && !permissions.includes("admin:full") && !permissions.includes("schema:write")) {
                res.status(403).json({ error: "DDL operations require schema:write permission" });
                return;
            }

            const start = Date.now();

            let result: any;
            if (tenantId) {
                result = await db.tenants.withTenant(tenantId, async (client) => {
                    return client.query(sql, params);
                });
            } else {
                result = await db.pool.query(sql, params);
            }

            const durationMs = Date.now() - start;

            // Log slow queries
            await db.slowQueries.log(sql, durationMs, tenantId).catch(() => { });

            // Record metric
            await db.observability.recordQuery({
                query: sql.substring(0, 500),
                durationMs,
                tenantId,
                rowsAffected: result.rowCount ?? 0,
                cached: false,
            }).catch(() => { });

            res.json({
                rows: result.rows,
                rowCount: result.rowCount,
                fields: result.fields?.map((f: any) => ({ name: f.name, dataTypeID: f.dataTypeID })),
                durationMs,
            });
        } catch (err: any) {
            res.status(400).json({ error: err.message });
        }
    });

    /** Explain a query plan */
    router.post("/explain", requirePermission("data:read"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { sql, params = [], analyze = false } = req.body;
            if (!sql) {
                res.status(400).json({ error: "sql is required" });
                return;
            }

            const result = analyze
                ? await explainQuery(db.pool, sql, params)
                : await explainQuerySafe(db.pool, sql, params);

            res.json({
                ...result,
                formatted: formatExplainResult(result),
            });
        } catch (err: any) {
            res.status(400).json({ error: err.message });
        }
    });

    // ── Index Management ──────────────────────────────────────

    /** List all indexes */
    router.get("/indexes", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const schema = (req.query.schema as string) || "public";
        try {
            const indexes = await db.indexes.getIndexes(schema);
            res.json({ indexes });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get unused indexes */
    router.get("/indexes/unused", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const schema = (req.query.schema as string) || "public";
        try {
            const indexes = await db.indexes.getUnusedIndexes(schema);
            res.json({ indexes });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get duplicate indexes */
    router.get("/indexes/duplicates", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const schema = (req.query.schema as string) || "public";
        try {
            const duplicates = await db.indexes.getDuplicateIndexes(schema);
            res.json({ duplicates });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get index suggestions */
    router.get("/indexes/suggest", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const schema = (req.query.schema as string) || "public";
        try {
            const suggestions = await db.indexes.suggestIndexes(schema);
            res.json({ suggestions });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Create an index */
    router.post("/indexes", requirePermission("schema:write"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { name, table, columns, unique, method, concurrently, where: whereClause, schema } = req.body;
            const sql = await db.indexes.createIndex(name, table, columns, { unique, method, concurrently, where: whereClause, schema });
            res.json({ success: true, sql });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Drop an index */
    router.delete("/indexes/:name", requirePermission("schema:write"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            await db.indexes.dropIndex(req.params.name);
            res.json({ success: true });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    // ── Slow Query Analysis ──────────────────────────────────

    /** Get slow query stats */
    router.get("/slow-queries/stats", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const stats = await db.slowQueries.getStats();
            res.json(stats);
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get recent slow queries */
    router.get("/slow-queries", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const limit = parseInt(req.query.limit as string) || 50;
        try {
            const queries = await db.slowQueries.getRecent(limit);
            res.json({ queries });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get slow query patterns */
    router.get("/slow-queries/patterns", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const patterns = await db.slowQueries.getPatterns();
            res.json({ patterns });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    return router;
}
