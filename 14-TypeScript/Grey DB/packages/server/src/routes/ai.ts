// ─────────────────────────────────────────────────────────────
// Grey DB — AI Routes
// NL→SQL, auto-documentation, semantic search
// ─────────────────────────────────────────────────────────────

import { Router, Request, Response } from "express";
import { GreyDB } from "@grey-db/core";
import { requirePermission } from "../middleware/auth";

export function aiRoutes(): Router {
    const router = Router();

    // ── NL→SQL ────────────────────────────────────────────────

    /** Load/refresh schema context for AI queries */
    router.post("/schema/load", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const schema = (req.body.schema as string) || "public";
        try {
            const context = await db.nlsql.loadSchema(schema);
            res.json({
                tablesLoaded: context.tables.length,
                tables: context.tables.map((t) => ({
                    name: t.name,
                    columns: t.columns.length,
                    rowCount: t.rowCount,
                })),
            });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Convert natural language to SQL */
    router.post("/nl-to-sql", requirePermission("data:read"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { query, allowWrite = false } = req.body;
            if (!query) {
                res.status(400).json({ error: "query is required" });
                return;
            }

            // Ensure schema is loaded
            if (!db.nlsql.getSchemaContext()) {
                await db.nlsql.loadSchema();
            }

            const prepared = db.nlsql.prepareNLQuery(query);

            // Return the prepared prompt for the client to send to their LLM
            // This keeps Grey DB LLM-agnostic
            res.json({
                systemPrompt: prepared.systemPrompt,
                userPrompt: prepared.userPrompt,
                schemaContext: {
                    tables: prepared.schemaContext.tables.map((t) => ({
                        name: t.name,
                        columns: t.columns.map((c) => ({ name: c.name, type: c.type })),
                    })),
                },
                // If the client already has a generated SQL, they can include it for validation
                ...(req.body.generatedSQL ? {
                    safety: db.nlsql.checkSafety(req.body.generatedSQL, allowWrite),
                } : {}),
            });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Validate generated SQL for safety */
    router.post("/validate-sql", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { sql, allowWrite = false } = req.body;
            if (!sql) {
                res.status(400).json({ error: "sql is required" });
                return;
            }
            const safety = db.nlsql.checkSafety(sql, allowWrite);
            res.json(safety);
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    // ── Auto Documentation ────────────────────────────────────

    /** Get documentation for a specific table */
    router.get("/docs/table/:name", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const schema = (req.query.schema as string) || "public";
        try {
            const doc = await db.docs.generateTableDoc(req.params.name, schema);
            res.json(doc);
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get documentation for all tables */
    router.get("/docs", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const schema = (req.query.schema as string) || "public";
        try {
            const docs = await db.docs.generateAllDocs(schema);
            res.json({ tables: docs });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Generate ER diagram in Mermaid format */
    router.get("/docs/diagram", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const schema = (req.query.schema as string) || "public";
        try {
            const diagram = await db.docs.generateERDiagram(schema);
            res.json({ format: "mermaid", diagram });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    // ── Semantic Search ───────────────────────────────────────

    /** Add embedding column to a table */
    router.post("/semantic/setup", requirePermission("schema:write"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { table, schema = "public", embeddingColumn = "embedding", dimensions = 1536, indexMethod = "hnsw", contentColumns = [] } = req.body;
            if (!table) {
                res.status(400).json({ error: "table is required" });
                return;
            }
            const sql = await db.search.addEmbeddingColumn({
                table, schema, embeddingColumn, dimensions, indexMethod, contentColumns,
            });
            res.json({ success: true, sql });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Store an embedding */
    router.post("/semantic/embed", requirePermission("data:write"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { table, id, embeddingColumn = "embedding", embedding, schema = "public" } = req.body;
            await db.search.storeEmbedding(table, id, embeddingColumn, embedding, schema);
            res.json({ success: true });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Semantic search */
    router.post("/semantic/search", requirePermission("data:read"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { table, embeddingColumn = "embedding", queryEmbedding, limit = 10, schema = "public", filters, tenantId } = req.body;
            if (!table || !queryEmbedding) {
                res.status(400).json({ error: "table and queryEmbedding are required" });
                return;
            }

            let results;
            if (tenantId) {
                results = await db.search.tenantSearch(table, embeddingColumn, queryEmbedding, tenantId, limit, schema);
            } else if (filters && filters.length > 0) {
                results = await db.search.hybridSearch(table, embeddingColumn, queryEmbedding, filters, limit, schema);
            } else {
                results = await db.search.search(table, embeddingColumn, queryEmbedding, limit, schema);
            }

            res.json({ results });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    return router;
}
