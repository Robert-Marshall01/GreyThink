// ─────────────────────────────────────────────────────────────
// Grey DB — Schema Routes
// Schema browser, migration management, registry
// ─────────────────────────────────────────────────────────────

import { Router, Request, Response } from "express";
import {
    GreyDB, greyTable, uuid, text, varchar, integer, bigint, boolean, timestamp, timestamptz,
    jsonb, serial, float, numeric, date, bytea, vector,
    generateCreateTable, diffSchemas, generateMigration, applyMigration,
    getCurrentVersion, getMigrationHistory, rollbackMigration, Table, ColumnBuilder,
} from "@grey-db/core";
import { requirePermission } from "../middleware/auth";

export function schemaRoutes(): Router {
    const router = Router();

    // ── Schema Browser ──────────────────────────────────────

    /** List all tables in database */
    router.get("/tables", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const schema = (req.query.schema as string) || "public";
        try {
            const result = await db.pool.query(`
        SELECT
          t.table_name,
          t.table_schema,
          obj_description((t.table_schema || '.' || t.table_name)::regclass) AS description,
          (SELECT COUNT(*) FROM information_schema.columns c WHERE c.table_name = t.table_name AND c.table_schema = t.table_schema) AS column_count,
          (SELECT reltuples::bigint FROM pg_class WHERE relname = t.table_name) AS row_estimate
        FROM information_schema.tables t
        WHERE t.table_schema = $1 AND t.table_type = 'BASE TABLE'
        ORDER BY t.table_name
      `, [schema]);
            res.json({ tables: result.rows });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get table details (columns, indexes, constraints) */
    router.get("/tables/:name", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        const { name } = req.params;
        const schema = (req.query.schema as string) || "public";
        try {
            // Get columns
            const colResult = await db.pool.query(`
        SELECT c.column_name, c.data_type, c.is_nullable, c.column_default,
               col_description((c.table_schema || '.' || c.table_name)::regclass, c.ordinal_position) AS description
        FROM information_schema.columns c
        WHERE c.table_schema = $1 AND c.table_name = $2
        ORDER BY c.ordinal_position
      `, [schema, name]);

            // Get indexes
            const idxResult = await db.pool.query(`
        SELECT indexname, indexdef FROM pg_indexes WHERE schemaname = $1 AND tablename = $2
      `, [schema, name]);

            // Get constraints
            const conResult = await db.pool.query(`
        SELECT tc.constraint_name, tc.constraint_type,
               kcu.column_name,
               ccu.table_name AS foreign_table,
               ccu.column_name AS foreign_column
        FROM information_schema.table_constraints tc
        LEFT JOIN information_schema.key_column_usage kcu ON tc.constraint_name = kcu.constraint_name
        LEFT JOIN information_schema.constraint_column_usage ccu ON tc.constraint_name = ccu.constraint_name
        WHERE tc.table_schema = $1 AND tc.table_name = $2
      `, [schema, name]);

            // Get row count
            const countResult = await db.pool.query(`SELECT COUNT(*) AS cnt FROM "${schema}"."${name}"`);

            res.json({
                name,
                schema,
                columns: colResult.rows,
                indexes: idxResult.rows,
                constraints: conResult.rows,
                rowCount: parseInt(countResult.rows[0].cnt),
            });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    // ── DDL Generation ──────────────────────────────────────

    /** Generate CREATE TABLE SQL from DSL definition */
    router.post("/generate", async (req: Request, res: Response) => {
        try {
            const { name, columns, schema: schemaName = "public", indexes, enableRLS, tenantScoped } = req.body;
            if (!name || !columns) {
                res.status(400).json({ error: "name and columns are required" });
                return;
            }

            // Reconstruct column builders from JSON
            const colBuilders: Record<string, ColumnBuilder> = {};
            for (const [colName, colDef] of Object.entries(columns) as any) {
                const builder = createColumnFromDef(colDef);
                if (builder) colBuilders[colName] = builder;
            }

            const table = greyTable(name, colBuilders, schemaName);
            if (enableRLS) table.withRLS();
            if (tenantScoped) table.withTenantScope();
            if (indexes) {
                for (const idx of indexes) {
                    table.index(idx.name, idx.columns, { unique: idx.unique, method: idx.method });
                }
            }

            const sql = generateCreateTable(table);
            res.json({ sql, table: table.definition });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    // ── Migrations ──────────────────────────────────────────

    /** Get migration history */
    router.get("/migrations", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const history = await getMigrationHistory(db.pool);
            const version = await getCurrentVersion(db.pool);
            res.json({ currentVersion: version, migrations: history });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Generate a migration (preview) */
    router.post("/migrations/preview", requirePermission("schema:write"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { currentTables = [], desiredTables = [], name = "migration" } = req.body;
            // Build Table objects from definitions
            const current = currentTables.map(buildTableFromDef);
            const desired = desiredTables.map(buildTableFromDef);

            const diff = diffSchemas(current, desired);
            const version = (await getCurrentVersion(db.pool)) + 1;
            const migration = generateMigration(name, diff, version);

            res.json({ migration, diff: { warnings: diff.warnings, safe: diff.safe, actionCount: diff.actions.length } });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Apply a migration */
    router.post("/migrations/apply", requirePermission("migration:run"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { upSQL, name = "manual_migration" } = req.body;
            if (!upSQL) {
                res.status(400).json({ error: "upSQL is required" });
                return;
            }

            const version = (await getCurrentVersion(db.pool)) + 1;
            const migration = {
                id: `${version}_${name}`,
                version,
                name,
                upSQL,
                downSQL: req.body.downSQL || "-- No down migration provided",
                createdAt: new Date(),
                safe: true,
                warnings: [],
                checksum: String(Date.now()),
            };

            await applyMigration(db.pool, migration);
            res.json({ success: true, version, migration });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Rollback the last migration */
    router.post("/migrations/rollback", requirePermission("migration:run"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const rolled = await rollbackMigration(db.pool);
            if (!rolled) {
                res.status(404).json({ error: "No migrations to rollback" });
                return;
            }
            res.json({ success: true, rolledBack: rolled });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    // ── Schema Registry ─────────────────────────────────────

    /** Save schema version */
    router.post("/registry/save", requirePermission("schema:write"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { tables, description = "" } = req.body;
            const tableObjs = (tables || []).map(buildTableFromDef);
            const version = await db.schema.saveVersion(tableObjs, description);
            res.json({ version });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get schema version history */
    router.get("/registry/history", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const history = await db.schema.getHistory();
            res.json({ versions: history });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get a specific schema version */
    router.get("/registry/:version", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const version = parseInt(req.params.version);
            const schema = await db.schema.getVersion(version);
            if (!schema) {
                res.status(404).json({ error: "Schema version not found" });
                return;
            }
            res.json(schema);
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    return router;
}

// ── Helpers ─────────────────────────────────────────────────

function createColumnFromDef(def: any): ColumnBuilder | null {
    const typeMap: Record<string, () => ColumnBuilder> = {
        uuid: () => uuid(),
        text: () => text(),
        varchar: () => varchar(def.length || 255),
        integer: () => integer(),
        bigint: () => bigint(),
        boolean: () => boolean(),
        timestamp: () => timestamp(),
        timestamptz: () => timestamptz(),
        date: () => date(),
        jsonb: () => jsonb(),
        serial: () => serial(),
        float: () => float(),
        numeric: () => numeric(def.precision, def.scale),
        bytea: () => bytea(),
        vector: () => vector(def.dimensions || 1536),
    };

    const factory = typeMap[def.type];
    if (!factory) return null;

    let col = factory();
    if (def.primaryKey) col = col.primaryKey();
    if (def.unique) col = col.unique();
    if (def.nullable === false) col = col.notNull();
    if (def.indexed) col = col.indexed();
    if (def.defaultNow) col = col.defaultNow();
    if (def.defaultUUID) col = col.defaultUUID();
    if (def.default !== undefined) col = col.default(def.default);
    if (def.references) col = col.references(def.references.table, def.references.column, def.references);
    return col;
}

function buildTableFromDef(def: any): Table {
    const cols: Record<string, ColumnBuilder> = {};
    for (const [name, colDef] of Object.entries(def.columns || {}) as any) {
        const col = createColumnFromDef(colDef);
        if (col) cols[name] = col;
    }
    const table = greyTable(def.name, cols, def.schema || "public");
    if (def.enableRLS) table.withRLS();
    if (def.tenantScoped) table.withTenantScope();
    for (const idx of def.indexes || []) {
        table.index(idx.name, idx.columns, { unique: idx.unique, method: idx.method });
    }
    return table;
}
