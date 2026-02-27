// ─────────────────────────────────────────────────────────────
// Grey DB — Main Entry Point
// Unified API for all subsystems
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";
import { createPool, closePool, GreyDBConfig, createPoolFromEnv } from "./connection";
import { SchemaRegistry } from "./schema/registry";
import { TenantEngine } from "./tenant/tenant-engine";
import { NLToSQL } from "./ai/nl-to-sql";
import { AutoDocGenerator } from "./ai/auto-doc";
import { SemanticSearch } from "./ai/semantic-search";
import { IndexManager } from "./query/index-manager";
import { SlowQueryAnalyzer } from "./query/slow-query";
import { BackupManager } from "./ops/backup";
import { RBACEngine } from "./ops/rbac";
import { ObservabilityEngine } from "./ops/observability";
import { ensureMigrationTable } from "./schema/migrations";
import { AuditTrail, ReferenceTableManager } from "./modeling/patterns";
import { ValidationEngine } from "./validation/engine";
import { RetentionEngine } from "./lifecycle/retention";
import { DeadLetterQueue } from "./ingestion/pipeline";

export class GreyDB {
    public readonly pool: Pool;
    public readonly schema: SchemaRegistry;
    public readonly tenants: TenantEngine;
    public readonly nlsql: NLToSQL;
    public readonly docs: AutoDocGenerator;
    public readonly search: SemanticSearch;
    public readonly indexes: IndexManager;
    public readonly slowQueries: SlowQueryAnalyzer;
    public readonly backups: BackupManager;
    public readonly rbac: RBACEngine;
    public readonly observability: ObservabilityEngine;
    public readonly audit: AuditTrail;
    public readonly references: ReferenceTableManager;
    public readonly validation: ValidationEngine;
    public readonly retention: RetentionEngine;
    public readonly dlq: DeadLetterQueue;

    constructor(pool: Pool) {
        this.pool = pool;
        this.schema = new SchemaRegistry(pool);
        this.tenants = new TenantEngine(pool);
        this.nlsql = new NLToSQL(pool);
        this.docs = new AutoDocGenerator(pool);
        this.search = new SemanticSearch(pool);
        this.indexes = new IndexManager(pool);
        this.slowQueries = new SlowQueryAnalyzer(pool);
        this.backups = new BackupManager(pool);
        this.rbac = new RBACEngine(pool);
        this.observability = new ObservabilityEngine(pool);
        this.audit = new AuditTrail(pool);
        this.references = new ReferenceTableManager(pool);
        this.validation = new ValidationEngine();
        this.retention = new RetentionEngine(pool);
        this.dlq = new DeadLetterQueue(pool);
    }

    /** Create a GreyDB instance from config */
    static create(config: GreyDBConfig): GreyDB {
        const pool = createPool(config);
        return new GreyDB(pool);
    }

    /** Create a GreyDB instance from environment variables */
    static fromEnv(): GreyDB {
        const pool = createPoolFromEnv();
        return new GreyDB(pool);
    }

    /** Initialize all subsystem tables */
    async init(): Promise<void> {
        await ensureMigrationTable(this.pool);
        await this.schema.init();
        await this.tenants.init();
        await this.slowQueries.init();
        await this.backups.init();
        await this.rbac.init();
        await this.observability.init();
        await this.search.init();
        await this.audit.init();
        await this.references.init();
        await this.retention.init();
        await this.dlq.init();
    }

    /** Execute a raw SQL query */
    async query(sql: string, params: any[] = []): Promise<any> {
        const start = Date.now();
        try {
            const result = await this.pool.query(sql, params);
            const duration = Date.now() - start;
            // Record metric
            await this.observability.recordQuery({
                query: sql.substring(0, 500),
                durationMs: duration,
                rowsAffected: result.rowCount ?? 0,
                cached: false,
            }).catch(() => { }); // non-blocking
            return result;
        } catch (err: any) {
            await this.observability.logError("query_error", err.message, err.stack, { sql }).catch(() => { });
            throw err;
        }
    }

    /** Close the connection pool */
    async close(): Promise<void> {
        await closePool();
    }
}

// Re-export everything
export * from "./connection";
export * from "./schema";
export * from "./tenant";
export * from "./query";
export * from "./ai";
export * from "./ops";
export * from "./modeling";
export * from "./validation";
export * from "./lifecycle";
export * from "./ingestion";
