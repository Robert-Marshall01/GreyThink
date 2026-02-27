// ─────────────────────────────────────────────────────────────
// Grey DB — Data Modeling Patterns
// SCD, soft deletes, event sourcing, audit trails, enums
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";
import {
    greyTable, Table, uuid, text, integer, boolean,
    timestamptz, jsonb, ColumnBuilder,
} from "../schema/dsl";

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 1. Slowly Changing Dimensions (SCD Type 2)
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Configuration for SCD Type 2 versioned column tracking.
 * Tracks historical changes to dimension attributes over time.
 *
 * SCD Type 1 = overwrite in place (default SQL UPDATE)
 * SCD Type 2 = insert new row with validity range (this)
 * SCD Type 3 = keep previous + current in same row (limited)
 */
export interface SCDConfig {
    /** Base table name (history table will be `{name}_history`) */
    tableName: string;
    /** Columns whose changes trigger a new history version */
    trackedColumns: string[];
    /** Schema namespace */
    schema?: string;
}

/**
 * Generate SCD Type 2 history table + trigger SQL.
 *
 * Design decisions:
 * - Uses `valid_from` / `valid_to` (NULL = current) temporal range
 * - Partial unique index ensures exactly one current row per entity
 * - Trigger fires BEFORE UPDATE so old version is captured automatically
 * - `is_current` boolean avoids expensive IS NULL scans
 */
export function generateSCDHistory(config: SCDConfig): {
    historyTableSQL: string;
    triggerSQL: string;
    queryCurrentSQL: string;
    queryAsOfSQL: string;
} {
    const { tableName, trackedColumns, schema = "public" } = config;
    const historyTable = `${tableName}_history`;
    const fq = `"${schema}"."${historyTable}"`;
    const src = `"${schema}"."${tableName}"`;
    const trackedCols = trackedColumns.map((c) => `"${c}"`).join(", ");

    const historyTableSQL = `
CREATE TABLE IF NOT EXISTS ${fq} (
  history_id    UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  entity_id     UUID NOT NULL,
  ${trackedColumns.map((c) => `"${c}" TEXT`).join(",\n  ")},
  valid_from    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  valid_to      TIMESTAMPTZ,
  is_current    BOOLEAN NOT NULL DEFAULT true,
  changed_by    UUID,
  change_reason TEXT,
  CONSTRAINT chk_valid_range CHECK (valid_to IS NULL OR valid_to > valid_from)
);

CREATE INDEX "idx_${historyTable}_entity" ON ${fq} ("entity_id");
CREATE INDEX "idx_${historyTable}_temporal" ON ${fq} ("entity_id", "valid_from", "valid_to");
CREATE UNIQUE INDEX "idx_${historyTable}_current" ON ${fq} ("entity_id") WHERE is_current = true;
`.trim();

    const triggerSQL = `
CREATE OR REPLACE FUNCTION ${schema}.fn_${tableName}_scd_trigger()
RETURNS TRIGGER AS $$
BEGIN
  -- Only fire when tracked columns actually changed
  IF ${trackedColumns.map((c) => `OLD."${c}" IS DISTINCT FROM NEW."${c}"`).join(" OR ")} THEN
    -- Close out the current history row
    UPDATE ${fq}
    SET valid_to = NOW(), is_current = false
    WHERE entity_id = OLD.id AND is_current = true;

    -- Insert new current version
    INSERT INTO ${fq} (entity_id, ${trackedCols}, valid_from, is_current)
    VALUES (NEW.id, ${trackedColumns.map((c) => `NEW."${c}"`).join(", ")}, NOW(), true);
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER trg_${tableName}_scd
  AFTER UPDATE ON ${src}
  FOR EACH ROW
  EXECUTE FUNCTION ${schema}.fn_${tableName}_scd_trigger();
`.trim();

    const queryCurrentSQL = `SELECT * FROM ${fq} WHERE entity_id = $1 AND is_current = true`;
    const queryAsOfSQL = `SELECT * FROM ${fq} WHERE entity_id = $1 AND valid_from <= $2 AND (valid_to IS NULL OR valid_to > $2)`;

    return { historyTableSQL, triggerSQL, queryCurrentSQL, queryAsOfSQL };
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 2. Soft Delete Pattern
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Soft delete columns & helpers.
 *
 * Philosophy:
 * - `deleted_at` (nullable timestamptz) marks soft-deleted rows
 * - `deleted_by` tracks who deleted it (audit requirement)
 * - Partial index WHERE deleted_at IS NULL keeps active queries fast
 * - All "default" queries should filter deleted_at IS NULL
 * - Periodic hard-delete job cleans rows older than retention window
 *
 * Tradeoffs vs hard deletes:
 * - ✅ Undo/restore capability
 * - ✅ Audit trail preserved
 * - ✅ FK integrity maintained (no cascade surprises)
 * - ❌ Table bloat without cleanup
 * - ❌ Every query must filter — partial index mitigates this
 * - ❌ UNIQUE constraints need to be partial (on active rows only)
 */
export interface SoftDeleteColumns {
    deletedAt: ColumnBuilder;
    deletedBy: ColumnBuilder;
}

export function softDeleteColumns(): SoftDeleteColumns {
    return {
        deletedAt: timestamptz().nullable(),
        deletedBy: uuid().nullable(),
    };
}

/**
 * Generate index & query helpers for a soft-delete table.
 */
export function softDeleteHelpers(tableName: string, schema = "public") {
    const fq = `"${schema}"."${tableName}"`;
    return {
        /** Partial index: only active (non-deleted) rows */
        activeIndex: `CREATE INDEX "idx_${tableName}_active" ON ${fq} ("id") WHERE "deletedAt" IS NULL;`,

        /** Partial unique: enforce uniqueness only on active rows */
        activeUnique: (column: string) =>
            `CREATE UNIQUE INDEX "idx_${tableName}_${column}_active" ON ${fq} ("${column}") WHERE "deletedAt" IS NULL;`,

        /** Standard active-only filter */
        activeFilter: `"deletedAt" IS NULL`,

        /** SQL to soft-delete */
        softDeleteSQL: `UPDATE ${fq} SET "deletedAt" = NOW(), "deletedBy" = $2 WHERE id = $1 AND "deletedAt" IS NULL;`,

        /** SQL to restore */
        restoreSQL: `UPDATE ${fq} SET "deletedAt" = NULL, "deletedBy" = NULL WHERE id = $1;`,

        /** Hard-delete rows older than retention */
        purgeSQL: (retentionDays: number) =>
            `DELETE FROM ${fq} WHERE "deletedAt" IS NOT NULL AND "deletedAt" < NOW() - INTERVAL '${retentionDays} days';`,
    };
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 3. Event Tables vs State Tables
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * An event table records immutable facts about things that happened.
 * Events are append-only — never updated or deleted.
 *
 * Use events when:
 * - You need a complete history of every change
 * - Multiple consumers derive different views from the same events
 * - The "current state" can be reconstructed by replaying events
 * - Compliance requires an immutable record
 *
 * Use state tables when:
 * - You only need the current snapshot
 * - Query patterns are dominated by "get latest" reads
 * - Storage cost of keeping all events is prohibitive
 * - Relationships require FK integrity (events can't enforce FKs reliably)
 */
export type EventCategory = "domain" | "system" | "integration" | "audit";

export interface EventTableConfig {
    tableName: string;
    category: EventCategory;
    schema?: string;
    tenantScoped?: boolean;
    /** Partition by time range (recommended for high-volume events) */
    partitionInterval?: "daily" | "weekly" | "monthly";
}

export function generateEventTable(config: EventTableConfig): {
    table: Table;
    createSQL: string;
    appendSQL: string;
    queryByEntitySQL: string;
    queryByTypeSQL: string;
} {
    const { tableName, category, schema = "public", tenantScoped = false } = config;
    const fq = `"${schema}"."${tableName}"`;

    const columns: Record<string, ColumnBuilder> = {
        id: uuid().primaryKey().defaultUUID(),
        eventType: text().notNull().indexed(),
        entityType: text().notNull(),
        entityId: uuid().notNull(),
        payload: jsonb().notNull(),
        metadata: jsonb().default("{}"),
        occurredAt: timestamptz().defaultNow().notNull(),
        processedAt: timestamptz().nullable(),
        version: integer().default(1),
    };

    if (tenantScoped) {
        columns.tenantId = uuid().notNull();
    }

    const table = greyTable(tableName, columns, schema)
        .index(`idx_${tableName}_entity`, ["entityType", "entityId"])
        .index(`idx_${tableName}_type`, ["eventType", "occurredAt"])
        .index(`idx_${tableName}_occurred`, ["occurredAt"]);

    if (tenantScoped) {
        table.withTenantScope();
        table.index(`idx_${tableName}_tenant`, ["tenantId", "occurredAt"]);
    }

    table.meta("category", category);
    table.meta("immutable", true);

    // The table is append-only: we use a RULE to prevent UPDATE/DELETE
    const createSQL = `
-- Event table: ${tableName} (${category})
-- Events are IMMUTABLE. Updates and deletes are blocked by rules.

${fq.includes("partition") ? "" : `CREATE TABLE IF NOT EXISTS ${fq} (
  "id"          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  "eventType"   TEXT NOT NULL,
  "entityType"  TEXT NOT NULL,
  "entityId"    UUID NOT NULL,
  "payload"     JSONB NOT NULL,
  "metadata"    JSONB DEFAULT '{}',
  "occurredAt"  TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  "processedAt" TIMESTAMPTZ,
  "version"     INTEGER DEFAULT 1${tenantScoped ? ',\n  "tenantId"    UUID NOT NULL' : ""}
);`}

CREATE INDEX IF NOT EXISTS "idx_${tableName}_entity" ON ${fq} ("entityType", "entityId");
CREATE INDEX IF NOT EXISTS "idx_${tableName}_type" ON ${fq} ("eventType", "occurredAt");
CREATE INDEX IF NOT EXISTS "idx_${tableName}_occurred" ON ${fq} ("occurredAt");

-- Immutability rules — prevent mutation of event records
CREATE OR REPLACE RULE prevent_update_${tableName} AS
  ON UPDATE TO ${fq} DO INSTEAD NOTHING;

CREATE OR REPLACE RULE prevent_delete_${tableName} AS
  ON DELETE TO ${fq} DO INSTEAD NOTHING;
`.trim();

    const tenantCol = tenantScoped ? ', "tenantId"' : "";
    const tenantParam = tenantScoped ? ", $6" : "";

    const appendSQL = `
INSERT INTO ${fq} ("eventType", "entityType", "entityId", "payload", "metadata"${tenantCol})
VALUES ($1, $2, $3, $4, $5${tenantParam})
RETURNING *;
`.trim();

    const queryByEntitySQL = `
SELECT * FROM ${fq}
WHERE "entityType" = $1 AND "entityId" = $2
ORDER BY "occurredAt" ASC;
`.trim();

    const queryByTypeSQL = `
SELECT * FROM ${fq}
WHERE "eventType" = $1
  AND "occurredAt" BETWEEN $2 AND $3
ORDER BY "occurredAt" ASC
LIMIT $4;
`.trim();

    return { table, createSQL, appendSQL, queryByEntitySQL, queryByTypeSQL };
}

/**
 * State projection: materializes event stream into current state.
 *
 * Pattern: read all events for an entity, apply a reducer, persist result.
 * This is the fundamental "event sourcing" read-model pattern.
 */
export interface StateProjection<TState> {
    entityType: string;
    initialState: TState;
    reducer: (state: TState, event: { eventType: string; payload: any }) => TState;
}

export async function projectState<TState>(
    pool: Pool,
    eventTable: string,
    projection: StateProjection<TState>,
    entityId: string,
    schema = "public"
): Promise<TState> {
    const result = await pool.query(
        `SELECT "eventType", "payload" FROM "${schema}"."${eventTable}"
     WHERE "entityType" = $1 AND "entityId" = $2
     ORDER BY "occurredAt" ASC`,
        [projection.entityType, entityId]
    );

    return result.rows.reduce(
        (state, row) => projection.reducer(state, { eventType: row.eventType, payload: row.payload }),
        { ...projection.initialState }
    );
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 4. Immutable Audit Trail
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Audit trail engine — append-only log of who did what, when.
 *
 * Design:
 * - Separate from event sourcing (audit is compliance, events are domain)
 * - Records both old and new values (full diff)
 * - Immutable via PostgreSQL RULE (no UPDATE/DELETE)
 * - Indexed by entity for fast "show me the history of X" queries
 * - Tenant-scoped by default
 */
export interface AuditEntry {
    action: "create" | "update" | "delete" | "restore" | "access";
    entityType: string;
    entityId: string;
    tenantId?: string;
    actorId?: string;
    actorType?: "user" | "system" | "api_key" | "webhook";
    oldValues?: Record<string, any>;
    newValues?: Record<string, any>;
    metadata?: Record<string, any>;
    ipAddress?: string;
    userAgent?: string;
}

export class AuditTrail {
    private tableName: string;
    private schema: string;

    constructor(private pool: Pool, tableName = "grey_audit_trail", schema = "public") {
        this.tableName = tableName;
        this.schema = schema;
    }

    async init(): Promise<void> {
        const fq = `"${this.schema}"."${this.tableName}"`;
        await this.pool.query(`
      CREATE TABLE IF NOT EXISTS ${fq} (
        id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        action      TEXT NOT NULL,
        entity_type TEXT NOT NULL,
        entity_id   UUID NOT NULL,
        tenant_id   UUID,
        actor_id    UUID,
        actor_type  TEXT DEFAULT 'user',
        old_values  JSONB,
        new_values  JSONB,
        metadata    JSONB DEFAULT '{}',
        ip_address  VARCHAR(45),
        user_agent  TEXT,
        created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
      );

      CREATE INDEX IF NOT EXISTS "idx_${this.tableName}_entity"
        ON ${fq} ("entity_type", "entity_id");
      CREATE INDEX IF NOT EXISTS "idx_${this.tableName}_actor"
        ON ${fq} ("actor_id", "created_at");
      CREATE INDEX IF NOT EXISTS "idx_${this.tableName}_tenant"
        ON ${fq} ("tenant_id", "created_at");
      CREATE INDEX IF NOT EXISTS "idx_${this.tableName}_created"
        ON ${fq} ("created_at");

      -- Immutability: prevent mutation of audit records
      CREATE OR REPLACE RULE prevent_update_${this.tableName} AS
        ON UPDATE TO ${fq} DO INSTEAD NOTHING;
      CREATE OR REPLACE RULE prevent_delete_${this.tableName} AS
        ON DELETE TO ${fq} DO INSTEAD NOTHING;
    `);
    }

    /** Record an audit entry */
    async record(entry: AuditEntry): Promise<string> {
        const fq = `"${this.schema}"."${this.tableName}"`;
        const result = await this.pool.query(
            `INSERT INTO ${fq}
         (action, entity_type, entity_id, tenant_id, actor_id, actor_type,
          old_values, new_values, metadata, ip_address, user_agent)
       VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
       RETURNING id`,
            [
                entry.action, entry.entityType, entry.entityId,
                entry.tenantId || null, entry.actorId || null,
                entry.actorType || "user",
                entry.oldValues ? JSON.stringify(entry.oldValues) : null,
                entry.newValues ? JSON.stringify(entry.newValues) : null,
                JSON.stringify(entry.metadata || {}),
                entry.ipAddress || null, entry.userAgent || null,
            ]
        );
        return result.rows[0].id;
    }

    /** Compute diff between old and new values (only changed fields) */
    static computeDiff(
        oldValues: Record<string, any>,
        newValues: Record<string, any>
    ): { old: Record<string, any>; new: Record<string, any> } {
        const diffOld: Record<string, any> = {};
        const diffNew: Record<string, any> = {};

        for (const key of new Set([...Object.keys(oldValues), ...Object.keys(newValues)])) {
            const o = oldValues[key];
            const n = newValues[key];
            if (JSON.stringify(o) !== JSON.stringify(n)) {
                diffOld[key] = o;
                diffNew[key] = n;
            }
        }

        return { old: diffOld, new: diffNew };
    }

    /** Query audit trail for an entity */
    async getEntityHistory(entityType: string, entityId: string, limit = 100): Promise<any[]> {
        const fq = `"${this.schema}"."${this.tableName}"`;
        const result = await this.pool.query(
            `SELECT * FROM ${fq}
       WHERE entity_type = $1 AND entity_id = $2
       ORDER BY created_at DESC
       LIMIT $3`,
            [entityType, entityId, limit]
        );
        return result.rows;
    }

    /** Query audit trail for a tenant (time-bounded for performance) */
    async getTenantActivity(
        tenantId: string,
        since: Date,
        until: Date = new Date(),
        limit = 500
    ): Promise<any[]> {
        const fq = `"${this.schema}"."${this.tableName}"`;
        const result = await this.pool.query(
            `SELECT * FROM ${fq}
       WHERE tenant_id = $1 AND created_at BETWEEN $2 AND $3
       ORDER BY created_at DESC
       LIMIT $4`,
            [tenantId, since, until, limit]
        );
        return result.rows;
    }

    /** Query audit trail for an actor */
    async getActorActivity(actorId: string, limit = 100): Promise<any[]> {
        const fq = `"${this.schema}"."${this.tableName}"`;
        const result = await this.pool.query(
            `SELECT * FROM ${fq}
       WHERE actor_id = $1
       ORDER BY created_at DESC
       LIMIT $2`,
            [actorId, limit]
        );
        return result.rows;
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 5. Reference Tables & Domain Enumerations
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Reference tables hold controlled vocabularies and domain enumerations.
 *
 * Why reference tables instead of CHECK constraints or PG enums:
 * - CHECK: can't add values without ALTER TABLE (DDL migration)
 * - PG ENUM: can add values but can't remove or rename them
 * - Reference table: full CRUD, audit trail, internationalization support
 *
 * Trade-off: extra JOIN vs simpler ALTER. At scale, reference tables win
 * because business users can manage values without migrations.
 */
export interface EnumDefinition {
    /** Domain name (e.g., "order_status", "priority_level") */
    domain: string;
    /** Enum values with optional metadata */
    values: EnumValue[];
}

export interface EnumValue {
    code: string;
    label: string;
    description?: string;
    sortOrder?: number;
    active?: boolean;
    metadata?: Record<string, any>;
}

export class ReferenceTableManager {
    constructor(private pool: Pool, private schema = "public") { }

    async init(): Promise<void> {
        const fq = `"${this.schema}"."grey_reference_values"`;
        await this.pool.query(`
      CREATE TABLE IF NOT EXISTS ${fq} (
        id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        domain      TEXT NOT NULL,
        code        TEXT NOT NULL,
        label       TEXT NOT NULL,
        description TEXT,
        sort_order  INTEGER DEFAULT 0,
        active      BOOLEAN DEFAULT true,
        metadata    JSONB DEFAULT '{}',
        created_at  TIMESTAMPTZ DEFAULT NOW(),
        updated_at  TIMESTAMPTZ DEFAULT NOW(),
        UNIQUE(domain, code)
      );

      CREATE INDEX IF NOT EXISTS "idx_ref_domain" ON ${fq} ("domain");
      CREATE INDEX IF NOT EXISTS "idx_ref_active" ON ${fq} ("domain", "active")
        WHERE active = true;
    `);
    }

    /** Seed a domain with values (idempotent via ON CONFLICT) */
    async seedDomain(def: EnumDefinition): Promise<void> {
        const fq = `"${this.schema}"."grey_reference_values"`;
        for (const val of def.values) {
            await this.pool.query(
                `INSERT INTO ${fq} (domain, code, label, description, sort_order, active, metadata)
         VALUES ($1, $2, $3, $4, $5, $6, $7)
         ON CONFLICT (domain, code) DO UPDATE SET
           label = EXCLUDED.label,
           description = EXCLUDED.description,
           sort_order = EXCLUDED.sort_order,
           active = EXCLUDED.active,
           metadata = EXCLUDED.metadata,
           updated_at = NOW()`,
                [
                    def.domain, val.code, val.label,
                    val.description || null, val.sortOrder ?? 0,
                    val.active ?? true, JSON.stringify(val.metadata || {}),
                ]
            );
        }
    }

    /** Get all active values for a domain */
    async getDomain(domain: string, includeInactive = false): Promise<EnumValue[]> {
        const fq = `"${this.schema}"."grey_reference_values"`;
        const activeFilter = includeInactive ? "" : " AND active = true";
        const result = await this.pool.query(
            `SELECT code, label, description, sort_order, active, metadata
       FROM ${fq}
       WHERE domain = $1${activeFilter}
       ORDER BY sort_order ASC, label ASC`,
            [domain]
        );
        return result.rows.map((r) => ({
            code: r.code,
            label: r.label,
            description: r.description,
            sortOrder: r.sort_order,
            active: r.active,
            metadata: r.metadata,
        }));
    }

    /** Validate a code belongs to a domain */
    async validate(domain: string, code: string): Promise<boolean> {
        const fq = `"${this.schema}"."grey_reference_values"`;
        const result = await this.pool.query(
            `SELECT 1 FROM ${fq} WHERE domain = $1 AND code = $2 AND active = true`,
            [domain, code]
        );
        return result.rows.length > 0;
    }

    /** Deactivate (soft-delete) a value */
    async deactivate(domain: string, code: string): Promise<void> {
        const fq = `"${this.schema}"."grey_reference_values"`;
        await this.pool.query(
            `UPDATE ${fq} SET active = false, updated_at = NOW()
       WHERE domain = $1 AND code = $2`,
            [domain, code]
        );
    }

    /** List all domains */
    async listDomains(): Promise<{ domain: string; count: number }[]> {
        const fq = `"${this.schema}"."grey_reference_values"`;
        const result = await this.pool.query(
            `SELECT domain, COUNT(*) AS count FROM ${fq} GROUP BY domain ORDER BY domain`
        );
        return result.rows.map((r) => ({ domain: r.domain, count: parseInt(r.count) }));
    }
}
