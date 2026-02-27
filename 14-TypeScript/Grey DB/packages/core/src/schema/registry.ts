// ─────────────────────────────────────────────────────────────
// Grey DB — Schema Registry
// Versioned schemas, per-tenant overlays, rollback
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";
import { Table } from "./dsl";

export interface SchemaVersion {
  version: number;
  tables: SerializedTable[];
  createdAt: Date;
  description: string;
  tenantOverrides?: Record<string, TenantOverlay>;
}

export interface SerializedTable {
  name: string;
  schema: string;
  columns: Record<string, SerializedColumn>;
  indexes: Array<{ name: string; columns: string[]; unique?: boolean; method?: string }>;
  enableRLS: boolean;
  tenantScoped: boolean;
}

export interface SerializedColumn {
  type: string;
  constraints: Record<string, any>;
  length?: number;
  precision?: number;
  scale?: number;
}

export interface TenantOverlay {
  tenantId: string;
  customFields: Record<string, Record<string, SerializedColumn>>;  // tableName → columns
  featureFlags: Record<string, boolean>;
  config: Record<string, any>;
}

/** Serialize a Table to a plain object for storage */
export function serializeTable(table: Table): SerializedTable {
  const cols: Record<string, SerializedColumn> = {};
  for (const [name, col] of Object.entries(table.definition.columns)) {
    cols[name] = {
      type: col._type,
      constraints: { ...col._constraints },
      length: col._length,
      precision: col._precision,
      scale: col._scale,
    };
  }
  return {
    name: table.definition.name,
    schema: table.definition.schema,
    columns: cols,
    indexes: table.definition.indexes.map((i) => ({
      name: i.name,
      columns: i.columns,
      unique: i.unique,
      method: i.method,
    })),
    enableRLS: table.definition.enableRLS,
    tenantScoped: table.definition.tenantScoped,
  };
}

export class SchemaRegistry {
  constructor(private pool: Pool) {}

  async init(): Promise<void> {
    await this.pool.query(`
      CREATE TABLE IF NOT EXISTS grey_schema_versions (
        version       SERIAL PRIMARY KEY,
        tables        JSONB NOT NULL,
        description   TEXT NOT NULL DEFAULT '',
        created_at    TIMESTAMPTZ DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS grey_tenant_overlays (
        tenant_id     UUID PRIMARY KEY,
        custom_fields JSONB DEFAULT '{}',
        feature_flags JSONB DEFAULT '{}',
        config        JSONB DEFAULT '{}',
        updated_at    TIMESTAMPTZ DEFAULT NOW()
      );
    `);
  }

  /** Save a new schema version */
  async saveVersion(tables: Table[], description: string): Promise<number> {
    const serialized = tables.map(serializeTable);
    const result = await this.pool.query(
      `INSERT INTO grey_schema_versions (tables, description) VALUES ($1, $2) RETURNING version`,
      [JSON.stringify(serialized), description]
    );
    return result.rows[0].version;
  }

  /** Get a specific schema version */
  async getVersion(version: number): Promise<SchemaVersion | null> {
    const result = await this.pool.query(
      `SELECT * FROM grey_schema_versions WHERE version = $1`,
      [version]
    );
    if (result.rows.length === 0) return null;
    const row = result.rows[0];
    return {
      version: row.version,
      tables: row.tables,
      createdAt: row.created_at,
      description: row.description,
    };
  }

  /** Get the latest schema version */
  async getLatestVersion(): Promise<SchemaVersion | null> {
    const result = await this.pool.query(
      `SELECT * FROM grey_schema_versions ORDER BY version DESC LIMIT 1`
    );
    if (result.rows.length === 0) return null;
    const row = result.rows[0];
    return {
      version: row.version,
      tables: row.tables,
      createdAt: row.created_at,
      description: row.description,
    };
  }

  /** Get full schema history */
  async getHistory(): Promise<SchemaVersion[]> {
    const result = await this.pool.query(
      `SELECT * FROM grey_schema_versions ORDER BY version ASC`
    );
    return result.rows.map((row) => ({
      version: row.version,
      tables: row.tables,
      createdAt: row.created_at,
      description: row.description,
    }));
  }

  /** Set tenant overlay (custom fields, feature flags, config) */
  async setTenantOverlay(tenantId: string, overlay: Partial<TenantOverlay>): Promise<void> {
    await this.pool.query(
      `INSERT INTO grey_tenant_overlays (tenant_id, custom_fields, feature_flags, config)
       VALUES ($1, $2, $3, $4)
       ON CONFLICT (tenant_id) DO UPDATE SET
         custom_fields = COALESCE($2, grey_tenant_overlays.custom_fields),
         feature_flags = COALESCE($3, grey_tenant_overlays.feature_flags),
         config = COALESCE($4, grey_tenant_overlays.config),
         updated_at = NOW()`,
      [
        tenantId,
        JSON.stringify(overlay.customFields ?? {}),
        JSON.stringify(overlay.featureFlags ?? {}),
        JSON.stringify(overlay.config ?? {}),
      ]
    );
  }

  /** Get tenant overlay */
  async getTenantOverlay(tenantId: string): Promise<TenantOverlay | null> {
    const result = await this.pool.query(
      `SELECT * FROM grey_tenant_overlays WHERE tenant_id = $1`,
      [tenantId]
    );
    if (result.rows.length === 0) return null;
    const row = result.rows[0];
    return {
      tenantId: row.tenant_id,
      customFields: row.custom_fields,
      featureFlags: row.feature_flags,
      config: row.config,
    };
  }
}
