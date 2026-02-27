// ─────────────────────────────────────────────────────────────
// Grey DB — Tenant Engine
// Multi-tenant isolation, RLS, per-tenant configuration
// ─────────────────────────────────────────────────────────────

import { Pool, PoolClient } from "pg";

export type IsolationStrategy = "shared" | "schema" | "hybrid";

export interface TenantConfig {
  id: string;
  name: string;
  slug: string;
  isolationStrategy: IsolationStrategy;
  featureFlags: Record<string, boolean>;
  storageLimitMB: number;
  config: Record<string, any>;
  createdAt: Date;
  active: boolean;
}

export interface TenantUsage {
  tenantId: string;
  rowCount: number;
  storageMB: number;
  queryCount: number;
  lastActiveAt: Date;
}

export class TenantEngine {
  constructor(private pool: Pool) { }

  /** Bootstrap tenant management tables */
  async init(): Promise<void> {
    await this.pool.query(`
      CREATE TABLE IF NOT EXISTS grey_tenants (
        id                UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        name              TEXT NOT NULL,
        slug              TEXT NOT NULL UNIQUE,
        isolation_strategy TEXT NOT NULL DEFAULT 'shared',
        feature_flags     JSONB DEFAULT '{}',
        storage_limit_mb  INTEGER DEFAULT 1024,
        config            JSONB DEFAULT '{}',
        created_at        TIMESTAMPTZ DEFAULT NOW(),
        active            BOOLEAN DEFAULT true
      );

      CREATE TABLE IF NOT EXISTS grey_tenant_usage (
        tenant_id   UUID REFERENCES grey_tenants(id),
        row_count   BIGINT DEFAULT 0,
        storage_mb  NUMERIC DEFAULT 0,
        query_count BIGINT DEFAULT 0,
        last_active TIMESTAMPTZ DEFAULT NOW(),
        PRIMARY KEY (tenant_id)
      );
    `);
  }

  /** Create a new tenant */
  async createTenant(config: Omit<TenantConfig, "id" | "createdAt">): Promise<TenantConfig> {
    const result = await this.pool.query(
      `INSERT INTO grey_tenants (name, slug, isolation_strategy, feature_flags, storage_limit_mb, config, active)
       VALUES ($1, $2, $3, $4, $5, $6, $7)
       RETURNING *`,
      [config.name, config.slug, config.isolationStrategy, JSON.stringify(config.featureFlags), config.storageLimitMB, JSON.stringify(config.config), config.active]
    );
    const row = result.rows[0];
    return this.rowToTenant(row);
  }

  /** List all tenants */
  async listTenants(): Promise<TenantConfig[]> {
    const result = await this.pool.query(`SELECT * FROM grey_tenants ORDER BY created_at ASC`);
    return result.rows.map(this.rowToTenant);
  }

  /** Get a single tenant */
  async getTenant(id: string): Promise<TenantConfig | null> {
    const result = await this.pool.query(`SELECT * FROM grey_tenants WHERE id = $1`, [id]);
    if (result.rows.length === 0) return null;
    return this.rowToTenant(result.rows[0]);
  }

  /** Update tenant config */
  async updateTenant(id: string, updates: Partial<TenantConfig>): Promise<TenantConfig | null> {
    const fields: string[] = [];
    const values: any[] = [];
    let idx = 1;

    if (updates.name !== undefined) { fields.push(`name = $${idx++}`); values.push(updates.name); }
    if (updates.slug !== undefined) { fields.push(`slug = $${idx++}`); values.push(updates.slug); }
    if (updates.isolationStrategy !== undefined) { fields.push(`isolation_strategy = $${idx++}`); values.push(updates.isolationStrategy); }
    if (updates.featureFlags !== undefined) { fields.push(`feature_flags = $${idx++}`); values.push(JSON.stringify(updates.featureFlags)); }
    if (updates.storageLimitMB !== undefined) { fields.push(`storage_limit_mb = $${idx++}`); values.push(updates.storageLimitMB); }
    if (updates.config !== undefined) { fields.push(`config = $${idx++}`); values.push(JSON.stringify(updates.config)); }
    if (updates.active !== undefined) { fields.push(`active = $${idx++}`); values.push(updates.active); }

    if (fields.length === 0) return this.getTenant(id);

    values.push(id);
    const result = await this.pool.query(
      `UPDATE grey_tenants SET ${fields.join(", ")} WHERE id = $${idx} RETURNING *`,
      values
    );
    if (result.rows.length === 0) return null;
    return this.rowToTenant(result.rows[0]);
  }

  /** Delete a tenant */
  async deleteTenant(id: string): Promise<boolean> {
    const result = await this.pool.query(`DELETE FROM grey_tenants WHERE id = $1`, [id]);
    return (result.rowCount ?? 0) > 0;
  }

  // ── RLS Policy Management ──────────────────────────────────

  /** Generate and apply RLS policies for a table */
  async applyRLSPolicy(tableName: string, schema = "public"): Promise<string> {
    const sql = [
      `ALTER TABLE "${schema}"."${tableName}" ENABLE ROW LEVEL SECURITY;`,
      `DROP POLICY IF EXISTS tenant_isolation ON "${schema}"."${tableName}";`,
      `CREATE POLICY tenant_isolation ON "${schema}"."${tableName}"`,
      `  FOR ALL`,
      `  USING (tenant_id = current_setting('grey.tenant_id')::uuid)`,
      `  WITH CHECK (tenant_id = current_setting('grey.tenant_id')::uuid);`,
    ].join("\n");

    await this.pool.query(sql);
    return sql;
  }

  /** Remove RLS policies from a table */
  async removeRLSPolicy(tableName: string, schema = "public"): Promise<void> {
    await this.pool.query(`
      DROP POLICY IF EXISTS tenant_isolation ON "${schema}"."${tableName}";
      ALTER TABLE "${schema}"."${tableName}" DISABLE ROW LEVEL SECURITY;
    `);
  }

  // ── Tenant Context ─────────────────────────────────────────

  /**
   * Execute a function within a tenant context.
   * Sets the RLS session variable so all queries are scoped.
   * Uses a transaction because SET LOCAL requires transaction context.
   */
  async withTenant<T>(tenantId: string, fn: (client: PoolClient) => Promise<T>): Promise<T> {
    const client = await this.pool.connect();
    try {
      await client.query("BEGIN");
      await client.query(`SET LOCAL grey.tenant_id = '${tenantId}'`);
      const result = await fn(client);
      await client.query("COMMIT");
      return result;
    } catch (err) {
      await client.query("ROLLBACK");
      throw err;
    } finally {
      client.release();
    }
  }

  /**
   * Execute within a tenant context + transaction.
   */
  async withTenantTransaction<T>(tenantId: string, fn: (client: PoolClient) => Promise<T>): Promise<T> {
    const client = await this.pool.connect();
    try {
      await client.query("BEGIN");
      await client.query(`SET LOCAL grey.tenant_id = '${tenantId}'`);
      const result = await fn(client);
      await client.query("COMMIT");
      return result;
    } catch (err) {
      await client.query("ROLLBACK");
      throw err;
    } finally {
      client.release();
    }
  }

  // ── Schema-per-tenant (hybrid mode) ─────────────────────

  /** Create a dedicated schema for a large tenant */
  async createTenantSchema(tenantId: string): Promise<string> {
    const schemaName = `tenant_${tenantId.replace(/-/g, "_")}`;
    await this.pool.query(`CREATE SCHEMA IF NOT EXISTS "${schemaName}"`);
    await this.updateTenant(tenantId, { isolationStrategy: "schema" });
    return schemaName;
  }

  // ── Usage tracking ──────────────────────────────────────

  async getUsage(tenantId: string): Promise<TenantUsage | null> {
    const result = await this.pool.query(
      `SELECT * FROM grey_tenant_usage WHERE tenant_id = $1`,
      [tenantId]
    );
    if (result.rows.length === 0) return null;
    const row = result.rows[0];
    return {
      tenantId: row.tenant_id,
      rowCount: parseInt(row.row_count),
      storageMB: parseFloat(row.storage_mb),
      queryCount: parseInt(row.query_count),
      lastActiveAt: row.last_active,
    };
  }

  async incrementQueryCount(tenantId: string): Promise<void> {
    await this.pool.query(
      `INSERT INTO grey_tenant_usage (tenant_id, query_count, last_active)
       VALUES ($1, 1, NOW())
       ON CONFLICT (tenant_id) DO UPDATE SET
         query_count = grey_tenant_usage.query_count + 1,
         last_active = NOW()`,
      [tenantId]
    );
  }

  private rowToTenant(row: any): TenantConfig {
    return {
      id: row.id,
      name: row.name,
      slug: row.slug,
      isolationStrategy: row.isolation_strategy,
      featureFlags: row.feature_flags || {},
      storageLimitMB: row.storage_limit_mb,
      config: row.config || {},
      createdAt: row.created_at,
      active: row.active,
    };
  }
}
