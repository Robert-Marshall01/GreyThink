// ─────────────────────────────────────────────────────────────
// Grey DB — Migration Engine
// Generates, tracks, and applies migrations safely
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";
import { createHash } from "crypto";
import { Table } from "./dsl";
import { generateCreateTable, generateDropTable } from "./sql-generator";
import { DiffAction, diffSchemas, SchemaDiffResult } from "./differ";

export interface Migration {
  id: string;
  version: number;
  name: string;
  upSQL: string;
  downSQL: string;
  createdAt: Date;
  appliedAt?: Date;
  safe: boolean;
  warnings: string[];
  checksum: string;
}

/** Bootstrap the migration tracking table */
export async function ensureMigrationTable(pool: Pool): Promise<void> {
  await pool.query(`
    CREATE TABLE IF NOT EXISTS "grey_migrations" (
      id            SERIAL PRIMARY KEY,
      version       INTEGER NOT NULL UNIQUE,
      name          TEXT NOT NULL,
      up_sql        TEXT NOT NULL,
      down_sql      TEXT NOT NULL,
      checksum      TEXT NOT NULL,
      safe          BOOLEAN NOT NULL DEFAULT true,
      warnings      JSONB DEFAULT '[]',
      applied_at    TIMESTAMPTZ DEFAULT NOW(),
      rolled_back   BOOLEAN DEFAULT false
    );
  `);
}

function computeChecksum(sql: string): string {
  return createHash("sha256").update(sql).digest("hex").substring(0, 16);
}

function generateActionSQL(action: DiffAction): { up: string; down: string } {
  switch (action.type) {
    case "CREATE_TABLE":
      return {
        up: generateCreateTable(action.table),
        down: generateDropTable(action.table),
      };
    case "DROP_TABLE":
      return {
        up: `DROP TABLE IF EXISTS "${action.schema}"."${action.tableName}" CASCADE;`,
        down: `-- Manual restoration required for dropped table "${action.tableName}"`,
      };
    case "ADD_COLUMN": {
      const colType = action.column._type.toUpperCase();
      const nullable = action.column._constraints.nullable !== false ? "" : " NOT NULL";
      return {
        up: `ALTER TABLE "${action.schema}"."${action.tableName}" ADD COLUMN "${action.columnName}" ${colType}${nullable};`,
        down: `ALTER TABLE "${action.schema}"."${action.tableName}" DROP COLUMN "${action.columnName}";`,
      };
    }
    case "DROP_COLUMN":
      return {
        up: `ALTER TABLE "${action.schema}"."${action.tableName}" DROP COLUMN "${action.columnName}";`,
        down: `-- Manual restoration required for dropped column "${action.columnName}"`,
      };
    case "ALTER_COLUMN": {
      const newType = action.to._type.toUpperCase();
      const oldType = action.from._type.toUpperCase();
      return {
        up: `ALTER TABLE "${action.schema}"."${action.tableName}" ALTER COLUMN "${action.columnName}" TYPE ${newType};`,
        down: `ALTER TABLE "${action.schema}"."${action.tableName}" ALTER COLUMN "${action.columnName}" TYPE ${oldType};`,
      };
    }
    case "ADD_INDEX": {
      const u = action.unique ? "UNIQUE " : "";
      const cols = action.columns.map((c) => `"${c}"`).join(", ");
      return {
        up: `CREATE ${u}INDEX "${action.indexName}" ON "${action.schema}"."${action.tableName}" (${cols});`,
        down: `DROP INDEX IF EXISTS "${action.indexName}";`,
      };
    }
    case "DROP_INDEX":
      return {
        up: `DROP INDEX IF EXISTS "${action.indexName}";`,
        down: `-- Manual restoration required for index "${action.indexName}"`,
      };
    case "ENABLE_RLS":
      return {
        up: `ALTER TABLE "${action.schema}"."${action.tableName}" ENABLE ROW LEVEL SECURITY;`,
        down: `ALTER TABLE "${action.schema}"."${action.tableName}" DISABLE ROW LEVEL SECURITY;`,
      };
    case "DISABLE_RLS":
      return {
        up: `ALTER TABLE "${action.schema}"."${action.tableName}" DISABLE ROW LEVEL SECURITY;`,
        down: `ALTER TABLE "${action.schema}"."${action.tableName}" ENABLE ROW LEVEL SECURITY;`,
      };
  }
}

/** Generate a migration from a schema diff */
export function generateMigration(name: string, diff: SchemaDiffResult, version: number): Migration {
  const upLines: string[] = [];
  const downLines: string[] = [];

  for (const action of diff.actions) {
    const { up, down } = generateActionSQL(action);
    upLines.push(up);
    downLines.push(down);
  }

  const upSQL = upLines.join("\n\n");
  const downSQL = downLines.join("\n\n");

  return {
    id: `${version}_${name.replace(/\s+/g, "_").toLowerCase()}`,
    version,
    name,
    upSQL,
    downSQL,
    createdAt: new Date(),
    safe: diff.safe,
    warnings: diff.warnings.map((w) => w.message),
    checksum: computeChecksum(upSQL),
  };
}

/** Apply a migration within a transaction */
export async function applyMigration(pool: Pool, migration: Migration, opts?: { dryRun?: boolean }): Promise<void> {
  const client = await pool.connect();
  try {
    await client.query("BEGIN");

    // Acquire advisory lock to prevent concurrent migrations
    await client.query("SELECT pg_advisory_xact_lock(2147483647)");

    // Guard against duplicate version
    const existing = await client.query(
      "SELECT version FROM grey_migrations WHERE version = $1 AND rolled_back = false",
      [migration.version]
    );
    if (existing.rows.length > 0) {
      throw new Error(`Migration version ${migration.version} already applied`);
    }

    if (opts?.dryRun) {
      await client.query("ROLLBACK");
      return;
    }

    // Execute the migration SQL
    await client.query(migration.upSQL);

    // Record the migration
    await client.query(
      `INSERT INTO grey_migrations (version, name, up_sql, down_sql, checksum, safe, warnings)
       VALUES ($1, $2, $3, $4, $5, $6, $7)`,
      [migration.version, migration.name, migration.upSQL, migration.downSQL, migration.checksum, migration.safe, JSON.stringify(migration.warnings)]
    );

    await client.query("COMMIT");
  } catch (err) {
    await client.query("ROLLBACK");
    throw err;
  } finally {
    client.release();
  }
}

/** Rollback the last migration */
export async function rollbackMigration(pool: Pool): Promise<Migration | null> {
  const client = await pool.connect();
  try {
    await client.query("BEGIN");

    // Acquire advisory lock to prevent concurrent migrations
    await client.query("SELECT pg_advisory_xact_lock(2147483647)");

    const result = await client.query(
      `SELECT * FROM grey_migrations WHERE rolled_back = false ORDER BY version DESC LIMIT 1`
    );
    if (result.rows.length === 0) {
      await client.query("ROLLBACK");
      return null;
    }

    const row = result.rows[0];
    await client.query(row.down_sql);
    await client.query(`UPDATE grey_migrations SET rolled_back = true WHERE id = $1`, [row.id]);
    await client.query("COMMIT");

    return {
      id: row.id,
      version: row.version,
      name: row.name,
      upSQL: row.up_sql,
      downSQL: row.down_sql,
      createdAt: row.applied_at,
      safe: row.safe,
      warnings: row.warnings,
      checksum: row.checksum,
    };
  } catch (err) {
    await client.query("ROLLBACK");
    throw err;
  } finally {
    client.release();
  }
}

/** Get migration history */
export async function getMigrationHistory(pool: Pool): Promise<Migration[]> {
  const result = await pool.query(
    `SELECT * FROM grey_migrations ORDER BY version ASC`
  );
  return result.rows.map((row) => ({
    id: row.id,
    version: row.version,
    name: row.name,
    upSQL: row.up_sql,
    downSQL: row.down_sql,
    createdAt: row.applied_at,
    appliedAt: row.applied_at,
    safe: row.safe,
    warnings: row.warnings || [],
    checksum: row.checksum,
  }));
}

/** Get the current version number */
export async function getCurrentVersion(pool: Pool): Promise<number> {
  const result = await pool.query(
    `SELECT COALESCE(MAX(version), 0) as version FROM grey_migrations WHERE rolled_back = false`
  );
  return result.rows[0].version;
}
