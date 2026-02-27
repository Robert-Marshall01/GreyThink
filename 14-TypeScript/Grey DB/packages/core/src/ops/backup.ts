// ─────────────────────────────────────────────────────────────
// Grey DB — Backup Manager
// Logical backups, per-tenant backups, restore flows
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";
import { execSync } from "child_process";
import * as fs from "fs";
import * as path from "path";

export interface BackupRecord {
    id: number;
    tenantId?: string;
    type: "full" | "tenant" | "table";
    format: "sql" | "custom" | "json";
    filePath: string;
    sizeBytes: number;
    tablesIncluded: string[];
    createdAt: Date;
    status: "completed" | "failed" | "in_progress";
    metadata: Record<string, any>;
}

export class BackupManager {
    private backupDir: string;

    constructor(private pool: Pool, backupDir = "./backups") {
        this.backupDir = backupDir;
    }

    async init(): Promise<void> {
        if (!fs.existsSync(this.backupDir)) {
            fs.mkdirSync(this.backupDir, { recursive: true });
        }

        await this.pool.query(`
      CREATE TABLE IF NOT EXISTS grey_backups (
        id              SERIAL PRIMARY KEY,
        tenant_id       UUID,
        backup_type     TEXT NOT NULL DEFAULT 'full',
        format          TEXT NOT NULL DEFAULT 'json',
        file_path       TEXT NOT NULL,
        size_bytes      BIGINT DEFAULT 0,
        tables_included TEXT[] DEFAULT '{}',
        status          TEXT DEFAULT 'in_progress',
        metadata        JSONB DEFAULT '{}',
        created_at      TIMESTAMPTZ DEFAULT NOW()
      );
    `);
    }

    /** Create a full logical backup as JSON */
    async createFullBackup(schema = "public"): Promise<BackupRecord> {
        const timestamp = new Date().toISOString().replace(/[:.]/g, "-");
        const fileName = `grey_backup_full_${timestamp}.json`;
        const filePath = path.join(this.backupDir, fileName);

        // Record backup start
        const insertResult = await this.pool.query(
            `INSERT INTO grey_backups (backup_type, format, file_path, status) VALUES ('full', 'json', $1, 'in_progress') RETURNING id`,
            [filePath]
        );
        const backupId = insertResult.rows[0].id;

        try {
            // Get all tables
            const tablesResult = await this.pool.query(
                `SELECT table_name FROM information_schema.tables WHERE table_schema = $1 AND table_type = 'BASE TABLE' ORDER BY table_name`,
                [schema]
            );
            const tableNames = tablesResult.rows.map((r) => r.table_name);

            const backup: Record<string, any> = {
                version: "1.0",
                type: "full",
                schema,
                timestamp: new Date().toISOString(),
                tables: {},
            };

            for (const table of tableNames) {
                const dataResult = await this.pool.query(`SELECT * FROM "${schema}"."${table}"`);
                backup.tables[table] = {
                    rowCount: dataResult.rows.length,
                    data: dataResult.rows,
                };
            }

            const json = JSON.stringify(backup, null, 2);
            fs.writeFileSync(filePath, json);

            const sizeBytes = Buffer.byteLength(json);
            await this.pool.query(
                `UPDATE grey_backups SET status = 'completed', size_bytes = $1, tables_included = $2 WHERE id = $3`,
                [sizeBytes, tableNames, backupId]
            );

            return this.getBackup(backupId) as Promise<BackupRecord>;
        } catch (err) {
            await this.pool.query(`UPDATE grey_backups SET status = 'failed', metadata = $1 WHERE id = $2`, [
                JSON.stringify({ error: String(err) }),
                backupId,
            ]);
            throw err;
        }
    }

    /** Create a per-tenant backup */
    async createTenantBackup(tenantId: string, schema = "public"): Promise<BackupRecord> {
        const timestamp = new Date().toISOString().replace(/[:.]/g, "-");
        const fileName = `grey_backup_tenant_${tenantId}_${timestamp}.json`;
        const filePath = path.join(this.backupDir, fileName);

        const insertResult = await this.pool.query(
            `INSERT INTO grey_backups (tenant_id, backup_type, format, file_path, status) VALUES ($1, 'tenant', 'json', $2, 'in_progress') RETURNING id`,
            [tenantId, filePath]
        );
        const backupId = insertResult.rows[0].id;

        try {
            // Get tables that have tenant_id column
            const tablesResult = await this.pool.query(`
        SELECT DISTINCT c.table_name
        FROM information_schema.columns c
        JOIN information_schema.tables t ON c.table_name = t.table_name AND c.table_schema = t.table_schema
        WHERE c.table_schema = $1 AND c.column_name = 'tenant_id' AND t.table_type = 'BASE TABLE'
      `, [schema]);
            const tableNames = tablesResult.rows.map((r) => r.table_name);

            const backup: Record<string, any> = {
                version: "1.0",
                type: "tenant",
                tenantId,
                schema,
                timestamp: new Date().toISOString(),
                tables: {},
            };

            for (const table of tableNames) {
                const dataResult = await this.pool.query(
                    `SELECT * FROM "${schema}"."${table}" WHERE tenant_id = $1`,
                    [tenantId]
                );
                backup.tables[table] = {
                    rowCount: dataResult.rows.length,
                    data: dataResult.rows,
                };
            }

            const json = JSON.stringify(backup, null, 2);
            fs.writeFileSync(filePath, json);

            const sizeBytes = Buffer.byteLength(json);
            await this.pool.query(
                `UPDATE grey_backups SET status = 'completed', size_bytes = $1, tables_included = $2 WHERE id = $3`,
                [sizeBytes, tableNames, backupId]
            );

            return this.getBackup(backupId) as Promise<BackupRecord>;
        } catch (err) {
            await this.pool.query(`UPDATE grey_backups SET status = 'failed', metadata = $1 WHERE id = $2`, [
                JSON.stringify({ error: String(err) }),
                backupId,
            ]);
            throw err;
        }
    }

    /** Restore from a JSON backup */
    async restore(backupFilePath: string, dryRun = false): Promise<{ tables: string[]; rowsRestored: Record<string, number> }> {
        const json = fs.readFileSync(backupFilePath, "utf-8");
        const backup = JSON.parse(json);
        const result: Record<string, number> = {};

        const client = await this.pool.connect();
        try {
            if (!dryRun) await client.query("BEGIN");

            for (const [table, tableData] of Object.entries(backup.tables) as any) {
                const rows = tableData.data;
                if (rows.length === 0) {
                    result[table] = 0;
                    continue;
                }

                const columns = Object.keys(rows[0]);
                const cols = columns.map((c) => `"${c}"`).join(", ");

                if (dryRun) {
                    result[table] = rows.length;
                    continue;
                }

                for (const row of rows) {
                    const values = columns.map((_, i) => `$${i + 1}`).join(", ");
                    const params = columns.map((c) => row[c]);
                    await client.query(
                        `INSERT INTO "${backup.schema}"."${table}" (${cols}) VALUES (${values}) ON CONFLICT DO NOTHING`,
                        params
                    );
                }
                result[table] = rows.length;
            }

            if (!dryRun) await client.query("COMMIT");

            return { tables: Object.keys(backup.tables), rowsRestored: result };
        } catch (err) {
            if (!dryRun) await client.query("ROLLBACK");
            throw err;
        } finally {
            client.release();
        }
    }

    /** List backup history */
    async listBackups(limit = 50): Promise<BackupRecord[]> {
        const result = await this.pool.query(
            `SELECT * FROM grey_backups ORDER BY created_at DESC LIMIT $1`,
            [limit]
        );
        return result.rows.map(this.rowToBackup);
    }

    async getBackup(id: number): Promise<BackupRecord | null> {
        const result = await this.pool.query(`SELECT * FROM grey_backups WHERE id = $1`, [id]);
        if (result.rows.length === 0) return null;
        return this.rowToBackup(result.rows[0]);
    }

    private rowToBackup(row: any): BackupRecord {
        return {
            id: row.id,
            tenantId: row.tenant_id,
            type: row.backup_type,
            format: row.format,
            filePath: row.file_path,
            sizeBytes: parseInt(row.size_bytes),
            tablesIncluded: row.tables_included || [],
            createdAt: row.created_at,
            status: row.status,
            metadata: row.metadata || {},
        };
    }
}
