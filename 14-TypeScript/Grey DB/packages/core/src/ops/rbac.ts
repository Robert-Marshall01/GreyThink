// ─────────────────────────────────────────────────────────────
// Grey DB — RBAC Engine
// Roles, API keys, scoped permissions
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";
import { randomBytes, createHash } from "crypto";

export type Role = "admin" | "developer" | "readonly";

export interface User {
    id: string;
    email: string;
    role: Role;
    tenantId?: string;
    active: boolean;
    createdAt: Date;
}

export interface APIKey {
    id: string;
    userId: string;
    name: string;
    keyPrefix: string;  // first 8 chars for display
    keyHash: string;
    permissions: Permission[];
    tenantScope?: string;
    expiresAt?: Date;
    lastUsedAt?: Date;
    active: boolean;
    createdAt: Date;
}

export type Permission =
    | "schema:read"
    | "schema:write"
    | "data:read"
    | "data:write"
    | "tenant:manage"
    | "backup:create"
    | "backup:restore"
    | "migration:run"
    | "admin:full";

const ROLE_PERMISSIONS: Record<Role, Permission[]> = {
    admin: [
        "schema:read", "schema:write", "data:read", "data:write",
        "tenant:manage", "backup:create", "backup:restore", "migration:run", "admin:full",
    ],
    developer: [
        "schema:read", "schema:write", "data:read", "data:write", "migration:run",
    ],
    readonly: [
        "schema:read", "data:read",
    ],
};

export class RBACEngine {
    constructor(private pool: Pool) { }

    async init(): Promise<void> {
        await this.pool.query(`
      CREATE TABLE IF NOT EXISTS grey_users (
        id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        email       TEXT NOT NULL UNIQUE,
        role        TEXT NOT NULL DEFAULT 'readonly',
        tenant_id   UUID,
        active      BOOLEAN DEFAULT true,
        created_at  TIMESTAMPTZ DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS grey_api_keys (
        id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        user_id       UUID REFERENCES grey_users(id),
        name          TEXT NOT NULL,
        key_prefix    TEXT NOT NULL,
        key_hash      TEXT NOT NULL UNIQUE,
        permissions   TEXT[] DEFAULT '{}',
        tenant_scope  UUID,
        expires_at    TIMESTAMPTZ,
        last_used_at  TIMESTAMPTZ,
        active        BOOLEAN DEFAULT true,
        created_at    TIMESTAMPTZ DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS grey_audit_log (
        id          SERIAL PRIMARY KEY,
        user_id     UUID,
        action      TEXT NOT NULL,
        resource    TEXT,
        details     JSONB DEFAULT '{}',
        ip_address  TEXT,
        tenant_id   UUID,
        created_at  TIMESTAMPTZ DEFAULT NOW()
      );

      CREATE INDEX IF NOT EXISTS idx_audit_user ON grey_audit_log (user_id);
      CREATE INDEX IF NOT EXISTS idx_audit_tenant ON grey_audit_log (tenant_id);
      CREATE INDEX IF NOT EXISTS idx_audit_time ON grey_audit_log (created_at DESC);
    `);
    }

    // ── User Management ─────────────────────────────────────

    async createUser(email: string, role: Role, tenantId?: string): Promise<User> {
        const result = await this.pool.query(
            `INSERT INTO grey_users (email, role, tenant_id) VALUES ($1, $2, $3) RETURNING *`,
            [email, role, tenantId]
        );
        return this.rowToUser(result.rows[0]);
    }

    async getUser(id: string): Promise<User | null> {
        const result = await this.pool.query(`SELECT * FROM grey_users WHERE id = $1`, [id]);
        if (result.rows.length === 0) return null;
        return this.rowToUser(result.rows[0]);
    }

    async getUserByEmail(email: string): Promise<User | null> {
        const result = await this.pool.query(`SELECT * FROM grey_users WHERE email = $1`, [email]);
        if (result.rows.length === 0) return null;
        return this.rowToUser(result.rows[0]);
    }

    async listUsers(): Promise<User[]> {
        const result = await this.pool.query(`SELECT * FROM grey_users ORDER BY created_at ASC`);
        return result.rows.map(this.rowToUser);
    }

    async updateUserRole(id: string, role: Role): Promise<User | null> {
        const result = await this.pool.query(
            `UPDATE grey_users SET role = $1 WHERE id = $2 RETURNING *`,
            [role, id]
        );
        if (result.rows.length === 0) return null;
        return this.rowToUser(result.rows[0]);
    }

    async deactivateUser(id: string): Promise<void> {
        await this.pool.query(`UPDATE grey_users SET active = false WHERE id = $1`, [id]);
        await this.pool.query(`UPDATE grey_api_keys SET active = false WHERE user_id = $1`, [id]);
    }

    // ── API Key Management ──────────────────────────────────

    /**
     * Generate a new API key. Returns the raw key ONCE — it is only stored as a hash.
     */
    async createAPIKey(
        userId: string,
        name: string,
        permissions?: Permission[],
        tenantScope?: string,
        expiresAt?: Date
    ): Promise<{ apiKey: APIKey; rawKey: string }> {
        const rawKey = `grey_${randomBytes(32).toString("hex")}`;
        const keyPrefix = rawKey.substring(0, 12);
        const keyHash = createHash("sha256").update(rawKey).digest("hex");

        // If no explicit permissions, derive from user role
        let perms = permissions;
        if (!perms) {
            const user = await this.getUser(userId);
            if (user) perms = ROLE_PERMISSIONS[user.role];
            else perms = ROLE_PERMISSIONS.readonly;
        }

        const result = await this.pool.query(
            `INSERT INTO grey_api_keys (user_id, name, key_prefix, key_hash, permissions, tenant_scope, expires_at)
       VALUES ($1, $2, $3, $4, $5, $6, $7) RETURNING *`,
            [userId, name, keyPrefix, keyHash, perms, tenantScope, expiresAt]
        );

        return { apiKey: this.rowToAPIKey(result.rows[0]), rawKey };
    }

    /** Validate an API key and return the associated user/permissions */
    async validateAPIKey(rawKey: string): Promise<{ user: User; apiKey: APIKey } | null> {
        const keyHash = createHash("sha256").update(rawKey).digest("hex");
        const result = await this.pool.query(
            `SELECT * FROM grey_api_keys WHERE key_hash = $1 AND active = true`,
            [keyHash]
        );
        if (result.rows.length === 0) return null;

        const apiKey = this.rowToAPIKey(result.rows[0]);

        // Check expiration
        if (apiKey.expiresAt && apiKey.expiresAt < new Date()) return null;

        // Update last used
        await this.pool.query(`UPDATE grey_api_keys SET last_used_at = NOW() WHERE id = $1`, [apiKey.id]);

        const user = await this.getUser(apiKey.userId);
        if (!user || !user.active) return null;

        return { user, apiKey };
    }

    /** Check if a user/key has a specific permission */
    hasPermission(apiKey: APIKey, permission: Permission): boolean {
        return apiKey.permissions.includes("admin:full") || apiKey.permissions.includes(permission);
    }

    async revokeAPIKey(id: string): Promise<void> {
        await this.pool.query(`UPDATE grey_api_keys SET active = false WHERE id = $1`, [id]);
    }

    async listAPIKeys(userId: string): Promise<APIKey[]> {
        const result = await this.pool.query(
            `SELECT * FROM grey_api_keys WHERE user_id = $1 ORDER BY created_at DESC`,
            [userId]
        );
        return result.rows.map(this.rowToAPIKey);
    }

    // ── Audit Logging ────────────────────────────────────────

    async auditLog(userId: string | null, action: string, resource?: string, details?: Record<string, any>, tenantId?: string, ip?: string): Promise<void> {
        await this.pool.query(
            `INSERT INTO grey_audit_log (user_id, action, resource, details, tenant_id, ip_address) VALUES ($1, $2, $3, $4, $5, $6)`,
            [userId, action, resource, details ? JSON.stringify(details) : "{}", tenantId, ip]
        );
    }

    async getAuditLog(options?: { userId?: string; tenantId?: string; action?: string; limit?: number }): Promise<any[]> {
        const conditions: string[] = [];
        const params: any[] = [];
        let idx = 1;

        if (options?.userId) { conditions.push(`user_id = $${idx++}`); params.push(options.userId); }
        if (options?.tenantId) { conditions.push(`tenant_id = $${idx++}`); params.push(options.tenantId); }
        if (options?.action) { conditions.push(`action = $${idx++}`); params.push(options.action); }

        const where = conditions.length > 0 ? `WHERE ${conditions.join(" AND ")}` : "";
        const limit = options?.limit ?? 100;
        params.push(limit);

        const result = await this.pool.query(
            `SELECT * FROM grey_audit_log ${where} ORDER BY created_at DESC LIMIT $${idx}`,
            params
        );
        return result.rows;
    }

    // ── Permission helpers ───────────────────────────────────

    static getPermissionsForRole(role: Role): Permission[] {
        return [...ROLE_PERMISSIONS[role]];
    }

    private rowToUser(row: any): User {
        return {
            id: row.id,
            email: row.email,
            role: row.role,
            tenantId: row.tenant_id,
            active: row.active,
            createdAt: row.created_at,
        };
    }

    private rowToAPIKey(row: any): APIKey {
        return {
            id: row.id,
            userId: row.user_id,
            name: row.name,
            keyPrefix: row.key_prefix,
            keyHash: row.key_hash,
            permissions: row.permissions,
            tenantScope: row.tenant_scope,
            expiresAt: row.expires_at,
            lastUsedAt: row.last_used_at,
            active: row.active,
            createdAt: row.created_at,
        };
    }
}
