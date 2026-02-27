// ─────────────────────────────────────────────────────────────
// Grey DB — Connection Manager
// Manages PostgreSQL connection pool and initialization
// ─────────────────────────────────────────────────────────────

import { Pool, PoolConfig } from "pg";

export interface GreyDBConfig {
    host: string;
    port: number;
    database: string;
    user: string;
    password: string;
    ssl?: boolean;
    maxConnections?: number;
    idleTimeoutMs?: number;
    connectionTimeoutMs?: number;
}

let globalPool: Pool | null = null;

export function createPool(config: GreyDBConfig): Pool {
    const poolConfig: PoolConfig = {
        host: config.host,
        port: config.port,
        database: config.database,
        user: config.user,
        password: config.password,
        ssl: config.ssl ? { rejectUnauthorized: false } : undefined,
        max: config.maxConnections ?? 20,
        idleTimeoutMillis: config.idleTimeoutMs ?? 30000,
        connectionTimeoutMillis: config.connectionTimeoutMs ?? 5000,
    };

    globalPool = new Pool(poolConfig);

    globalPool.on("error", (err) => {
        console.error("[Grey DB] Unexpected pool error:", err.message);
    });

    return globalPool;
}

export function getPool(): Pool {
    if (!globalPool) throw new Error("Grey DB pool not initialized. Call createPool() first.");
    return globalPool;
}

export async function closePool(): Promise<void> {
    if (globalPool) {
        await globalPool.end();
        globalPool = null;
    }
}

export function createPoolFromEnv(): Pool {
    return createPool({
        host: process.env.GREY_DB_HOST || process.env.PGHOST || "localhost",
        port: parseInt(process.env.GREY_DB_PORT || process.env.PGPORT || "5432"),
        database: process.env.GREY_DB_NAME || process.env.PGDATABASE || "greydb",
        user: process.env.GREY_DB_USER || process.env.PGUSER || "postgres",
        password: process.env.GREY_DB_PASSWORD || process.env.PGPASSWORD || "postgres",
        ssl: process.env.GREY_DB_SSL === "true",
        maxConnections: parseInt(process.env.GREY_DB_MAX_CONNECTIONS || "20"),
    });
}
