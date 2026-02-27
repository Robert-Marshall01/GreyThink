// ─────────────────────────────────────────────────────────────
// Shared CLI utilities — connection, formatting, error handling
// ─────────────────────────────────────────────────────────────

import { GreyDB } from "@grey-db/core";
import chalk from "chalk";

let _db: GreyDB | null = null;

export function getDB(): GreyDB {
    if (!_db) {
        _db = GreyDB.fromEnv();
    }
    return _db;
}

export async function withDB<T>(fn: (db: GreyDB) => Promise<T>): Promise<T> {
    const db = getDB();
    try {
        return await fn(db);
    } finally {
        await db.close();
    }
}

export function success(msg: string): void {
    console.log(chalk.green("✓ ") + msg);
}

export function warn(msg: string): void {
    console.log(chalk.yellow("⚠ ") + msg);
}

export function error(msg: string): void {
    console.error(chalk.red("✗ ") + msg);
}

export function info(msg: string): void {
    console.log(chalk.blue("ℹ ") + msg);
}

export function heading(msg: string): void {
    console.log("\n" + chalk.bold(msg));
    console.log(chalk.dim("─".repeat(60)));
}

export function handleError(err: unknown): never {
    if (err instanceof Error) {
        error(err.message);
        if (process.env.GREYDB_DEBUG === "1") {
            console.error(chalk.dim(err.stack));
        }
    } else {
        error(String(err));
    }
    process.exit(1);
}

export function formatBytes(bytes: number): string {
    if (bytes < 1024) return bytes + " B";
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + " KB";
    return (bytes / (1024 * 1024)).toFixed(1) + " MB";
}

export function formatDuration(ms: number): string {
    if (ms < 1) return (ms * 1000).toFixed(0) + "µs";
    if (ms < 1000) return ms.toFixed(1) + "ms";
    return (ms / 1000).toFixed(2) + "s";
}
