// ─────────────────────────────────────────────────────────────
// greydb status — Database connection & system health
// ─────────────────────────────────────────────────────────────

import { Command } from "commander";
import chalk from "chalk";
import { getCurrentVersion } from "@grey-db/core";
import { withDB, heading, handleError, info, success, error, formatDuration } from "../utils";

export function registerStatusCommand(program: Command): void {
    program
        .command("status")
        .description("Check database connection and system status")
        .option("--json", "Output as JSON", false)
        .action(async (opts) => {
            try {
                await withDB(async (db) => {
                    const checks: Record<string, any> = {};

                    // 1. Connection check
                    const connStart = performance.now();
                    try {
                        const res = await db.pool.query("SELECT 1 AS ok, version() AS pg_version, NOW() AS server_time");
                        checks.connection = {
                            status: "ok",
                            latencyMs: Math.round(performance.now() - connStart),
                            pgVersion: res.rows[0].pg_version.split(" ").slice(0, 2).join(" "),
                            serverTime: res.rows[0].server_time,
                        };
                    } catch {
                        checks.connection = { status: "failed" };
                    }

                    // 2. Migration version
                    try {
                        checks.migrations = {
                            currentVersion: await getCurrentVersion(db.pool),
                        };
                    } catch {
                        checks.migrations = { currentVersion: "unknown (table may not exist)" };
                    }

                    // 3. Pool stats
                    const pool = db.pool as any;
                    checks.pool = {
                        total: pool.totalCount,
                        idle: pool.idleCount,
                        waiting: pool.waitingCount,
                    };

                    // 4. Database size
                    try {
                        const sizeRes = await db.pool.query(
                            "SELECT pg_size_pretty(pg_database_size(current_database())) AS size"
                        );
                        checks.database = {
                            size: sizeRes.rows[0].size,
                        };
                    } catch {
                        checks.database = { size: "unknown" };
                    }

                    // 5. Extensions
                    try {
                        const extRes = await db.pool.query(
                            "SELECT extname FROM pg_extension ORDER BY extname"
                        );
                        checks.extensions = extRes.rows.map((r) => r.extname);
                    } catch {
                        checks.extensions = [];
                    }

                    if (opts.json) {
                        console.log(JSON.stringify(checks, null, 2));
                        return;
                    }

                    heading("Grey DB Status");

                    // Connection
                    if (checks.connection.status === "ok") {
                        success(
                            `Connected ${chalk.dim("(" + formatDuration(checks.connection.latencyMs) + ")")}`
                        );
                        console.log(`  PostgreSQL:  ${chalk.dim(checks.connection.pgVersion)}`);
                    } else {
                        error("Connection failed");
                    }

                    // Migrations
                    console.log(
                        `  Migration:   v${chalk.bold(String(checks.migrations.currentVersion))}`
                    );

                    // Pool
                    console.log(
                        `  Pool:        ${checks.pool.total} total, ${checks.pool.idle} idle, ${checks.pool.waiting} waiting`
                    );

                    // Database
                    console.log(`  DB size:     ${checks.database.size}`);

                    // Extensions
                    if (checks.extensions.length > 0) {
                        console.log(
                            `  Extensions:  ${chalk.dim(checks.extensions.join(", "))}`
                        );
                    }

                    console.log("");
                });
            } catch (err) {
                handleError(err);
            }
        });
}
