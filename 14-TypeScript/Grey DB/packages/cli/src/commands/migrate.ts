// ─────────────────────────────────────────────────────────────
// greydb migrate — Migration lifecycle commands
// ─────────────────────────────────────────────────────────────

import { Command } from "commander";
import chalk from "chalk";
import Table from "cli-table3";
import {
    ensureMigrationTable,
    applyMigration,
    rollbackMigration,
    getMigrationHistory,
    getCurrentVersion,
    generateMigration,
    Migration,
} from "@grey-db/core";
import { withDB, success, warn, error, heading, handleError, info } from "../utils";

export function registerMigrateCommands(program: Command): void {
    const migrate = program
        .command("migrate")
        .description("Manage database migrations");

    // ── greydb migrate init ─────────────────────────────────
    migrate
        .command("init")
        .description("Initialize the migration tracking table")
        .action(async () => {
            try {
                await withDB(async (db) => {
                    await ensureMigrationTable(db.pool);
                    success("Migration table initialized (grey_migrations)");
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb migrate apply ────────────────────────────────
    migrate
        .command("apply")
        .description("Apply a migration from a SQL file or inline SQL")
        .requiredOption("-n, --name <name>", "Migration name")
        .requiredOption("-s, --sql <sql>", "Up SQL to execute")
        .option("-d, --down <sql>", "Down SQL for rollback", "-- No rollback SQL provided")
        .option("--dry-run", "Preview without applying", false)
        .option("--force", "Apply even if marked unsafe", false)
        .action(async (opts) => {
            try {
                await withDB(async (db) => {
                    const version = (await getCurrentVersion(db.pool)) + 1;

                    const migration: Migration = {
                        id: `${version}_${opts.name.replace(/\s+/g, "_").toLowerCase()}`,
                        version,
                        name: opts.name,
                        upSQL: opts.sql,
                        downSQL: opts.down,
                        createdAt: new Date(),
                        safe: true,
                        warnings: [],
                        checksum: require("crypto").createHash("sha256").update(opts.sql).digest("hex").substring(0, 16),
                    };

                    if (opts.dryRun) {
                        heading("Dry Run — Migration Preview");
                        info(`Version: ${version}`);
                        info(`Name: ${opts.name}`);
                        console.log(chalk.dim("\n" + migration.upSQL + "\n"));
                        warn("No changes applied (dry-run mode)");
                        return;
                    }

                    await applyMigration(db.pool, migration);
                    success(`Migration v${version} "${opts.name}" applied`);
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb migrate rollback ─────────────────────────────
    migrate
        .command("rollback")
        .description("Rollback the last applied migration")
        .option("-y, --yes", "Skip confirmation", false)
        .action(async (opts) => {
            try {
                await withDB(async (db) => {
                    if (!opts.yes) {
                        const inquirer = await import("inquirer");
                        const { confirm } = await inquirer.default.prompt([
                            {
                                type: "confirm",
                                name: "confirm",
                                message: "Are you sure you want to rollback the last migration?",
                                default: false,
                            },
                        ]);
                        if (!confirm) {
                            info("Rollback cancelled");
                            return;
                        }
                    }

                    const rolled = await rollbackMigration(db.pool);
                    if (rolled) {
                        success(`Rolled back migration v${rolled.version} "${rolled.name}"`);
                    } else {
                        warn("No migrations to rollback");
                    }
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb migrate history ──────────────────────────────
    migrate
        .command("history")
        .description("Show migration history")
        .option("--json", "Output as JSON", false)
        .action(async (opts) => {
            try {
                await withDB(async (db) => {
                    const history = await getMigrationHistory(db.pool);

                    if (history.length === 0) {
                        info("No migrations applied");
                        return;
                    }

                    if (opts.json) {
                        console.log(JSON.stringify(history, null, 2));
                        return;
                    }

                    heading("Migration History");

                    const table = new Table({
                        head: [
                            chalk.cyan("Version"),
                            chalk.cyan("Name"),
                            chalk.cyan("Safe"),
                            chalk.cyan("Checksum"),
                            chalk.cyan("Applied At"),
                        ],
                        colWidths: [10, 30, 8, 20, 26],
                    });

                    for (const m of history) {
                        table.push([
                            m.version,
                            m.name,
                            m.safe ? chalk.green("✓") : chalk.red("✗"),
                            chalk.dim(m.checksum),
                            m.appliedAt ? new Date(m.appliedAt).toISOString() : "—",
                        ]);
                    }

                    console.log(table.toString());
                    console.log(chalk.dim(`\n  ${history.length} migration(s) total`));
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb migrate status ──────────────────────────────
    migrate
        .command("status")
        .description("Show current migration version")
        .action(async () => {
            try {
                await withDB(async (db) => {
                    const version = await getCurrentVersion(db.pool);
                    info(`Current migration version: ${chalk.bold(String(version))}`);
                });
            } catch (err) {
                handleError(err);
            }
        });
}
