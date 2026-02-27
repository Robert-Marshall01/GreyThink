// ─────────────────────────────────────────────────────────────
// greydb tenant — Tenant management commands
// ─────────────────────────────────────────────────────────────

import { Command } from "commander";
import chalk from "chalk";
import Table from "cli-table3";
import { withDB, success, warn, error, heading, handleError, info } from "../utils";

export function registerTenantCommands(program: Command): void {
    const tenant = program
        .command("tenant")
        .description("Manage tenants");

    // ── greydb tenant list ──────────────────────────────────
    tenant
        .command("list")
        .description("List all tenants")
        .option("--json", "Output as JSON", false)
        .action(async (opts) => {
            try {
                await withDB(async (db) => {
                    const tenants = await db.tenants.listTenants();

                    if (tenants.length === 0) {
                        info("No tenants found");
                        return;
                    }

                    if (opts.json) {
                        console.log(JSON.stringify(tenants, null, 2));
                        return;
                    }

                    heading("Tenants");

                    const table = new Table({
                        head: [
                            chalk.cyan("ID"),
                            chalk.cyan("Name"),
                            chalk.cyan("Slug"),
                            chalk.cyan("Isolation"),
                            chalk.cyan("Active"),
                            chalk.cyan("Created"),
                        ],
                        colWidths: [38, 20, 16, 12, 8, 22],
                    });

                    for (const t of tenants) {
                        table.push([
                            chalk.dim(t.id),
                            t.name,
                            t.slug,
                            t.isolationStrategy,
                            t.active ? chalk.green("✓") : chalk.red("✗"),
                            new Date(t.createdAt).toLocaleDateString(),
                        ]);
                    }

                    console.log(table.toString());
                    console.log(chalk.dim(`\n  ${tenants.length} tenant(s) total`));
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb tenant create ────────────────────────────────
    tenant
        .command("create")
        .description("Create a new tenant")
        .requiredOption("-n, --name <name>", "Tenant display name")
        .requiredOption("-s, --slug <slug>", "Unique slug identifier")
        .option("-i, --isolation <strategy>", "Isolation strategy: shared | schema | hybrid", "shared")
        .option("--storage-limit <mb>", "Storage limit in MB", "1024")
        .action(async (opts) => {
            try {
                await withDB(async (db) => {
                    await db.tenants.init();

                    const tenant = await db.tenants.createTenant({
                        name: opts.name,
                        slug: opts.slug,
                        isolationStrategy: opts.isolation,
                        featureFlags: {},
                        storageLimitMB: parseInt(opts.storageLimit),
                        config: {},
                        active: true,
                    });

                    success(`Tenant created: ${chalk.bold(tenant.name)}`);
                    info(`ID: ${chalk.dim(tenant.id)}`);
                    info(`Slug: ${tenant.slug}`);
                    info(`Isolation: ${tenant.isolationStrategy}`);
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb tenant get ───────────────────────────────────
    tenant
        .command("get <id>")
        .description("Get tenant details")
        .option("--json", "Output as JSON", false)
        .action(async (id: string, opts) => {
            try {
                await withDB(async (db) => {
                    const t = await db.tenants.getTenant(id);
                    if (!t) {
                        error(`Tenant not found: ${id}`);
                        process.exit(1);
                    }

                    if (opts.json) {
                        console.log(JSON.stringify(t, null, 2));
                        return;
                    }

                    heading(`Tenant: ${t.name}`);
                    console.log(`  ID:         ${chalk.dim(t.id)}`);
                    console.log(`  Name:       ${t.name}`);
                    console.log(`  Slug:       ${t.slug}`);
                    console.log(`  Isolation:  ${t.isolationStrategy}`);
                    console.log(`  Active:     ${t.active ? chalk.green("yes") : chalk.red("no")}`);
                    console.log(`  Storage:    ${t.storageLimitMB} MB limit`);
                    console.log(`  Created:    ${new Date(t.createdAt).toISOString()}`);

                    if (Object.keys(t.featureFlags).length > 0) {
                        console.log(`  Flags:      ${JSON.stringify(t.featureFlags)}`);
                    }
                    if (Object.keys(t.config).length > 0) {
                        console.log(`  Config:     ${JSON.stringify(t.config)}`);
                    }
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb tenant deactivate ────────────────────────────
    tenant
        .command("deactivate <id>")
        .description("Deactivate a tenant")
        .action(async (id: string) => {
            try {
                await withDB(async (db) => {
                    const updated = await db.tenants.updateTenant(id, { active: false });
                    if (updated) {
                        success(`Tenant "${updated.name}" deactivated`);
                    } else {
                        error(`Tenant not found: ${id}`);
                    }
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb tenant activate ──────────────────────────────
    tenant
        .command("activate <id>")
        .description("Activate a tenant")
        .action(async (id: string) => {
            try {
                await withDB(async (db) => {
                    const updated = await db.tenants.updateTenant(id, { active: true });
                    if (updated) {
                        success(`Tenant "${updated.name}" activated`);
                    } else {
                        error(`Tenant not found: ${id}`);
                    }
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb tenant rls-apply ─────────────────────────────
    tenant
        .command("rls-apply <table>")
        .description("Apply RLS tenant isolation policy to a table")
        .option("--schema <schema>", "Database schema", "public")
        .action(async (table: string, opts) => {
            try {
                await withDB(async (db) => {
                    const sql = await db.tenants.applyRLSPolicy(table, opts.schema);
                    success(`RLS policy applied to ${opts.schema}.${table}`);
                    console.log(chalk.dim("\n" + sql));
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb tenant rls-remove ────────────────────────────
    tenant
        .command("rls-remove <table>")
        .description("Remove RLS tenant isolation policy from a table")
        .option("--schema <schema>", "Database schema", "public")
        .action(async (table: string, opts) => {
            try {
                await withDB(async (db) => {
                    await db.tenants.removeRLSPolicy(table, opts.schema);
                    success(`RLS policy removed from ${opts.schema}.${table}`);
                });
            } catch (err) {
                handleError(err);
            }
        });
}
