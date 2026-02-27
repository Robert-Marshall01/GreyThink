// ─────────────────────────────────────────────────────────────
// greydb diff — Schema diff between current DSL and database
// ─────────────────────────────────────────────────────────────

import { Command } from "commander";
import chalk from "chalk";
import {
    diffSchemas,
    generateMigration,
    getCurrentVersion,
    Table,
} from "@grey-db/core";
import { withDB, success, warn, error, heading, handleError, info } from "../utils";

export function registerDiffCommand(program: Command): void {
    program
        .command("diff")
        .description("Show schema differences and generate migration SQL")
        .option("-s, --schema <path>", "Path to schema definition file", "./schema.ts")
        .option("--sql", "Output migration SQL only", false)
        .option("--json", "Output diff as JSON", false)
        .action(async (opts) => {
            try {
                // Load schema from file
                let desiredSchema: Table[];
                try {
                    const mod = require(require("path").resolve(opts.schema));
                    // Support default export, named `schema` export, or array of tables
                    desiredSchema = mod.default || mod.schema || mod.tables || [];
                    if (!Array.isArray(desiredSchema)) {
                        error(`Schema file must export an array of Table definitions`);
                        process.exit(1);
                    }
                } catch (loadErr: any) {
                    if (loadErr.code === "MODULE_NOT_FOUND") {
                        error(`Schema file not found: ${opts.schema}`);
                        info(`Create a schema file that exports Table definitions:`);
                        console.log(chalk.dim(`
  // schema.ts
  import { greyTable, uuid, text, timestamptz } from "@grey-db/core";

  export const schema = [
    greyTable("users", {
      id: uuid().primaryKey().defaultUUID(),
      email: text().unique().indexed(),
      createdAt: timestamptz().defaultNow(),
    }),
  ];
`));
                        process.exit(1);
                    }
                    throw loadErr;
                }

                // For now, diff against empty (no introspection yet)
                // A full implementation would introspect the current database schema
                const currentSchema: Table[] = [];

                const diff = diffSchemas(currentSchema, desiredSchema);

                if (opts.json) {
                    console.log(JSON.stringify(diff, null, 2));
                    return;
                }

                if (diff.actions.length === 0) {
                    success("Schema is up to date — no changes detected");
                    return;
                }

                heading("Schema Diff");

                for (const warning of diff.warnings) {
                    const icon =
                        warning.severity === "danger"
                            ? chalk.red("✗")
                            : warning.severity === "warning"
                                ? chalk.yellow("⚠")
                                : chalk.blue("ℹ");
                    console.log(`  ${icon} ${warning.message}`);
                }

                console.log("");

                if (!diff.safe) {
                    warn(
                        `This diff contains ${diff.destructiveActions.length} destructive action(s)`
                    );
                    for (const da of diff.destructiveActions) {
                        console.log(chalk.red(`    → ${da}`));
                    }
                    console.log("");
                }

                // Generate migration SQL
                await withDB(async (db) => {
                    const version = (await getCurrentVersion(db.pool)) + 1;
                    const migration = generateMigration("schema-diff", diff, version);

                    if (opts.sql) {
                        console.log(migration.upSQL);
                        return;
                    }

                    heading(`Migration v${version} — Up SQL`);
                    console.log(chalk.green(migration.upSQL));

                    heading(`Migration v${version} — Down SQL`);
                    console.log(chalk.yellow(migration.downSQL));

                    console.log("");
                    info(`Checksum: ${chalk.dim(migration.checksum)}`);
                    info(
                        `Safe: ${migration.safe ? chalk.green("yes") : chalk.red("no — review required")
                        }`
                    );
                });
            } catch (err) {
                handleError(err);
            }
        });
}
