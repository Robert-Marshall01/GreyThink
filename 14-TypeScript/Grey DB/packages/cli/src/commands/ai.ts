// ─────────────────────────────────────────────────────────────
// greydb ai — AI-assisted query commands
// ─────────────────────────────────────────────────────────────

import { Command } from "commander";
import chalk from "chalk";
import { NLToSQL } from "@grey-db/core";
import { withDB, heading, handleError, info, warn, error, success } from "../utils";

export function registerAICommands(program: Command): void {
    const ai = program
        .command("ai")
        .description("AI-assisted database operations");

    // ── greydb ai query ─────────────────────────────────────
    ai
        .command("query <question>")
        .description("Convert natural language to SQL (requires schema)")
        .option("--schema <name>", "Database schema to load", "public")
        .option("--allow-write", "Allow write operations", false)
        .option("--json", "Output as JSON", false)
        .action(async (question: string, opts) => {
            try {
                await withDB(async (db) => {
                    heading("NL → SQL");
                    info(`Question: "${question}"`);
                    console.log("");

                    // Load schema context
                    info("Loading schema metadata...");
                    const schemaContext = await db.nlsql.loadSchema(opts.schema);
                    info(
                        `Found ${schemaContext.tables.length} table(s) in schema "${opts.schema}"`
                    );
                    console.log("");

                    // Prepare the prompt
                    const prepared = db.nlsql.prepareNLQuery(question);

                    if (opts.json) {
                        console.log(
                            JSON.stringify(
                                {
                                    systemPrompt: prepared.systemPrompt,
                                    userPrompt: prepared.userPrompt,
                                    tables: schemaContext.tables.map((t) => ({
                                        name: t.name,
                                        columns: t.columns.map((c) => c.name),
                                        rowCount: t.rowCount,
                                    })),
                                },
                                null,
                                2
                            )
                        );
                        return;
                    }

                    // Display schema context
                    heading("Schema Context");
                    for (const table of schemaContext.tables) {
                        console.log(
                            `  ${chalk.bold(table.name)} ${chalk.dim(
                                `(${table.columns.length} cols, ~${(table.rowCount || 0).toLocaleString()} rows)`
                            )}`
                        );
                        for (const col of table.columns) {
                            let desc = `    ${col.name}: ${chalk.dim(col.type)}`;
                            if (col.isPrimaryKey) desc += chalk.cyan(" [PK]");
                            if (col.isForeignKey)
                                desc += chalk.yellow(
                                    ` [FK→${col.references?.table}.${col.references?.column}]`
                                );
                            console.log(desc);
                        }
                    }

                    // Display the prompt that would go to LLM
                    heading("Generated Prompt");
                    console.log(chalk.dim("  System prompt length: " + prepared.systemPrompt.length + " chars"));
                    console.log(chalk.dim("  User prompt: ") + prepared.userPrompt);

                    console.log("");
                    info(
                        "Pass this prompt to your LLM provider (OpenAI, Anthropic, etc.) to get SQL."
                    );
                    info(
                        "Grey DB intentionally does not bundle an LLM — you choose the provider."
                    );
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb ai check ─────────────────────────────────────
    ai
        .command("check <sql>")
        .description("Check a SQL query for safety issues")
        .option("--allow-write", "Allow write operations", false)
        .action(async (sql: string, opts) => {
            try {
                await withDB(async (db) => {
                    const safety = db.nlsql.checkSafety(sql, opts.allowWrite);

                    heading("SQL Safety Check");
                    console.log(`  Query: ${chalk.dim(sql.substring(0, 100))}`);
                    console.log("");

                    const levelColors: Record<string, (s: string) => string> = {
                        read: chalk.green,
                        write: chalk.yellow,
                        ddl: chalk.red,
                        dangerous: chalk.bgRed.white,
                    };

                    const colorFn = levelColors[safety.level] || chalk.white;
                    console.log(`  Level:  ${colorFn(safety.level.toUpperCase())}`);
                    console.log(
                        `  Safe:   ${safety.safe ? chalk.green("Yes") : chalk.red("No — BLOCKED")
                        }`
                    );

                    if (safety.warnings.length > 0) {
                        console.log("");
                        for (const w of safety.warnings) {
                            warn(w);
                        }
                    }

                    if (safety.blockedOperations.length > 0) {
                        console.log("");
                        error("Blocked operations:");
                        for (const op of safety.blockedOperations) {
                            console.log(chalk.red(`    → ${op}`));
                        }
                    }

                    console.log("");
                    if (safety.safe) {
                        success("Query passed safety check");
                    } else {
                        error("Query failed safety check — will not be executed");
                    }
                });
            } catch (err) {
                handleError(err);
            }
        });

    // ── greydb ai schema ────────────────────────────────────
    ai
        .command("schema")
        .description("Display loaded schema metadata for AI context")
        .option("--schema <name>", "Database schema to load", "public")
        .option("--json", "Output as JSON", false)
        .action(async (opts) => {
            try {
                await withDB(async (db) => {
                    const schemaContext = await db.nlsql.loadSchema(opts.schema);

                    if (opts.json) {
                        console.log(JSON.stringify(schemaContext, null, 2));
                        return;
                    }

                    heading(`Schema: ${opts.schema}`);
                    info(`${schemaContext.tables.length} table(s) discovered\n`);

                    for (const table of schemaContext.tables) {
                        console.log(
                            `  ${chalk.bold(table.name)} ${chalk.dim(
                                `~${(table.rowCount || 0).toLocaleString()} rows`
                            )}`
                        );
                        console.log(
                            `  ${chalk.dim("Indexes: " + (table.indexes.join(", ") || "none"))}`
                        );
                        for (const col of table.columns) {
                            let line = `    ${col.name.padEnd(24)} ${chalk.dim(
                                col.type.padEnd(16)
                            )}`;
                            const tags: string[] = [];
                            if (col.isPrimaryKey) tags.push(chalk.cyan("PK"));
                            if (col.isForeignKey)
                                tags.push(
                                    chalk.yellow(`FK→${col.references?.table}`)
                                );
                            if (!col.nullable) tags.push("NOT NULL");
                            if (tags.length) line += tags.join(" ");
                            console.log(line);
                        }
                        console.log("");
                    }
                });
            } catch (err) {
                handleError(err);
            }
        });
}
