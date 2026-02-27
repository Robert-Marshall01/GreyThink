// ─────────────────────────────────────────────────────────────
// greydb explain — Run EXPLAIN ANALYZE on a query
// ─────────────────────────────────────────────────────────────

import { Command } from "commander";
import chalk from "chalk";
import { explainQuery, explainQuerySafe, formatExplainResult } from "@grey-db/core";
import { withDB, heading, handleError, info, warn } from "../utils";

export function registerExplainCommand(program: Command): void {
    program
        .command("explain <sql>")
        .description("Run EXPLAIN on a SQL query and analyze the plan")
        .option("--analyze", "Run EXPLAIN ANALYZE (executes the query)", false)
        .option("--json", "Output raw plan as JSON", false)
        .action(async (sql: string, opts) => {
            try {
                await withDB(async (db) => {
                    heading("Query Plan Analysis");
                    info(`Query: ${chalk.dim(sql.substring(0, 120))}`);
                    console.log("");

                    const result = opts.analyze
                        ? await explainQuery(db.pool, sql)
                        : await explainQuerySafe(db.pool, sql);

                    if (opts.json) {
                        console.log(JSON.stringify(result.rawPlan, null, 2));
                        return;
                    }

                    console.log(formatExplainResult(result));

                    // Additional advice
                    console.log("");
                    if (result.warnings.length === 0) {
                        info(chalk.green("No performance issues detected"));
                    } else {
                        const critical = result.warnings.filter(
                            (w) => w.severity === "critical"
                        );
                        const warnings = result.warnings.filter(
                            (w) => w.severity === "warning"
                        );
                        if (critical.length > 0) {
                            warn(
                                chalk.red(
                                    `${critical.length} critical issue(s) — this query will be slow`
                                )
                            );
                        }
                        if (warnings.length > 0) {
                            warn(`${warnings.length} warning(s) — may impact performance`);
                        }
                    }

                    if (result.executionTimeMs > 0) {
                        const color =
                            result.executionTimeMs < 10
                                ? chalk.green
                                : result.executionTimeMs < 100
                                    ? chalk.yellow
                                    : chalk.red;
                        console.log(
                            `\n  Execution: ${color(
                                result.executionTimeMs.toFixed(2) + "ms"
                            )} | Planning: ${result.planningTimeMs.toFixed(2)}ms`
                        );
                    }
                });
            } catch (err) {
                handleError(err);
            }
        });
}
