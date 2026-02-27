// ─────────────────────────────────────────────────────────────
// Grey DB — Explain Plan Analyzer
// Runs EXPLAIN ANALYZE and interprets query plans
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";

export interface ExplainNode {
    nodeType: string;
    startupCost: number;
    totalCost: number;
    planRows: number;
    planWidth: number;
    actualTime?: { startup: number; total: number };
    actualRows?: number;
    actualLoops?: number;
    indexName?: string;
    relationName?: string;
    filter?: string;
    joinType?: string;
    children?: ExplainNode[];
    warnings: string[];
}

export interface ExplainResult {
    query: string;
    plan: ExplainNode;
    executionTimeMs: number;
    planningTimeMs: number;
    warnings: ExplainWarning[];
    rawPlan: any;
}

export interface ExplainWarning {
    severity: "info" | "warning" | "critical";
    message: string;
    suggestion: string;
    node?: string;
}

/**
 * Run EXPLAIN ANALYZE on a query and parse the results.
 */
export async function explainQuery(pool: Pool, sql: string, params: any[] = []): Promise<ExplainResult> {
    const explainSQL = `EXPLAIN (ANALYZE, COSTS, VERBOSE, BUFFERS, FORMAT JSON) ${sql}`;
    const result = await pool.query(explainSQL, params);
    const rawPlan = result.rows[0]["QUERY PLAN"][0];

    const plan = parseNode(rawPlan.Plan);
    const warnings = analyzeWarnings(plan);

    return {
        query: sql,
        plan,
        executionTimeMs: rawPlan["Execution Time"] ?? 0,
        planningTimeMs: rawPlan["Planning Time"] ?? 0,
        warnings,
        rawPlan,
    };
}

/**
 * Run EXPLAIN (no ANALYZE — safe for production) and parse.
 */
export async function explainQuerySafe(pool: Pool, sql: string, params: any[] = []): Promise<ExplainResult> {
    const explainSQL = `EXPLAIN (COSTS, VERBOSE, FORMAT JSON) ${sql}`;
    const result = await pool.query(explainSQL, params);
    const rawPlan = result.rows[0]["QUERY PLAN"][0];

    const plan = parseNode(rawPlan.Plan);
    const warnings = analyzeWarnings(plan);

    return {
        query: sql,
        plan,
        executionTimeMs: 0,
        planningTimeMs: rawPlan["Planning Time"] ?? 0,
        warnings,
        rawPlan,
    };
}

function parseNode(raw: any): ExplainNode {
    const node: ExplainNode = {
        nodeType: raw["Node Type"] || "Unknown",
        startupCost: raw["Startup Cost"] ?? 0,
        totalCost: raw["Total Cost"] ?? 0,
        planRows: raw["Plan Rows"] ?? 0,
        planWidth: raw["Plan Width"] ?? 0,
        warnings: [],
    };

    if (raw["Actual Startup Time"] !== undefined) {
        node.actualTime = {
            startup: raw["Actual Startup Time"],
            total: raw["Actual Total Time"],
        };
        node.actualRows = raw["Actual Rows"];
        node.actualLoops = raw["Actual Loops"];
    }

    if (raw["Index Name"]) node.indexName = raw["Index Name"];
    if (raw["Relation Name"]) node.relationName = raw["Relation Name"];
    if (raw["Filter"]) node.filter = raw["Filter"];
    if (raw["Join Type"]) node.joinType = raw["Join Type"];

    if (raw.Plans && Array.isArray(raw.Plans)) {
        node.children = raw.Plans.map(parseNode);
    }

    return node;
}

function analyzeWarnings(node: ExplainNode, result: ExplainWarning[] = []): ExplainWarning[] {
    // Sequential scan on large tables
    if (node.nodeType === "Seq Scan" && node.planRows > 1000) {
        result.push({
            severity: "warning",
            message: `Sequential scan on "${node.relationName}" (est. ${node.planRows} rows)`,
            suggestion: `Consider adding an index on the filtered columns of "${node.relationName}"`,
            node: node.nodeType,
        });
    }

    // Very large sequential scan
    if (node.nodeType === "Seq Scan" && node.planRows > 100000) {
        result.push({
            severity: "critical",
            message: `Large sequential scan on "${node.relationName}" (est. ${node.planRows} rows)`,
            suggestion: `This query will be very slow. Add an index or restructure the query.`,
            node: node.nodeType,
        });
    }

    // Nested loop with high row counts
    if (node.nodeType === "Nested Loop" && node.planRows > 10000) {
        result.push({
            severity: "warning",
            message: `Nested loop join producing ${node.planRows} rows`,
            suggestion: `Consider if a hash or merge join would be more efficient. Check join conditions.`,
            node: node.nodeType,
        });
    }

    // Sort with high cost
    if (node.nodeType === "Sort" && node.totalCost > 1000) {
        result.push({
            severity: "info",
            message: `Expensive sort operation (cost: ${node.totalCost})`,
            suggestion: `Consider adding an index on the sort columns to avoid in-memory sorting.`,
            node: node.nodeType,
        });
    }

    // Hash join with bitmap heap scan (may indicate missing index)
    if (node.nodeType === "Bitmap Heap Scan" && node.planRows > 50000) {
        result.push({
            severity: "info",
            message: `Bitmap heap scan on "${node.relationName}" — many rows`,
            suggestion: `If this is a frequent query, a covering index may improve performance.`,
            node: node.nodeType,
        });
    }

    // Actual vs estimated row mismatch
    if (node.actualRows !== undefined && node.planRows > 0) {
        const ratio = node.actualRows / node.planRows;
        if (ratio > 10 || ratio < 0.1) {
            result.push({
                severity: "warning",
                message: `Row estimate mismatch on "${node.relationName || node.nodeType}": estimated ${node.planRows}, actual ${node.actualRows}`,
                suggestion: `Run ANALYZE on the table to update statistics.`,
                node: node.nodeType,
            });
        }
    }

    // Recurse into children
    if (node.children) {
        for (const child of node.children) {
            analyzeWarnings(child, result);
        }
    }

    return result;
}

/**
 * Format an explain result as a human-readable string.
 */
export function formatExplainResult(result: ExplainResult): string {
    const lines: string[] = [];
    lines.push(`── Query Plan ──────────────────────────────`);
    lines.push(`Query: ${result.query.substring(0, 100)}${result.query.length > 100 ? "..." : ""}`);
    if (result.executionTimeMs > 0) {
        lines.push(`Execution: ${result.executionTimeMs.toFixed(2)}ms | Planning: ${result.planningTimeMs.toFixed(2)}ms`);
    }
    lines.push("");

    formatNode(result.plan, lines, 0);

    if (result.warnings.length > 0) {
        lines.push("");
        lines.push(`── Warnings (${result.warnings.length}) ─────────────────────`);
        for (const w of result.warnings) {
            const icon = w.severity === "critical" ? "🔴" : w.severity === "warning" ? "🟡" : "🔵";
            lines.push(`${icon} ${w.message}`);
            lines.push(`   → ${w.suggestion}`);
        }
    }

    return lines.join("\n");
}

function formatNode(node: ExplainNode, lines: string[], depth: number): void {
    const indent = "  ".repeat(depth);
    let line = `${indent}→ ${node.nodeType}`;
    if (node.relationName) line += ` on ${node.relationName}`;
    if (node.indexName) line += ` using ${node.indexName}`;
    line += ` (cost=${node.totalCost.toFixed(2)} rows=${node.planRows})`;
    if (node.actualRows !== undefined) {
        line += ` [actual rows=${node.actualRows}]`;
    }
    lines.push(line);

    if (node.filter) {
        lines.push(`${indent}  Filter: ${node.filter}`);
    }

    if (node.children) {
        for (const child of node.children) {
            formatNode(child, lines, depth + 1);
        }
    }
}
