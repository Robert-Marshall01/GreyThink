// ─────────────────────────────────────────────────────────────
// Grey DB — Data Quality & Validation Engine
// Column rules, cross-table checks, data profiling
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 1. Column-level Validation Rules
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Constraint enforcement philosophy:
 *
 * Grey DB uses a LAYERED validation strategy:
 *
 * Layer 1 — Database constraints (CHECK, NOT NULL, FK, UNIQUE)
 *   Enforcement: Absolute. Cannot be bypassed.
 *   Use for: Invariants that must NEVER be violated.
 *   Example: email format, positive amounts, valid status codes
 *
 * Layer 2 — Application-level validation (this engine)
 *   Enforcement: Configurable (warn, reject, quarantine)
 *   Use for: Business rules, data quality policies, soft constraints
 *   Example: "phone should be 10+ digits", "name should not be empty"
 *
 * Layer 3 — Cross-table / aggregate validation
 *   Enforcement: Batch job (periodic, not on every write)
 *   Use for: Referential consistency, business invariants across tables
 *   Example: "order total should equal sum of line items"
 *
 * Why not just CHECK constraints for everything?
 * - CHECK can't reference other tables
 * - CHECK can't produce warnings (only hard reject)
 * - CHECK can't quarantine bad rows for manual review
 * - CHECK error messages aren't user-friendly
 * - Business rules change more often than invariants
 */

export type ValidationSeverity = "error" | "warning" | "info";
export type ValidationAction = "reject" | "warn" | "quarantine";

export interface ValidationRule {
    /** Unique rule identifier */
    id: string;
    /** Human-readable description */
    description: string;
    /** Which column(s) this rule applies to */
    columns: string[];
    /** Severity level */
    severity: ValidationSeverity;
    /** What to do when violation found */
    action: ValidationAction;
    /** SQL expression that must be TRUE for valid data */
    sqlCheck?: string;
    /** In-memory predicate for application-level validation */
    predicate?: (value: any, row: Record<string, any>) => boolean;
    /** Custom error message template (use {column}, {value} placeholders) */
    message?: string;
}

export interface ValidationResult {
    valid: boolean;
    errors: ValidationViolation[];
    warnings: ValidationViolation[];
}

export interface ValidationViolation {
    ruleId: string;
    column: string;
    value: any;
    message: string;
    severity: ValidationSeverity;
    action: ValidationAction;
}

// ── Built-in validation rule factories ─────────────────────

export const rules = {
    /** Email format validation */
    email: (column: string, opts?: Partial<ValidationRule>): ValidationRule => ({
        id: `email_${column}`,
        description: `${column} must be a valid email address`,
        columns: [column],
        severity: "error",
        action: "reject",
        predicate: (v) => typeof v === "string" && /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(v),
        sqlCheck: `"${column}" ~ '^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$'`,
        message: `{column} "{value}" is not a valid email address`,
        ...opts,
    }),

    /** Non-empty string */
    notEmpty: (column: string, opts?: Partial<ValidationRule>): ValidationRule => ({
        id: `not_empty_${column}`,
        description: `${column} must not be empty`,
        columns: [column],
        severity: "error",
        action: "reject",
        predicate: (v) => typeof v === "string" && v.trim().length > 0,
        sqlCheck: `TRIM("${column}") <> ''`,
        message: `{column} must not be empty`,
        ...opts,
    }),

    /** Numeric range */
    range: (column: string, min: number, max: number, opts?: Partial<ValidationRule>): ValidationRule => ({
        id: `range_${column}_${min}_${max}`,
        description: `${column} must be between ${min} and ${max}`,
        columns: [column],
        severity: "error",
        action: "reject",
        predicate: (v) => typeof v === "number" && v >= min && v <= max,
        sqlCheck: `"${column}" >= ${min} AND "${column}" <= ${max}`,
        message: `{column} value {value} is outside range [${min}, ${max}]`,
        ...opts,
    }),

    /** Positive number */
    positive: (column: string, opts?: Partial<ValidationRule>): ValidationRule => ({
        id: `positive_${column}`,
        description: `${column} must be positive`,
        columns: [column],
        severity: "error",
        action: "reject",
        predicate: (v) => typeof v === "number" && v > 0,
        sqlCheck: `"${column}" > 0`,
        message: `{column} must be positive, got {value}`,
        ...opts,
    }),

    /** String length bounds */
    length: (column: string, min: number, max: number, opts?: Partial<ValidationRule>): ValidationRule => ({
        id: `length_${column}_${min}_${max}`,
        description: `${column} length must be between ${min} and ${max}`,
        columns: [column],
        severity: "error",
        action: "reject",
        predicate: (v) => typeof v === "string" && v.length >= min && v.length <= max,
        sqlCheck: `LENGTH("${column}") BETWEEN ${min} AND ${max}`,
        message: `{column} length must be ${min}-${max} characters`,
        ...opts,
    }),

    /** Regex pattern match */
    pattern: (column: string, regex: string, label: string, opts?: Partial<ValidationRule>): ValidationRule => ({
        id: `pattern_${column}_${label}`,
        description: `${column} must match ${label} pattern`,
        columns: [column],
        severity: "error",
        action: "reject",
        predicate: (v) => typeof v === "string" && new RegExp(regex).test(v),
        sqlCheck: `"${column}" ~ '${regex}'`,
        message: `{column} does not match expected ${label} format`,
        ...opts,
    }),

    /** Value must be one of a set */
    oneOf: (column: string, allowed: (string | number)[], opts?: Partial<ValidationRule>): ValidationRule => ({
        id: `one_of_${column}`,
        description: `${column} must be one of: ${allowed.join(", ")}`,
        columns: [column],
        severity: "error",
        action: "reject",
        predicate: (v) => allowed.includes(v),
        sqlCheck: `"${column}" IN (${allowed.map((a) => typeof a === "string" ? `'${a}'` : a).join(", ")})`,
        message: `{column} value "{value}" is not in allowed set`,
        ...opts,
    }),

    /** Future date */
    futureDate: (column: string, opts?: Partial<ValidationRule>): ValidationRule => ({
        id: `future_date_${column}`,
        description: `${column} must be a future date`,
        columns: [column],
        severity: "warning",
        action: "warn",
        predicate: (v) => v instanceof Date && v.getTime() > Date.now(),
        sqlCheck: `"${column}" > NOW()`,
        message: `{column} should be a future date`,
        ...opts,
    }),

    /** Not null */
    required: (column: string, opts?: Partial<ValidationRule>): ValidationRule => ({
        id: `required_${column}`,
        description: `${column} is required`,
        columns: [column],
        severity: "error",
        action: "reject",
        predicate: (v) => v !== null && v !== undefined,
        message: `{column} is required`,
        ...opts,
    }),

    /** Custom SQL predicate */
    custom: (id: string, columns: string[], sqlCheck: string, description: string, opts?: Partial<ValidationRule>): ValidationRule => ({
        id,
        description,
        columns,
        severity: "error",
        action: "reject",
        sqlCheck,
        message: description,
        ...opts,
    }),
};

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 2. Validation Engine
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Validates rows against a collection of rules.
 * Can operate in-memory (for pre-insert validation) or
 * generate SQL for database-side validation of existing data.
 */
export class ValidationEngine {
    private rulesByTable: Map<string, ValidationRule[]> = new Map();

    /** Register rules for a table */
    register(tableName: string, tableRules: ValidationRule[]): void {
        const existing = this.rulesByTable.get(tableName) || [];
        this.rulesByTable.set(tableName, [...existing, ...tableRules]);
    }

    /** Get all rules for a table */
    getRules(tableName: string): ValidationRule[] {
        return this.rulesByTable.get(tableName) || [];
    }

    /** Validate a single row in memory (pre-insert / pre-update) */
    validateRow(tableName: string, row: Record<string, any>): ValidationResult {
        const tableRules = this.rulesByTable.get(tableName) || [];
        const errors: ValidationViolation[] = [];
        const warnings: ValidationViolation[] = [];

        for (const rule of tableRules) {
            if (!rule.predicate) continue;

            for (const col of rule.columns) {
                const value = row[col];
                if (!rule.predicate(value, row)) {
                    const violation: ValidationViolation = {
                        ruleId: rule.id,
                        column: col,
                        value,
                        message: (rule.message || rule.description)
                            .replace("{column}", col)
                            .replace("{value}", String(value)),
                        severity: rule.severity,
                        action: rule.action,
                    };

                    if (rule.severity === "error") errors.push(violation);
                    else warnings.push(violation);
                }
            }
        }

        return { valid: errors.length === 0, errors, warnings };
    }

    /** Validate a batch of rows */
    validateBatch(tableName: string, rows: Record<string, any>[]): {
        valid: Record<string, any>[];
        rejected: { row: Record<string, any>; violations: ValidationViolation[] }[];
        warnings: { row: Record<string, any>; violations: ValidationViolation[] }[];
    } {
        const valid: Record<string, any>[] = [];
        const rejected: { row: Record<string, any>; violations: ValidationViolation[] }[] = [];
        const allWarnings: { row: Record<string, any>; violations: ValidationViolation[] }[] = [];

        for (const row of rows) {
            const result = this.validateRow(tableName, row);
            if (result.valid) {
                valid.push(row);
                if (result.warnings.length > 0) {
                    allWarnings.push({ row, violations: result.warnings });
                }
            } else {
                rejected.push({ row, violations: [...result.errors, ...result.warnings] });
            }
        }

        return { valid, rejected, warnings: allWarnings };
    }

    /**
     * Generate SQL to find existing rows that violate rules.
     * Useful for data quality audits on existing data.
     */
    generateAuditSQL(tableName: string, schema = "public"): string[] {
        const tableRules = this.rulesByTable.get(tableName) || [];
        const fq = `"${schema}"."${tableName}"`;

        return tableRules
            .filter((r) => r.sqlCheck)
            .map((r) => `
-- Rule: ${r.id} (${r.severity}) — ${r.description}
SELECT id, ${r.columns.map((c) => `"${c}"`).join(", ")},
  '${r.id}' AS rule_id,
  '${r.severity}' AS severity,
  '${r.description.replace(/'/g, "''")}' AS description
FROM ${fq}
WHERE NOT (${r.sqlCheck});
`.trim());
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 3. Cross-table Validation
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Cross-table consistency checks that run as periodic batch jobs.
 *
 * These catch data quality issues that can't be enforced with
 * single-table constraints: orphaned records, sum mismatches,
 * FK violations in eventually-consistent systems, etc.
 */
export interface CrossTableCheck {
    id: string;
    description: string;
    /** SQL that returns violating rows (empty result = pass) */
    sql: string;
    severity: ValidationSeverity;
    /** How often to run (cron hint — scheduling is external) */
    schedule?: string;
}

export const crossTableChecks = {
    /** Detect orphaned child records (missing parent FK) */
    orphanedRecords: (
        childTable: string,
        childFkColumn: string,
        parentTable: string,
        parentPkColumn = "id",
        schema = "public"
    ): CrossTableCheck => ({
        id: `orphan_${childTable}_${parentTable}`,
        description: `${childTable}.${childFkColumn} references missing ${parentTable}.${parentPkColumn}`,
        sql: `
      SELECT c.id, c."${childFkColumn}"
      FROM "${schema}"."${childTable}" c
      LEFT JOIN "${schema}"."${parentTable}" p ON c."${childFkColumn}" = p."${parentPkColumn}"
      WHERE p."${parentPkColumn}" IS NULL AND c."${childFkColumn}" IS NOT NULL;
    `.trim(),
        severity: "error",
    }),

    /** Verify aggregate consistency (e.g., order total = sum of line items) */
    aggregateMismatch: (
        parentTable: string,
        totalColumn: string,
        childTable: string,
        amountColumn: string,
        joinColumn: string,
        schema = "public"
    ): CrossTableCheck => ({
        id: `agg_mismatch_${parentTable}_${childTable}`,
        description: `${parentTable}.${totalColumn} should equal SUM(${childTable}.${amountColumn})`,
        sql: `
      SELECT p.id, p."${totalColumn}" AS expected,
             COALESCE(SUM(c."${amountColumn}"), 0) AS actual,
             p."${totalColumn}" - COALESCE(SUM(c."${amountColumn}"), 0) AS diff
      FROM "${schema}"."${parentTable}" p
      LEFT JOIN "${schema}"."${childTable}" c ON c."${joinColumn}" = p.id
      GROUP BY p.id, p."${totalColumn}"
      HAVING p."${totalColumn}" <> COALESCE(SUM(c."${amountColumn}"), 0);
    `.trim(),
        severity: "error",
    }),

    /** Detect duplicate logical records (e.g., same email in different casing) */
    duplicates: (
        tableName: string,
        column: string,
        caseInsensitive = true,
        schema = "public"
    ): CrossTableCheck => ({
        id: `dup_${tableName}_${column}`,
        description: `Duplicate ${column} values in ${tableName}`,
        sql: `
      SELECT ${caseInsensitive ? `LOWER("${column}")` : `"${column}"`} AS value,
             COUNT(*) AS count,
             ARRAY_AGG(id) AS duplicate_ids
      FROM "${schema}"."${tableName}"
      WHERE "${column}" IS NOT NULL
      GROUP BY ${caseInsensitive ? `LOWER("${column}")` : `"${column}"`}
      HAVING COUNT(*) > 1;
    `.trim(),
        severity: "warning",
    }),

    /** Custom SQL check */
    custom: (id: string, description: string, sql: string, severity: ValidationSeverity = "error"): CrossTableCheck => ({
        id,
        description,
        sql,
        severity,
    }),
};

/**
 * Run cross-table checks and collect violations.
 */
export async function runCrossTableChecks(
    pool: Pool,
    checks: CrossTableCheck[]
): Promise<{
    passed: string[];
    failed: { check: CrossTableCheck; violations: any[] }[];
    summary: { total: number; passed: number; failed: number };
}> {
    const passed: string[] = [];
    const failed: { check: CrossTableCheck; violations: any[] }[] = [];

    for (const check of checks) {
        try {
            const result = await pool.query(check.sql);
            if (result.rows.length === 0) {
                passed.push(check.id);
            } else {
                failed.push({ check, violations: result.rows });
            }
        } catch (err: any) {
            failed.push({
                check,
                violations: [{ error: err.message }],
            });
        }
    }

    return {
        passed,
        failed,
        summary: {
            total: checks.length,
            passed: passed.length,
            failed: failed.length,
        },
    };
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 4. Data Profiling
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Data profiling computes statistical metadata about table columns.
 * Useful for understanding data quality before writing validation rules.
 *
 * Profile results feed into:
 * - Automated rule suggestion ("97% of emails match our regex — make it a rule")
 * - Anomaly detection ("null rate jumped from 0.1% to 15%")
 * - Schema evolution decisions ("this JSON column always has the same keys — normalize it")
 */
export interface ColumnProfile {
    column: string;
    dataType: string;
    totalRows: number;
    nullCount: number;
    nullPercent: number;
    distinctCount: number;
    distinctPercent: number;
    minValue?: any;
    maxValue?: any;
    avgValue?: number;
    topValues?: { value: any; count: number }[];
    avgLength?: number;
    minLength?: number;
    maxLength?: number;
}

export interface TableProfile {
    tableName: string;
    schema: string;
    rowCount: number;
    columns: ColumnProfile[];
    profiledAt: Date;
}

/**
 * Profile a table's data distribution.
 *
 * Performance note: this runs multiple aggregate queries.
 * For large tables (>1M rows), consider sampling with TABLESAMPLE BERNOULLI.
 */
export async function profileTable(
    pool: Pool,
    tableName: string,
    schema = "public",
    options?: { samplePercent?: number; topN?: number }
): Promise<TableProfile> {
    const fq = `"${schema}"."${tableName}"`;
    const sample = options?.samplePercent
        ? `TABLESAMPLE BERNOULLI(${options.samplePercent})`
        : "";
    const topN = options?.topN ?? 5;

    // Get row count + column metadata
    const countResult = await pool.query(`SELECT COUNT(*)::int AS cnt FROM ${fq}`);
    const totalRows = countResult.rows[0].cnt;

    const colResult = await pool.query(
        `SELECT column_name, data_type
     FROM information_schema.columns
     WHERE table_schema = $1 AND table_name = $2
     ORDER BY ordinal_position`,
        [schema, tableName]
    );

    const columns: ColumnProfile[] = [];

    for (const col of colResult.rows) {
        const name = col.column_name;
        const dtype = col.data_type;
        const isNumeric = ["integer", "bigint", "smallint", "numeric", "real", "double precision"].includes(dtype);
        const isText = ["text", "character varying", "character"].includes(dtype);

        // Base stats: null count, distinct count
        const statsSQL = `
      SELECT
        COUNT(*) FILTER (WHERE "${name}" IS NULL)::int AS null_count,
        COUNT(DISTINCT "${name}")::int AS distinct_count
        ${isNumeric ? `, MIN("${name}") AS min_val, MAX("${name}") AS max_val, AVG("${name}"::numeric)::numeric AS avg_val` : ""}
        ${isText ? `, MIN(LENGTH("${name}"))::int AS min_len, MAX(LENGTH("${name}"))::int AS max_len, AVG(LENGTH("${name}"))::numeric AS avg_len` : ""}
      FROM ${fq} ${sample}
    `;

        const stats = (await pool.query(statsSQL)).rows[0];

        // Top values
        const topSQL = `
      SELECT "${name}" AS value, COUNT(*)::int AS count
      FROM ${fq} ${sample}
      WHERE "${name}" IS NOT NULL
      GROUP BY "${name}"
      ORDER BY count DESC
      LIMIT ${topN}
    `;

        let topValues: { value: any; count: number }[] = [];
        try {
            topValues = (await pool.query(topSQL)).rows;
        } catch {
            // Some column types (bytea, json) can't be grouped — skip
        }

        const profile: ColumnProfile = {
            column: name,
            dataType: dtype,
            totalRows,
            nullCount: stats.null_count,
            nullPercent: totalRows > 0 ? (stats.null_count / totalRows) * 100 : 0,
            distinctCount: stats.distinct_count,
            distinctPercent: totalRows > 0 ? (stats.distinct_count / totalRows) * 100 : 0,
            topValues,
        };

        if (isNumeric) {
            profile.minValue = stats.min_val;
            profile.maxValue = stats.max_val;
            profile.avgValue = stats.avg_val ? parseFloat(stats.avg_val) : undefined;
        }

        if (isText) {
            profile.minLength = stats.min_len;
            profile.maxLength = stats.max_len;
            profile.avgLength = stats.avg_len ? parseFloat(stats.avg_len) : undefined;
        }

        columns.push(profile);
    }

    return {
        tableName,
        schema,
        rowCount: totalRows,
        columns,
        profiledAt: new Date(),
    };
}

/**
 * Suggest validation rules based on a table profile.
 * Heuristic-based: looks for patterns like high-distinctness (unique?),
 * low null rate (required?), numeric ranges, etc.
 */
export function suggestRules(profile: TableProfile): ValidationRule[] {
    const suggested: ValidationRule[] = [];

    for (const col of profile.columns) {
        // Suggest required if null rate < 1%
        if (col.nullPercent < 1 && col.nullPercent >= 0 && col.totalRows > 100) {
            suggested.push(rules.required(col.column, {
                severity: "warning",
                action: "warn",
                message: `{column} is almost never null (${col.nullPercent.toFixed(2)}% null) — consider making it required`,
            }));
        }

        // Suggest range for numeric columns
        if (col.minValue !== undefined && col.maxValue !== undefined && typeof col.minValue === "number") {
            const margin = Math.abs(col.maxValue - col.minValue) * 0.1;
            suggested.push(rules.range(col.column, col.minValue - margin, col.maxValue + margin, {
                severity: "warning",
                action: "warn",
                message: `{column} observed range [${col.minValue}, ${col.maxValue}] — consider adding bounds`,
            }));
        }

        // Suggest length bounds for text columns
        if (col.minLength !== undefined && col.maxLength !== undefined) {
            suggested.push(rules.length(col.column, Math.max(0, col.minLength), col.maxLength * 2, {
                severity: "warning",
                action: "warn",
                message: `{column} observed length [${col.minLength}, ${col.maxLength}] — consider adding bounds`,
            }));
        }
    }

    return suggested;
}
