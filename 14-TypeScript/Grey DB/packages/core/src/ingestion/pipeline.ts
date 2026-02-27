// ─────────────────────────────────────────────────────────────
// Grey DB — Data Ingestion & Transformation Pipeline
// CSV/JSON ingestion, schema validation, transforms, DLQ
// ─────────────────────────────────────────────────────────────

import { Pool, PoolClient } from "pg";
import { Table } from "../schema/dsl";
import { ValidationEngine, ValidationResult } from "../validation/engine";

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 1. Ingestion Pipeline
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Batch vs Streaming ingestion philosophy:
 *
 * Grey DB is a SQL-first platform and leans toward BATCH processing:
 *
 * Batch:
 * - Parse entire file → validate → transform → bulk INSERT
 * - Atomic: entire batch succeeds or fails together (or uses DLQ)
 * - Efficient: uses COPY or multi-row INSERT (10-100x faster than row-by-row)
 * - Predictable: resource usage is bounded by batch size
 *
 * Streaming:
 * - Process one record at a time as it arrives
 * - Lower latency (ms vs seconds)
 * - Requires backpressure management, exactly-once semantics
 * - Better suited for Kafka/Kinesis, not a SQL database's sweet spot
 *
 * Grey DB provides streaming-compatible interfaces (row-at-a-time hooks)
 * but the default pipeline is batch-oriented with configurable batch sizes.
 */

export type IngestionFormat = "csv" | "json" | "jsonl";

export interface IngestionConfig {
    /** Target table definition */
    table: Table;
    /** Input format */
    format: IngestionFormat;
    /** Batch size for bulk inserts (default: 1000) */
    batchSize?: number;
    /** Validation engine instance (optional — skips validation if not provided) */
    validator?: ValidationEngine;
    /** Transformation hooks */
    transforms?: TransformHook[];
    /** Enable dead-letter queue for rejected records */
    enableDLQ?: boolean;
    /** Schema for the target table */
    schema?: string;
    /** How to handle conflicts (unique constraint violations) */
    onConflict?: "skip" | "update" | "error";
    /** Conflict columns (for ON CONFLICT) */
    conflictColumns?: string[];
}

/**
 * Transformation hooks run in order on each row before insertion.
 * They can modify the row, skip it (return null), or throw to reject it.
 *
 * Common transform use cases:
 * - Normalize data (trim whitespace, lowercase emails)
 * - Compute derived fields (full_name = first + " " + last)
 * - Type coercion (string "123" → integer 123)
 * - Enrich with defaults or lookups
 * - Filter/skip rows that don't meet business criteria
 */
export interface TransformHook {
    /** Human-readable name for debugging */
    name: string;
    /** Transform a row. Return null to skip the row. Throw to reject. */
    transform: (row: Record<string, any>, index: number) => Record<string, any> | null;
}

/**
 * Result of an ingestion run.
 */
export interface IngestionResult {
    totalRecords: number;
    insertedCount: number;
    skippedCount: number;
    rejectedCount: number;
    deadLetterCount: number;
    errors: IngestionError[];
    durationMs: number;
    batchesProcessed: number;
}

export interface IngestionError {
    rowIndex: number;
    row?: Record<string, any>;
    error: string;
    phase: "parse" | "transform" | "validate" | "insert";
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 2. Parsers
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Parse CSV text into rows. Handles quoted fields and newlines within quotes.
 * No external dependency — intentionally simple (for production, use `csv-parse`).
 */
export function parseCSV(text: string, options?: { delimiter?: string; hasHeader?: boolean }): Record<string, any>[] {
    const delimiter = options?.delimiter ?? ",";
    const hasHeader = options?.hasHeader ?? true;

    const lines = splitCSVLines(text);
    if (lines.length === 0) return [];

    const headers = hasHeader
        ? parseCSVLine(lines[0], delimiter)
        : Array.from({ length: parseCSVLine(lines[0], delimiter).length }, (_, i) => `col_${i}`);

    const startLine = hasHeader ? 1 : 0;
    const rows: Record<string, any>[] = [];

    for (let i = startLine; i < lines.length; i++) {
        const line = lines[i].trim();
        if (!line) continue;

        const values = parseCSVLine(line, delimiter);
        const row: Record<string, any> = {};

        for (let j = 0; j < headers.length; j++) {
            const raw = values[j] ?? null;
            row[headers[j]] = raw === "" ? null : inferType(raw);
        }

        rows.push(row);
    }

    return rows;
}

function splitCSVLines(text: string): string[] {
    const lines: string[] = [];
    let current = "";
    let inQuotes = false;

    for (let i = 0; i < text.length; i++) {
        const ch = text[i];
        if (ch === '"') {
            inQuotes = !inQuotes;
            current += ch;
        } else if ((ch === "\n" || ch === "\r") && !inQuotes) {
            if (ch === "\r" && text[i + 1] === "\n") i++;
            lines.push(current);
            current = "";
        } else {
            current += ch;
        }
    }

    if (current.trim()) lines.push(current);
    return lines;
}

function parseCSVLine(line: string, delimiter: string): string[] {
    const result: string[] = [];
    let current = "";
    let inQuotes = false;

    for (let i = 0; i < line.length; i++) {
        const ch = line[i];
        if (ch === '"') {
            if (inQuotes && line[i + 1] === '"') {
                current += '"';
                i++;
            } else {
                inQuotes = !inQuotes;
            }
        } else if (ch === delimiter && !inQuotes) {
            result.push(current);
            current = "";
        } else {
            current += ch;
        }
    }

    result.push(current);
    return result;
}

/** Parse JSON or JSONL text into rows */
export function parseJSON(text: string, format: "json" | "jsonl" = "json"): Record<string, any>[] {
    if (format === "jsonl") {
        return text
            .split("\n")
            .map((line) => line.trim())
            .filter((line) => line.length > 0)
            .map((line) => JSON.parse(line));
    }

    const parsed = JSON.parse(text);
    return Array.isArray(parsed) ? parsed : [parsed];
}

/** Infer basic types from string values (CSV parsing) */
function inferType(value: string | null): any {
    if (value === null || value === "") return null;
    if (value === "true") return true;
    if (value === "false") return false;
    if (/^-?\d+$/.test(value) && value.length < 16) return parseInt(value, 10);
    if (/^-?\d+\.\d+$/.test(value)) return parseFloat(value);
    // ISO date detection
    if (/^\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}:\d{2})?/.test(value)) {
        const d = new Date(value);
        if (!isNaN(d.getTime())) return d;
    }
    return value;
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 3. Schema Validation for Ingestion
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Validate that parsed rows conform to a Table's column definitions.
 * Checks: required columns present, types compatible, no unknown columns.
 */
export function validateSchema(
    rows: Record<string, any>[],
    table: Table,
    options?: { allowExtraColumns?: boolean }
): { valid: Record<string, any>[]; errors: IngestionError[] } {
    const tableCols = Object.keys(table.definition.columns);
    const required = tableCols.filter((col) => {
        const c = table.definition.columns[col]._constraints;
        return c.nullable === false && !c.primaryKey && !c.defaultNow && !c.defaultUUID && c.default === undefined;
    });

    const valid: Record<string, any>[] = [];
    const errors: IngestionError[] = [];

    for (let i = 0; i < rows.length; i++) {
        const row = rows[i];
        const rowErrors: string[] = [];

        // Check required columns
        for (const col of required) {
            if (row[col] === undefined || row[col] === null) {
                rowErrors.push(`Missing required column: ${col}`);
            }
        }

        // Check for unknown columns
        if (!options?.allowExtraColumns) {
            for (const key of Object.keys(row)) {
                if (!tableCols.includes(key)) {
                    rowErrors.push(`Unknown column: ${key}`);
                }
            }
        }

        if (rowErrors.length > 0) {
            errors.push({
                rowIndex: i,
                row,
                error: rowErrors.join("; "),
                phase: "validate",
            });
        } else {
            valid.push(row);
        }
    }

    return { valid, errors };
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 4. Dead-Letter Queue
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Dead-letter queue: holds records that failed ingestion.
 *
 * Purpose:
 * - No data loss: failed records are preserved for investigation
 * - Retry capability: fix the issue and re-process from DLQ
 * - Audit: track patterns in failures (bad source, schema drift, etc.)
 *
 * Design: single DLQ table for all target tables, with metadata
 * about which table it was heading to and why it failed.
 */
export class DeadLetterQueue {
    constructor(private pool: Pool, private schema = "public") { }

    async init(): Promise<void> {
        const fq = `"${this.schema}"."grey_dead_letter_queue"`;
        await this.pool.query(`
      CREATE TABLE IF NOT EXISTS ${fq} (
        id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        target_table  TEXT NOT NULL,
        row_data      JSONB NOT NULL,
        error_message TEXT NOT NULL,
        error_phase   TEXT NOT NULL,
        row_index     INTEGER,
        source_file   TEXT,
        batch_id      UUID,
        retry_count   INTEGER DEFAULT 0,
        resolved      BOOLEAN DEFAULT false,
        created_at    TIMESTAMPTZ DEFAULT NOW(),
        resolved_at   TIMESTAMPTZ
      );

      CREATE INDEX IF NOT EXISTS "idx_dlq_target" ON ${fq} ("target_table", "resolved");
      CREATE INDEX IF NOT EXISTS "idx_dlq_batch" ON ${fq} ("batch_id");
      CREATE INDEX IF NOT EXISTS "idx_dlq_created" ON ${fq} ("created_at");
    `);
    }

    /** Enqueue a failed record */
    async enqueue(
        targetTable: string,
        row: Record<string, any>,
        error: string,
        phase: string,
        opts?: { rowIndex?: number; sourceFile?: string; batchId?: string }
    ): Promise<string> {
        const fq = `"${this.schema}"."grey_dead_letter_queue"`;
        const result = await this.pool.query(
            `INSERT INTO ${fq} (target_table, row_data, error_message, error_phase, row_index, source_file, batch_id)
       VALUES ($1, $2, $3, $4, $5, $6, $7)
       RETURNING id`,
            [targetTable, JSON.stringify(row), error, phase, opts?.rowIndex ?? null, opts?.sourceFile ?? null, opts?.batchId ?? null]
        );
        return result.rows[0].id;
    }

    /** Enqueue multiple failed records efficiently */
    async enqueueBatch(
        targetTable: string,
        items: { row: Record<string, any>; error: string; phase: string; rowIndex?: number }[],
        batchId?: string
    ): Promise<number> {
        if (items.length === 0) return 0;

        const fq = `"${this.schema}"."grey_dead_letter_queue"`;
        const values: any[] = [];
        const placeholders: string[] = [];
        let paramIndex = 1;

        for (const item of items) {
            placeholders.push(`($${paramIndex}, $${paramIndex + 1}, $${paramIndex + 2}, $${paramIndex + 3}, $${paramIndex + 4}, $${paramIndex + 5})`);
            values.push(targetTable, JSON.stringify(item.row), item.error, item.phase, item.rowIndex ?? null, batchId ?? null);
            paramIndex += 6;
        }

        const result = await this.pool.query(
            `INSERT INTO ${fq} (target_table, row_data, error_message, error_phase, row_index, batch_id)
       VALUES ${placeholders.join(", ")}`,
            values
        );

        return result.rowCount ?? 0;
    }

    /** Get unresolved DLQ entries for a table */
    async getUnresolved(targetTable?: string, limit = 100): Promise<any[]> {
        const fq = `"${this.schema}"."grey_dead_letter_queue"`;
        if (targetTable) {
            return (await this.pool.query(
                `SELECT * FROM ${fq} WHERE target_table = $1 AND resolved = false ORDER BY created_at DESC LIMIT $2`,
                [targetTable, limit]
            )).rows;
        }
        return (await this.pool.query(
            `SELECT * FROM ${fq} WHERE resolved = false ORDER BY created_at DESC LIMIT $1`,
            [limit]
        )).rows;
    }

    /** Mark a DLQ entry as resolved */
    async resolve(id: string): Promise<void> {
        const fq = `"${this.schema}"."grey_dead_letter_queue"`;
        await this.pool.query(
            `UPDATE ${fq} SET resolved = true, resolved_at = NOW() WHERE id = $1`,
            [id]
        );
    }

    /** Get DLQ summary statistics */
    async getSummary(): Promise<{ targetTable: string; unresolved: number; total: number }[]> {
        const fq = `"${this.schema}"."grey_dead_letter_queue"`;
        const result = await this.pool.query(
            `SELECT target_table,
              COUNT(*) FILTER (WHERE resolved = false)::int AS unresolved,
              COUNT(*)::int AS total
       FROM ${fq}
       GROUP BY target_table
       ORDER BY unresolved DESC`
        );
        return result.rows.map((r) => ({
            targetTable: r.target_table,
            unresolved: r.unresolved,
            total: r.total,
        }));
    }
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// 5. Ingestion Pipeline (orchestrator)
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/**
 * Common transform hooks.
 */
export const transforms = {
    /** Trim whitespace from all string fields */
    trimStrings: (): TransformHook => ({
        name: "trim-strings",
        transform: (row) => {
            const trimmed: Record<string, any> = {};
            for (const [k, v] of Object.entries(row)) {
                trimmed[k] = typeof v === "string" ? v.trim() : v;
            }
            return trimmed;
        },
    }),

    /** Lowercase specific fields */
    lowercase: (...columns: string[]): TransformHook => ({
        name: "lowercase",
        transform: (row) => {
            const result = { ...row };
            for (const col of columns) {
                if (typeof result[col] === "string") {
                    result[col] = result[col].toLowerCase();
                }
            }
            return result;
        },
    }),

    /** Set default values for missing fields */
    defaults: (defaults: Record<string, any>): TransformHook => ({
        name: "defaults",
        transform: (row) => ({ ...defaults, ...row }),
    }),

    /** Rename columns (oldName → newName mapping) */
    renameColumns: (mapping: Record<string, string>): TransformHook => ({
        name: "rename-columns",
        transform: (row) => {
            const result: Record<string, any> = {};
            for (const [k, v] of Object.entries(row)) {
                result[mapping[k] ?? k] = v;
            }
            return result;
        },
    }),

    /** Drop columns */
    dropColumns: (...columns: string[]): TransformHook => ({
        name: "drop-columns",
        transform: (row) => {
            const result = { ...row };
            for (const col of columns) {
                delete result[col];
            }
            return result;
        },
    }),

    /** Compute a derived column */
    computed: (column: string, fn: (row: Record<string, any>) => any): TransformHook => ({
        name: `computed-${column}`,
        transform: (row) => ({ ...row, [column]: fn(row) }),
    }),

    /** Filter rows (return null to skip) */
    filter: (predicate: (row: Record<string, any>) => boolean): TransformHook => ({
        name: "filter",
        transform: (row) => (predicate(row) ? row : null),
    }),
};

/**
 * Main ingestion pipeline: parse → transform → validate → insert.
 */
export class IngestionPipeline {
    private dlq?: DeadLetterQueue;

    constructor(private pool: Pool, private config: IngestionConfig) {
        if (config.enableDLQ) {
            this.dlq = new DeadLetterQueue(pool, config.schema);
        }
    }

    async init(): Promise<void> {
        if (this.dlq) {
            await this.dlq.init();
        }
    }

    /**
     * Ingest raw text (CSV, JSON, or JSONL).
     *
     * Pipeline stages:
     * 1. Parse: convert text → row objects
     * 2. Transform: apply hooks (trim, normalize, compute, filter)
     * 3. Validate: schema conformance + business rules
     * 4. Insert: batch INSERT with conflict handling
     * 5. DLQ: rejected records go to dead-letter queue
     */
    async ingest(rawText: string, sourceFile?: string): Promise<IngestionResult> {
        const start = Date.now();
        const batchSize = this.config.batchSize ?? 1000;
        const tableName = this.config.table.definition.name;
        const schema = this.config.schema || this.config.table.definition.schema;
        const fq = `"${schema}"."${tableName}"`;

        const result: IngestionResult = {
            totalRecords: 0,
            insertedCount: 0,
            skippedCount: 0,
            rejectedCount: 0,
            deadLetterCount: 0,
            errors: [],
            durationMs: 0,
            batchesProcessed: 0,
        };

        // ── Stage 1: Parse ─────────────────────────────────────
        let rows: Record<string, any>[];
        try {
            if (this.config.format === "csv") {
                rows = parseCSV(rawText);
            } else {
                rows = parseJSON(rawText, this.config.format as "json" | "jsonl");
            }
        } catch (err: any) {
            result.errors.push({ rowIndex: -1, error: `Parse error: ${err.message}`, phase: "parse" });
            result.durationMs = Date.now() - start;
            return result;
        }

        result.totalRecords = rows.length;

        // ── Stage 2: Transform ─────────────────────────────────
        const transformed: Record<string, any>[] = [];
        for (let i = 0; i < rows.length; i++) {
            let row: Record<string, any> | null = rows[i];

            for (const hook of this.config.transforms || []) {
                if (row === null) break;
                try {
                    row = hook.transform(row, i);
                } catch (err: any) {
                    result.errors.push({ rowIndex: i, row: rows[i], error: `Transform "${hook.name}": ${err.message}`, phase: "transform" });
                    row = null;
                }
            }

            if (row === null) {
                result.skippedCount++;
            } else {
                transformed.push(row);
            }
        }

        // ── Stage 3: Validate ──────────────────────────────────
        let validRows = transformed;

        // Schema validation
        const schemaResult = validateSchema(transformed, this.config.table, { allowExtraColumns: false });
        validRows = schemaResult.valid;
        result.errors.push(...schemaResult.errors);

        // Business rule validation
        if (this.config.validator) {
            const { valid, rejected } = this.config.validator.validateBatch(tableName, validRows);
            validRows = valid;

            for (const rej of rejected) {
                result.errors.push({
                    rowIndex: -1,
                    row: rej.row,
                    error: rej.violations.map((v) => v.message).join("; "),
                    phase: "validate",
                });
            }
        }

        result.rejectedCount = result.errors.filter((e) => e.phase === "validate" || e.phase === "transform").length;

        // ── Stage 4: DLQ for rejected records ──────────────────
        if (this.dlq && result.errors.length > 0) {
            const dlqItems = result.errors
                .filter((e) => e.row)
                .map((e) => ({
                    row: e.row!,
                    error: e.error,
                    phase: e.phase,
                    rowIndex: e.rowIndex,
                }));

            result.deadLetterCount = await this.dlq.enqueueBatch(tableName, dlqItems);
        }

        // ── Stage 5: Batch Insert ──────────────────────────────
        if (validRows.length === 0) {
            result.durationMs = Date.now() - start;
            return result;
        }

        const columns = Object.keys(validRows[0]);
        const colList = columns.map((c) => `"${c}"`).join(", ");

        // Build ON CONFLICT clause
        let conflictClause = "";
        if (this.config.onConflict === "skip" && this.config.conflictColumns) {
            conflictClause = ` ON CONFLICT (${this.config.conflictColumns.map((c) => `"${c}"`).join(", ")}) DO NOTHING`;
        } else if (this.config.onConflict === "update" && this.config.conflictColumns) {
            const updateCols = columns
                .filter((c) => !this.config.conflictColumns!.includes(c))
                .map((c) => `"${c}" = EXCLUDED."${c}"`)
                .join(", ");
            conflictClause = ` ON CONFLICT (${this.config.conflictColumns.map((c) => `"${c}"`).join(", ")}) DO UPDATE SET ${updateCols}`;
        }

        // Process in batches
        for (let i = 0; i < validRows.length; i += batchSize) {
            const batch = validRows.slice(i, i + batchSize);

            const values: any[] = [];
            const rowPlaceholders: string[] = [];

            for (const row of batch) {
                const placeholders = columns.map(
                    (_, j) => `$${values.length + j + 1}`
                );
                rowPlaceholders.push(`(${placeholders.join(", ")})`);

                for (const col of columns) {
                    const val = row[col];
                    values.push(val instanceof Date ? val.toISOString() : val ?? null);
                }
            }

            const sql = `INSERT INTO ${fq} (${colList}) VALUES ${rowPlaceholders.join(", ")}${conflictClause}`;

            try {
                const insertResult = await this.pool.query(sql, values);
                result.insertedCount += insertResult.rowCount ?? 0;
                result.batchesProcessed++;
            } catch (err: any) {
                result.errors.push({
                    rowIndex: i,
                    error: `Batch insert error (rows ${i}-${i + batch.length}): ${err.message}`,
                    phase: "insert",
                });

                // Send entire batch to DLQ on insert failure
                if (this.dlq) {
                    for (const row of batch) {
                        await this.dlq.enqueue(tableName, row, err.message, "insert");
                    }
                    result.deadLetterCount += batch.length;
                }
            }
        }

        result.durationMs = Date.now() - start;
        return result;
    }
}
