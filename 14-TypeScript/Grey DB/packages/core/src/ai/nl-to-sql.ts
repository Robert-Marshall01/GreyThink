// ─────────────────────────────────────────────────────────────
// Grey DB — NL → SQL Engine
// Schema-aware natural language to SQL generation
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";

export interface SchemaContext {
    tables: TableMeta[];
    currentTenantId?: string;
}

export interface TableMeta {
    name: string;
    schema: string;
    columns: ColumnMeta[];
    indexes: string[];
    rowCount?: number;
    description?: string;
}

export interface ColumnMeta {
    name: string;
    type: string;
    nullable: boolean;
    isPrimaryKey: boolean;
    isForeignKey: boolean;
    references?: { table: string; column: string };
    description?: string;
}

export interface NLQueryResult {
    naturalLanguage: string;
    sql: string;
    explanation: string;
    safety: SafetyCheck;
    confidence: number;
    suggestedParams?: Record<string, any>;
}

export interface SafetyCheck {
    safe: boolean;
    level: "read" | "write" | "ddl" | "dangerous";
    warnings: string[];
    blockedOperations: string[];
}

const DANGEROUS_PATTERNS = [
    { pattern: /\bDROP\s+(TABLE|DATABASE|SCHEMA|INDEX)/i, label: "DROP operation", level: "dangerous" as const },
    { pattern: /\bTRUNCATE\b/i, label: "TRUNCATE operation", level: "dangerous" as const },
    { pattern: /\bALTER\s+TABLE\b/i, label: "ALTER TABLE operation", level: "ddl" as const },
    { pattern: /\bDELETE\s+FROM\b/i, label: "DELETE operation", level: "write" as const },
    { pattern: /\bUPDATE\b/i, label: "UPDATE operation", level: "write" as const },
    { pattern: /\bINSERT\b/i, label: "INSERT operation", level: "write" as const },
    { pattern: /\bCREATE\s+(TABLE|INDEX)/i, label: "CREATE operation", level: "ddl" as const },
    // Injection & privilege escalation patterns
    { pattern: /;\s*(DROP|DELETE|ALTER|TRUNCATE)\b/i, label: "Multi-statement with destructive operation", level: "dangerous" as const },
    { pattern: /\bpg_(shadow|authid)\b/i, label: "System auth catalog access", level: "dangerous" as const },
    { pattern: /\bCOPY\b.+\b(TO|FROM)\b/i, label: "COPY operation (file system access)", level: "dangerous" as const },
    { pattern: /\bGRANT\b|\bREVOKE\b/i, label: "Privilege modification", level: "dangerous" as const },
    { pattern: /\bSET\s+ROLE\b/i, label: "Role switching", level: "dangerous" as const },
];

export class NLToSQL {
    private schemaContext: SchemaContext | null = null;

    constructor(private pool: Pool) { }

    /** Load schema metadata from the database */
    async loadSchema(schemaName = "public"): Promise<SchemaContext> {
        // Get tables
        const tablesResult = await this.pool.query(`
      SELECT
        t.table_name,
        t.table_schema,
        obj_description((t.table_schema || '.' || t.table_name)::regclass) AS description,
        (SELECT reltuples::bigint FROM pg_class WHERE relname = t.table_name) AS row_count
      FROM information_schema.tables t
      WHERE t.table_schema = $1
        AND t.table_type = 'BASE TABLE'
        AND t.table_name NOT LIKE 'grey_%'
      ORDER BY t.table_name
    `, [schemaName]);

        const tables: TableMeta[] = [];

        for (const tableRow of tablesResult.rows) {
            // Get columns
            const colsResult = await this.pool.query(`
        SELECT
          c.column_name,
          c.data_type,
          c.is_nullable,
          c.column_default,
          col_description((c.table_schema || '.' || c.table_name)::regclass, c.ordinal_position) AS description
        FROM information_schema.columns c
        WHERE c.table_schema = $1 AND c.table_name = $2
        ORDER BY c.ordinal_position
      `, [schemaName, tableRow.table_name]);

            // Get primary keys
            const pkResult = await this.pool.query(`
        SELECT a.attname
        FROM pg_index i
        JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey)
        WHERE i.indrelid = ($1 || '.' || $2)::regclass AND i.indisprimary
      `, [schemaName, tableRow.table_name]);
            const pkCols = new Set(pkResult.rows.map((r) => r.attname));

            // Get foreign keys
            const fkResult = await this.pool.query(`
        SELECT
          kcu.column_name,
          ccu.table_name AS foreign_table,
          ccu.column_name AS foreign_column
        FROM information_schema.table_constraints tc
        JOIN information_schema.key_column_usage kcu ON tc.constraint_name = kcu.constraint_name
        JOIN information_schema.constraint_column_usage ccu ON tc.constraint_name = ccu.constraint_name
        WHERE tc.constraint_type = 'FOREIGN KEY'
          AND tc.table_schema = $1
          AND tc.table_name = $2
      `, [schemaName, tableRow.table_name]);
            const fkMap = new Map(fkResult.rows.map((r) => [r.column_name, { table: r.foreign_table, column: r.foreign_column }]));

            // Get indexes
            const idxResult = await this.pool.query(`
        SELECT indexname FROM pg_indexes WHERE schemaname = $1 AND tablename = $2
      `, [schemaName, tableRow.table_name]);

            const columns: ColumnMeta[] = colsResult.rows.map((col) => ({
                name: col.column_name,
                type: col.data_type,
                nullable: col.is_nullable === "YES",
                isPrimaryKey: pkCols.has(col.column_name),
                isForeignKey: fkMap.has(col.column_name),
                references: fkMap.get(col.column_name),
                description: col.description,
            }));

            tables.push({
                name: tableRow.table_name,
                schema: schemaName,
                columns,
                indexes: idxResult.rows.map((r) => r.indexname),
                rowCount: parseInt(tableRow.row_count) || 0,
                description: tableRow.description,
            });
        }

        this.schemaContext = { tables };
        return this.schemaContext;
    }

    /** Generate a system prompt for schema-aware SQL generation */
    buildSchemaPrompt(): string {
        if (!this.schemaContext) throw new Error("Schema not loaded. Call loadSchema() first.");

        const lines: string[] = [
            "You are a PostgreSQL SQL expert. Generate ONLY valid PostgreSQL SQL based on the schema below.",
            "Always use proper quoting for identifiers. Never generate dangerous operations unless explicitly asked.",
            "Return ONLY the SQL query, no explanation.",
            "",
            "## Database Schema",
            "",
        ];

        for (const table of this.schemaContext.tables) {
            lines.push(`### Table: ${table.schema}.${table.name}${table.description ? ` — ${table.description}` : ""}`);
            if (table.rowCount) lines.push(`  (~${table.rowCount.toLocaleString()} rows)`);
            lines.push("  Columns:");
            for (const col of table.columns) {
                let colDesc = `    - ${col.name}: ${col.type}`;
                if (col.isPrimaryKey) colDesc += " [PK]";
                if (col.isForeignKey && col.references) colDesc += ` [FK → ${col.references.table}.${col.references.column}]`;
                if (!col.nullable) colDesc += " NOT NULL";
                if (col.description) colDesc += ` — ${col.description}`;
                lines.push(colDesc);
            }
            if (table.indexes.length > 0) {
                lines.push(`  Indexes: ${table.indexes.join(", ")}`);
            }
            lines.push("");
        }

        return lines.join("\n");
    }

    /**
     * Convert natural language to SQL.
     * This provides the structure — actual LLM call is handled by the server layer
     * so it can work with any provider (OpenAI, Anthropic, local models, etc.)
     */
    prepareNLQuery(naturalLanguage: string): {
        systemPrompt: string;
        userPrompt: string;
        schemaContext: SchemaContext;
    } {
        const systemPrompt = this.buildSchemaPrompt();
        const userPrompt = `Convert this to a PostgreSQL query:\n\n${naturalLanguage}`;
        return { systemPrompt, userPrompt, schemaContext: this.schemaContext! };
    }

    /** Validate generated SQL for safety */
    checkSafety(sql: string, allowWrite = false): SafetyCheck {
        const warnings: string[] = [];
        const blockedOperations: string[] = [];
        let level: SafetyCheck["level"] = "read";

        for (const { pattern, label, level: opLevel } of DANGEROUS_PATTERNS) {
            if (pattern.test(sql)) {
                if (opLevel === "dangerous") {
                    blockedOperations.push(label);
                    warnings.push(`Blocked: ${label} detected`);
                    level = "dangerous";
                } else if (opLevel === "ddl") {
                    blockedOperations.push(label);
                    warnings.push(`Blocked: ${label} detected`);
                    if (level !== "dangerous") level = "ddl";
                } else if (opLevel === "write") {
                    if (!allowWrite) {
                        warnings.push(`Warning: ${label} detected`);
                    }
                    if (level === "read") level = "write";
                }
            }
        }

        // Check for missing WHERE on write operations
        if (/\b(DELETE|UPDATE)\b/i.test(sql) && !/\bWHERE\b/i.test(sql)) {
            warnings.push("CRITICAL: Write operation without WHERE clause — would affect all rows!");
            blockedOperations.push("Unscoped write operation");
            level = "dangerous";
        }

        return {
            safe: blockedOperations.length === 0,
            level,
            warnings,
            blockedOperations,
        };
    }

    /** Package a full NL→SQL result with safety check */
    buildResult(naturalLanguage: string, generatedSQL: string, explanation: string, confidence: number): NLQueryResult {
        return {
            naturalLanguage,
            sql: generatedSQL,
            explanation,
            safety: this.checkSafety(generatedSQL),
            confidence,
        };
    }

    getSchemaContext(): SchemaContext | null {
        return this.schemaContext;
    }
}
