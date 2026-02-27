// ─────────────────────────────────────────────────────────────
// Grey DB — Auto Documentation Generator
// Table summaries, relationship diagrams, guides
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";
import { SchemaContext, TableMeta, ColumnMeta } from "./nl-to-sql";

export interface TableDoc {
    name: string;
    schema: string;
    summary: string;
    columns: ColumnDoc[];
    relationships: RelationshipDoc[];
    indexes: IndexDoc[];
    statistics: TableStats;
    queryGuides: QueryGuide[];
}

export interface ColumnDoc {
    name: string;
    type: string;
    description: string;
    constraints: string[];
    sampleValues?: string[];
}

export interface RelationshipDoc {
    type: "belongs_to" | "has_many" | "has_one" | "many_to_many";
    relatedTable: string;
    localColumn: string;
    foreignColumn: string;
    description: string;
}

export interface IndexDoc {
    name: string;
    columns: string[];
    type: string;
    unique: boolean;
    explanation: string;
}

export interface TableStats {
    rowCount: number;
    sizeOnDisk: string;
    deadTuples: number;
    lastVacuum?: Date;
    lastAnalyze?: Date;
}

export interface QueryGuide {
    title: string;
    description: string;
    sql: string;
}

export class AutoDocGenerator {
    constructor(private pool: Pool) { }

    /** Generate comprehensive documentation for a table */
    async generateTableDoc(tableName: string, schema = "public"): Promise<TableDoc> {
        const [columns, relationships, indexes, stats] = await Promise.all([
            this.getColumnDocs(tableName, schema),
            this.getRelationships(tableName, schema),
            this.getIndexDocs(tableName, schema),
            this.getTableStats(tableName, schema),
        ]);

        const summary = this.generateSummary(tableName, columns, relationships);
        const queryGuides = this.generateQueryGuides(tableName, schema, columns, relationships);

        return {
            name: tableName,
            schema,
            summary,
            columns,
            relationships,
            indexes,
            statistics: stats,
            queryGuides,
        };
    }

    /** Generate docs for all tables */
    async generateAllDocs(schema = "public"): Promise<TableDoc[]> {
        const result = await this.pool.query(
            `SELECT table_name FROM information_schema.tables
       WHERE table_schema = $1 AND table_type = 'BASE TABLE' AND table_name NOT LIKE 'grey_%'
       ORDER BY table_name`,
            [schema]
        );
        const docs: TableDoc[] = [];
        for (const row of result.rows) {
            docs.push(await this.generateTableDoc(row.table_name, schema));
        }
        return docs;
    }

    /** Generate a Mermaid ER diagram */
    async generateERDiagram(schema = "public"): Promise<string> {
        const result = await this.pool.query(`
      SELECT
        tc.table_name,
        kcu.column_name,
        ccu.table_name AS foreign_table,
        ccu.column_name AS foreign_column
      FROM information_schema.table_constraints tc
      JOIN information_schema.key_column_usage kcu ON tc.constraint_name = kcu.constraint_name
      JOIN information_schema.constraint_column_usage ccu ON tc.constraint_name = ccu.constraint_name
      WHERE tc.constraint_type = 'FOREIGN KEY' AND tc.table_schema = $1
    `, [schema]);

        const lines: string[] = ["erDiagram"];
        const tables = new Set<string>();

        for (const row of result.rows) {
            tables.add(row.table_name);
            tables.add(row.foreign_table);
            lines.push(`  ${row.foreign_table} ||--o{ ${row.table_name} : "${row.column_name}"`);
        }

        // Add tables with columns
        const tablesResult = await this.pool.query(`
      SELECT t.table_name, c.column_name, c.data_type
      FROM information_schema.tables t
      JOIN information_schema.columns c ON t.table_name = c.table_name AND t.table_schema = c.table_schema
      WHERE t.table_schema = $1 AND t.table_type = 'BASE TABLE' AND t.table_name NOT LIKE 'grey_%'
      ORDER BY t.table_name, c.ordinal_position
    `, [schema]);

        const tableColumns = new Map<string, Array<{ name: string; type: string }>>();
        for (const row of tablesResult.rows) {
            if (!tableColumns.has(row.table_name)) tableColumns.set(row.table_name, []);
            tableColumns.get(row.table_name)!.push({ name: row.column_name, type: row.data_type });
        }

        for (const [table, cols] of tableColumns) {
            lines.push(`  ${table} {`);
            for (const col of cols) {
                lines.push(`    ${col.type.replace(/\s+/g, "_")} ${col.name}`);
            }
            lines.push("  }");
        }

        return lines.join("\n");
    }

    // ── Private helpers ─────────────────────────────────────

    private async getColumnDocs(tableName: string, schema: string): Promise<ColumnDoc[]> {
        const result = await this.pool.query(`
      SELECT
        c.column_name,
        c.data_type,
        c.is_nullable,
        c.column_default,
        col_description((c.table_schema || '.' || c.table_name)::regclass, c.ordinal_position) AS description
      FROM information_schema.columns c
      WHERE c.table_schema = $1 AND c.table_name = $2
      ORDER BY c.ordinal_position
    `, [schema, tableName]);

        return result.rows.map((row) => {
            const constraints: string[] = [];
            if (row.is_nullable === "NO") constraints.push("NOT NULL");
            if (row.column_default) constraints.push(`DEFAULT: ${row.column_default}`);

            return {
                name: row.column_name,
                type: row.data_type,
                description: row.description || this.inferColumnDescription(row.column_name, row.data_type),
                constraints,
            };
        });
    }

    private async getRelationships(tableName: string, schema: string): Promise<RelationshipDoc[]> {
        const result = await this.pool.query(`
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
    `, [schema, tableName]);

        return result.rows.map((row) => ({
            type: "belongs_to" as const,
            relatedTable: row.foreign_table,
            localColumn: row.column_name,
            foreignColumn: row.foreign_column,
            description: `${tableName} belongs to ${row.foreign_table} via ${row.column_name}`,
        }));
    }

    private async getIndexDocs(tableName: string, schema: string): Promise<IndexDoc[]> {
        const result = await this.pool.query(`
      SELECT
        i.relname AS index_name,
        ix.indisunique AS is_unique,
        am.amname AS method,
        array_agg(a.attname ORDER BY k.n) AS columns
      FROM pg_index ix
      JOIN pg_class i ON i.oid = ix.indexrelid
      JOIN pg_class t ON t.oid = ix.indrelid
      JOIN pg_namespace n ON n.oid = t.relnamespace
      JOIN pg_am am ON am.oid = i.relam
      CROSS JOIN LATERAL unnest(ix.indkey) WITH ORDINALITY AS k(attnum, n)
      JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = k.attnum
      WHERE n.nspname = $1 AND t.relname = $2
      GROUP BY i.relname, ix.indisunique, am.amname
    `, [schema, tableName]);

        return result.rows.map((row) => ({
            name: row.index_name,
            columns: row.columns,
            type: row.method,
            unique: row.is_unique,
            explanation: this.explainIndex(row.index_name, row.columns, row.method, row.is_unique),
        }));
    }

    private async getTableStats(tableName: string, schema: string): Promise<TableStats> {
        const result = await this.pool.query(`
      SELECT
        n_live_tup AS row_count,
        pg_size_pretty(pg_total_relation_size((schemaname || '.' || relname)::regclass)) AS size,
        n_dead_tup AS dead_tuples,
        last_vacuum,
        last_analyze
      FROM pg_stat_user_tables
      WHERE schemaname = $1 AND relname = $2
    `, [schema, tableName]);

        if (result.rows.length === 0) {
            return { rowCount: 0, sizeOnDisk: "0 bytes", deadTuples: 0 };
        }

        const row = result.rows[0];
        return {
            rowCount: parseInt(row.row_count) || 0,
            sizeOnDisk: row.size || "0 bytes",
            deadTuples: parseInt(row.dead_tuples) || 0,
            lastVacuum: row.last_vacuum,
            lastAnalyze: row.last_analyze,
        };
    }

    private generateSummary(tableName: string, columns: ColumnDoc[], relationships: RelationshipDoc[]): string {
        const colCount = columns.length;
        const relCount = relationships.length;
        const hasId = columns.some((c) => c.name === "id");
        const hasTimestamps = columns.some((c) => c.name.includes("created_at") || c.name.includes("updated_at"));

        let summary = `Table "${tableName}" has ${colCount} columns`;
        if (relCount > 0) summary += ` and ${relCount} foreign key relationship${relCount > 1 ? "s" : ""}`;
        summary += ".";
        if (hasId && hasTimestamps) summary += " Standard entity pattern with ID and timestamps.";
        return summary;
    }

    private generateQueryGuides(tableName: string, schema: string, columns: ColumnDoc[], relationships: RelationshipDoc[]): QueryGuide[] {
        const guides: QueryGuide[] = [];

        // Basic select
        guides.push({
            title: `Select all from ${tableName}`,
            description: "Basic query to retrieve all rows",
            sql: `SELECT * FROM "${schema}"."${tableName}" LIMIT 100;`,
        });

        // Count
        guides.push({
            title: `Count rows in ${tableName}`,
            description: "Get the total number of rows",
            sql: `SELECT COUNT(*) FROM "${schema}"."${tableName}";`,
        });

        // Join guides for relationships
        for (const rel of relationships) {
            guides.push({
                title: `Join ${tableName} with ${rel.relatedTable}`,
                description: `Query ${tableName} with its related ${rel.relatedTable}`,
                sql: `SELECT t.*, r.* FROM "${schema}"."${tableName}" t\nJOIN "${schema}"."${rel.relatedTable}" r ON t."${rel.localColumn}" = r."${rel.foreignColumn}"\nLIMIT 100;`,
            });
        }

        return guides;
    }

    private inferColumnDescription(colName: string, dataType: string): string {
        const patterns: Record<string, string> = {
            id: "Unique identifier",
            uuid: "UUID identifier",
            email: "Email address",
            name: "Name field",
            title: "Title",
            description: "Description text",
            created_at: "Creation timestamp",
            updated_at: "Last update timestamp",
            deleted_at: "Soft delete timestamp",
            tenant_id: "Tenant identifier for multi-tenant isolation",
            user_id: "Reference to the user",
            status: "Current status",
            active: "Whether this record is active",
            password: "Hashed password",
            slug: "URL-friendly identifier",
        };

        for (const [pattern, desc] of Object.entries(patterns)) {
            if (colName.includes(pattern)) return desc;
        }
        return `${dataType} column`;
    }

    private explainIndex(name: string, columns: string[], method: string, unique: boolean): string {
        const u = unique ? "unique " : "";
        return `${u}${method} index on (${columns.join(", ")}) — speeds up queries filtering or sorting by ${columns.join(" and ")}.`;
    }
}
