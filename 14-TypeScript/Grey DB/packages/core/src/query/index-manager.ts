// ─────────────────────────────────────────────────────────────
// Grey DB — Index Manager
// Declarative index definitions, usage analysis, suggestions
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";

export interface IndexInfo {
    name: string;
    tableName: string;
    schema: string;
    columns: string[];
    unique: boolean;
    method: string;
    size: string;
    scans: number;
    tuplesRead: number;
    tuplesFetched: number;
    usageRatio: number; // 0-1 how often used vs total queries
}

export interface IndexSuggestion {
    table: string;
    columns: string[];
    reason: string;
    estimatedImpact: "high" | "medium" | "low";
    createSQL: string;
}

export class IndexManager {
    constructor(private pool: Pool) { }

    /** Get all indexes with usage statistics */
    async getIndexes(schema = "public"): Promise<IndexInfo[]> {
        const result = await this.pool.query(`
      SELECT
        i.relname AS index_name,
        t.relname AS table_name,
        n.nspname AS schema_name,
        am.amname AS method,
        ix.indisunique AS is_unique,
        pg_size_pretty(pg_relation_size(i.oid)) AS size,
        COALESCE(s.idx_scan, 0) AS scans,
        COALESCE(s.idx_tup_read, 0) AS tuples_read,
        COALESCE(s.idx_tup_fetch, 0) AS tuples_fetched,
        array_agg(a.attname ORDER BY k.n) AS columns
      FROM pg_index ix
      JOIN pg_class i ON i.oid = ix.indexrelid
      JOIN pg_class t ON t.oid = ix.indrelid
      JOIN pg_namespace n ON n.oid = t.relnamespace
      JOIN pg_am am ON am.oid = i.relam
      LEFT JOIN pg_stat_user_indexes s ON s.indexrelid = i.oid
      CROSS JOIN LATERAL unnest(ix.indkey) WITH ORDINALITY AS k(attnum, n)
      JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = k.attnum
      WHERE n.nspname = $1
        AND NOT ix.indisprimary
      GROUP BY i.relname, t.relname, n.nspname, am.amname, ix.indisunique,
               pg_relation_size(i.oid), s.idx_scan, s.idx_tup_read, s.idx_tup_fetch
      ORDER BY t.relname, i.relname
    `, [schema]);

        return result.rows.map((row) => ({
            name: row.index_name,
            tableName: row.table_name,
            schema: row.schema_name,
            columns: row.columns,
            unique: row.is_unique,
            method: row.method,
            size: row.size,
            scans: parseInt(row.scans),
            tuplesRead: parseInt(row.tuples_read),
            tuplesFetched: parseInt(row.tuples_fetched),
            usageRatio: 0, // computed below
        }));
    }

    /** Find unused indexes */
    async getUnusedIndexes(schema = "public"): Promise<IndexInfo[]> {
        const indexes = await this.getIndexes(schema);
        return indexes.filter((idx) => idx.scans === 0);
    }

    /** Find duplicate indexes (same columns, same table) */
    async getDuplicateIndexes(schema = "public"): Promise<Array<{ group: string; indexes: IndexInfo[] }>> {
        const indexes = await this.getIndexes(schema);
        const groups = new Map<string, IndexInfo[]>();

        for (const idx of indexes) {
            const key = `${idx.tableName}:${idx.columns.join(",")}`;
            if (!groups.has(key)) groups.set(key, []);
            groups.get(key)!.push(idx);
        }

        return Array.from(groups.entries())
            .filter(([, idxs]) => idxs.length > 1)
            .map(([group, indexes]) => ({ group, indexes }));
    }

    /** Suggest indexes based on sequential scan analysis */
    async suggestIndexes(schema = "public"): Promise<IndexSuggestion[]> {
        const suggestions: IndexSuggestion[] = [];

        // Find tables with high sequential scan ratios
        const result = await this.pool.query(`
      SELECT
        schemaname,
        relname AS table_name,
        seq_scan,
        idx_scan,
        seq_tup_read,
        n_live_tup,
        CASE WHEN (seq_scan + idx_scan) > 0
          THEN round(100.0 * seq_scan / (seq_scan + idx_scan), 2)
          ELSE 0 END AS seq_scan_pct
      FROM pg_stat_user_tables
      WHERE schemaname = $1
        AND n_live_tup > 100
        AND seq_scan > idx_scan
      ORDER BY seq_tup_read DESC
      LIMIT 20
    `, [schema]);

        for (const row of result.rows) {
            if (row.seq_scan_pct > 50 && row.n_live_tup > 1000) {
                suggestions.push({
                    table: row.table_name,
                    columns: [], // Would need query log analysis for specific columns
                    reason: `Table "${row.table_name}" has ${row.seq_scan_pct}% sequential scans (${row.seq_scan} seq vs ${row.idx_scan} idx). ${row.n_live_tup} live tuples.`,
                    estimatedImpact: row.seq_scan_pct > 80 ? "high" : "medium",
                    createSQL: `-- Analyze your WHERE clauses on "${row.table_name}" and add appropriate indexes`,
                });
            }
        }

        return suggestions;
    }

    /** Create an index (with optional CONCURRENTLY) */
    async createIndex(
        name: string,
        table: string,
        columns: string[],
        opts?: { unique?: boolean; method?: string; concurrently?: boolean; where?: string; schema?: string }
    ): Promise<string> {
        const schema = opts?.schema ?? "public";
        const u = opts?.unique ? "UNIQUE " : "";
        const method = opts?.method ? ` USING ${opts.method}` : "";
        const conc = opts?.concurrently ? " CONCURRENTLY" : "";
        const cols = columns.map((c) => `"${c}"`).join(", ");
        const where = opts?.where ? ` WHERE ${opts.where}` : "";

        const sql = `CREATE ${u}INDEX${conc} "${name}" ON "${schema}"."${table}"${method} (${cols})${where};`;
        await this.pool.query(sql);
        return sql;
    }

    /** Drop an index */
    async dropIndex(name: string, concurrently = false): Promise<void> {
        const conc = concurrently ? " CONCURRENTLY" : "";
        await this.pool.query(`DROP INDEX${conc} IF EXISTS "${name}";`);
    }
}
