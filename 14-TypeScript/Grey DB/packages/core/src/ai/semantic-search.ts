// ─────────────────────────────────────────────────────────────
// Grey DB — Semantic Search Engine
// Embeddings, vector indexes, hybrid search
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";

export interface SemanticSearchConfig {
    table: string;
    schema: string;
    embeddingColumn: string;
    contentColumns: string[];
    dimensions: number;
    indexMethod: "ivfflat" | "hnsw";
}

export interface SearchResult {
    id: any;
    similarity: number;
    data: Record<string, any>;
}

export class SemanticSearch {
    constructor(private pool: Pool) { }

    /** Ensure pgvector extension is available */
    async init(): Promise<void> {
        await this.pool.query(`CREATE EXTENSION IF NOT EXISTS vector;`);
    }

    /** Add an embedding column to a table */
    async addEmbeddingColumn(config: SemanticSearchConfig): Promise<string> {
        const sql = `
      ALTER TABLE "${config.schema}"."${config.table}"
      ADD COLUMN IF NOT EXISTS "${config.embeddingColumn}" vector(${config.dimensions});

      CREATE INDEX IF NOT EXISTS "idx_${config.table}_${config.embeddingColumn}"
      ON "${config.schema}"."${config.table}"
      USING ${config.indexMethod} ("${config.embeddingColumn}" vector_cosine_ops);
    `;
        await this.pool.query(sql);
        return sql;
    }

    /** Store an embedding for a row */
    async storeEmbedding(table: string, id: any, embeddingColumn: string, embedding: number[], schema = "public"): Promise<void> {
        const vectorStr = `[${embedding.join(",")}]`;
        await this.pool.query(
            `UPDATE "${schema}"."${table}" SET "${embeddingColumn}" = $1::vector WHERE id = $2`,
            [vectorStr, id]
        );
    }

    /** Pure semantic search using cosine similarity */
    async search(
        table: string,
        embeddingColumn: string,
        queryEmbedding: number[],
        limit = 10,
        schema = "public",
        selectColumns = ["*"]
    ): Promise<SearchResult[]> {
        const vectorStr = `[${queryEmbedding.join(",")}]`;
        const cols = selectColumns.join(", ");
        const result = await this.pool.query(
            `SELECT ${cols}, 1 - ("${embeddingColumn}" <=> $1::vector) AS similarity
       FROM "${schema}"."${table}"
       WHERE "${embeddingColumn}" IS NOT NULL
       ORDER BY "${embeddingColumn}" <=> $1::vector
       LIMIT $2`,
            [vectorStr, limit]
        );

        return result.rows.map((row) => {
            const { similarity, ...data } = row;
            return { id: data.id, similarity: parseFloat(similarity), data };
        });
    }

    /** Hybrid search: semantic + SQL filters */
    async hybridSearch(
        table: string,
        embeddingColumn: string,
        queryEmbedding: number[],
        filters: { column: string; op: string; value: any }[],
        limit = 10,
        schema = "public",
        selectColumns = ["*"]
    ): Promise<SearchResult[]> {
        const vectorStr = `[${queryEmbedding.join(",")}]`;
        const cols = selectColumns.join(", ");

        let paramIdx = 2;
        const params: any[] = [vectorStr];
        const whereParts: string[] = [`"${embeddingColumn}" IS NOT NULL`];

        for (const filter of filters) {
            whereParts.push(`"${filter.column}" ${filter.op} $${paramIdx++}`);
            params.push(filter.value);
        }

        params.push(limit);

        const result = await this.pool.query(
            `SELECT ${cols}, 1 - ("${embeddingColumn}" <=> $1::vector) AS similarity
       FROM "${schema}"."${table}"
       WHERE ${whereParts.join(" AND ")}
       ORDER BY "${embeddingColumn}" <=> $1::vector
       LIMIT $${paramIdx}`,
            params
        );

        return result.rows.map((row) => {
            const { similarity, ...data } = row;
            return { id: data.id, similarity: parseFloat(similarity), data };
        });
    }

    /** Search with tenant isolation */
    async tenantSearch(
        table: string,
        embeddingColumn: string,
        queryEmbedding: number[],
        tenantId: string,
        limit = 10,
        schema = "public"
    ): Promise<SearchResult[]> {
        return this.hybridSearch(
            table,
            embeddingColumn,
            queryEmbedding,
            [{ column: "tenant_id", op: "=", value: tenantId }],
            limit,
            schema
        );
    }
}
