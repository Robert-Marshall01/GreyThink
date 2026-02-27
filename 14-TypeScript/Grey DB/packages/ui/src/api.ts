// ─────────────────────────────────────────────────────────────
// Grey DB — API Client
// ─────────────────────────────────────────────────────────────

const BASE_URL = "/api";

async function request(path: string, options: RequestInit = {}): Promise<any> {
    const res = await fetch(`${BASE_URL}${path}`, {
        headers: {
            "Content-Type": "application/json",
            ...options.headers,
        },
        ...options,
    });
    const data = await res.json();
    if (!res.ok) throw new Error(data.error || `HTTP ${res.status}`);
    return data;
}

export const api = {
    // Health
    health: () => request("/health"),

    // Schema
    getTables: (schema = "public") => request(`/schema/tables?schema=${schema}`),
    getTable: (name: string, schema = "public") => request(`/schema/tables/${name}?schema=${schema}`),
    generateSQL: (body: any) => request("/schema/generate", { method: "POST", body: JSON.stringify(body) }),
    getMigrations: () => request("/schema/migrations"),
    applyMigration: (body: any) => request("/schema/migrations/apply", { method: "POST", body: JSON.stringify(body) }),
    rollbackMigration: () => request("/schema/migrations/rollback", { method: "POST" }),
    getSchemaHistory: () => request("/schema/registry/history"),

    // Tenants
    getTenants: () => request("/tenants"),
    getTenant: (id: string) => request(`/tenants/${id}`),
    createTenant: (body: any) => request("/tenants", { method: "POST", body: JSON.stringify(body) }),
    updateTenant: (id: string, body: any) => request(`/tenants/${id}`, { method: "PATCH", body: JSON.stringify(body) }),
    deleteTenant: (id: string) => request(`/tenants/${id}`, { method: "DELETE" }),

    // Query
    executeQuery: (sql: string, params?: any[], tenantId?: string) =>
        request("/query/execute", { method: "POST", body: JSON.stringify({ sql, params: params || [], tenantId }) }),
    explainQuery: (sql: string, analyze = false) =>
        request("/query/explain", { method: "POST", body: JSON.stringify({ sql, analyze }) }),
    getIndexes: (schema = "public") => request(`/query/indexes?schema=${schema}`),
    getUnusedIndexes: (schema = "public") => request(`/query/indexes/unused?schema=${schema}`),
    getDuplicateIndexes: (schema = "public") => request(`/query/indexes/duplicates?schema=${schema}`),
    getIndexSuggestions: (schema = "public") => request(`/query/indexes/suggest?schema=${schema}`),
    createIndex: (body: any) => request("/query/indexes", { method: "POST", body: JSON.stringify(body) }),
    dropIndex: (name: string) => request(`/query/indexes/${name}`, { method: "DELETE" }),
    getSlowQueries: (limit = 50) => request(`/query/slow-queries?limit=${limit}`),
    getSlowQueryStats: () => request("/query/slow-queries/stats"),
    getSlowQueryPatterns: () => request("/query/slow-queries/patterns"),

    // AI
    loadAISchema: (schema = "public") => request("/ai/schema/load", { method: "POST", body: JSON.stringify({ schema }) }),
    nlToSql: (query: string) => request("/ai/nl-to-sql", { method: "POST", body: JSON.stringify({ query }) }),
    validateSQL: (sql: string) => request("/ai/validate-sql", { method: "POST", body: JSON.stringify({ sql }) }),
    checkSqlSafety: (sql: string) => request("/ai/validate-sql", { method: "POST", body: JSON.stringify({ sql }) }),
    getTableDocs: (name: string) => request(`/ai/docs/table/${name}`),
    getAllDocs: () => request("/ai/docs"),
    getERDiagram: () => request("/ai/docs/diagram"),

    // Ops
    getDashboard: (hours = 24) => request(`/ops/dashboard?hours=${hours}`),
    getAuditLog: (limit = 100) => request(`/ops/audit-log?limit=${limit}`),
    createBackup: () => request("/ops/backups/full", { method: "POST" }),
    getBackups: () => request("/ops/backups"),
    getUsers: () => request("/ops/users"),
    getRoles: () => request("/ops/roles"),
};
