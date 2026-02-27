// ─────────────────────────────────────────────────────────────
// Grey DB — Pure Logic Benchmarks (no Postgres required)
// Run: npx ts-node packages/core/benchmarks/pure.ts
// ─────────────────────────────────────────────────────────────

import { greyTable, uuid, text, integer, timestamptz, boolean, jsonb, varchar, numeric } from "../src/schema/dsl";
import { diffSchemas } from "../src/schema/differ";
import { generateMigration } from "../src/schema/migrations";
import { QueryBuilder, InsertBuilder, UpdateBuilder, DeleteBuilder } from "../src/query/query-builder";
import { NLToSQL } from "../src/ai/nl-to-sql";

interface BenchResult {
    name: string;
    iterations: number;
    totalMs: number;
    avgUs: number;
    opsPerSec: number;
}

function bench(name: string, fn: () => void, iterations: number): BenchResult {
    // Warmup
    for (let i = 0; i < Math.min(100, iterations); i++) fn();

    const start = performance.now();
    for (let i = 0; i < iterations; i++) fn();
    const totalMs = performance.now() - start;

    return {
        name,
        iterations,
        totalMs: Math.round(totalMs * 100) / 100,
        avgUs: Math.round(((totalMs / iterations) * 1000) * 100) / 100,
        opsPerSec: Math.round(iterations / (totalMs / 1000)),
    };
}

function printResult(r: BenchResult) {
    console.log(
        `  ${r.name.padEnd(52)} ${(r.avgUs + "µs").padStart(10)} ${r.opsPerSec.toLocaleString().padStart(12)} ops/s`
    );
}

// ── Setup Schemas ───────────────────────────────────────────

const smallSchema = [
    greyTable("users", {
        id: uuid().primaryKey().defaultUUID(),
        email: text().unique().indexed(),
        name: text().notNull(),
        active: boolean().default(true),
        createdAt: timestamptz().defaultNow(),
    }),
];

const mediumSchema = [
    ...smallSchema,
    greyTable("organizations", {
        id: uuid().primaryKey().defaultUUID(),
        name: text().notNull(),
        slug: varchar(100).unique(),
        plan: text().default("free"),
        createdAt: timestamptz().defaultNow(),
    }),
    greyTable("projects", {
        id: uuid().primaryKey().defaultUUID(),
        orgId: uuid().notNull().references("organizations", "id", { onDelete: "CASCADE" }),
        name: text().notNull(),
        description: text().nullable(),
        status: text().default("active"),
        createdAt: timestamptz().defaultNow(),
    }),
    greyTable("tasks", {
        id: uuid().primaryKey().defaultUUID(),
        projectId: uuid().notNull().references("projects", "id", { onDelete: "CASCADE" }),
        assigneeId: uuid().nullable().references("users", "id"),
        title: text().notNull(),
        description: text().nullable(),
        priority: integer().default(0),
        status: text().default("todo"),
        metadata: jsonb().nullable(),
        createdAt: timestamptz().defaultNow(),
        updatedAt: timestamptz().defaultNow(),
    }),
];

const largeSchema = [
    ...mediumSchema,
    greyTable("invoices", {
        id: uuid().primaryKey().defaultUUID(),
        orgId: uuid().notNull().references("organizations", "id"),
        amount: numeric(12, 2).notNull(),
        currency: varchar(3).default("USD"),
        status: text().default("draft"),
        issuedAt: timestamptz().nullable(),
        dueAt: timestamptz().nullable(),
        paidAt: timestamptz().nullable(),
        createdAt: timestamptz().defaultNow(),
    }),
    greyTable("line_items", {
        id: uuid().primaryKey().defaultUUID(),
        invoiceId: uuid().notNull().references("invoices", "id", { onDelete: "CASCADE" }),
        description: text().notNull(),
        quantity: integer().notNull().default(1),
        unitPrice: numeric(10, 2).notNull(),
        createdAt: timestamptz().defaultNow(),
    }),
    greyTable("audit_log", {
        id: uuid().primaryKey().defaultUUID(),
        userId: uuid().references("users", "id"),
        action: text().notNull(),
        entity: text().notNull(),
        entityId: uuid().notNull(),
        changes: jsonb().nullable(),
        ip: varchar(45).nullable(),
        createdAt: timestamptz().defaultNow(),
    }).withRLS().withTenantScope(),
    greyTable("notifications", {
        id: uuid().primaryKey().defaultUUID(),
        userId: uuid().notNull().references("users", "id"),
        type: text().notNull(),
        title: text().notNull(),
        body: text().nullable(),
        read: boolean().default(false),
        createdAt: timestamptz().defaultNow(),
    }),
];

// ── Evolved schemas (for diff benchmarks) ─────────────────

const evolvedLargeSchema = [
    ...largeSchema.slice(0, -1), // drop notifications
    greyTable("audit_log", {
        id: uuid().primaryKey().defaultUUID(),
        userId: uuid().references("users", "id"),
        tenantId: uuid().notNull(), // added
        action: text().notNull(),
        entity: text().notNull(),
        entityId: uuid().notNull(),
        changes: jsonb().nullable(),
        ip: varchar(45).nullable(),
        userAgent: text().nullable(), // added
        createdAt: timestamptz().defaultNow(),
    }).withRLS().withTenantScope(),
    greyTable("webhooks", { // new table
        id: uuid().primaryKey().defaultUUID(),
        orgId: uuid().notNull().references("organizations", "id"),
        url: text().notNull(),
        secret: text().notNull(),
        events: jsonb().notNull(),
        active: boolean().default(true),
        createdAt: timestamptz().defaultNow(),
    }),
];

// ── Run benchmarks ──────────────────────────────────────────

console.log("═══════════════════════════════════════════════════════════════════════════");
console.log("  Grey DB — Pure Logic Benchmarks (no database required)");
console.log("═══════════════════════════════════════════════════════════════════════════");
console.log("");

const results: BenchResult[] = [];
const N = 10_000;

// ── Schema Diff Benchmarks ──────────────────────────────────
console.log("  Schema Diff");
console.log("  " + "─".repeat(74));

results.push(bench("Diff identical small schema (1 table)", () => {
    diffSchemas(smallSchema, smallSchema);
}, N));

results.push(bench("Diff identical medium schema (4 tables)", () => {
    diffSchemas(mediumSchema, mediumSchema);
}, N));

results.push(bench("Diff identical large schema (8 tables)", () => {
    diffSchemas(largeSchema, largeSchema);
}, N));

results.push(bench("Diff with changes (8 vs evolved 8 tables)", () => {
    diffSchemas(largeSchema, evolvedLargeSchema);
}, N));

results.push(bench("Diff empty → large schema (full create)", () => {
    diffSchemas([], largeSchema);
}, N));

for (const r of results.slice(0, 5)) printResult(r);

// ── Migration Generation ────────────────────────────────────
console.log("\n  Migration Generation");
console.log("  " + "─".repeat(74));

const complexDiff = diffSchemas(largeSchema, evolvedLargeSchema);
const createDiff = diffSchemas([], largeSchema);

results.push(bench("Generate migration from complex diff", () => {
    generateMigration("test-migration", complexDiff, 1);
}, N));

results.push(bench("Generate migration from full-create diff", () => {
    generateMigration("initial", createDiff, 1);
}, N));

for (const r of results.slice(5, 7)) printResult(r);

// ── Query Builder ───────────────────────────────────────────
console.log("\n  Query Builder — SQL Compilation");
console.log("  " + "─".repeat(74));

results.push(bench("Simple SELECT (all columns)", () => {
    QueryBuilder.from("users").toSQL();
}, N));

results.push(bench("SELECT + WHERE + ORDER + LIMIT", () => {
    QueryBuilder.from("users")
        .select("id", "email", "name")
        .where("active", "=", true)
        .orderBy("createdAt", "DESC")
        .limit(20)
        .toSQL();
}, N));

results.push(bench("Complex query (JOIN + GROUP + HAVING + CTE)", () => {
    QueryBuilder.from("tasks")
        .withCTE("active_projects", "SELECT id FROM projects WHERE status = 'active'")
        .select("u.name")
        .selectAgg("COUNT", "t.id", "task_count")
        .selectAgg("AVG", "t.priority", "avg_priority")
        .join("users", "u.id = t.assignee_id", "LEFT", "u")
        .where("t.status", "!=", "done")
        .groupBy("u.name")
        .having("COUNT(t.id) > 5")
        .orderBy("task_count", "DESC")
        .limit(10)
        .toSQL();
}, N));

results.push(bench("Tenant-scoped query", () => {
    QueryBuilder.from("orders")
        .select("id", "amount", "status")
        .forTenant("550e8400-e29b-41d4-a716-446655440000")
        .where("status", "=", "pending")
        .orderBy("createdAt", "DESC")
        .toSQL();
}, N));

results.push(bench("INSERT builder (single row)", () => {
    InsertBuilder.into("users")
        .values({ id: "abc", email: "test@test.com", name: "Test" })
        .returning("id")
        .toSQL();
}, N));

results.push(bench("INSERT builder (10 rows)", () => {
    const rows = Array.from({ length: 10 }, (_, i) => ({
        id: `id-${i}`, email: `user${i}@test.com`, name: `User ${i}`,
    }));
    InsertBuilder.into("users").values(...rows).toSQL();
}, N));

results.push(bench("UPDATE builder (3 fields + WHERE)", () => {
    UpdateBuilder.table("users")
        .setAll({ name: "Updated Name", active: false, updatedAt: new Date().toISOString() })
        .where("id", "=", "abc")
        .returning("*")
        .toSQL();
}, N));

results.push(bench("DELETE builder (WHERE)", () => {
    DeleteBuilder.from("sessions")
        .where("expires_at", "<", new Date().toISOString())
        .toSQL();
}, N));

for (const r of results.slice(7, 15)) printResult(r);

// ── NL→SQL Safety Check ─────────────────────────────────────
console.log("\n  NL→SQL Safety Checker");
console.log("  " + "─".repeat(74));

// We need a pool mock since NLToSQL constructor requires one
const mockPool = {} as any;
const nlsql = new NLToSQL(mockPool);

results.push(bench("Safe SELECT query", () => {
    nlsql.checkSafety("SELECT * FROM users WHERE email = 'test@test.com'");
}, N));

results.push(bench("Write query (INSERT)", () => {
    nlsql.checkSafety("INSERT INTO users (email, name) VALUES ('a@b.com', 'Test')");
}, N));

results.push(bench("Dangerous query (DROP TABLE)", () => {
    nlsql.checkSafety("DROP TABLE users CASCADE");
}, N));

results.push(bench("Injection attempt (multi-statement)", () => {
    nlsql.checkSafety("SELECT 1; DROP TABLE users; --");
}, N));

results.push(bench("Complex safe query (CTE + JOIN + aggregate)", () => {
    nlsql.checkSafety(`
    WITH active_users AS (
      SELECT id, name FROM users WHERE active = true
    )
    SELECT au.name, COUNT(o.id) as order_count
    FROM active_users au
    JOIN orders o ON o.user_id = au.id
    GROUP BY au.name
    HAVING COUNT(o.id) > 5
    ORDER BY order_count DESC
  `);
}, N));

for (const r of results.slice(15)) printResult(r);

// ── Summary ─────────────────────────────────────────────────
console.log("\n\n  Summary");
console.log("  " + "═".repeat(74));
console.log(
    "  " + "Benchmark".padEnd(52) + "Avg".padStart(10) + "Throughput".padStart(14)
);
console.log("  " + "─".repeat(74));
for (const r of results) {
    console.log(
        "  " +
        r.name.substring(0, 51).padEnd(52) +
        (r.avgUs + "µs").padStart(10) +
        (r.opsPerSec.toLocaleString() + "/s").padStart(14)
    );
}
console.log("  " + "─".repeat(74));
console.log(`\n  Total benchmarks: ${results.length}`);
console.log(`  All measurements: ${N.toLocaleString()} iterations each\n`);
