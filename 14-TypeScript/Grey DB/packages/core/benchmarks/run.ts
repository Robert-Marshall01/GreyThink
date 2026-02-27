// ─────────────────────────────────────────────────────────────
// Grey DB — Performance Benchmarks
// Run: npx ts-node packages/core/benchmarks/run.ts
// Requires: a running PostgreSQL instance
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";

const pool = new Pool({
    host: process.env.PGHOST || "localhost",
    port: parseInt(process.env.PGPORT || "5432"),
    database: process.env.PGDATABASE || "greydb_test",
    user: process.env.PGUSER || "postgres",
    password: process.env.PGPASSWORD || "postgres",
    max: 20,
});

interface BenchmarkResult {
    name: string;
    iterations: number;
    totalMs: number;
    avgMs: number;
    p50Ms: number;
    p95Ms: number;
    p99Ms: number;
    opsPerSec: number;
}

async function benchmark(
    name: string,
    fn: () => Promise<void>,
    iterations: number
): Promise<BenchmarkResult> {
    const durations: number[] = [];

    // Warmup
    for (let i = 0; i < Math.min(10, iterations); i++) {
        await fn();
    }

    // Actual measurement
    for (let i = 0; i < iterations; i++) {
        const start = performance.now();
        await fn();
        durations.push(performance.now() - start);
    }

    durations.sort((a, b) => a - b);
    const totalMs = durations.reduce((a, b) => a + b, 0);

    return {
        name,
        iterations,
        totalMs: Math.round(totalMs * 100) / 100,
        avgMs: Math.round((totalMs / iterations) * 100) / 100,
        p50Ms: Math.round(durations[Math.floor(iterations * 0.5)] * 100) / 100,
        p95Ms: Math.round(durations[Math.floor(iterations * 0.95)] * 100) / 100,
        p99Ms: Math.round(durations[Math.floor(iterations * 0.99)] * 100) / 100,
        opsPerSec: Math.round(iterations / (totalMs / 1000)),
    };
}

function printResult(r: BenchmarkResult) {
    console.log(`\n  ${r.name}`);
    console.log(`  ${"─".repeat(50)}`);
    console.log(`  Iterations:  ${r.iterations}`);
    console.log(`  Total:       ${r.totalMs.toFixed(1)}ms`);
    console.log(`  Avg:         ${r.avgMs.toFixed(2)}ms`);
    console.log(`  P50:         ${r.p50Ms.toFixed(2)}ms`);
    console.log(`  P95:         ${r.p95Ms.toFixed(2)}ms`);
    console.log(`  P99:         ${r.p99Ms.toFixed(2)}ms`);
    console.log(`  Ops/sec:     ${r.opsPerSec.toLocaleString()}`);
}

// ── Setup ───────────────────────────────────────────────────

async function setup() {
    console.log("\n🔧 Setting up benchmark tables...");

    await pool.query("DROP TABLE IF EXISTS bench_orders CASCADE");
    await pool.query("DROP TABLE IF EXISTS bench_users CASCADE");

    await pool.query(`
    CREATE TABLE bench_users (
      id SERIAL PRIMARY KEY,
      tenant_id UUID NOT NULL DEFAULT gen_random_uuid(),
      email TEXT NOT NULL,
      name TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'active',
      created_at TIMESTAMPTZ DEFAULT NOW()
    )
  `);

    await pool.query(`
    CREATE TABLE bench_orders (
      id SERIAL PRIMARY KEY,
      tenant_id UUID NOT NULL,
      user_id INTEGER REFERENCES bench_users(id),
      amount NUMERIC(12,2) NOT NULL,
      status TEXT NOT NULL DEFAULT 'pending',
      created_at TIMESTAMPTZ DEFAULT NOW()
    )
  `);

    // Insert 100K users with 10 distinct tenant_ids
    console.log("  Inserting 100,000 users...");
    const tenantIds: string[] = [];
    for (let i = 0; i < 10; i++) {
        const res = await pool.query("SELECT gen_random_uuid() AS id");
        tenantIds.push(res.rows[0].id);
    }

    const BATCH = 5000;
    for (let batch = 0; batch < 100000 / BATCH; batch++) {
        const values: string[] = [];
        const params: any[] = [];
        let idx = 1;
        for (let i = 0; i < BATCH; i++) {
            const rowNum = batch * BATCH + i;
            const tid = tenantIds[rowNum % 10];
            values.push(`($${idx++}, $${idx++}, $${idx++})`);
            params.push(tid, `user${rowNum}@bench.com`, `User ${rowNum}`);
        }
        await pool.query(
            `INSERT INTO bench_users (tenant_id, email, name) VALUES ${values.join(",")}`,
            params
        );
    }

    // Insert 500K orders
    console.log("  Inserting 500,000 orders...");
    for (let batch = 0; batch < 500000 / BATCH; batch++) {
        const values: string[] = [];
        const params: any[] = [];
        let idx = 1;
        for (let i = 0; i < BATCH; i++) {
            const userId = (batch * BATCH + i) % 100000 + 1;
            const tid = tenantIds[(batch * BATCH + i) % 10];
            const amount = Math.round(Math.random() * 100000) / 100;
            const status = ["pending", "paid", "shipped", "cancelled"][Math.floor(Math.random() * 4)];
            values.push(`($${idx++}, $${idx++}, $${idx++}, $${idx++})`);
            params.push(tid, userId, amount, status);
        }
        await pool.query(
            `INSERT INTO bench_orders (tenant_id, user_id, amount, status) VALUES ${values.join(",")}`,
            params
        );
    }

    await pool.query("ANALYZE bench_users");
    await pool.query("ANALYZE bench_orders");

    console.log("  ✓ Setup complete (100K users, 500K orders, 10 tenants)\n");
    return tenantIds;
}

// ── Benchmarks ──────────────────────────────────────────────

async function run() {
    console.log("═══════════════════════════════════════════════════════");
    console.log("  Grey DB — Performance Benchmarks");
    console.log("═══════════════════════════════════════════════════════");

    const tenantIds = await setup();
    const results: BenchmarkResult[] = [];

    // 1. Point query WITHOUT index (seq scan on email)
    results.push(
        await benchmark(
            "Point query — email lookup (NO index)",
            async () => {
                const email = `user${Math.floor(Math.random() * 100000)}@bench.com`;
                await pool.query("SELECT * FROM bench_users WHERE email = $1", [email]);
            },
            200
        )
    );

    // 2. Point query WITH index
    await pool.query("CREATE INDEX idx_bench_users_email ON bench_users (email)");
    await pool.query("ANALYZE bench_users");

    results.push(
        await benchmark(
            "Point query — email lookup (WITH index)",
            async () => {
                const email = `user${Math.floor(Math.random() * 100000)}@bench.com`;
                await pool.query("SELECT * FROM bench_users WHERE email = $1", [email]);
            },
            200
        )
    );

    // 3. Tenant-scoped query WITHOUT tenant index
    results.push(
        await benchmark(
            "Tenant query — orders by tenant (NO tenant index)",
            async () => {
                const tid = tenantIds[Math.floor(Math.random() * 10)];
                await pool.query(
                    "SELECT COUNT(*), SUM(amount) FROM bench_orders WHERE tenant_id = $1",
                    [tid]
                );
            },
            100
        )
    );

    // 4. Tenant-scoped query WITH tenant index
    await pool.query("CREATE INDEX idx_bench_orders_tenant ON bench_orders (tenant_id)");
    await pool.query("ANALYZE bench_orders");

    results.push(
        await benchmark(
            "Tenant query — orders by tenant (WITH tenant index)",
            async () => {
                const tid = tenantIds[Math.floor(Math.random() * 10)];
                await pool.query(
                    "SELECT COUNT(*), SUM(amount) FROM bench_orders WHERE tenant_id = $1",
                    [tid]
                );
            },
            100
        )
    );

    // 5. Join query
    results.push(
        await benchmark(
            "Join query — users + orders (top 10 spenders per tenant)",
            async () => {
                const tid = tenantIds[Math.floor(Math.random() * 10)];
                await pool.query(
                    `SELECT u.name, SUM(o.amount) AS total
           FROM bench_users u
           JOIN bench_orders o ON u.id = o.user_id
           WHERE o.tenant_id = $1
           GROUP BY u.name
           ORDER BY total DESC
           LIMIT 10`,
                    [tid]
                );
            },
            50
        )
    );

    // 6. Aggregate query
    results.push(
        await benchmark(
            "Aggregate — order stats across all tenants",
            async () => {
                await pool.query(`
          SELECT
            tenant_id,
            COUNT(*) AS order_count,
            AVG(amount) AS avg_amount,
            MAX(amount) AS max_amount
          FROM bench_orders
          GROUP BY tenant_id
        `);
            },
            50
        )
    );

    // 7. Migration timing (DDL under load)
    results.push(
        await benchmark(
            "Migration — ADD COLUMN (online DDL)",
            async () => {
                await pool.query(
                    "ALTER TABLE bench_users ADD COLUMN IF NOT EXISTS bench_col TEXT"
                );
                await pool.query(
                    "ALTER TABLE bench_users DROP COLUMN IF EXISTS bench_col"
                );
            },
            20
        )
    );

    // 8. Concurrent reads (simulate multi-tenant)
    results.push(
        await benchmark(
            "Multi-tenant concurrent reads (10 tenants, round-robin)",
            async () => {
                const promises = tenantIds.map((tid) =>
                    pool.query("SELECT * FROM bench_orders WHERE tenant_id = $1 LIMIT 10", [tid])
                );
                await Promise.all(promises);
            },
            50
        )
    );

    // ── Print Results ───────────────────────────────────────────

    console.log("\n═══════════════════════════════════════════════════════");
    console.log("  Results");
    console.log("═══════════════════════════════════════════════════════");

    for (const r of results) {
        printResult(r);
    }

    // ── Summary Table ─────────────────────────────────────────

    console.log("\n\n  Summary");
    console.log("  " + "─".repeat(72));
    console.log(
        "  " +
        "Benchmark".padEnd(52) +
        "Avg (ms)".padStart(10) +
        "P95 (ms)".padStart(10)
    );
    console.log("  " + "─".repeat(72));
    for (const r of results) {
        console.log(
            "  " +
            r.name.padEnd(52) +
            r.avgMs.toFixed(2).padStart(10) +
            r.p95Ms.toFixed(2).padStart(10)
        );
    }
    console.log("  " + "─".repeat(72));

    // ── Cleanup ─────────────────────────────────────────────

    console.log("\n🧹 Cleaning up...");
    await pool.query("DROP TABLE IF EXISTS bench_orders CASCADE");
    await pool.query("DROP TABLE IF EXISTS bench_users CASCADE");
    await pool.end();
    console.log("  ✓ Done\n");
}

run().catch(console.error);
