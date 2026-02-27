// ─────────────────────────────────────────────────────────────
// Grey DB — Integration Tests (require running PostgreSQL)
// Tests multi-tenant isolation, migration apply/rollback,
// and query planner behavior with real database
// ─────────────────────────────────────────────────────────────

import { Pool } from "pg";
import { TenantEngine } from "../src/tenant/tenant-engine";
import {
    ensureMigrationTable, generateMigration, applyMigration,
    rollbackMigration, getCurrentVersion, getMigrationHistory,
} from "../src/schema/migrations";
import { greyTable, uuid, text, integer, timestamptz } from "../src/schema/dsl";
import { diffSchemas } from "../src/schema/differ";

// Skip integration tests if PG is not available
const PG_HOST = process.env.GREY_DB_HOST || process.env.PGHOST || "localhost";
const PG_PORT = parseInt(process.env.GREY_DB_PORT || process.env.PGPORT || "5432");
const PG_DB = process.env.GREY_DB_TEST_DB || process.env.PGDATABASE || "greydb_test";
const PG_USER = process.env.GREY_DB_USER || process.env.PGUSER || "postgres";
const PG_PASS = process.env.GREY_DB_PASSWORD || process.env.PGPASSWORD || "postgres";

let pool: Pool;

beforeAll(async () => {
    pool = new Pool({
        host: PG_HOST,
        port: PG_PORT,
        database: PG_DB,
        user: PG_USER,
        password: PG_PASS,
        max: 5,
    });

    // Verify connection
    try {
        await pool.query("SELECT 1");
    } catch (err) {
        console.warn("⚠ PostgreSQL not available — skipping integration tests");
        pool = null as any;
    }
});

afterAll(async () => {
    if (pool) await pool.end();
});

function skipIfNoDb() {
    if (!pool) {
        // Jest doesn't have Jasmine's `pending()` — use conditional skip pattern
        return true;
    }
    return false;
}

/** Wrap test body: skip gracefully when no DB */
function itDb(name: string, fn: () => Promise<void>) {
    it(name, async () => {
        if (skipIfNoDb()) return;
        await fn();
    });
}

// ── Migration Integration Tests ─────────────────────────────

describe("Migration Engine (integration)", () => {
    beforeEach(async () => {
        if (!pool) return;
        await pool.query("DROP TABLE IF EXISTS grey_migrations CASCADE");
        await pool.query("DROP TABLE IF EXISTS integration_test CASCADE");
        await ensureMigrationTable(pool);
    });

    itDb("applies a migration and records it", async () => {

        const diff = diffSchemas([], [
            greyTable("integration_test", {
                id: uuid().primaryKey(),
                name: text().notNull(),
                createdAt: timestamptz().defaultNow(),
            }),
        ]);

        const migration = generateMigration("create_integration_test", diff, 1);
        await applyMigration(pool, migration);

        // Verify table exists
        const result = await pool.query(
            "SELECT table_name FROM information_schema.tables WHERE table_name = 'integration_test'"
        );
        expect(result.rows).toHaveLength(1);

        // Verify migration recorded
        const version = await getCurrentVersion(pool);
        expect(version).toBe(1);
    });

    itDb("prevents duplicate migration application", async () => {

        const diff = diffSchemas([], [
            greyTable("integration_test", { id: uuid().primaryKey() }),
        ]);
        const migration = generateMigration("create_table", diff, 1);

        await applyMigration(pool, migration);

        // Second apply should throw
        await expect(applyMigration(pool, migration)).rejects.toThrow(
            "already applied"
        );
    });

    itDb("supports dry-run mode", async () => {

        const diff = diffSchemas([], [
            greyTable("integration_test", { id: uuid().primaryKey() }),
        ]);
        const migration = generateMigration("create_table", diff, 1);

        await applyMigration(pool, migration, { dryRun: true });

        // Table should NOT exist
        const result = await pool.query(
            "SELECT table_name FROM information_schema.tables WHERE table_name = 'integration_test'"
        );
        expect(result.rows).toHaveLength(0);

        // Migration should NOT be recorded
        const version = await getCurrentVersion(pool);
        expect(version).toBe(0);
    });

    itDb("rolls back the last migration", async () => {

        const diff = diffSchemas([], [
            greyTable("integration_test", { id: uuid().primaryKey() }),
        ]);
        const migration = generateMigration("create_table", diff, 1);
        await applyMigration(pool, migration);

        const rolledBack = await rollbackMigration(pool);
        expect(rolledBack).not.toBeNull();
        expect(rolledBack!.version).toBe(1);

        // Table should be dropped
        const result = await pool.query(
            "SELECT table_name FROM information_schema.tables WHERE table_name = 'integration_test'"
        );
        expect(result.rows).toHaveLength(0);

        // Version should be 0
        const version = await getCurrentVersion(pool);
        expect(version).toBe(0);
    });

    itDb("returns null when no migration to rollback", async () => {

        const result = await rollbackMigration(pool);
        expect(result).toBeNull();
    });

    itDb("tracks migration history", async () => {

        const diff1 = diffSchemas([], [
            greyTable("integration_test", { id: uuid().primaryKey() }),
        ]);
        const diff2 = diffSchemas(
            [greyTable("integration_test", { id: uuid().primaryKey() })],
            [greyTable("integration_test", { id: uuid().primaryKey(), name: text() })],
        );

        await applyMigration(pool, generateMigration("v1", diff1, 1));
        await applyMigration(pool, generateMigration("v2", diff2, 2));

        const history = await getMigrationHistory(pool);
        expect(history).toHaveLength(2);
        expect(history[0].version).toBe(1);
        expect(history[1].version).toBe(2);
    });
});

// ── Multi-Tenant Isolation Tests ────────────────────────────

describe("Tenant Isolation (integration)", () => {
    let tenants: TenantEngine;

    beforeEach(async () => {
        if (!pool) return;
        tenants = new TenantEngine(pool);

        // Set up test table with RLS
        await pool.query("DROP TABLE IF EXISTS tenant_test_items CASCADE");
        await pool.query("DROP TABLE IF EXISTS grey_tenants CASCADE");
        await pool.query("DROP TABLE IF EXISTS grey_tenant_usage CASCADE");
        await tenants.init();

        await pool.query(`
      CREATE TABLE tenant_test_items (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        tenant_id UUID NOT NULL,
        name TEXT NOT NULL
      )
    `);
        await pool.query(`ALTER TABLE tenant_test_items ENABLE ROW LEVEL SECURITY`);
        await pool.query(`ALTER TABLE tenant_test_items FORCE ROW LEVEL SECURITY`);
        await pool.query(`
      CREATE POLICY tenant_isolation ON tenant_test_items
        FOR ALL
        USING (tenant_id = current_setting('grey.tenant_id')::uuid)
        WITH CHECK (tenant_id = current_setting('grey.tenant_id')::uuid)
    `);
    });

    afterEach(async () => {
        if (!pool) return;
        await pool.query("DROP TABLE IF EXISTS tenant_test_items CASCADE");
        await pool.query("DROP TABLE IF EXISTS grey_tenant_usage CASCADE");
        await pool.query("DROP TABLE IF EXISTS grey_tenants CASCADE");
    });

    itDb("creates and retrieves tenants", async () => {

        const tenant = await tenants.createTenant({
            name: "Acme Corp",
            slug: "acme",
            isolationStrategy: "shared",
            featureFlags: {},
            storageLimitMB: 1024,
            config: {},
            active: true,
        });

        expect(tenant.name).toBe("Acme Corp");
        expect(tenant.slug).toBe("acme");
        expect(tenant.id).toBeDefined();

        const fetched = await tenants.getTenant(tenant.id);
        expect(fetched).not.toBeNull();
        expect(fetched!.name).toBe("Acme Corp");
    });

    itDb("isolates data between tenants via RLS", async () => {

        const tenantA = await tenants.createTenant({
            name: "Tenant A", slug: "tenant-a",
            isolationStrategy: "shared", featureFlags: {},
            storageLimitMB: 1024, config: {}, active: true,
        });
        const tenantB = await tenants.createTenant({
            name: "Tenant B", slug: "tenant-b",
            isolationStrategy: "shared", featureFlags: {},
            storageLimitMB: 1024, config: {}, active: true,
        });

        // Insert data for tenant A
        await tenants.withTenant(tenantA.id, async (client) => {
            await client.query(
                "INSERT INTO tenant_test_items (tenant_id, name) VALUES ($1, $2)",
                [tenantA.id, "Item from A"]
            );
        });

        // Insert data for tenant B
        await tenants.withTenant(tenantB.id, async (client) => {
            await client.query(
                "INSERT INTO tenant_test_items (tenant_id, name) VALUES ($1, $2)",
                [tenantB.id, "Item from B"]
            );
        });

        // Tenant A should only see their own items
        const aItems = await tenants.withTenant(tenantA.id, async (client) => {
            const res = await client.query("SELECT * FROM tenant_test_items");
            return res.rows;
        });
        expect(aItems).toHaveLength(1);
        expect(aItems[0].name).toBe("Item from A");

        // Tenant B should only see their own items
        const bItems = await tenants.withTenant(tenantB.id, async (client) => {
            const res = await client.query("SELECT * FROM tenant_test_items");
            return res.rows;
        });
        expect(bItems).toHaveLength(1);
        expect(bItems[0].name).toBe("Item from B");
    });

    itDb("prevents tenant from writing data with wrong tenant_id", async () => {

        const tenantA = await tenants.createTenant({
            name: "Tenant A", slug: "tenant-write-a",
            isolationStrategy: "shared", featureFlags: {},
            storageLimitMB: 1024, config: {}, active: true,
        });
        const tenantB = await tenants.createTenant({
            name: "Tenant B", slug: "tenant-write-b",
            isolationStrategy: "shared", featureFlags: {},
            storageLimitMB: 1024, config: {}, active: true,
        });

        // Tenant A tries to insert data with Tenant B's ID
        await expect(
            tenants.withTenant(tenantA.id, async (client) => {
                await client.query(
                    "INSERT INTO tenant_test_items (tenant_id, name) VALUES ($1, $2)",
                    [tenantB.id, "Sneaky item"]
                );
            })
        ).rejects.toThrow(); // RLS WITH CHECK prevents this
    });

    itDb("transaction rollback in withTenant does not leak state", async () => {

        const tenant = await tenants.createTenant({
            name: "Rollback Test", slug: "rollback-test",
            isolationStrategy: "shared", featureFlags: {},
            storageLimitMB: 1024, config: {}, active: true,
        });

        // An error inside withTenant should rollback
        try {
            await tenants.withTenant(tenant.id, async (client) => {
                await client.query(
                    "INSERT INTO tenant_test_items (tenant_id, name) VALUES ($1, $2)",
                    [tenant.id, "should-be-rolled-back"]
                );
                throw new Error("Intentional failure");
            });
        } catch (e) {
            // expected
        }

        // Row should not exist (was rolled back)
        const result = await tenants.withTenant(tenant.id, async (client) => {
            const res = await client.query("SELECT * FROM tenant_test_items");
            return res.rows;
        });
        expect(result).toHaveLength(0);
    });
});

// ── Query Planner Tests ─────────────────────────────────────

describe("Query Planner (integration)", () => {
    beforeEach(async () => {
        if (!pool) return;
        await pool.query("DROP TABLE IF EXISTS planner_test CASCADE");
        await pool.query(`
      CREATE TABLE planner_test (
        id SERIAL PRIMARY KEY,
        email TEXT NOT NULL,
        status TEXT NOT NULL DEFAULT 'active',
        created_at TIMESTAMPTZ DEFAULT NOW()
      )
    `);
        // Insert test data
        await pool.query(`
      INSERT INTO planner_test (email, status)
      SELECT
        'user' || i || '@test.com',
        CASE WHEN i % 3 = 0 THEN 'inactive' ELSE 'active' END
      FROM generate_series(1, 10000) AS i
    `);
        await pool.query("ANALYZE planner_test");
    });

    afterEach(async () => {
        if (!pool) return;
        await pool.query("DROP TABLE IF EXISTS planner_test CASCADE");
    });

    itDb("uses index scan when index exists on filtered column", async () => {

        // Create index
        await pool.query("CREATE INDEX idx_planner_email ON planner_test (email)");
        await pool.query("ANALYZE planner_test");

        // Check that the planner uses the index
        const result = await pool.query(
            "EXPLAIN (FORMAT JSON) SELECT * FROM planner_test WHERE email = 'user42@test.com'"
        );
        const plan = result.rows[0]["QUERY PLAN"][0].Plan;

        // Should use Index Scan or Bitmap Index Scan, not Seq Scan
        expect(plan["Node Type"]).not.toBe("Seq Scan");
    });

    itDb("does sequential scan when no index exists", async () => {

        const result = await pool.query(
            "EXPLAIN (FORMAT JSON) SELECT * FROM planner_test WHERE status = 'active'"
        );
        const plan = result.rows[0]["QUERY PLAN"][0].Plan;

        // Without an index, Postgres should do a Seq Scan on this query
        // (status has low selectivity — ~66% of rows are 'active')
        expect(plan["Node Type"]).toBe("Seq Scan");
    });

    itDb("uses index for high-selectivity queries", async () => {

        await pool.query("CREATE INDEX idx_planner_status ON planner_test (status)");
        await pool.query("ANALYZE planner_test");

        // Status 'inactive' is ~33% of rows — may or may not use index
        // But a PK lookup should always use the index
        const result = await pool.query(
            "EXPLAIN (FORMAT JSON) SELECT * FROM planner_test WHERE id = 42"
        );
        const plan = result.rows[0]["QUERY PLAN"][0].Plan;

        expect(plan["Node Type"]).toContain("Index");
    });
});
