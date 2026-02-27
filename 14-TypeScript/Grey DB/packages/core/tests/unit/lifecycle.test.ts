// ─────────────────────────────────────────────────────────────
// Tests — Data Lifecycle & Retention
// ─────────────────────────────────────────────────────────────

import {
    generatePartitionDDL,
    generateCompactionSQL,
    type RetentionPolicy,
    type PartitionConfig,
    type CompactionJob,
} from "../../src/lifecycle/retention";

describe("Data Lifecycle & Retention", () => {
    // ── Partitioning ──────────────────────────────────────────
    describe("Partition DDL Generation", () => {
        it("generates monthly range partitions", () => {
            const config: PartitionConfig = {
                tableName: "events",
                partitionType: "range",
                partitionKey: "created_at",
                rangeInterval: "monthly",
            };

            const { parentDDL, childDDL } = generatePartitionDDL(config);
            expect(parentDDL).toContain("PARTITION BY RANGE");
            expect(parentDDL).toContain('"created_at"');
            expect(childDDL.length).toBe(12); // 12 months of partitions
            expect(childDDL[0]).toContain("PARTITION OF");
            expect(childDDL[0]).toContain("FOR VALUES FROM");
        });

        it("generates daily range partitions", () => {
            const { childDDL } = generatePartitionDDL({
                tableName: "logs",
                partitionType: "range",
                partitionKey: "timestamp",
                rangeInterval: "daily",
            });
            expect(childDDL.length).toBe(12);
            childDDL.forEach((ddl) => {
                expect(ddl).toContain("PARTITION OF");
            });
        });

        it("generates yearly range partitions", () => {
            const { childDDL } = generatePartitionDDL({
                tableName: "archive",
                partitionType: "range",
                partitionKey: "year",
                rangeInterval: "yearly",
            });
            expect(childDDL.length).toBe(12);
        });

        it("generates list partitions", () => {
            const { childDDL } = generatePartitionDDL({
                tableName: "data",
                partitionType: "list",
                partitionKey: "region",
                listValues: [["us-east", "us-west"], ["eu-west", "eu-central"]],
            });
            expect(childDDL.length).toBe(2);
            expect(childDDL[0]).toContain("FOR VALUES IN");
            expect(childDDL[0]).toContain("'us-east'");
        });

        it("generates hash partitions", () => {
            const { childDDL } = generatePartitionDDL({
                tableName: "data",
                partitionType: "hash",
                partitionKey: "id",
                hashModulus: 4,
            });
            expect(childDDL.length).toBe(4);
            expect(childDDL[0]).toContain("MODULUS 4");
            expect(childDDL[0]).toContain("REMAINDER 0");
            expect(childDDL[3]).toContain("REMAINDER 3");
        });

        it("generates detach SQL", () => {
            const { detachSQL } = generatePartitionDDL({
                tableName: "events",
                partitionType: "range",
                partitionKey: "created_at",
                rangeInterval: "monthly",
            });
            const sql = detachSQL("events_202401");
            expect(sql).toContain("DETACH PARTITION");
            expect(sql).toContain("events_202401");
        });

        it("supports custom schema", () => {
            const { parentDDL, childDDL } = generatePartitionDDL({
                tableName: "events",
                schema: "analytics",
                partitionType: "hash",
                partitionKey: "id",
                hashModulus: 2,
            });
            expect(parentDDL).toContain('"analytics"');
            expect(childDDL[0]).toContain('"analytics"');
        });
    });

    // ── Compaction ────────────────────────────────────────────
    describe("Compaction SQL Generation", () => {
        it("generates VACUUM ANALYZE", () => {
            const sqls = generateCompactionSQL({
                tableName: "users",
                operations: [{ type: "vacuum", analyze: true }],
            });
            expect(sqls).toHaveLength(1);
            expect(sqls[0]).toContain("VACUUM");
            expect(sqls[0]).toContain("ANALYZE");
        });

        it("generates VACUUM FULL", () => {
            const sqls = generateCompactionSQL({
                tableName: "users",
                operations: [{ type: "vacuum", full: true }],
            });
            expect(sqls[0]).toContain("VACUUM FULL");
        });

        it("generates REINDEX CONCURRENTLY", () => {
            const sqls = generateCompactionSQL({
                tableName: "users",
                operations: [{ type: "reindex", concurrently: true }],
            });
            expect(sqls[0]).toContain("REINDEX TABLE CONCURRENTLY");
        });

        it("generates CLUSTER with index name", () => {
            const sqls = generateCompactionSQL({
                tableName: "users",
                operations: [{ type: "cluster", indexName: "idx_users_created" }],
            });
            expect(sqls[0]).toContain("CLUSTER");
            expect(sqls[0]).toContain("idx_users_created");
        });

        it("generates ANALYZE", () => {
            const sqls = generateCompactionSQL({
                tableName: "users",
                operations: [{ type: "analyze" }],
            });
            expect(sqls[0]).toContain("ANALYZE");
        });

        it("generates multiple operations in sequence", () => {
            const sqls = generateCompactionSQL({
                tableName: "users",
                operations: [
                    { type: "vacuum", analyze: true },
                    { type: "reindex", concurrently: true },
                    { type: "analyze" },
                ],
            });
            expect(sqls).toHaveLength(3);
        });

        it("uses custom schema", () => {
            const sqls = generateCompactionSQL({
                tableName: "users",
                schema: "app",
                operations: [{ type: "analyze" }],
            });
            expect(sqls[0]).toContain('"app"."users"');
        });
    });

    // ── Retention Policy Config ───────────────────────────────
    describe("Retention Policy Types", () => {
        it("defines a valid retention policy", () => {
            const policy: RetentionPolicy = {
                tableName: "logs",
                ageColumn: "created_at",
                archiveAfterDays: 90,
                purgeAfterDays: 365,
                enabled: true,
            };
            expect(policy.archiveAfterDays).toBe(90);
            expect(policy.purgeAfterDays).toBe(365);
        });

        it("supports per-tenant overrides", () => {
            const overrides = new Map<string, { archiveAfterDays?: number }>();
            overrides.set("enterprise-tenant", { archiveAfterDays: 365 });

            const policy: RetentionPolicy = {
                tableName: "events",
                ageColumn: "occurred_at",
                archiveAfterDays: 30,
                tenantOverrides: overrides,
                enabled: true,
            };

            expect(policy.tenantOverrides?.get("enterprise-tenant")?.archiveAfterDays).toBe(365);
        });

        it("supports optional filter clause", () => {
            const policy: RetentionPolicy = {
                tableName: "notifications",
                ageColumn: "created_at",
                archiveAfterDays: 7,
                filter: "status = 'read'",
                enabled: true,
            };
            expect(policy.filter).toBe("status = 'read'");
        });
    });
});
