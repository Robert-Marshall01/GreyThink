// ─────────────────────────────────────────────────────────────
// Tests — Data Modeling Patterns
// ─────────────────────────────────────────────────────────────

import {
    generateSCDHistory,
    softDeleteColumns,
    softDeleteHelpers,
    generateEventTable,
    AuditTrail,
    ReferenceTableManager,
    projectState,
    StateProjection,
} from "../../src/modeling/patterns";

describe("Data Modeling Patterns", () => {
    // ── SCD Type 2 ────────────────────────────────────────────
    describe("SCD Type 2 (generateSCDHistory)", () => {
        const config = {
            tableName: "customers",
            trackedColumns: ["name", "email", "tier"],
        };

        it("generates history table DDL with correct structure", () => {
            const { historyTableSQL } = generateSCDHistory(config);
            expect(historyTableSQL).toContain("CREATE TABLE IF NOT EXISTS");
            expect(historyTableSQL).toContain("customers_history");
            expect(historyTableSQL).toContain("entity_id");
            expect(historyTableSQL).toContain("valid_from");
            expect(historyTableSQL).toContain("valid_to");
            expect(historyTableSQL).toContain("is_current");
            expect(historyTableSQL).toContain("changed_by");
            expect(historyTableSQL).toContain("change_reason");
        });

        it("generates temporal range constraint", () => {
            const { historyTableSQL } = generateSCDHistory(config);
            expect(historyTableSQL).toContain("chk_valid_range");
            expect(historyTableSQL).toContain("valid_to > valid_from");
        });

        it("creates unique index on current row per entity", () => {
            const { historyTableSQL } = generateSCDHistory(config);
            expect(historyTableSQL).toContain("WHERE is_current = true");
        });

        it("generates trigger that fires on tracked column changes", () => {
            const { triggerSQL } = generateSCDHistory(config);
            expect(triggerSQL).toContain("fn_customers_scd_trigger");
            expect(triggerSQL).toContain('OLD."name" IS DISTINCT FROM NEW."name"');
            expect(triggerSQL).toContain('OLD."email" IS DISTINCT FROM NEW."email"');
            expect(triggerSQL).toContain('OLD."tier" IS DISTINCT FROM NEW."tier"');
            expect(triggerSQL).toContain("AFTER UPDATE ON");
        });

        it("generates query helpers for current and as-of queries", () => {
            const { queryCurrentSQL, queryAsOfSQL } = generateSCDHistory(config);
            expect(queryCurrentSQL).toContain("is_current = true");
            expect(queryAsOfSQL).toContain("valid_from <=");
            expect(queryAsOfSQL).toContain("valid_to");
        });

        it("supports custom schema", () => {
            const { historyTableSQL } = generateSCDHistory({ ...config, schema: "analytics" });
            expect(historyTableSQL).toContain('"analytics"."customers_history"');
        });
    });

    // ── Soft Delete ───────────────────────────────────────────
    describe("Soft Delete Pattern", () => {
        it("creates nullable timestamp and UUID columns", () => {
            const cols = softDeleteColumns();
            expect(cols.deletedAt._type).toBe("timestamptz");
            expect(cols.deletedAt._constraints.nullable).toBe(true);
            expect(cols.deletedBy._type).toBe("uuid");
            expect(cols.deletedBy._constraints.nullable).toBe(true);
        });

        it("generates active-only partial index", () => {
            const helpers = softDeleteHelpers("users");
            expect(helpers.activeIndex).toContain('WHERE "deletedAt" IS NULL');
        });

        it("generates active unique constraint", () => {
            const helpers = softDeleteHelpers("users");
            const uniqueSQL = helpers.activeUnique("email");
            expect(uniqueSQL).toContain("UNIQUE INDEX");
            expect(uniqueSQL).toContain('"email"');
            expect(uniqueSQL).toContain('WHERE "deletedAt" IS NULL');
        });

        it("generates soft-delete and restore SQL", () => {
            const helpers = softDeleteHelpers("users");
            expect(helpers.softDeleteSQL).toContain("SET");
            expect(helpers.softDeleteSQL).toContain('"deletedAt"');
            expect(helpers.restoreSQL).toContain('"deletedAt" = NULL');
        });

        it("generates purge SQL with retention window", () => {
            const helpers = softDeleteHelpers("users");
            const purge = helpers.purgeSQL(90);
            expect(purge).toContain("DELETE FROM");
            expect(purge).toContain("90 days");
        });
    });

    // ── Event Tables ──────────────────────────────────────────
    describe("Event Tables", () => {
        it("creates an event table with required columns", () => {
            const { createSQL } = generateEventTable({
                tableName: "domain_events",
                category: "domain",
            });
            expect(createSQL).toContain("eventType");
            expect(createSQL).toContain("entityType");
            expect(createSQL).toContain("entityId");
            expect(createSQL).toContain("payload");
            expect(createSQL).toContain("occurredAt");
        });

        it("adds immutability rules to prevent UPDATE and DELETE", () => {
            const { createSQL } = generateEventTable({
                tableName: "domain_events",
                category: "domain",
            });
            expect(createSQL).toContain("prevent_update_domain_events");
            expect(createSQL).toContain("prevent_delete_domain_events");
            expect(createSQL).toContain("DO INSTEAD NOTHING");
        });

        it("supports tenant scoping", () => {
            const { createSQL, appendSQL } = generateEventTable({
                tableName: "tenant_events",
                category: "domain",
                tenantScoped: true,
            });
            expect(createSQL).toContain("tenantId");
            expect(appendSQL).toContain("tenantId");
        });

        it("generates append-only INSERT SQL", () => {
            const { appendSQL } = generateEventTable({
                tableName: "events",
                category: "system",
            });
            expect(appendSQL).toContain("INSERT INTO");
            expect(appendSQL).toContain("RETURNING *");
        });

        it("generates query helpers for entity and type lookups", () => {
            const { queryByEntitySQL, queryByTypeSQL } = generateEventTable({
                tableName: "events",
                category: "domain",
            });
            expect(queryByEntitySQL).toContain("entityType");
            expect(queryByEntitySQL).toContain("entityId");
            expect(queryByTypeSQL).toContain("eventType");
            expect(queryByTypeSQL).toContain("BETWEEN");
        });

        it("sets immutable metadata on the Table definition", () => {
            const { table } = generateEventTable({
                tableName: "events",
                category: "audit",
            });
            expect(table.definition.metadata.immutable).toBe(true);
            expect(table.definition.metadata.category).toBe("audit");
        });
    });

    // ── Audit Trail ───────────────────────────────────────────
    describe("AuditTrail", () => {
        it("computes diff between old and new values", () => {
            const diff = AuditTrail.computeDiff(
                { name: "Alice", email: "a@b.com", age: 30 },
                { name: "Alice", email: "a@new.com", age: 31 }
            );
            expect(diff.old).toEqual({ email: "a@b.com", age: 30 });
            expect(diff.new).toEqual({ email: "a@new.com", age: 31 });
        });

        it("returns empty diff when values are identical", () => {
            const diff = AuditTrail.computeDiff(
                { name: "Alice" },
                { name: "Alice" }
            );
            expect(diff.old).toEqual({});
            expect(diff.new).toEqual({});
        });

        it("detects new properties", () => {
            const diff = AuditTrail.computeDiff(
                { name: "Alice" },
                { name: "Alice", email: "a@b.com" }
            );
            expect(diff.old).toEqual({ email: undefined });
            expect(diff.new).toEqual({ email: "a@b.com" });
        });
    });

    // ── State Projection ─────────────────────────────────────
    describe("State Projection (event sourcing read model)", () => {
        it("defines a correct projection interface", () => {
            const projection: StateProjection<{ balance: number }> = {
                entityType: "account",
                initialState: { balance: 0 },
                reducer: (state, event) => {
                    if (event.eventType === "deposit") return { balance: state.balance + event.payload.amount };
                    if (event.eventType === "withdraw") return { balance: state.balance - event.payload.amount };
                    return state;
                },
            };

            // Simulate local reduce
            const events = [
                { eventType: "deposit", payload: { amount: 100 } },
                { eventType: "deposit", payload: { amount: 50 } },
                { eventType: "withdraw", payload: { amount: 30 } },
            ];

            const finalState = events.reduce(
                (s, e) => projection.reducer(s, e),
                { ...projection.initialState }
            );

            expect(finalState.balance).toBe(120);
        });
    });
});
