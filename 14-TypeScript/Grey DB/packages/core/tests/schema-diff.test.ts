// ─────────────────────────────────────────────────────────────
// Grey DB — Schema Diff Tests
// Pure unit tests — no database required
// ─────────────────────────────────────────────────────────────

import { greyTable, uuid, text, integer, timestamptz, varchar, boolean, jsonb } from "../src/schema/dsl";
import { diffSchemas, SchemaDiffResult } from "../src/schema/differ";

function expectSafe(result: SchemaDiffResult) {
    expect(result.safe).toBe(true);
    expect(result.requiresConfirmation).toBe(false);
    expect(result.destructiveActions).toHaveLength(0);
}

function expectDangerous(result: SchemaDiffResult) {
    expect(result.safe).toBe(false);
    expect(result.requiresConfirmation).toBe(true);
    expect(result.destructiveActions.length).toBeGreaterThan(0);
}

describe("diffSchemas", () => {
    // ── No changes ────────────────────────────────────────────

    it("returns empty diff when schemas are identical", () => {
        const table = greyTable("users", {
            id: uuid().primaryKey(),
            email: text().unique(),
        });

        const result = diffSchemas([table], [table]);

        expect(result.actions).toHaveLength(0);
        expect(result.warnings).toHaveLength(0);
        expectSafe(result);
    });

    // ── Table creation ────────────────────────────────────────

    it("detects new table creation", () => {
        const current: any[] = [];
        const desired = [
            greyTable("users", {
                id: uuid().primaryKey(),
                email: text().unique(),
            }),
        ];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("CREATE_TABLE");
        expectSafe(result);
    });

    it("detects multiple new tables", () => {
        const current: any[] = [];
        const desired = [
            greyTable("users", { id: uuid().primaryKey() }),
            greyTable("orders", { id: uuid().primaryKey() }),
            greyTable("products", { id: uuid().primaryKey() }),
        ];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(3);
        expect(result.actions.every((a) => a.type === "CREATE_TABLE")).toBe(true);
        expectSafe(result);
    });

    // ── Table drops ───────────────────────────────────────────

    it("detects table drop as dangerous", () => {
        const current = [
            greyTable("users", { id: uuid().primaryKey(), email: text() }),
        ];
        const desired: any[] = [];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("DROP_TABLE");
        expectDangerous(result);
        expect(result.destructiveActions[0]).toContain("DROPPED");
    });

    // ── Column additions ──────────────────────────────────────

    it("detects new column addition", () => {
        const current = [
            greyTable("users", { id: uuid().primaryKey(), email: text() }),
        ];
        const desired = [
            greyTable("users", { id: uuid().primaryKey(), email: text(), name: text() }),
        ];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("ADD_COLUMN");
        if (result.actions[0].type === "ADD_COLUMN") {
            expect(result.actions[0].columnName).toBe("name");
        }
        expectSafe(result);
    });

    // ── Column drops ──────────────────────────────────────────

    it("detects column drop as dangerous", () => {
        const current = [
            greyTable("users", { id: uuid().primaryKey(), email: text(), legacy: text() }),
        ];
        const desired = [
            greyTable("users", { id: uuid().primaryKey(), email: text() }),
        ];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("DROP_COLUMN");
        expectDangerous(result);
    });

    // ── Column type changes ───────────────────────────────────

    it("detects column type change as dangerous", () => {
        const current = [
            greyTable("orders", { id: uuid().primaryKey(), amount: integer() }),
        ];
        const desired = [
            greyTable("orders", { id: uuid().primaryKey(), amount: text() }),
        ];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("ALTER_COLUMN");
        expectDangerous(result);
        expect(result.destructiveActions[0]).toContain("type change");
    });

    // ── Constraint changes ────────────────────────────────────

    it("detects constraint change as warning", () => {
        const current = [
            greyTable("users", { id: uuid().primaryKey(), email: text() }),
        ];
        const desired = [
            greyTable("users", { id: uuid().primaryKey(), email: text().unique() }),
        ];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("ALTER_COLUMN");
        expect(result.warnings[0].severity).toBe("warning");
        // Constraint changes are warnings, not danger
        expect(result.safe).toBe(true);
    });

    // ── Index changes ─────────────────────────────────────────

    it("detects new index", () => {
        const current = [greyTable("users", { id: uuid().primaryKey(), email: text() })];
        const desired = [
            greyTable("users", { id: uuid().primaryKey(), email: text() }).index("idx_email", ["email"]),
        ];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("ADD_INDEX");
    });

    it("detects dropped index", () => {
        const current = [
            greyTable("users", { id: uuid().primaryKey(), email: text() }).index("idx_email", ["email"]),
        ];
        const desired = [greyTable("users", { id: uuid().primaryKey(), email: text() })];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("DROP_INDEX");
    });

    // ── RLS changes ───────────────────────────────────────────

    it("detects RLS enablement", () => {
        const current = [greyTable("orders", { id: uuid().primaryKey() })];
        const desired = [greyTable("orders", { id: uuid().primaryKey() }).withRLS()];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("ENABLE_RLS");
        expectSafe(result);
    });

    it("detects RLS disablement", () => {
        const current = [greyTable("orders", { id: uuid().primaryKey() }).withRLS()];
        const desired = [greyTable("orders", { id: uuid().primaryKey() })];

        const result = diffSchemas(current, desired);

        expect(result.actions).toHaveLength(1);
        expect(result.actions[0].type).toBe("DISABLE_RLS");
    });

    // ── Complex multi-change diff ─────────────────────────────

    it("handles complex diff with multiple changes", () => {
        const current = [
            greyTable("users", {
                id: uuid().primaryKey(),
                email: text().unique(),
                legacy: text(),
            }),
            greyTable("old_table", { id: uuid().primaryKey() }),
        ];

        const desired = [
            greyTable("users", {
                id: uuid().primaryKey(),
                email: text().unique(),
                displayName: varchar(100),
            }),
            greyTable("new_table", { id: uuid().primaryKey() }),
        ];

        const result = diffSchemas(current, desired);

        const types = result.actions.map((a) => a.type);
        expect(types).toContain("CREATE_TABLE");   // new_table
        expect(types).toContain("DROP_TABLE");     // old_table
        expect(types).toContain("ADD_COLUMN");     // displayName
        expect(types).toContain("DROP_COLUMN");    // legacy
        expectDangerous(result);
    });

    // ── Schema-aware diffing ──────────────────────────────────

    it("treats same table name in different schemas as different tables", () => {
        const current = [greyTable("users", { id: uuid().primaryKey() }, "schema_a")];
        const desired = [greyTable("users", { id: uuid().primaryKey() }, "schema_b")];

        const result = diffSchemas(current, desired);

        // Should see a CREATE (schema_b.users) and a DROP (schema_a.users)
        expect(result.actions).toHaveLength(2);
        const types = result.actions.map((a) => a.type);
        expect(types).toContain("CREATE_TABLE");
        expect(types).toContain("DROP_TABLE");
    });
});
