// ─────────────────────────────────────────────────────────────
// Grey DB — Migration Engine Tests
// Tests migration generation, checksums, and safety flags
// ─────────────────────────────────────────────────────────────

import { greyTable, uuid, text, integer, timestamptz } from "../src/schema/dsl";
import { diffSchemas } from "../src/schema/differ";
import { generateMigration, Migration } from "../src/schema/migrations";

describe("generateMigration", () => {
    it("generates a migration from a schema diff", () => {
        const current: any[] = [];
        const desired = [
            greyTable("users", {
                id: uuid().primaryKey(),
                email: text().unique(),
                createdAt: timestamptz().defaultNow(),
            }),
        ];

        const diff = diffSchemas(current, desired);
        const migration = generateMigration("create_users", diff, 1);

        expect(migration.version).toBe(1);
        expect(migration.name).toBe("create_users");
        expect(migration.id).toBe("1_create_users");
        expect(migration.safe).toBe(true);
        expect(migration.upSQL).toContain("CREATE TABLE");
        expect(migration.upSQL).toContain('"users"');
        expect(migration.downSQL).toContain("DROP TABLE");
        expect(migration.checksum).toHaveLength(16);
    });

    it("produces consistent checksums for same SQL", () => {
        const diff = diffSchemas([], [
            greyTable("products", { id: uuid().primaryKey(), name: text() }),
        ]);

        const m1 = generateMigration("create_products", diff, 1);
        const m2 = generateMigration("create_products", diff, 1);

        expect(m1.checksum).toBe(m2.checksum);
    });

    it("produces different checksums for different SQL", () => {
        const diff1 = diffSchemas([], [
            greyTable("a", { id: uuid().primaryKey() }),
        ]);
        const diff2 = diffSchemas([], [
            greyTable("b", { id: uuid().primaryKey() }),
        ]);

        const m1 = generateMigration("create_a", diff1, 1);
        const m2 = generateMigration("create_b", diff2, 2);

        expect(m1.checksum).not.toBe(m2.checksum);
    });

    it("marks migration as unsafe when diff has danger warnings", () => {
        const current = [
            greyTable("users", { id: uuid().primaryKey(), email: text(), legacy: text() }),
        ];
        const desired = [
            greyTable("users", { id: uuid().primaryKey(), email: text() }),
        ];

        const diff = diffSchemas(current, desired);
        const migration = generateMigration("drop_legacy_column", diff, 2);

        expect(migration.safe).toBe(false);
        expect(migration.warnings.length).toBeGreaterThan(0);
        expect(migration.upSQL).toContain("DROP COLUMN");
        expect(migration.downSQL).toContain("Manual restoration required");
    });

    it("generates valid up/down SQL for column addition", () => {
        const current = [
            greyTable("users", { id: uuid().primaryKey() }),
        ];
        const desired = [
            greyTable("users", { id: uuid().primaryKey(), bio: text() }),
        ];

        const diff = diffSchemas(current, desired);
        const migration = generateMigration("add_bio", diff, 3);

        expect(migration.upSQL).toContain("ADD COLUMN");
        expect(migration.upSQL).toContain('"bio"');
        expect(migration.downSQL).toContain("DROP COLUMN");
        expect(migration.downSQL).toContain('"bio"');
    });

    it("generates valid SQL for index creation", () => {
        const current = [greyTable("users", { id: uuid().primaryKey(), email: text() })];
        const desired = [
            greyTable("users", { id: uuid().primaryKey(), email: text() })
                .index("idx_users_email", ["email"], { unique: true }),
        ];

        const diff = diffSchemas(current, desired);
        const migration = generateMigration("add_email_index", diff, 4);

        expect(migration.upSQL).toContain("CREATE UNIQUE INDEX");
        expect(migration.upSQL).toContain("idx_users_email");
        expect(migration.downSQL).toContain("DROP INDEX");
    });

    it("generates RLS enable/disable SQL", () => {
        const current = [greyTable("orders", { id: uuid().primaryKey() })];
        const desired = [greyTable("orders", { id: uuid().primaryKey() }).withRLS()];

        const diff = diffSchemas(current, desired);
        const migration = generateMigration("enable_rls", diff, 5);

        expect(migration.upSQL).toContain("ENABLE ROW LEVEL SECURITY");
        expect(migration.downSQL).toContain("DISABLE ROW LEVEL SECURITY");
    });

    it("generates a kebab-case id from name", () => {
        const diff = diffSchemas([], [
            greyTable("t", { id: uuid().primaryKey() }),
        ]);
        const migration = generateMigration("Add User Table", diff, 10);

        expect(migration.id).toBe("10_add_user_table");
    });

    it("handles empty diff gracefully", () => {
        const table = greyTable("users", { id: uuid().primaryKey() });
        const diff = diffSchemas([table], [table]);
        const migration = generateMigration("no_changes", diff, 99);

        expect(migration.upSQL).toBe("");
        expect(migration.downSQL).toBe("");
        expect(migration.safe).toBe(true);
    });
});
