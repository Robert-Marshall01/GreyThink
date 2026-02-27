// ─────────────────────────────────────────────────────────────
// Tests — Data Ingestion & Transformation Pipeline
// ─────────────────────────────────────────────────────────────

import {
    parseCSV,
    parseJSON,
    validateSchema,
    transforms,
    IngestionPipeline,
    type IngestionConfig,
} from "../../src/ingestion/pipeline";
import { greyTable, uuid, text, integer, timestamptz } from "../../src/schema/dsl";
import { ValidationEngine, rules } from "../../src/validation/engine";

describe("Data Ingestion & Transformation", () => {
    // ── CSV Parser ────────────────────────────────────────────
    describe("CSV Parser", () => {
        it("parses simple CSV with headers", () => {
            const csv = "name,age,email\nAlice,30,a@b.com\nBob,25,b@c.com";
            const rows = parseCSV(csv);
            expect(rows).toHaveLength(2);
            expect(rows[0]).toEqual({ name: "Alice", age: 30, email: "a@b.com" });
            expect(rows[1]).toEqual({ name: "Bob", age: 25, email: "b@c.com" });
        });

        it("handles quoted fields with commas", () => {
            const csv = 'name,bio\n"Smith, John","Loves coding, testing"';
            const rows = parseCSV(csv);
            expect(rows[0].name).toBe("Smith, John");
            expect(rows[0].bio).toBe("Loves coding, testing");
        });

        it("handles escaped quotes", () => {
            const csv = 'name,quote\nAlice,"She said ""hello"""';
            const rows = parseCSV(csv);
            expect(rows[0].quote).toBe('She said "hello"');
        });

        it("infers boolean types", () => {
            const csv = "active,verified\ntrue,false";
            const rows = parseCSV(csv);
            expect(rows[0].active).toBe(true);
            expect(rows[0].verified).toBe(false);
        });

        it("infers integer types", () => {
            const csv = "count\n42";
            const rows = parseCSV(csv);
            expect(rows[0].count).toBe(42);
        });

        it("infers float types", () => {
            const csv = "price\n19.99";
            const rows = parseCSV(csv);
            expect(rows[0].price).toBe(19.99);
        });

        it("treats empty values as null", () => {
            const csv = "name,age\nAlice,";
            const rows = parseCSV(csv);
            expect(rows[0].age).toBeNull();
        });

        it("handles custom delimiter", () => {
            const csv = "name\tage\nAlice\t30";
            const rows = parseCSV(csv, { delimiter: "\t" });
            expect(rows[0]).toEqual({ name: "Alice", age: 30 });
        });

        it("handles no-header mode", () => {
            const csv = "Alice,30\nBob,25";
            const rows = parseCSV(csv, { hasHeader: false });
            expect(rows).toHaveLength(2);
            expect(rows[0].col_0).toBe("Alice");
            expect(rows[0].col_1).toBe(30);
        });

        it("skips empty lines", () => {
            const csv = "name\nAlice\n\nBob\n";
            const rows = parseCSV(csv);
            expect(rows).toHaveLength(2);
        });

        it("handles Windows line endings", () => {
            const csv = "name\r\nAlice\r\nBob";
            const rows = parseCSV(csv);
            expect(rows).toHaveLength(2);
        });
    });

    // ── JSON Parser ───────────────────────────────────────────
    describe("JSON Parser", () => {
        it("parses JSON array", () => {
            const json = '[{"name":"Alice"},{"name":"Bob"}]';
            const rows = parseJSON(json);
            expect(rows).toHaveLength(2);
            expect(rows[0].name).toBe("Alice");
        });

        it("wraps single JSON object in array", () => {
            const json = '{"name":"Alice"}';
            const rows = parseJSON(json);
            expect(rows).toHaveLength(1);
        });

        it("parses JSONL format", () => {
            const jsonl = '{"name":"Alice"}\n{"name":"Bob"}\n{"name":"Charlie"}';
            const rows = parseJSON(jsonl, "jsonl");
            expect(rows).toHaveLength(3);
        });

        it("skips empty lines in JSONL", () => {
            const jsonl = '{"name":"Alice"}\n\n{"name":"Bob"}';
            const rows = parseJSON(jsonl, "jsonl");
            expect(rows).toHaveLength(2);
        });
    });

    // ── Schema Validation ─────────────────────────────────────
    describe("Schema Validation", () => {
        const usersTable = greyTable("users", {
            id: uuid().primaryKey().defaultUUID(),
            name: text().notNull(),
            email: text().notNull(),
            age: integer().nullable(),
            createdAt: timestamptz().defaultNow(),
        });

        it("validates conforming rows", () => {
            const { valid, errors } = validateSchema(
                [{ name: "Alice", email: "a@b.com", age: 30 }],
                usersTable
            );
            expect(valid).toHaveLength(1);
            expect(errors).toHaveLength(0);
        });

        it("rejects rows missing required columns", () => {
            const { valid, errors } = validateSchema(
                [{ age: 30 }],
                usersTable
            );
            expect(valid).toHaveLength(0);
            expect(errors).toHaveLength(1);
            expect(errors[0].error).toContain("Missing required column");
        });

        it("rejects unknown columns by default", () => {
            const { errors } = validateSchema(
                [{ name: "Alice", email: "a@b.com", unknownCol: "val" }],
                usersTable
            );
            expect(errors).toHaveLength(1);
            expect(errors[0].error).toContain("Unknown column");
        });

        it("allows extra columns when configured", () => {
            const { valid, errors } = validateSchema(
                [{ name: "Alice", email: "a@b.com", extra: "fine" }],
                usersTable,
                { allowExtraColumns: true }
            );
            expect(valid).toHaveLength(1);
            expect(errors).toHaveLength(0);
        });
    });

    // ── Transform Hooks ───────────────────────────────────────
    describe("Transform Hooks", () => {
        it("trimStrings trims whitespace", () => {
            const hook = transforms.trimStrings();
            const result = hook.transform({ name: "  Alice  ", age: 30 }, 0);
            expect(result!.name).toBe("Alice");
            expect(result!.age).toBe(30);
        });

        it("lowercase lowercases specified columns", () => {
            const hook = transforms.lowercase("email");
            const result = hook.transform({ email: "Alice@B.COM", name: "Alice" }, 0);
            expect(result!.email).toBe("alice@b.com");
            expect(result!.name).toBe("Alice");
        });

        it("defaults fills in missing values", () => {
            const hook = transforms.defaults({ role: "user", active: true });
            const result = hook.transform({ name: "Alice" }, 0);
            expect(result!.role).toBe("user");
            expect(result!.active).toBe(true);
            expect(result!.name).toBe("Alice");
        });

        it("defaults do not override existing values", () => {
            const hook = transforms.defaults({ role: "user" });
            const result = hook.transform({ name: "Alice", role: "admin" }, 0);
            expect(result!.role).toBe("admin");
        });

        it("renameColumns renames keys", () => {
            const hook = transforms.renameColumns({ first_name: "firstName", last_name: "lastName" });
            const result = hook.transform({ first_name: "Alice", last_name: "Smith" }, 0);
            expect(result!.firstName).toBe("Alice");
            expect(result!.lastName).toBe("Smith");
        });

        it("dropColumns removes specified columns", () => {
            const hook = transforms.dropColumns("password", "ssn");
            const result = hook.transform({ name: "Alice", password: "secret", ssn: "123" }, 0);
            expect(result!.name).toBe("Alice");
            expect(result!.password).toBeUndefined();
            expect(result!.ssn).toBeUndefined();
        });

        it("computed adds a derived column", () => {
            const hook = transforms.computed("fullName", (row) => `${row.first} ${row.last}`);
            const result = hook.transform({ first: "Alice", last: "Smith" }, 0);
            expect(result!.fullName).toBe("Alice Smith");
        });

        it("filter returns null for non-matching rows", () => {
            const hook = transforms.filter((row) => row.active === true);
            expect(hook.transform({ active: true }, 0)).not.toBeNull();
            expect(hook.transform({ active: false }, 0)).toBeNull();
        });
    });

    // ── Pipeline Integration (in-memory only) ─────────────────
    describe("Ingestion Pipeline (unit-level)", () => {
        const testTable = greyTable("test_import", {
            id: uuid().primaryKey().defaultUUID(),
            name: text().notNull(),
            email: text().notNull(),
            age: integer().nullable(),
        });

        it("parses CSV input and validates schema", () => {
            const csv = "name,email,age\nAlice,a@b.com,30\nBob,b@c.com,25";
            const rows = parseCSV(csv);
            const { valid, errors } = validateSchema(rows, testTable);
            expect(valid).toHaveLength(2);
            expect(errors).toHaveLength(0);
        });

        it("rejects rows with schema errors after transforms", () => {
            const csv = "name,email,age\nAlice,A@B.COM,30\n,b@c.com,25";
            const rows = parseCSV(csv);

            // Apply transforms
            const hook = transforms.lowercase("email");
            const transformed = rows.map((r, i) => hook.transform(r, i)!);

            // Validate
            const { valid, errors } = validateSchema(transformed, testTable);
            expect(valid).toHaveLength(1); // null name row rejected
            expect(errors).toHaveLength(1);
        });

        it("full pipeline: parse → transform → validate rules", () => {
            const csv = "name,email,age\nAlice,a@b.com,30\nBob,bad-email,25\n,c@d.com,20";
            const rows = parseCSV(csv);

            // Transform
            const trimmed = rows.map((r, i) => transforms.trimStrings().transform(r, i)!);

            // Schema validate
            const { valid: schemaValid } = validateSchema(trimmed, testTable, { allowExtraColumns: true });

            // Business validate
            const engine = new ValidationEngine();
            engine.register("test_import", [rules.email("email")]);
            const { valid, rejected } = engine.validateBatch("test_import", schemaValid);

            expect(valid).toHaveLength(1); // Only Alice passes email validation
            expect(rejected).toHaveLength(1); // Bob's bad-email rejected
        });
    });

    // ── JSON Ingestion ────────────────────────────────────────
    describe("JSON Ingestion", () => {
        const table = greyTable("products", {
            id: uuid().primaryKey().defaultUUID(),
            name: text().notNull(),
            price: integer().notNull(),
        });

        it("validates JSON array against table schema", () => {
            const json = '[{"name":"Widget","price":10},{"name":"Gadget","price":20}]';
            const rows = parseJSON(json);
            const { valid } = validateSchema(rows, table);
            expect(valid).toHaveLength(2);
        });

        it("rejects JSON with missing required fields", () => {
            const json = '[{"name":"Widget"}]';
            const rows = parseJSON(json);
            const { errors } = validateSchema(rows, table);
            expect(errors).toHaveLength(1);
            expect(errors[0].error).toContain("price");
        });
    });
});
