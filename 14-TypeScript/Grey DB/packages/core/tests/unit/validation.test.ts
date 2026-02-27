// ─────────────────────────────────────────────────────────────
// Tests — Data Quality & Validation Engine
// ─────────────────────────────────────────────────────────────

import {
    ValidationEngine,
    rules,
    crossTableChecks,
    suggestRules,
    type ColumnProfile,
    type TableProfile,
} from "../../src/validation/engine";

describe("Data Quality & Validation", () => {
    // ── Built-in Rule Factories ───────────────────────────────
    describe("Built-in Rules", () => {
        it("email rule validates correct emails", () => {
            const rule = rules.email("email");
            expect(rule.predicate!("user@example.com", {})).toBe(true);
            expect(rule.predicate!("bad-email", {})).toBe(false);
            expect(rule.predicate!("", {})).toBe(false);
            expect(rule.predicate!(123, {})).toBe(false);
        });

        it("email rule generates SQL check", () => {
            const rule = rules.email("email");
            expect(rule.sqlCheck).toContain('"email"');
            expect(rule.sqlCheck).toContain("~");
        });

        it("notEmpty rule rejects whitespace-only strings", () => {
            const rule = rules.notEmpty("name");
            expect(rule.predicate!("Alice", {})).toBe(true);
            expect(rule.predicate!("", {})).toBe(false);
            expect(rule.predicate!("   ", {})).toBe(false);
        });

        it("range rule validates numeric bounds", () => {
            const rule = rules.range("age", 0, 150);
            expect(rule.predicate!(25, {})).toBe(true);
            expect(rule.predicate!(0, {})).toBe(true);
            expect(rule.predicate!(150, {})).toBe(true);
            expect(rule.predicate!(-1, {})).toBe(false);
            expect(rule.predicate!(200, {})).toBe(false);
        });

        it("positive rule rejects zero and negative", () => {
            const rule = rules.positive("amount");
            expect(rule.predicate!(1, {})).toBe(true);
            expect(rule.predicate!(0, {})).toBe(false);
            expect(rule.predicate!(-5, {})).toBe(false);
        });

        it("length rule validates string length bounds", () => {
            const rule = rules.length("code", 2, 10);
            expect(rule.predicate!("AB", {})).toBe(true);
            expect(rule.predicate!("ABCDEFGHIJ", {})).toBe(true);
            expect(rule.predicate!("A", {})).toBe(false);
            expect(rule.predicate!("ABCDEFGHIJK", {})).toBe(false);
        });

        it("pattern rule matches regex", () => {
            const rule = rules.pattern("phone", "^\\d{10}$", "phone");
            expect(rule.predicate!("1234567890", {})).toBe(true);
            expect(rule.predicate!("12345", {})).toBe(false);
        });

        it("oneOf rule validates against allowed set", () => {
            const rule = rules.oneOf("status", ["active", "inactive", "pending"]);
            expect(rule.predicate!("active", {})).toBe(true);
            expect(rule.predicate!("deleted", {})).toBe(false);
        });

        it("oneOf generates SQL IN clause", () => {
            const rule = rules.oneOf("status", ["active", "inactive"]);
            expect(rule.sqlCheck).toContain("IN");
            expect(rule.sqlCheck).toContain("'active'");
        });

        it("required rule rejects null and undefined", () => {
            const rule = rules.required("id");
            expect(rule.predicate!("abc", {})).toBe(true);
            expect(rule.predicate!(null, {})).toBe(false);
            expect(rule.predicate!(undefined, {})).toBe(false);
        });

        it("custom rule accepts arbitrary SQL and metadata", () => {
            const rule = rules.custom(
                "positive_total",
                ["total", "discount"],
                '"total" - "discount" > 0',
                "Net total must be positive"
            );
            expect(rule.id).toBe("positive_total");
            expect(rule.columns).toEqual(["total", "discount"]);
            expect(rule.sqlCheck).toContain('"total" - "discount" > 0');
        });

        it("rule factories support override options", () => {
            const rule = rules.email("email", { severity: "warning", action: "warn" });
            expect(rule.severity).toBe("warning");
            expect(rule.action).toBe("warn");
        });
    });

    // ── Validation Engine ─────────────────────────────────────
    describe("ValidationEngine", () => {
        let engine: ValidationEngine;

        beforeEach(() => {
            engine = new ValidationEngine();
            engine.register("users", [
                rules.email("email"),
                rules.notEmpty("name"),
                rules.positive("age"),
            ]);
        });

        it("validates a valid row", () => {
            const result = engine.validateRow("users", {
                email: "a@b.com",
                name: "Alice",
                age: 25,
            });
            expect(result.valid).toBe(true);
            expect(result.errors).toHaveLength(0);
        });

        it("rejects invalid email", () => {
            const result = engine.validateRow("users", {
                email: "bad",
                name: "Alice",
                age: 25,
            });
            expect(result.valid).toBe(false);
            expect(result.errors).toHaveLength(1);
            expect(result.errors[0].ruleId).toBe("email_email");
            expect(result.errors[0].column).toBe("email");
        });

        it("collects multiple violations", () => {
            const result = engine.validateRow("users", {
                email: "bad",
                name: "",
                age: -5,
            });
            expect(result.valid).toBe(false);
            expect(result.errors).toHaveLength(3);
        });

        it("separates warnings from errors", () => {
            engine.register("orders", [
                rules.positive("amount"),
                rules.futureDate("deliveryDate"),
            ]);

            const result = engine.validateRow("orders", {
                amount: 10,
                deliveryDate: new Date("2020-01-01"),
            });

            expect(result.valid).toBe(true); // futureDate is warning severity
            expect(result.warnings).toHaveLength(1);
            expect(result.warnings[0].severity).toBe("warning");
        });

        it("validates a batch and separates valid from rejected", () => {
            const { valid, rejected, warnings } = engine.validateBatch("users", [
                { email: "a@b.com", name: "Alice", age: 25 },
                { email: "bad", name: "Bob", age: 30 },
                { email: "c@d.com", name: "Charlie", age: 0 },
            ]);

            expect(valid).toHaveLength(1);
            expect(rejected).toHaveLength(2);
        });

        it("returns empty results for unregistered table", () => {
            const result = engine.validateRow("unknown_table", { anything: "goes" });
            expect(result.valid).toBe(true);
        });

        it("message placeholders are substituted", () => {
            const result = engine.validateRow("users", {
                email: "bad",
                name: "Alice",
                age: 25,
            });
            expect(result.errors[0].message).toContain("email");
            expect(result.errors[0].message).toContain("bad");
        });

        it("generates audit SQL for each rule with sqlCheck", () => {
            const sqls = engine.generateAuditSQL("users");
            expect(sqls.length).toBeGreaterThanOrEqual(3);
            sqls.forEach((sql) => {
                expect(sql).toContain("SELECT");
                expect(sql).toContain("WHERE NOT");
            });
        });

        it("getRules returns registered rules", () => {
            const r = engine.getRules("users");
            expect(r).toHaveLength(3);
        });
    });

    // ── Cross-table Checks ────────────────────────────────────
    describe("Cross-table Checks", () => {
        it("orphanedRecords generates LEFT JOIN anti-pattern SQL", () => {
            const check = crossTableChecks.orphanedRecords("orders", "customerId", "customers");
            expect(check.sql).toContain("LEFT JOIN");
            expect(check.sql).toContain("IS NULL");
            expect(check.id).toBe("orphan_orders_customers");
        });

        it("aggregateMismatch generates SUM comparison SQL", () => {
            const check = crossTableChecks.aggregateMismatch(
                "orders", "total", "order_lines", "amount", "orderId"
            );
            expect(check.sql).toContain("SUM");
            expect(check.sql).toContain("HAVING");
        });

        it("duplicates generates GROUP BY HAVING COUNT > 1", () => {
            const check = crossTableChecks.duplicates("users", "email");
            expect(check.sql).toContain("LOWER");
            expect(check.sql).toContain("HAVING COUNT(*) > 1");
        });

        it("duplicates supports case-sensitive mode", () => {
            const check = crossTableChecks.duplicates("users", "code", false);
            expect(check.sql).not.toContain("LOWER");
        });

        it("custom check accepts arbitrary SQL", () => {
            const check = crossTableChecks.custom(
                "balance_check",
                "Account balances must be non-negative",
                "SELECT id FROM accounts WHERE balance < 0"
            );
            expect(check.id).toBe("balance_check");
            expect(check.sql).toContain("balance < 0");
        });
    });

    // ── Data Profiling (suggestRules) ─────────────────────────
    describe("Rule Suggestion from Profile", () => {
        it("suggests required rule for low-null columns", () => {
            const profile: TableProfile = {
                tableName: "test",
                schema: "public",
                rowCount: 10000,
                profiledAt: new Date(),
                columns: [
                    {
                        column: "email",
                        dataType: "text",
                        totalRows: 10000,
                        nullCount: 5,
                        nullPercent: 0.05,
                        distinctCount: 9900,
                        distinctPercent: 99,
                    },
                ],
            };

            const suggested = suggestRules(profile);
            expect(suggested.some((r) => r.id === "required_email")).toBe(true);
        });

        it("suggests range rule for numeric columns", () => {
            const profile: TableProfile = {
                tableName: "test",
                schema: "public",
                rowCount: 10000,
                profiledAt: new Date(),
                columns: [
                    {
                        column: "age",
                        dataType: "integer",
                        totalRows: 10000,
                        nullCount: 0,
                        nullPercent: 0,
                        distinctCount: 80,
                        distinctPercent: 0.8,
                        minValue: 18,
                        maxValue: 95,
                    },
                ],
            };

            const suggested = suggestRules(profile);
            expect(suggested.some((r) => r.id.startsWith("range_age"))).toBe(true);
        });

        it("suggests length rule for text columns", () => {
            const profile: TableProfile = {
                tableName: "test",
                schema: "public",
                rowCount: 10000,
                profiledAt: new Date(),
                columns: [
                    {
                        column: "code",
                        dataType: "text",
                        totalRows: 10000,
                        nullCount: 0,
                        nullPercent: 0,
                        distinctCount: 500,
                        distinctPercent: 5,
                        minLength: 3,
                        maxLength: 10,
                    },
                ],
            };

            const suggested = suggestRules(profile);
            expect(suggested.some((r) => r.id.startsWith("length_code"))).toBe(true);
        });

        it("does not suggest required for small datasets", () => {
            const profile: TableProfile = {
                tableName: "test",
                schema: "public",
                rowCount: 10,
                profiledAt: new Date(),
                columns: [
                    {
                        column: "email",
                        dataType: "text",
                        totalRows: 10,
                        nullCount: 0,
                        nullPercent: 0,
                        distinctCount: 10,
                        distinctPercent: 100,
                    },
                ],
            };

            const suggested = suggestRules(profile);
            expect(suggested.some((r) => r.id === "required_email")).toBe(false);
        });
    });
});
