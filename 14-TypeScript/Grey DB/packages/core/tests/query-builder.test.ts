// ─────────────────────────────────────────────────────────────
// Grey DB — Query Builder Tests
// Pure unit tests — no database required
// ─────────────────────────────────────────────────────────────

import { QueryBuilder, InsertBuilder, UpdateBuilder, DeleteBuilder } from "../src/query/query-builder";

describe("QueryBuilder", () => {
    // ── Basic SELECT ──────────────────────────────────────────

    it("generates a simple SELECT *", () => {
        const { text, values } = QueryBuilder.from("users").toSQL();

        expect(text).toContain('SELECT *');
        expect(text).toContain('FROM "public"."users"');
        expect(values).toHaveLength(0);
    });

    it("generates SELECT with specific columns", () => {
        const { text } = QueryBuilder.from("users").select("id", "email", "name").toSQL();

        expect(text).toContain("SELECT id, email, name");
    });

    it("supports DISTINCT", () => {
        const { text } = QueryBuilder.from("users").select("email").distinct().toSQL();

        expect(text).toContain("SELECT DISTINCT email");
    });

    // ── WHERE clauses ────────────────────────────────────────

    it("generates parameterized WHERE clause", () => {
        const { text, values } = QueryBuilder.from("users")
            .where("email", "=", "test@example.com")
            .toSQL();

        expect(text).toContain("WHERE email = $1");
        expect(values).toEqual(["test@example.com"]);
    });

    it("supports multiple WHERE conditions (AND)", () => {
        const { text, values } = QueryBuilder.from("users")
            .where("active", "=", true)
            .where("role", "=", "admin")
            .toSQL();

        expect(text).toContain("WHERE active = $1 AND role = $2");
        expect(values).toEqual([true, "admin"]);
    });

    it("supports OR WHERE", () => {
        const { text, values } = QueryBuilder.from("users")
            .where("role", "=", "admin")
            .orWhere("role", "=", "superadmin")
            .toSQL();

        expect(text).toContain("$1");
        expect(text).toContain("OR role = $2");
        expect(values).toEqual(["admin", "superadmin"]);
    });

    it("supports WHERE IN", () => {
        const { text, values } = QueryBuilder.from("users")
            .whereIn("status", ["active", "pending"])
            .toSQL();

        expect(text).toContain("status IN ($1, $2)");
        expect(values).toEqual(["active", "pending"]);
    });

    it("supports WHERE IS NULL", () => {
        const { text, values } = QueryBuilder.from("users")
            .whereNull("deleted_at")
            .toSQL();

        expect(text).toContain("deleted_at IS NULL");
        expect(values).toHaveLength(0);
    });

    it("supports WHERE IS NOT NULL", () => {
        const { text } = QueryBuilder.from("users")
            .whereNotNull("email")
            .toSQL();

        expect(text).toContain("email IS NOT NULL");
    });

    it("supports WHERE BETWEEN", () => {
        const { text, values } = QueryBuilder.from("orders")
            .whereBetween("amount", 100, 500)
            .toSQL();

        expect(text).toContain("amount BETWEEN $1 AND $2");
        expect(values).toEqual([100, 500]);
    });

    it("supports whereRaw with parameterized placeholders", () => {
        const { text, values } = QueryBuilder.from("users")
            .whereRaw("age > ? AND age < ?", 18, 65)
            .toSQL();

        expect(text).toContain("age > $1 AND age < $2");
        expect(values).toEqual([18, 65]);
    });

    // ── JOINs ────────────────────────────────────────────────

    it("generates INNER JOIN", () => {
        const { text } = QueryBuilder.from("users")
            .join("orders", "users.id = orders.user_id")
            .toSQL();

        expect(text).toContain('INNER JOIN "orders" ON users.id = orders.user_id');
    });

    it("generates LEFT JOIN", () => {
        const { text } = QueryBuilder.from("users")
            .leftJoin("orders", "users.id = orders.user_id")
            .toSQL();

        expect(text).toContain('LEFT JOIN "orders" ON users.id = orders.user_id');
    });

    // ── ORDER BY ──────────────────────────────────────────────

    it("generates ORDER BY", () => {
        const { text } = QueryBuilder.from("users")
            .orderBy("created_at", "DESC")
            .toSQL();

        expect(text).toContain("ORDER BY created_at DESC");
    });

    it("supports NULLS FIRST/LAST", () => {
        const { text } = QueryBuilder.from("users")
            .orderBy("name", "ASC", "LAST")
            .toSQL();

        expect(text).toContain("ORDER BY name ASC NULLS LAST");
    });

    // ── LIMIT / OFFSET ───────────────────────────────────────

    it("generates LIMIT and OFFSET", () => {
        const { text } = QueryBuilder.from("users")
            .limit(10)
            .offset(20)
            .toSQL();

        expect(text).toContain("LIMIT 10");
        expect(text).toContain("OFFSET 20");
    });

    // ── GROUP BY / HAVING ─────────────────────────────────────

    it("generates GROUP BY with aggregate", () => {
        const { text } = QueryBuilder.from("orders")
            .select("user_id")
            .selectAgg("COUNT", "*", "order_count")
            .groupBy("user_id")
            .having("COUNT(*) > 5")
            .toSQL();

        expect(text).toContain("COUNT(*) AS \"order_count\"");
        expect(text).toContain("GROUP BY user_id");
        expect(text).toContain("HAVING COUNT(*) > 5");
    });

    // ── CTEs ──────────────────────────────────────────────────

    it("generates WITH CTE", () => {
        const { text } = QueryBuilder.from("results")
            .withCTE("active_users", "SELECT id FROM users WHERE active = true")
            .toSQL();

        expect(text).toContain("WITH");
        expect(text).toContain('"active_users" AS');
        expect(text).toContain("SELECT id FROM users WHERE active = true");
    });

    // ── Tenant scoping ───────────────────────────────────────

    it("adds tenant_id filter when forTenant is called", () => {
        const { text, values } = QueryBuilder.from("orders")
            .forTenant("abc-123")
            .where("status", "=", "paid")
            .toSQL();

        expect(text).toContain("tenant_id = $1");
        expect(text).toContain("status = $2");
        expect(values[0]).toBe("abc-123");
        expect(values[1]).toBe("paid");
    });

    // ── FOR UPDATE ────────────────────────────────────────────

    it("generates FOR UPDATE", () => {
        const { text } = QueryBuilder.from("users")
            .where("id", "=", "abc")
            .forUpdate()
            .toSQL();

        expect(text).toContain("FOR UPDATE");
    });

    // ── Custom schema ─────────────────────────────────────────

    it("uses custom schema", () => {
        const { text } = QueryBuilder.from("users", "tenant_123").toSQL();

        expect(text).toContain('"tenant_123"."users"');
    });

    // ── Alias ─────────────────────────────────────────────────

    it("supports table alias", () => {
        const { text } = QueryBuilder.from("users").as("u").toSQL();

        expect(text).toContain('AS "u"');
    });
});

// ── InsertBuilder ───────────────────────────────────────────

describe("InsertBuilder", () => {
    it("generates INSERT with values", () => {
        const { text, values } = InsertBuilder.into("users")
            .values({ email: "a@b.com", name: "Alice" })
            .toSQL();

        expect(text).toContain('INSERT INTO "public"."users"');
        expect(text).toContain('"email", "name"');
        expect(text).toContain("$1, $2");
        expect(values).toEqual(["a@b.com", "Alice"]);
    });

    it("generates INSERT with RETURNING", () => {
        const { text } = InsertBuilder.into("users")
            .values({ email: "a@b.com" })
            .returning("id", "email")
            .toSQL();

        expect(text).toContain("RETURNING id, email");
    });

    it("generates ON CONFLICT DO NOTHING", () => {
        const { text } = InsertBuilder.into("users")
            .values({ email: "a@b.com" })
            .onConflict(["email"], "DO NOTHING")
            .toSQL();

        expect(text).toContain('ON CONFLICT ("email") DO NOTHING');
    });

    it("generates ON CONFLICT DO UPDATE (upsert)", () => {
        const { text } = InsertBuilder.into("users")
            .columns("email", "name")
            .values({ email: "a@b.com", name: "Alice" })
            .onConflict(["email"], "DO UPDATE")
            .toSQL();

        expect(text).toContain("DO UPDATE SET");
        expect(text).toContain('EXCLUDED."name"');
    });

    it("generates multi-row INSERT", () => {
        const { text, values } = InsertBuilder.into("users")
            .values(
                { email: "a@b.com", name: "Alice" },
                { email: "c@d.com", name: "Bob" },
            )
            .toSQL();

        expect(text).toContain("$1, $2");
        expect(text).toContain("$3, $4");
        expect(values).toHaveLength(4);
    });
});

// ── UpdateBuilder ───────────────────────────────────────────

describe("UpdateBuilder", () => {
    it("generates UPDATE with SET and WHERE", () => {
        const { text, values } = UpdateBuilder.table("users")
            .set("name", "UpdatedName")
            .where("id", "=", "abc-123")
            .toSQL();

        expect(text).toContain('UPDATE "public"."users" SET "name" = $1');
        expect(text).toContain("WHERE id = $2");
        expect(values).toEqual(["UpdatedName", "abc-123"]);
    });

    it("supports setAll", () => {
        const { text, values } = UpdateBuilder.table("users")
            .setAll({ name: "Alice", active: true })
            .where("id", "=", "x")
            .toSQL();

        expect(text).toContain('"name" = $1');
        expect(text).toContain('"active" = $2');
        expect(values).toEqual(["Alice", true, "x"]);
    });

    it("generates RETURNING clause", () => {
        const { text } = UpdateBuilder.table("users")
            .set("name", "X")
            .where("id", "=", "1")
            .returning("*")
            .toSQL();

        expect(text).toContain("RETURNING *");
    });
});

// ── DeleteBuilder ───────────────────────────────────────────

describe("DeleteBuilder", () => {
    it("generates DELETE with WHERE", () => {
        const { text, values } = DeleteBuilder.from("users")
            .where("id", "=", "abc")
            .toSQL();

        expect(text).toContain('DELETE FROM "public"."users"');
        expect(text).toContain("WHERE id = $1");
        expect(values).toEqual(["abc"]);
    });

    it("generates DELETE with IS NULL", () => {
        const { text, values } = DeleteBuilder.from("sessions")
            .where("expired_at", "IS NOT NULL")
            .toSQL();

        expect(text).toContain("expired_at IS NOT NULL");
        expect(values).toHaveLength(0);
    });

    it("generates DELETE with RETURNING", () => {
        const { text } = DeleteBuilder.from("users")
            .where("id", "=", "x")
            .returning("id")
            .toSQL();

        expect(text).toContain("RETURNING id");
    });
});
