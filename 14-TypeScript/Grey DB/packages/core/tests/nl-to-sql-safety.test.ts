// ─────────────────────────────────────────────────────────────
// Grey DB — NL→SQL Safety Validation Tests
// Tests that dangerous SQL patterns are correctly identified
// ─────────────────────────────────────────────────────────────

import { NLToSQL, SafetyCheck } from "../src/ai/nl-to-sql";

// We need a Pool mock since the constructor requires it,
// but checkSafety is a pure function that doesn't use the pool
const mockPool = {} as any;

describe("NLToSQL.checkSafety", () => {
    let nlsql: NLToSQL;

    beforeEach(() => {
        nlsql = new NLToSQL(mockPool);
    });

    // ── Safe queries ──────────────────────────────────────────

    it("allows simple SELECT", () => {
        const result = nlsql.checkSafety("SELECT * FROM users WHERE id = 1");

        expect(result.safe).toBe(true);
        expect(result.level).toBe("read");
        expect(result.blockedOperations).toHaveLength(0);
    });

    it("allows SELECT with JOIN", () => {
        const result = nlsql.checkSafety(
            "SELECT u.name, o.total FROM users u JOIN orders o ON u.id = o.user_id"
        );

        expect(result.safe).toBe(true);
        expect(result.level).toBe("read");
    });

    it("allows SELECT with aggregate", () => {
        const result = nlsql.checkSafety(
            "SELECT COUNT(*), AVG(amount) FROM orders GROUP BY user_id"
        );

        expect(result.safe).toBe(true);
    });

    it("allows SELECT with CTE", () => {
        const result = nlsql.checkSafety(
            "WITH active AS (SELECT * FROM users WHERE active = true) SELECT * FROM active"
        );

        expect(result.safe).toBe(true);
    });

    // ── Write operations ──────────────────────────────────────

    it("warns on INSERT", () => {
        const result = nlsql.checkSafety(
            "INSERT INTO users (email) VALUES ('test@test.com')"
        );

        expect(result.level).toBe("write");
        expect(result.warnings.length).toBeGreaterThan(0);
        // INSERT is warned but not blocked by default
    });

    it("warns on UPDATE with WHERE", () => {
        const result = nlsql.checkSafety(
            "UPDATE users SET name = 'Alice' WHERE id = 1"
        );

        expect(result.level).toBe("write");
    });

    it("allows writes when allowWrite is true", () => {
        const result = nlsql.checkSafety(
            "INSERT INTO users (email) VALUES ('x@y.com')",
            true // allowWrite
        );

        expect(result.level).toBe("write");
        // No warning when writes are explicitly allowed
    });

    // ── Dangerous: unscoped writes ────────────────────────────

    it("blocks DELETE without WHERE", () => {
        const result = nlsql.checkSafety("DELETE FROM users");

        expect(result.safe).toBe(false);
        expect(result.level).toBe("dangerous");
        expect(result.blockedOperations).toContain("Unscoped write operation");
    });

    it("blocks UPDATE without WHERE", () => {
        const result = nlsql.checkSafety("UPDATE users SET active = false");

        expect(result.safe).toBe(false);
        expect(result.level).toBe("dangerous");
        expect(result.blockedOperations).toContain("Unscoped write operation");
    });

    // ── Dangerous: DDL operations ─────────────────────────────

    it("blocks DROP TABLE", () => {
        const result = nlsql.checkSafety("DROP TABLE users");

        expect(result.safe).toBe(false);
        expect(result.level).toBe("dangerous");
    });

    it("blocks DROP DATABASE", () => {
        const result = nlsql.checkSafety("DROP DATABASE production");

        expect(result.safe).toBe(false);
        expect(result.level).toBe("dangerous");
    });

    it("blocks TRUNCATE", () => {
        const result = nlsql.checkSafety("TRUNCATE TABLE users");

        expect(result.safe).toBe(false);
        expect(result.level).toBe("dangerous");
    });

    it("blocks ALTER TABLE", () => {
        const result = nlsql.checkSafety("ALTER TABLE users DROP COLUMN email");

        expect(result.safe).toBe(false);
        expect(result.level).toBe("ddl");
    });

    it("blocks CREATE TABLE", () => {
        const result = nlsql.checkSafety("CREATE TABLE evil (id INT)");

        expect(result.safe).toBe(false);
        expect(result.level).toBe("ddl");
    });

    // ── Injection patterns ────────────────────────────────────

    it("blocks multi-statement injection", () => {
        const result = nlsql.checkSafety(
            "SELECT * FROM users; DROP TABLE users;"
        );

        expect(result.safe).toBe(false);
        expect(result.blockedOperations.some((o) =>
            o.includes("Multi-statement") || o.includes("DROP")
        )).toBe(true);
    });

    it("blocks pg_shadow access", () => {
        const result = nlsql.checkSafety(
            "SELECT * FROM pg_shadow"
        );

        expect(result.safe).toBe(false);
        expect(result.blockedOperations.some((o) => o.includes("System auth"))).toBe(true);
    });

    it("blocks pg_authid access", () => {
        const result = nlsql.checkSafety(
            "SELECT rolname, rolpassword FROM pg_authid"
        );

        expect(result.safe).toBe(false);
    });

    it("blocks COPY TO", () => {
        const result = nlsql.checkSafety(
            "COPY users TO '/tmp/data.csv'"
        );

        expect(result.safe).toBe(false);
        expect(result.blockedOperations.some((o) => o.includes("COPY"))).toBe(true);
    });

    it("blocks COPY FROM", () => {
        const result = nlsql.checkSafety(
            "COPY users FROM '/etc/passwd'"
        );

        expect(result.safe).toBe(false);
    });

    it("blocks GRANT", () => {
        const result = nlsql.checkSafety(
            "GRANT ALL PRIVILEGES ON ALL TABLES TO evil_user"
        );

        expect(result.safe).toBe(false);
        expect(result.blockedOperations.some((o) => o.includes("Privilege"))).toBe(true);
    });

    it("blocks REVOKE", () => {
        const result = nlsql.checkSafety(
            "REVOKE ALL ON SCHEMA public FROM app_user"
        );

        expect(result.safe).toBe(false);
    });

    it("blocks SET ROLE", () => {
        const result = nlsql.checkSafety(
            "SET ROLE postgres"
        );

        expect(result.safe).toBe(false);
        expect(result.blockedOperations.some((o) => o.includes("Role"))).toBe(true);
    });

    // ── Edge cases ────────────────────────────────────────────

    it("handles empty SQL", () => {
        const result = nlsql.checkSafety("");

        expect(result.safe).toBe(true);
        expect(result.level).toBe("read");
    });

    it("is case-insensitive", () => {
        const result = nlsql.checkSafety("drop table USERS");

        expect(result.safe).toBe(false);
        expect(result.level).toBe("dangerous");
    });

    it("detects danger within a larger query", () => {
        const result = nlsql.checkSafety(
            "WITH data AS (SELECT 1) SELECT * FROM data; TRUNCATE orders;"
        );

        expect(result.safe).toBe(false);
    });
});

describe("NLToSQL.buildResult", () => {
    let nlsql: NLToSQL;

    beforeEach(() => {
        nlsql = new NLToSQL(mockPool);
    });

    it("packages a full NL→SQL result with safety check", () => {
        const result = nlsql.buildResult(
            "show all users",
            "SELECT * FROM users",
            "Simple select of all users",
            0.95
        );

        expect(result.naturalLanguage).toBe("show all users");
        expect(result.sql).toBe("SELECT * FROM users");
        expect(result.explanation).toBe("Simple select of all users");
        expect(result.confidence).toBe(0.95);
        expect(result.safety.safe).toBe(true);
        expect(result.safety.level).toBe("read");
    });

    it("flags dangerous generated SQL in results", () => {
        const result = nlsql.buildResult(
            "delete everything",
            "DELETE FROM users",
            "Deleting all users",
            0.8
        );

        expect(result.safety.safe).toBe(false);
        expect(result.safety.level).toBe("dangerous");
    });
});
