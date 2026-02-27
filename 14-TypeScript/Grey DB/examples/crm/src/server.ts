// ─────────────────────────────────────────────────────────────
// CRM API Server — Reference SaaS application using Grey DB
// Demonstrates: query builder, tenant isolation (RLS),
// AI-assisted queries, EXPLAIN analysis, audit logging
// ─────────────────────────────────────────────────────────────

import dotenv from "dotenv";
dotenv.config();

import express, { Request, Response, NextFunction } from "express";
import {
    GreyDB,
    QueryBuilder,
    InsertBuilder,
    UpdateBuilder,
    DeleteBuilder,
    explainQuerySafe,
    formatExplainResult,
} from "@grey-db/core";

const app = express();
app.use(express.json());

// ── Grey DB Instance ────────────────────────────────────────

const db = GreyDB.fromEnv();

// ── Middleware: Extract tenant from header ───────────────────

interface TenantRequest extends Request {
    tenantId?: string;
}

function requireTenant(req: TenantRequest, res: Response, next: NextFunction): void {
    const tenantId = req.headers["x-tenant-id"] as string;
    if (!tenantId) {
        res.status(400).json({ error: "Missing X-Tenant-ID header" });
        return;
    }
    req.tenantId = tenantId;
    next();
}

// ── Health ──────────────────────────────────────────────────

app.get("/health", async (_req, res) => {
    try {
        await db.pool.query("SELECT 1");
        res.json({ status: "ok", timestamp: new Date().toISOString() });
    } catch {
        res.status(503).json({ status: "unhealthy" });
    }
});

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Contacts CRUD
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

// GET /contacts — List contacts (tenant-scoped via RLS)
app.get("/contacts", requireTenant, async (req: TenantRequest, res) => {
    try {
        const { status, search, limit = "50", offset = "0" } = req.query;

        const qb = QueryBuilder.from("contacts")
            .select(
                "id", "firstName", "lastName", "email",
                "company", "title", "status", "source", "createdAt"
            )
            .forTenant(req.tenantId!)
            .orderBy("createdAt", "DESC")
            .limit(parseInt(limit as string))
            .offset(parseInt(offset as string));

        if (status) qb.where("status", "=", status);
        if (search) qb.whereRaw("(firstName ILIKE ? OR lastName ILIKE ? OR email ILIKE ?)",
            `%${search}%`, `%${search}%`, `%${search}%`);

        const { text, values } = qb.toSQL();

        // Use tenant context for RLS enforcement
        const rows = await db.tenants.withTenant(req.tenantId!, async (client) => {
            const result = await client.query(text, values);
            return result.rows;
        });

        res.json({ data: rows, count: rows.length });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// GET /contacts/:id — Get single contact
app.get("/contacts/:id", requireTenant, async (req: TenantRequest, res) => {
    try {
        const qb = QueryBuilder.from("contacts")
            .forTenant(req.tenantId!)
            .where("id", "=", req.params.id);

        const { text, values } = qb.toSQL();
        const rows = await db.tenants.withTenant(req.tenantId!, async (client) => {
            const result = await client.query(text, values);
            return result.rows;
        });

        if (rows.length === 0) {
            res.status(404).json({ error: "Contact not found" });
            return;
        }

        res.json({ data: rows[0] });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// POST /contacts — Create a new contact
app.post("/contacts", requireTenant, async (req: TenantRequest, res) => {
    try {
        const { firstName, lastName, email, phone, company, title, source, status, ownerId, tags } = req.body;

        const insert = InsertBuilder.into("contacts")
            .values({
                tenantId: req.tenantId!,
                firstName,
                lastName,
                email: email || null,
                phone: phone || null,
                company: company || null,
                title: title || null,
                source: source || "manual",
                status: status || "lead",
                ownerId: ownerId || null,
                tags: JSON.stringify(tags || []),
            })
            .returning("*");

        const { text, values } = insert.toSQL();

        const rows = await db.tenants.withTenant(req.tenantId!, async (client) => {
            const result = await client.query(text, values);
            return result.rows;
        });

        res.status(201).json({ data: rows[0] });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// PUT /contacts/:id — Update a contact
app.put("/contacts/:id", requireTenant, async (req: TenantRequest, res) => {
    try {
        const update = UpdateBuilder.table("contacts")
            .setAll(req.body)
            .set("updatedAt", new Date().toISOString())
            .where("id", "=", req.params.id)
            .where("tenant_id", "=", req.tenantId!)
            .returning("*");

        const { text, values } = update.toSQL();
        const result = await db.pool.query(text, values);

        if (result.rows.length === 0) {
            res.status(404).json({ error: "Contact not found" });
            return;
        }

        res.json({ data: result.rows[0] });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Deals CRUD
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

// GET /deals — List deals with pipeline summary
app.get("/deals", requireTenant, async (req: TenantRequest, res) => {
    try {
        const { stage } = req.query;

        const qb = QueryBuilder.from("deals", "public")
            .as("d")
            .select(
                "d.id", "d.name", "d.amount", "d.stage", "d.probability",
                "d.expectedCloseDate", "d.createdAt"
            )
            .selectRaw('c."firstName" || \' \' || c."lastName" AS contact_name')
            .join("contacts", 'c.id = d."contactId"', "LEFT", "c")
            .forTenant(req.tenantId!)
            .orderBy("d.amount", "DESC");

        if (stage) qb.where("d.stage", "=", stage);

        const { text, values } = qb.toSQL();

        const rows = await db.tenants.withTenant(req.tenantId!, async (client) => {
            const result = await client.query(text, values);
            return result.rows;
        });

        res.json({ data: rows, count: rows.length });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// GET /deals/pipeline — Pipeline summary (aggregates)
app.get("/deals/pipeline", requireTenant, async (req: TenantRequest, res) => {
    try {
        const qb = QueryBuilder.from("deals")
            .select("stage")
            .selectAgg("COUNT", "*", "count")
            .selectAgg("SUM", "amount", "total_amount")
            .selectAgg("AVG", "probability", "avg_probability")
            .forTenant(req.tenantId!)
            .groupBy("stage")
            .orderBy("stage", "ASC");

        const { text, values } = qb.toSQL();

        const rows = await db.tenants.withTenant(req.tenantId!, async (client) => {
            const result = await client.query(text, values);
            return result.rows;
        });

        res.json({ data: rows });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// POST /deals — Create a deal
app.post("/deals", requireTenant, async (req: TenantRequest, res) => {
    try {
        const { contactId, ownerId, name, amount, stage, probability, expectedCloseDate } = req.body;

        const insert = InsertBuilder.into("deals")
            .values({
                tenantId: req.tenantId!,
                contactId,
                ownerId: ownerId || null,
                name,
                amount: amount || 0,
                stage: stage || "discovery",
                probability: probability || 0,
                expectedCloseDate: expectedCloseDate || null,
            })
            .returning("*");

        const { text, values } = insert.toSQL();

        const rows = await db.tenants.withTenant(req.tenantId!, async (client) => {
            const result = await client.query(text, values);
            return result.rows;
        });

        res.status(201).json({ data: rows[0] });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Activities
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

// GET /activities — List activities for a contact or deal
app.get("/activities", requireTenant, async (req: TenantRequest, res) => {
    try {
        const { contactId, dealId, type } = req.query;

        const qb = QueryBuilder.from("activities", "public")
            .as("a")
            .select("a.id", "a.type", "a.subject", "a.body", "a.createdAt")
            .selectRaw('u."name" AS user_name')
            .join("crm_users", 'u.id = a."userId"', "LEFT", "u")
            .forTenant(req.tenantId!)
            .orderBy("a.createdAt", "DESC")
            .limit(50);

        if (contactId) qb.where("a.contactId", "=", contactId);
        if (dealId) qb.where("a.dealId", "=", dealId);
        if (type) qb.where("a.type", "=", type);

        const { text, values } = qb.toSQL();

        const rows = await db.tenants.withTenant(req.tenantId!, async (client) => {
            const result = await client.query(text, values);
            return result.rows;
        });

        res.json({ data: rows });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// POST /activities — Log an activity
app.post("/activities", requireTenant, async (req: TenantRequest, res) => {
    try {
        const { contactId, dealId, userId, type, subject, body, scheduledAt } = req.body;

        const insert = InsertBuilder.into("activities")
            .values({
                tenantId: req.tenantId!,
                contactId: contactId || null,
                dealId: dealId || null,
                userId,
                type,
                subject,
                body: body || null,
                scheduledAt: scheduledAt || null,
            })
            .returning("*");

        const { text, values } = insert.toSQL();

        const rows = await db.tenants.withTenant(req.tenantId!, async (client) => {
            const result = await client.query(text, values);
            return result.rows;
        });

        res.status(201).json({ data: rows[0] });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// AI Layer
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

// POST /ai/query — Natural language to SQL
app.post("/ai/query", requireTenant, async (req: TenantRequest, res) => {
    try {
        const { question } = req.body;
        if (!question) {
            res.status(400).json({ error: "Missing 'question' in request body" });
            return;
        }

        // Load schema metadata
        await db.nlsql.loadSchema();

        // Prepare prompt for external LLM
        const prepared = db.nlsql.prepareNLQuery(question);

        res.json({
            question,
            systemPrompt: prepared.systemPrompt,
            userPrompt: prepared.userPrompt,
            schemaContext: {
                tables: prepared.schemaContext.tables.map((t) => ({
                    name: t.name,
                    columns: t.columns.map((c) => c.name),
                })),
            },
            note: "Send systemPrompt + userPrompt to your LLM to get SQL. Grey DB provides the context, you choose the model.",
        });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// POST /ai/check — Safety check SQL
app.post("/ai/check", async (req, res) => {
    try {
        const { sql, allowWrite } = req.body;
        if (!sql) {
            res.status(400).json({ error: "Missing 'sql' in request body" });
            return;
        }

        const safety = db.nlsql.checkSafety(sql, allowWrite || false);
        res.json(safety);
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Query Analysis
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

// POST /explain — Run EXPLAIN on a query
app.post("/explain", async (req, res) => {
    try {
        const { sql, params } = req.body;
        if (!sql) {
            res.status(400).json({ error: "Missing 'sql' in request body" });
            return;
        }

        const result = await explainQuerySafe(db.pool, sql, params || []);

        res.json({
            query: result.query,
            executionTimeMs: result.executionTimeMs,
            planningTimeMs: result.planningTimeMs,
            warnings: result.warnings,
            formatted: formatExplainResult(result),
            rawPlan: result.rawPlan,
        });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Dashboard Stats
// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

// GET /dashboard — Tenant dashboard metrics
app.get("/dashboard", requireTenant, async (req: TenantRequest, res) => {
    try {
        const stats = await db.tenants.withTenant(req.tenantId!, async (client) => {
            const [contacts, deals, activities] = await Promise.all([
                client.query(
                    `SELECT status, COUNT(*) as count FROM contacts WHERE tenant_id = $1 GROUP BY status`,
                    [req.tenantId!]
                ),
                client.query(
                    `SELECT
            COUNT(*) as total_deals,
            SUM(amount) as total_pipeline,
            SUM(CASE WHEN stage = 'closed_won' THEN amount ELSE 0 END) as won_revenue,
            AVG(probability) as avg_probability
           FROM deals WHERE tenant_id = $1`,
                    [req.tenantId!]
                ),
                client.query(
                    `SELECT type, COUNT(*) as count FROM activities WHERE tenant_id = $1 GROUP BY type`,
                    [req.tenantId!]
                ),
            ]);

            return {
                contacts: contacts.rows,
                deals: deals.rows[0],
                activities: activities.rows,
            };
        });

        res.json({ data: stats });
    } catch (err: any) {
        res.status(500).json({ error: err.message });
    }
});

// ── Start Server ────────────────────────────────────────────

const PORT = parseInt(process.env.CRM_PORT || "3001");

app.listen(PORT, () => {
    console.log(`\n  CRM API Server running on http://localhost:${PORT}`);
    console.log(`  ─────────────────────────────────────────────`);
    console.log(`  Endpoints:`);
    console.log(`    GET    /health              Health check`);
    console.log(`    GET    /dashboard           Tenant dashboard`);
    console.log(`    GET    /contacts            List contacts`);
    console.log(`    GET    /contacts/:id        Get contact`);
    console.log(`    POST   /contacts            Create contact`);
    console.log(`    PUT    /contacts/:id        Update contact`);
    console.log(`    GET    /deals               List deals`);
    console.log(`    GET    /deals/pipeline      Pipeline summary`);
    console.log(`    POST   /deals               Create deal`);
    console.log(`    GET    /activities           List activities`);
    console.log(`    POST   /activities           Log activity`);
    console.log(`    POST   /ai/query            NL → SQL`);
    console.log(`    POST   /ai/check            SQL safety check`);
    console.log(`    POST   /explain             EXPLAIN query`);
    console.log(`  ─────────────────────────────────────────────`);
    console.log(`  Set X-Tenant-ID header for multi-tenant routes\n`);
});

// Graceful shutdown
process.on("SIGTERM", async () => {
    console.log("  Shutting down...");
    await db.close();
    process.exit(0);
});
