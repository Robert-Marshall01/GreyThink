// ─────────────────────────────────────────────────────────────
// Grey DB — Tenant Routes
// ─────────────────────────────────────────────────────────────

import { Router, Request, Response } from "express";
import { GreyDB } from "@grey-db/core";
import { requirePermission } from "../middleware/auth";

export function tenantRoutes(): Router {
    const router = Router();

    /** List all tenants */
    router.get("/", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const tenants = await db.tenants.listTenants();
            res.json({ tenants });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get a single tenant */
    router.get("/:id", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const tenant = await db.tenants.getTenant(req.params.id);
            if (!tenant) {
                res.status(404).json({ error: "Tenant not found" });
                return;
            }
            res.json(tenant);
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Create a new tenant */
    router.post("/", requirePermission("tenant:manage"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { name, slug, isolationStrategy = "shared", featureFlags = {}, storageLimitMB = 1024, config = {} } = req.body;
            if (!name || !slug) {
                res.status(400).json({ error: "name and slug are required" });
                return;
            }
            const tenant = await db.tenants.createTenant({
                name, slug, isolationStrategy, featureFlags, storageLimitMB, config, active: true,
            });
            res.status(201).json(tenant);
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Update a tenant */
    router.patch("/:id", requirePermission("tenant:manage"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const tenant = await db.tenants.updateTenant(req.params.id, req.body);
            if (!tenant) {
                res.status(404).json({ error: "Tenant not found" });
                return;
            }
            res.json(tenant);
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Delete a tenant */
    router.delete("/:id", requirePermission("tenant:manage"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const deleted = await db.tenants.deleteTenant(req.params.id);
            if (!deleted) {
                res.status(404).json({ error: "Tenant not found" });
                return;
            }
            res.json({ success: true });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Apply RLS to a table */
    router.post("/rls/apply", requirePermission("tenant:manage"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const { tableName, schema = "public" } = req.body;
            if (!tableName) {
                res.status(400).json({ error: "tableName is required" });
                return;
            }
            const sql = await db.tenants.applyRLSPolicy(tableName, schema);
            res.json({ success: true, sql });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Get tenant usage */
    router.get("/:id/usage", async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const usage = await db.tenants.getUsage(req.params.id);
            res.json(usage || { tenantId: req.params.id, rowCount: 0, storageMB: 0, queryCount: 0 });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    /** Create tenant schema (hybrid mode) */
    router.post("/:id/schema", requirePermission("tenant:manage"), async (req: Request, res: Response) => {
        const db: GreyDB = (req as any).db;
        try {
            const schemaName = await db.tenants.createTenantSchema(req.params.id);
            res.json({ schemaName });
        } catch (err: any) {
            res.status(500).json({ error: err.message });
        }
    });

    return router;
}
