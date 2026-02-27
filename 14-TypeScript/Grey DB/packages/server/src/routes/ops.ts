// ─────────────────────────────────────────────────────────────
// Grey DB — Ops Routes
// Backups, RBAC, observability, dashboards
// ─────────────────────────────────────────────────────────────

import { Router, Request, Response } from "express";
import { GreyDB, RBACEngine } from "@grey-db/core";
import { requirePermission } from "../middleware/auth";

export function opsRoutes(): Router {
  const router = Router();

  // ── Backups ──────────────────────────────────────────────

  /** Create a full backup */
  router.post("/backups/full", requirePermission("backup:create"), async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      const backup = await db.backups.createFullBackup(req.body.schema || "public");
      res.json(backup);
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  /** Create a tenant backup */
  router.post("/backups/tenant/:tenantId", requirePermission("backup:create"), async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      const backup = await db.backups.createTenantBackup(req.params.tenantId, req.body.schema || "public");
      res.json(backup);
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  /** List backup history */
  router.get("/backups", async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      const backups = await db.backups.listBackups();
      res.json({ backups });
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  /** Restore from backup (dry-run by default) */
  router.post("/backups/restore", requirePermission("backup:restore"), async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      const { filePath, dryRun = true } = req.body;
      if (!filePath) {
        res.status(400).json({ error: "filePath is required" });
        return;
      }
      const result = await db.backups.restore(filePath, dryRun);
      res.json({ ...result, dryRun });
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  // ── RBAC ──────────────────────────────────────────────────

  /** Create a user */
  router.post("/users", requirePermission("admin:full"), async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      const { email, role = "readonly", tenantId } = req.body;
      if (!email) {
        res.status(400).json({ error: "email is required" });
        return;
      }
      const user = await db.rbac.createUser(email, role, tenantId);
      res.status(201).json(user);
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  /** List users */
  router.get("/users", requirePermission("admin:full"), async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      const users = await db.rbac.listUsers();
      res.json({ users });
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  /** Generate API key */
  router.post("/api-keys", requirePermission("admin:full"), async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      const { userId, name, permissions, tenantScope, expiresAt } = req.body;
      if (!userId || !name) {
        res.status(400).json({ error: "userId and name are required" });
        return;
      }
      const { apiKey, rawKey } = await db.rbac.createAPIKey(
        userId,
        name,
        permissions,
        tenantScope,
        expiresAt ? new Date(expiresAt) : undefined
      );
      res.status(201).json({
        apiKey,
        rawKey,
        warning: "Save this key securely — it will NOT be shown again.",
      });
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  /** Revoke API key */
  router.delete("/api-keys/:id", requirePermission("admin:full"), async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      await db.rbac.revokeAPIKey(req.params.id);
      res.json({ success: true });
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  /** Get available roles and permissions */
  router.get("/roles", (_req: Request, res: Response) => {
    res.json({
      roles: {
        admin: RBACEngine.getPermissionsForRole("admin"),
        developer: RBACEngine.getPermissionsForRole("developer"),
        readonly: RBACEngine.getPermissionsForRole("readonly"),
      },
    });
  });

  // ── Audit Log ─────────────────────────────────────────────

  /** Get audit log */
  router.get("/audit-log", requirePermission("admin:full"), async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      const entries = await db.rbac.getAuditLog({
        userId: req.query.userId as string,
        tenantId: req.query.tenantId as string,
        action: req.query.action as string,
        limit: parseInt(req.query.limit as string) || 100,
      });
      res.json({ entries });
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  // ── Observability / Dashboard ─────────────────────────────

  /** Get dashboard data */
  router.get("/dashboard", async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    const hours = parseInt(req.query.hours as string) || 24;
    try {
      const dashboard = await db.observability.getDashboard(hours);
      res.json(dashboard);
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  /** Get tenant-specific usage */
  router.get("/dashboard/tenant/:tenantId", async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    const hours = parseInt(req.query.hours as string) || 24;
    try {
      const usage = await db.observability.getTenantUsage(req.params.tenantId, hours);
      res.json(usage);
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  /** Health check (detailed) */
  router.get("/health", async (req: Request, res: Response) => {
    const db: GreyDB = (req as any).db;
    try {
      const health = await db.observability.healthCheck();
      res.json(health);
    } catch (err: any) {
      res.status(500).json({ error: err.message });
    }
  });

  return router;
}
