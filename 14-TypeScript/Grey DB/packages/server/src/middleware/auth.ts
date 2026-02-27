// ─────────────────────────────────────────────────────────────
// Grey DB — Auth Middleware
// API key validation and permission checks
// ─────────────────────────────────────────────────────────────

import { Request, Response, NextFunction } from "express";
import { GreyDB, Permission } from "@grey-db/core";

export function authMiddleware(req: Request, res: Response, next: NextFunction): void {
  const db: GreyDB = (req as any).db;
  const authHeader = req.headers.authorization;

  if (!authHeader || !authHeader.startsWith("Bearer ")) {
    // No auth — set as anonymous with read-only
    (req as any).user = null;
    (req as any).apiKey = null;
    (req as any).permissions = ["schema:read", "data:read"];
    return next();
  }

  const rawKey = authHeader.substring(7);

  db.rbac.validateAPIKey(rawKey).then((result) => {
    if (!result) {
      res.status(401).json({ error: "Invalid or expired API key" });
      return;
    }
    (req as any).user = result.user;
    (req as any).apiKey = result.apiKey;
    (req as any).permissions = result.apiKey.permissions;

    // Audit log
    db.rbac.auditLog(
      result.user.id,
      `${req.method} ${req.path}`,
      req.path,
      { method: req.method },
      result.apiKey.tenantScope,
      req.ip
    ).catch(() => {});

    next();
  }).catch((err) => {
    next(err);
  });
}

/** Require a specific permission */
export function requirePermission(permission: Permission) {
  return (req: Request, res: Response, next: NextFunction) => {
    const permissions: Permission[] = (req as any).permissions || [];
    if (permissions.includes("admin:full") || permissions.includes(permission)) {
      return next();
    }
    res.status(403).json({ error: `Insufficient permissions. Required: ${permission}` });
  };
}
