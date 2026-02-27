// ─────────────────────────────────────────────────────────────
// Grey DB — API Server Entry Point
// Express server exposing all Grey DB subsystems
// ─────────────────────────────────────────────────────────────

import dotenv from "dotenv";
dotenv.config();

import express from "express";
import cors from "cors";
import helmet from "helmet";
import morgan from "morgan";
import { GreyDB } from "@grey-db/core";
import { schemaRoutes } from "./routes/schema";
import { tenantRoutes } from "./routes/tenants";
import { queryRoutes } from "./routes/query";
import { aiRoutes } from "./routes/ai";
import { opsRoutes } from "./routes/ops";
import { authMiddleware } from "./middleware/auth";

const PORT = parseInt(process.env.PORT || "4000");

async function main() {
  // Initialize Grey DB
  const db = GreyDB.fromEnv();
  await db.init();
  console.log("[Grey DB] All subsystems initialized");

  const app = express();

  // Global middleware
  app.use(helmet({ contentSecurityPolicy: false }));
  app.use(cors({ origin: process.env.CORS_ORIGIN || "*" }));
  app.use(express.json({ limit: "10mb" }));
  app.use(morgan("short"));

  // Health check (public)
  app.get("/api/health", async (_req, res) => {
    try {
      const health = await db.observability.healthCheck();
      res.json(health);
    } catch (err: any) {
      res.status(500).json({ status: "unhealthy", error: err.message });
    }
  });

  // Attach db instance to requests
  app.use((req, _res, next) => {
    (req as any).db = db;
    next();
  });

  // Auth middleware (optional — checks API key if provided)
  app.use("/api", authMiddleware);

  // Routes
  app.use("/api/schema", schemaRoutes());
  app.use("/api/tenants", tenantRoutes());
  app.use("/api/query", queryRoutes());
  app.use("/api/ai", aiRoutes());
  app.use("/api/ops", opsRoutes());

  // Error handler
  app.use((err: any, _req: express.Request, res: express.Response, _next: express.NextFunction) => {
    console.error("[Grey DB] Error:", err);
    res.status(err.status || 500).json({
      error: err.message || "Internal server error",
      code: err.code,
    });
  });

  app.listen(PORT, () => {
    console.log(`[Grey DB] Server running on http://localhost:${PORT}`);
    console.log(`[Grey DB] API docs: http://localhost:${PORT}/api/health`);
  });
}

main().catch((err) => {
  console.error("[Grey DB] Failed to start:", err);
  process.exit(1);
});
