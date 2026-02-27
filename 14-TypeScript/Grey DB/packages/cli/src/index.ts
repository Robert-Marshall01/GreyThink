#!/usr/bin/env node
// ─────────────────────────────────────────────────────────────
// greydb — CLI for Grey DB
// Schema migrations, tenant management, query analysis, AI
// ─────────────────────────────────────────────────────────────

import { Command } from "commander";
import dotenv from "dotenv";

// Load .env before anything else
dotenv.config();

import { registerMigrateCommands } from "./commands/migrate";
import { registerDiffCommand } from "./commands/diff";
import { registerExplainCommand } from "./commands/explain";
import { registerTenantCommands } from "./commands/tenant";
import { registerAICommands } from "./commands/ai";
import { registerStatusCommand } from "./commands/status";

const program = new Command();

program
    .name("greydb")
    .description("Grey DB — Postgres-backed data platform CLI")
    .version("1.0.0");

registerMigrateCommands(program);
registerDiffCommand(program);
registerExplainCommand(program);
registerTenantCommands(program);
registerAICommands(program);
registerStatusCommand(program);

program.parse();
