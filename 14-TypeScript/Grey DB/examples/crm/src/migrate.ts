// ─────────────────────────────────────────────────────────────
// CRM Migration Script — Uses Grey DB migration engine
// Run: npx ts-node src/migrate.ts
// ─────────────────────────────────────────────────────────────

import dotenv from "dotenv";
dotenv.config();

import {
    GreyDB,
    diffSchemas,
    generateMigration,
    applyMigration,
    getCurrentVersion,
    Table,
} from "@grey-db/core";
import { schema } from "./schema";

async function migrate() {
    const db = GreyDB.fromEnv();

    try {
        // 1. Initialize Grey DB internal tables
        console.log("Initializing Grey DB...");
        await db.init();

        // 2. Diff current (empty on first run) vs desired schema
        const currentSchema: Table[] = []; // First run: no tables exist
        const diff = diffSchemas(currentSchema, schema);

        if (diff.actions.length === 0) {
            console.log("✓ Schema is up to date — no migration needed");
            return;
        }

        console.log(`Found ${diff.actions.length} schema change(s):`);
        for (const warning of diff.warnings) {
            const icon = warning.severity === "danger" ? "✗" : warning.severity === "warning" ? "⚠" : "ℹ";
            console.log(`  ${icon} ${warning.message}`);
        }

        // 3. Generate migration
        const version = (await getCurrentVersion(db.pool)) + 1;
        const migration = generateMigration("crm-schema-init", diff, version);

        console.log(`\nGenerated migration v${version} (checksum: ${migration.checksum})`);
        console.log(`Safe: ${migration.safe ? "yes" : "NO — review required"}`);

        if (migration.warnings.length > 0) {
            console.log("Warnings:");
            for (const w of migration.warnings) {
                console.log(`  ⚠ ${w}`);
            }
        }

        // 4. Apply migration
        console.log("\nApplying migration...");
        await applyMigration(db.pool, migration);
        console.log(`✓ Migration v${version} applied successfully`);

        // 5. Show current version
        const current = await getCurrentVersion(db.pool);
        console.log(`\nCurrent version: v${current}`);
    } catch (err: any) {
        console.error("Migration failed:", err.message);
        process.exit(1);
    } finally {
        await db.close();
    }
}

migrate();
