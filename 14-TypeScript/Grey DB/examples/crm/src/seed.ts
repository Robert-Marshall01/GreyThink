// ─────────────────────────────────────────────────────────────
// CRM Seed Script — Populate demo data
// Run: npx ts-node src/seed.ts
// ─────────────────────────────────────────────────────────────

import dotenv from "dotenv";
dotenv.config();

import { GreyDB, InsertBuilder } from "@grey-db/core";

async function seed() {
    const db = GreyDB.fromEnv();

    try {
        console.log("Seeding CRM demo data...\n");

        // ── Create two organizations (tenants) ──────────────────
        const orgsSQL = InsertBuilder.into("organizations")
            .values(
                { name: "Acme Corp", slug: "acme", plan: "pro", maxUsers: 25 },
                { name: "Globex Inc", slug: "globex", plan: "free", maxUsers: 5 }
            )
            .returning("id", "name")
            .toSQL();

        const orgsResult = await db.pool.query(orgsSQL.text, orgsSQL.values);
        const [acme, globex] = orgsResult.rows;
        console.log(`  ✓ Created org: ${acme.name} (${acme.id})`);
        console.log(`  ✓ Created org: ${globex.name} (${globex.id})`);

        // ── Create users ────────────────────────────────────────
        const usersSQL = InsertBuilder.into("crm_users")
            .values(
                { tenantId: acme.id, email: "alice@acme.com", name: "Alice Johnson", role: "admin" },
                { tenantId: acme.id, email: "bob@acme.com", name: "Bob Smith", role: "member" },
                { tenantId: globex.id, email: "carol@globex.io", name: "Carol Williams", role: "admin" }
            )
            .returning("id", "name", "tenantId")
            .toSQL();

        const usersResult = await db.pool.query(usersSQL.text, usersSQL.values);
        const users = usersResult.rows;
        console.log(`  ✓ Created ${users.length} users`);

        const alice = users.find((u) => u.name === "Alice Johnson")!;
        const bob = users.find((u) => u.name === "Bob Smith")!;
        const carol = users.find((u) => u.name === "Carol Williams")!;

        // ── Create contacts ─────────────────────────────────────
        const contactsSQL = InsertBuilder.into("contacts")
            .values(
                {
                    tenantId: acme.id, ownerId: alice.id,
                    firstName: "David", lastName: "Chen",
                    email: "david@techstartup.io", company: "TechStartup", title: "CTO",
                    source: "inbound", status: "prospect",
                    tags: JSON.stringify(["tech", "enterprise"]),
                },
                {
                    tenantId: acme.id, ownerId: bob.id,
                    firstName: "Emily", lastName: "Park",
                    email: "emily@bigcorp.com", company: "BigCorp", title: "VP Engineering",
                    source: "referral", status: "customer",
                    tags: JSON.stringify(["enterprise", "renewal"]),
                },
                {
                    tenantId: acme.id, ownerId: alice.id,
                    firstName: "Frank", lastName: "Martinez",
                    email: "frank@startup.dev", company: "Startup.dev", title: "Founder",
                    source: "cold-outbound", status: "lead",
                    tags: JSON.stringify(["startup"]),
                },
                {
                    tenantId: globex.id, ownerId: carol.id,
                    firstName: "Grace", lastName: "Lee",
                    email: "grace@enterprise.co", company: "Enterprise Co", title: "Director",
                    source: "conference", status: "prospect",
                    tags: JSON.stringify(["enterprise"]),
                }
            )
            .returning("id", "firstName", "lastName", "tenantId")
            .toSQL();

        const contactsResult = await db.pool.query(contactsSQL.text, contactsSQL.values);
        const contacts = contactsResult.rows;
        console.log(`  ✓ Created ${contacts.length} contacts`);

        // ── Create deals ────────────────────────────────────────
        const dealsSQL = InsertBuilder.into("deals")
            .values(
                {
                    tenantId: acme.id, contactId: contacts[0].id, ownerId: alice.id,
                    name: "TechStartup Platform License", amount: 48000,
                    stage: "proposal", probability: 60,
                },
                {
                    tenantId: acme.id, contactId: contacts[1].id, ownerId: bob.id,
                    name: "BigCorp Annual Renewal", amount: 120000,
                    stage: "negotiation", probability: 85,
                },
                {
                    tenantId: acme.id, contactId: contacts[2].id, ownerId: alice.id,
                    name: "Startup.dev Pilot", amount: 5000,
                    stage: "discovery", probability: 20,
                },
                {
                    tenantId: globex.id, contactId: contacts[3].id, ownerId: carol.id,
                    name: "Enterprise Co Expansion", amount: 75000,
                    stage: "qualification", probability: 40,
                }
            )
            .returning("id", "name", "tenantId")
            .toSQL();

        const dealsResult = await db.pool.query(dealsSQL.text, dealsSQL.values);
        const deals = dealsResult.rows;
        console.log(`  ✓ Created ${deals.length} deals`);

        // ── Create activities ───────────────────────────────────
        const activitiesSQL = InsertBuilder.into("activities")
            .values(
                {
                    tenantId: acme.id, contactId: contacts[0].id, dealId: deals[0].id,
                    userId: alice.id, type: "call", subject: "Discovery call with David",
                    body: "Discussed platform requirements and timeline.",
                },
                {
                    tenantId: acme.id, contactId: contacts[0].id, dealId: deals[0].id,
                    userId: alice.id, type: "email", subject: "Follow-up: Pricing proposal",
                    body: "Sent pricing deck and case studies.",
                },
                {
                    tenantId: acme.id, contactId: contacts[1].id, dealId: deals[1].id,
                    userId: bob.id, type: "meeting", subject: "QBR with BigCorp",
                    body: "Quarterly business review — renewal discussion.",
                },
                {
                    tenantId: globex.id, contactId: contacts[3].id, dealId: deals[3].id,
                    userId: carol.id, type: "note", subject: "Met at DevConf 2024",
                    body: "Grace expressed interest in our analytics module.",
                }
            )
            .returning("id")
            .toSQL();

        const activitiesResult = await db.pool.query(activitiesSQL.text, activitiesSQL.values);
        console.log(`  ✓ Created ${activitiesResult.rows.length} activities`);

        // ── Summary ─────────────────────────────────────────────
        console.log("\n✓ Seed complete!");
        console.log(`  Organizations: 2 (Acme Corp, Globex Inc)`);
        console.log(`  Users: 3`);
        console.log(`  Contacts: 4`);
        console.log(`  Deals: 4 ($${(48000 + 120000 + 5000 + 75000).toLocaleString()} pipeline)`);
        console.log(`  Activities: 4`);
        console.log(`\n  Acme tenant ID:  ${acme.id}`);
        console.log(`  Globex tenant ID: ${globex.id}`);
    } catch (err: any) {
        console.error("Seed failed:", err.message);
        process.exit(1);
    } finally {
        await db.close();
    }
}

seed();
