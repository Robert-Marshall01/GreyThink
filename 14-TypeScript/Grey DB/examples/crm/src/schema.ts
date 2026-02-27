// ─────────────────────────────────────────────────────────────
// CRM Schema — Declarative table definitions using Grey DB DSL
// Demonstrates: greyTable, all column types, FK relationships,
// tenant scoping, RLS, indexes, audit trails
// ─────────────────────────────────────────────────────────────

import {
    greyTable,
    uuid,
    text,
    varchar,
    integer,
    boolean,
    timestamptz,
    jsonb,
    numeric,
    Table,
} from "@grey-db/core";

// ── Organizations (top-level tenant entity) ─────────────────

export const Organizations = greyTable("organizations", {
    id: uuid().primaryKey().defaultUUID(),
    name: text().notNull(),
    slug: varchar(100).unique().notNull(),
    plan: text().default("free"),
    maxUsers: integer().default(5),
    settings: jsonb().default("{}"),
    createdAt: timestamptz().defaultNow(),
    updatedAt: timestamptz().defaultNow(),
})
    .index("idx_organizations_slug", ["slug"], { unique: true });

// ── Users ───────────────────────────────────────────────────

export const Users = greyTable("crm_users", {
    id: uuid().primaryKey().defaultUUID(),
    tenantId: uuid().notNull().references("organizations", "id", { onDelete: "CASCADE" }),
    email: text().unique().notNull(),
    name: text().notNull(),
    role: text().default("member").check("role IN ('admin', 'member', 'viewer')"),
    active: boolean().default(true),
    lastLoginAt: timestamptz().nullable(),
    createdAt: timestamptz().defaultNow(),
})
    .index("idx_crm_users_tenant", ["tenantId"])
    .index("idx_crm_users_email", ["email"], { unique: true })
    .withTenantScope();

// ── Contacts (the CRM M in CRM) ────────────────────────────

export const Contacts = greyTable("contacts", {
    id: uuid().primaryKey().defaultUUID(),
    tenantId: uuid().notNull().references("organizations", "id", { onDelete: "CASCADE" }),
    ownerId: uuid().nullable().references("crm_users", "id"),
    firstName: text().notNull(),
    lastName: text().notNull(),
    email: text().nullable().indexed(),
    phone: varchar(20).nullable(),
    company: text().nullable(),
    title: text().nullable(),
    source: text().default("manual"),
    status: text().default("lead").check("status IN ('lead', 'prospect', 'customer', 'churned')"),
    tags: jsonb().default("[]"),
    metadata: jsonb().default("{}"),
    createdAt: timestamptz().defaultNow(),
    updatedAt: timestamptz().defaultNow(),
})
    .index("idx_contacts_tenant", ["tenantId"])
    .index("idx_contacts_status", ["tenantId", "status"])
    .index("idx_contacts_owner", ["ownerId"])
    .withTenantScope();

// ── Deals (pipeline tracking) ───────────────────────────────

export const Deals = greyTable("deals", {
    id: uuid().primaryKey().defaultUUID(),
    tenantId: uuid().notNull().references("organizations", "id", { onDelete: "CASCADE" }),
    contactId: uuid().notNull().references("contacts", "id", { onDelete: "CASCADE" }),
    ownerId: uuid().nullable().references("crm_users", "id"),
    name: text().notNull(),
    amount: numeric(12, 2).default(0),
    currency: varchar(3).default("USD"),
    stage: text().default("discovery").check(
        "stage IN ('discovery', 'qualification', 'proposal', 'negotiation', 'closed_won', 'closed_lost')"
    ),
    probability: integer().default(0).check("probability >= 0 AND probability <= 100"),
    expectedCloseDate: timestamptz().nullable(),
    closedAt: timestamptz().nullable(),
    notes: text().nullable(),
    createdAt: timestamptz().defaultNow(),
    updatedAt: timestamptz().defaultNow(),
})
    .index("idx_deals_tenant", ["tenantId"])
    .index("idx_deals_stage", ["tenantId", "stage"])
    .index("idx_deals_contact", ["contactId"])
    .withTenantScope();

// ── Activities (calls, emails, meetings) ────────────────────

export const Activities = greyTable("activities", {
    id: uuid().primaryKey().defaultUUID(),
    tenantId: uuid().notNull().references("organizations", "id", { onDelete: "CASCADE" }),
    contactId: uuid().nullable().references("contacts", "id", { onDelete: "SET NULL" }),
    dealId: uuid().nullable().references("deals", "id", { onDelete: "SET NULL" }),
    userId: uuid().notNull().references("crm_users", "id"),
    type: text().notNull().check("type IN ('call', 'email', 'meeting', 'note', 'task')"),
    subject: text().notNull(),
    body: text().nullable(),
    scheduledAt: timestamptz().nullable(),
    completedAt: timestamptz().nullable(),
    metadata: jsonb().default("{}"),
    createdAt: timestamptz().defaultNow(),
})
    .index("idx_activities_tenant", ["tenantId"])
    .index("idx_activities_contact", ["contactId"])
    .index("idx_activities_deal", ["dealId"])
    .index("idx_activities_type", ["tenantId", "type"])
    .withTenantScope();

// ── Audit Log ───────────────────────────────────────────────

export const AuditLog = greyTable("crm_audit_log", {
    id: uuid().primaryKey().defaultUUID(),
    tenantId: uuid().notNull().references("organizations", "id", { onDelete: "CASCADE" }),
    userId: uuid().nullable().references("crm_users", "id"),
    action: text().notNull(),
    entity: text().notNull(),
    entityId: uuid().notNull(),
    changes: jsonb().nullable(),
    ipAddress: varchar(45).nullable(),
    createdAt: timestamptz().defaultNow(),
})
    .index("idx_audit_tenant", ["tenantId"])
    .index("idx_audit_entity", ["entity", "entityId"])
    .withTenantScope();

// ── Full schema export ──────────────────────────────────────

export const schema: Table[] = [
    Organizations,
    Users,
    Contacts,
    Deals,
    Activities,
    AuditLog,
];
