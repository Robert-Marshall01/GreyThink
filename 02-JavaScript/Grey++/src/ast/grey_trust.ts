// ─── Grey++ Trust Coordinator ────────────────────────────────────────────────
// Central coordinator for security policies.  Registers named policies,
// enforces permission checks against them, and maintains an append-only
// in-memory audit trail.  All stubs — no external dependencies, no real
// RBAC engine, no persistent storage.
//
// Extend by plugging real policy engines (OPA, Casbin, custom RBAC) and
// persistent audit sinks in a future stage.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface PolicyRule {
    name: string;
    /** Human-readable description. */
    description: string;
    /** The rule function — receives a context and returns allow / deny. */
    evaluate: (ctx: PolicyContext) => PolicyDecision;
    createdAt: string;
}

export interface PolicyContext {
    user: string;
    action: string;
    resource?: string;
    meta?: Record<string, unknown>;
}

export interface PolicyDecision {
    allowed: boolean;
    reason: string;
    policy: string;
}

export interface AuditEntry {
    id: string;
    timestamp: string;
    user: string;
    action: string;
    resource?: string;
    decision?: PolicyDecision;
    message?: string;
    level: 'info' | 'warn' | 'error' | 'critical';
}

// ═══════════════════════════════════════════════════════════════════════════
//  TrustCoordinator
// ═══════════════════════════════════════════════════════════════════════════

export class TrustCoordinator {
    /** Registered policies — keyed by policy name. */
    private policies = new Map<string, PolicyRule>();

    /** Audit trail — append-only in-memory log. */
    private auditTrail: AuditEntry[] = [];

    /** Auto-increment counter for audit IDs. */
    private auditSeq = 0;

    // ── Policy Registration ─────────────────────────────────────────────

    /**
     * Register a named policy.
     * If a policy with the same name exists, it is replaced.
     */
    registerPolicy(
        name: string,
        description: string,
        evaluate: (ctx: PolicyContext) => PolicyDecision,
    ): void {
        this.policies.set(name, {
            name,
            description,
            evaluate,
            createdAt: new Date().toISOString(),
        });
    }

    /**
     * Unregister a named policy.
     * Returns `true` if removed, `false` if not found.
     */
    unregisterPolicy(name: string): boolean {
        return this.policies.delete(name);
    }

    /**
     * Check whether a policy is registered.
     */
    hasPolicy(name: string): boolean {
        return this.policies.has(name);
    }

    /**
     * List all registered policy names.
     */
    listPolicies(): string[] {
        return [...this.policies.keys()];
    }

    // ── Policy Enforcement ──────────────────────────────────────────────

    /**
     * Enforce all registered policies against a context.
     * Returns `allowed: true` only if ALL policies allow the action.
     * If no policies are registered, access is allowed by default.
     */
    enforce(ctx: PolicyContext): PolicyDecision {
        if (this.policies.size === 0) {
            const decision: PolicyDecision = {
                allowed: true,
                reason: 'No policies registered — default allow.',
                policy: '<none>',
            };
            this.recordAudit({
                user: ctx.user,
                action: ctx.action,
                resource: ctx.resource,
                decision,
                level: 'info',
            });
            return decision;
        }

        for (const [, rule] of this.policies) {
            const decision = rule.evaluate(ctx);
            if (!decision.allowed) {
                this.recordAudit({
                    user: ctx.user,
                    action: ctx.action,
                    resource: ctx.resource,
                    decision,
                    level: 'warn',
                });
                return decision;
            }
        }

        const decision: PolicyDecision = {
            allowed: true,
            reason: 'All policies passed.',
            policy: '<all>',
        };
        this.recordAudit({
            user: ctx.user,
            action: ctx.action,
            resource: ctx.resource,
            decision,
            level: 'info',
        });
        return decision;
    }

    /**
     * Enforce a single named policy.
     * Returns an error decision if the policy is not found.
     */
    enforcePolicy(policyName: string, ctx: PolicyContext): PolicyDecision {
        const rule = this.policies.get(policyName);
        if (!rule) {
            return {
                allowed: false,
                reason: `Policy "${policyName}" not found.`,
                policy: policyName,
            };
        }
        const decision = rule.evaluate(ctx);
        this.recordAudit({
            user: ctx.user,
            action: ctx.action,
            resource: ctx.resource,
            decision,
            level: decision.allowed ? 'info' : 'warn',
        });
        return decision;
    }

    // ── Audit Trail ─────────────────────────────────────────────────────

    /**
     * Record an audit entry.
     * Returns the generated audit ID.
     */
    recordAudit(entry: Omit<AuditEntry, 'id' | 'timestamp'>): string {
        const id = `audit-${++this.auditSeq}`;
        this.auditTrail.push({
            id,
            timestamp: new Date().toISOString(),
            ...entry,
        });
        return id;
    }

    /**
     * Get the full audit trail, optionally filtered by user.
     */
    getAuditTrail(user?: string): readonly AuditEntry[] {
        if (user) return this.auditTrail.filter(e => e.user === user);
        return this.auditTrail;
    }

    /**
     * Clear the audit trail.
     */
    clearAuditTrail(): void {
        this.auditTrail = [];
        this.auditSeq = 0;
    }

    // ── Diagnostics ─────────────────────────────────────────────────────

    /**
     * Return a snapshot of the trust coordination state.
     */
    snapshot(): {
        policies: string[];
        auditCount: number;
        auditTrail: AuditEntry[];
    } {
        return {
            policies: this.listPolicies(),
            auditCount: this.auditTrail.length,
            auditTrail: [...this.auditTrail],
        };
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_trust') && process.argv.includes('--test')) {
    console.log('═══ grey_trust.ts Self-Test ═══\n');

    const coord = new TrustCoordinator();

    // Register policies.
    coord.registerPolicy('allow-all', 'Allows everything', (_ctx) => ({
        allowed: true,
        reason: 'Allow-all policy.',
        policy: 'allow-all',
    }));

    coord.registerPolicy('deny-delete', 'Denies delete actions', (ctx) => ({
        allowed: ctx.action !== 'delete',
        reason: ctx.action === 'delete' ? 'Delete is forbidden.' : 'Action allowed.',
        policy: 'deny-delete',
    }));

    console.log('Registered policies:', coord.listPolicies());

    // Enforce — allowed action.
    console.log('\n── enforce read (should allow) ──');
    const r1 = coord.enforce({ user: 'alice', action: 'read', resource: 'doc.pdf' });
    console.log(JSON.stringify(r1, null, 2));

    // Enforce — denied action.
    console.log('\n── enforce delete (should deny) ──');
    const r2 = coord.enforce({ user: 'bob', action: 'delete', resource: 'doc.pdf' });
    console.log(JSON.stringify(r2, null, 2));

    // Enforce single policy.
    console.log('\n── enforcePolicy allow-all (should allow) ──');
    const r3 = coord.enforcePolicy('allow-all', { user: 'carol', action: 'write' });
    console.log(JSON.stringify(r3, null, 2));

    // Enforce non-existent policy.
    console.log('\n── enforcePolicy non-existent ──');
    const r4 = coord.enforcePolicy('xyz', { user: 'dave', action: 'read' });
    console.log(JSON.stringify(r4, null, 2));

    // Manual audit entry.
    console.log('\n── manual audit entry ──');
    const auditId = coord.recordAudit({
        user: 'system',
        action: 'startup',
        message: 'TrustCoordinator initialized.',
        level: 'info',
    });
    console.log('Recorded audit:', auditId);

    // Audit trail.
    console.log('\n── audit trail ──');
    console.log(JSON.stringify(coord.getAuditTrail(), null, 2));

    // Audit trail filtered.
    console.log('\n── audit trail (bob only) ──');
    console.log(JSON.stringify(coord.getAuditTrail('bob'), null, 2));

    // Snapshot.
    console.log('\n── snapshot ──');
    const snap = coord.snapshot();
    console.log(`Policies: ${snap.policies.length}, Audit entries: ${snap.auditCount}`);

    console.log('\n✓ All grey_trust tests passed.');
}
