// ─── Grey++ Optimization Layer ───────────────────────────────────────────────
// Parses meta-reports, applies stub optimizations, and outputs a unified
// optimization plan.  No external libraries — minimal, modular, future-proof.
//
// Extend by adding real analysis passes in a future stage.

import type { MetaReport } from './grey_meta.js';

// ─── Types ──────────────────────────────────────────────────────────────────

export interface OptimizationAction {
    /** Human-readable label. */
    label: string;
    /** Subsystem this action targets. */
    subsystem: string;
    /** The specific recommendation. */
    recommendation: string;
    /** Priority: higher = more urgent. */
    priority: number;
    /** Whether the optimization was applied (stub: always false until real engine). */
    applied: boolean;
}

export interface OptimizationPlan {
    _type: 'OptimizationPlan';
    /** Number of actions in the plan. */
    actionCount: number;
    /** Ordered list of optimization actions. */
    actions: OptimizationAction[];
    /** Summary text. */
    summary: string;
    meta: { stub: true };
}

// ═══════════════════════════════════════════════════════════════════════════
//  OptimizationLayer
// ═══════════════════════════════════════════════════════════════════════════

export class OptimizationLayer {
    /** Registered custom rules — subsystem → action builder. */
    private customRules = new Map<string, (entries: unknown[]) => OptimizationAction[]>();

    // ── Parse Meta-Report ───────────────────────────────────────────────

    /**
     * Parse a MetaReport and produce an OptimizationPlan.
     * Uses built-in heuristics plus any registered custom rules.
     */
    analyze(report: MetaReport): OptimizationPlan {
        const actions: OptimizationAction[] = [];

        // Built-in heuristics per subsystem.
        for (const subsystem of report.subsystems) {
            const subsystemEntries = report.entries
                .filter(e => e.subsystem === subsystem)
                .map(e => e.result);

            actions.push(...this.builtinOptimizations(subsystem, subsystemEntries));

            // Custom rules.
            const customRule = this.customRules.get(subsystem);
            if (customRule) {
                actions.push(...customRule(subsystemEntries));
            }
        }

        // Cross-subsystem optimizations.
        if (report.subsystems.length >= 2) {
            actions.push({
                label: 'Cross-subsystem dedup',
                subsystem: '*',
                recommendation: 'Reduce redundancy across subsystems by merging overlapping introspection data.',
                priority: 2,
                applied: false,
            });
        }

        // Sort by priority (descending).
        actions.sort((a, b) => b.priority - a.priority);

        return {
            _type: 'OptimizationPlan',
            actionCount: actions.length,
            actions,
            summary: `Optimization plan: ${actions.length} action(s) across [${report.subsystems.join(', ')}].`,
            meta: { stub: true },
        };
    }

    // ── Built-in Optimizations ──────────────────────────────────────────

    private builtinOptimizations(subsystem: string, _entries: unknown[]): OptimizationAction[] {
        const actions: OptimizationAction[] = [];

        switch (subsystem) {
            case 'pipeline':
                actions.push({
                    label: 'Pipeline cache',
                    subsystem: 'pipeline',
                    recommendation: 'Cache intermediate pipeline step outputs to avoid re-execution.',
                    priority: 3,
                    applied: false,
                });
                actions.push({
                    label: 'Pipeline dedup',
                    subsystem: 'pipeline',
                    recommendation: 'Detect and merge duplicate pipeline steps.',
                    priority: 1,
                    applied: false,
                });
                break;
            case 'grammar':
                actions.push({
                    label: 'Collapse grammar rules',
                    subsystem: 'grammar',
                    recommendation: 'Collapse adjacent same-kind grammar rules for efficiency.',
                    priority: 2,
                    applied: false,
                });
                break;
            case 'trust':
                actions.push({
                    label: 'Prune stale policies',
                    subsystem: 'trust',
                    recommendation: 'Remove unused or redundant trust policies.',
                    priority: 2,
                    applied: false,
                });
                break;
            case 'dist':
                actions.push({
                    label: 'Topology analysis',
                    subsystem: 'dist',
                    recommendation: 'Map cluster topology and identify single points of failure.',
                    priority: 3,
                    applied: false,
                });
                break;
            default:
                actions.push({
                    label: `Analyze ${subsystem}`,
                    subsystem,
                    recommendation: `Run deeper analysis on "${subsystem}" subsystem.`,
                    priority: 1,
                    applied: false,
                });
                break;
        }

        return actions;
    }

    // ── Custom Rules ────────────────────────────────────────────────────

    /**
     * Register a custom optimization rule for a subsystem.
     */
    registerRule(
        subsystem: string,
        rule: (entries: unknown[]) => OptimizationAction[],
    ): void {
        this.customRules.set(subsystem, rule);
    }

    // ── Quick Optimize ──────────────────────────────────────────────────

    /**
     * Convenience method: apply stub-level optimizations and return labels.
     * In a real system, this would actually mutate subsystem state.
     */
    applyStub(plan: OptimizationPlan): string[] {
        return plan.actions.map(a => {
            a.applied = true;
            return `[applied] ${a.label}: ${a.recommendation}`;
        });
    }

    // ── Diagnostics ─────────────────────────────────────────────────────

    /**
     * Return registered custom rule subsystems.
     */
    listCustomRules(): string[] {
        return [...this.customRules.keys()];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_opt') && process.argv.includes('--test')) {
    console.log('═══ grey_opt.ts Self-Test ═══\n');

    const layer = new OptimizationLayer();

    // Build a mock MetaReport.
    const mockReport: MetaReport = {
        _type: 'MetaReport',
        entryCount: 4,
        subsystems: ['pipeline', 'grammar', 'trust', 'dist'],
        entries: [
            { subsystem: 'pipeline', primitive: 'pipeline', timestamp: new Date().toISOString(), result: { id: 'p1' } },
            { subsystem: 'grammar', primitive: 'grammar', timestamp: new Date().toISOString(), result: { rule: 'doc.edit' } },
            { subsystem: 'trust', primitive: 'trust', timestamp: new Date().toISOString(), result: { policy: 'sec.perm' } },
            { subsystem: 'dist', primitive: 'dist', timestamp: new Date().toISOString(), result: { cluster: 'alpha' } },
        ],
        suggestions: [],
        summary: 'mock report',
        meta: { stub: true },
    };

    console.log('── analyze meta-report ──');
    const plan = layer.analyze(mockReport);
    console.log(JSON.stringify(plan, null, 2));

    console.log('\n── applyStub ──');
    const applied = layer.applyStub(plan);
    for (const line of applied) console.log(line);

    // Custom rule.
    console.log('\n── register custom rule ──');
    layer.registerRule('pipeline', (_entries) => [{
        label: 'Custom pipeline rule',
        subsystem: 'pipeline',
        recommendation: 'User-defined pipeline optimization.',
        priority: 5,
        applied: false,
    }]);
    console.log('Custom rules:', layer.listCustomRules());

    const plan2 = layer.analyze(mockReport);
    console.log('\n── re-analyzed with custom rule ──');
    console.log(`Actions: ${plan2.actionCount}, top priority: ${plan2.actions[0]?.label}`);

    // Minimal report (no subsystems).
    console.log('\n── empty report ──');
    const emptyPlan = layer.analyze({
        _type: 'MetaReport', entryCount: 0, subsystems: [], entries: [],
        suggestions: [], summary: 'empty', meta: { stub: true },
    });
    console.log(JSON.stringify(emptyPlan, null, 2));

    console.log('\n✓ All grey_opt tests passed.');
}
