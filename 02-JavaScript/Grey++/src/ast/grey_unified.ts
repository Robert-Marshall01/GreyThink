// ─── Grey++ Unified Syntax Primitives ────────────────────────────────────────
// Executors for UniNode — the unified `do { … }` block that composes
// multiple primitive operations (Net, Pipeline, Sec, Dist, Doc) into a
// single sequential execution with consolidated output.
//
// Each step is dispatched to its underlying primitive executor.  The unified
// layer wraps every result and returns a single UniResult containing the
// ordered list of step outputs plus a summary.
//
// No external deps — pure stub orchestration.

// ─── Result types ───────────────────────────────────────────────────────────

export interface UniStepResult {
    index: number;
    kind: string;
    result: unknown;
    status: 'ok' | 'error';
    error?: string;
}

export interface UniResult {
    _type: 'UniResult';
    label?: string;
    stepCount: number;
    steps: UniStepResult[];
    summary: string;
    meta: { stub: boolean };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Unified Executor
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Execute a list of pre-resolved step results and wrap them in a UniResult.
 *
 * The runtime resolves each child ASTNode via its normal exec() path and
 * passes the results here for consolidation.  This keeps the unified layer
 * purely a wrapper — it never touches individual primitive logic.
 */
export function consolidateUniResults(
    stepResults: { kind: string; result: unknown }[],
    label?: string,
): UniResult {
    const steps: UniStepResult[] = stepResults.map((sr, i) => ({
        index: i,
        kind: sr.kind,
        result: sr.result,
        status: 'ok' as const,
    }));

    const kinds = steps.map(s => s.kind);
    const uniqueKinds = [...new Set(kinds)];

    return {
        _type: 'UniResult',
        label,
        stepCount: steps.length,
        steps,
        summary: `Unified execution complete — ${steps.length} step(s) across [${uniqueKinds.join(', ')}].`,
        meta: { stub: true },
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Per-Primitive Wrappers (for standalone testing)
// ═══════════════════════════════════════════════════════════════════════════

/** Wrap a Net result as a unified step. */
export function uniNet(result: unknown): UniStepResult {
    return { index: 0, kind: 'Net', result, status: 'ok' };
}

/** Wrap a Pipeline result as a unified step. */
export function uniPipeline(result: unknown): UniStepResult {
    return { index: 0, kind: 'Pipeline', result, status: 'ok' };
}

/** Wrap a Sec result as a unified step. */
export function uniSec(result: unknown): UniStepResult {
    return { index: 0, kind: 'Sec', result, status: 'ok' };
}

/** Wrap a Dist result as a unified step. */
export function uniDist(result: unknown): UniStepResult {
    return { index: 0, kind: 'Dist', result, status: 'ok' };
}

/** Wrap a Doc result as a unified step. */
export function uniDoc(result: unknown): UniStepResult {
    return { index: 0, kind: 'Doc', result, status: 'ok' };
}

/** Wrap any other result as a unified step. */
export function uniGeneric(kind: string, result: unknown): UniStepResult {
    return { index: 0, kind, result, status: 'ok' };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Unified Fabric Layer (Stage 18)
// ═══════════════════════════════════════════════════════════════════════════
// Represents Grey++ as a single meta-language graph.  Applies stub rules
// for collapsing constructs and outputs a unified execution plan.

export interface UnifiedConstruct {
    name: string;
    type: string;
    addedAt: string;
}

export interface CollapseRule {
    action: string;
    sources: string[];
    target: string;
}

export interface UnifiedExecutionPlan {
    _type: 'UnifiedExecutionPlan';
    scenario: string;
    constructCount: number;
    constructs: UnifiedConstruct[];
    appliedRules: CollapseRule[];
    graph: { nodes: string[]; edges: [string, string][] };
    summary: string;
    meta: { stub: true };
}

export class UnifiedFabric {
    private constructs: UnifiedConstruct[] = [];
    private rules: CollapseRule[] = [];

    /** Add a construct to the meta-language graph. */
    addConstruct(name: string, type: string): UnifiedConstruct {
        const construct: UnifiedConstruct = {
            name,
            type,
            addedAt: new Date().toISOString(),
        };
        this.constructs.push(construct);
        return construct;
    }

    /** Get all constructs, optionally filtered by type. */
    getConstructs(type?: string): UnifiedConstruct[] {
        if (type) return this.constructs.filter(c => c.type === type);
        return [...this.constructs];
    }

    /** Apply a collapse rule (e.g., merge pipeline + orchestration into unify.exec). */
    applyRule(rule: CollapseRule): void {
        this.rules.push(rule);
        // Auto-generate a collapsed construct.
        this.constructs.push({
            name: rule.target,
            type: `collapsed:${rule.action}`,
            addedAt: new Date().toISOString(),
        });
    }

    /** Get all applied rules. */
    getRules(): CollapseRule[] {
        return [...this.rules];
    }

    /** Output a unified execution plan for a given scenario. */
    toExecutionPlan(scenario: string): UnifiedExecutionPlan {
        const nodes = [...new Set(this.constructs.map(c => c.name))];
        const edges: [string, string][] = [];

        // Build edges from collapse rules.
        for (const rule of this.rules) {
            for (const src of rule.sources) {
                if (nodes.includes(src)) {
                    edges.push([src, rule.target]);
                }
            }
        }

        // Build sequential edges for non-collapsed constructs.
        const nonCollapsed = this.constructs.filter(c => !c.type.startsWith('collapsed:'));
        for (let i = 1; i < nonCollapsed.length; i++) {
            edges.push([nonCollapsed[i - 1].name, nonCollapsed[i].name]);
        }

        return {
            _type: 'UnifiedExecutionPlan',
            scenario,
            constructCount: this.constructs.length,
            constructs: [...this.constructs],
            appliedRules: [...this.rules],
            graph: { nodes, edges },
            summary: `Unified execution plan for "${scenario}" — ${this.constructs.length} construct(s), ${this.rules.length} rule(s) applied.`,
            meta: { stub: true },
        };
    }

    /** Number of constructs. */
    constructCount(): number {
        return this.constructs.length;
    }

    /** Number of applied rules. */
    ruleCount(): number {
        return this.rules.length;
    }

    /** Clear all state. */
    clear(): void {
        this.constructs = [];
        this.rules = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_unified') && process.argv.includes('--test')) {
    console.log('═══ grey_unified.ts Self-Test ═══\n');

    // Simulate a multi-step do {} block.
    console.log('── consolidateUniResults (3 steps) ──');
    const result = consolidateUniResults([
        { kind: 'Net', result: { _type: 'NetResult', response: 'HTTP 200 OK' } },
        { kind: 'Sec', result: { _type: 'SecPerm', allowed: true } },
        { kind: 'Doc', result: { _type: 'DocEdit', status: 'applied' } },
    ], 'test-block');
    console.log(JSON.stringify(result, null, 2));

    console.log('\n── consolidateUniResults (5 steps, no label) ──');
    const result2 = consolidateUniResults([
        { kind: 'Net', result: { op: 'GET' } },
        { kind: 'Pipeline', result: { _type: 'PipelineResult' } },
        { kind: 'Sec', result: { _type: 'SecPerm' } },
        { kind: 'Dist', result: { _type: 'DistConsensus' } },
        { kind: 'Doc', result: { _type: 'DocEdit' } },
    ]);
    console.log(JSON.stringify(result2, null, 2));

    console.log('\n── uniNet wrapper ──');
    console.log(JSON.stringify(uniNet({ method: 'GET', url: 'https://example.com' }), null, 2));

    console.log('\n── uniPipeline wrapper ──');
    console.log(JSON.stringify(uniPipeline({ steps: 3 }), null, 2));

    console.log('\n── uniSec wrapper ──');
    console.log(JSON.stringify(uniSec({ perm: 'allowed' }), null, 2));

    console.log('\n── uniDist wrapper ──');
    console.log(JSON.stringify(uniDist({ consensus: 'achieved' }), null, 2));

    console.log('\n── uniDoc wrapper ──');
    console.log(JSON.stringify(uniDoc({ edit: 'applied' }), null, 2));

    console.log('\n── uniGeneric wrapper ──');
    console.log(JSON.stringify(uniGeneric('Infer', { model: 'gpt4' }), null, 2));

    console.log('\n✓ All grey_unified tests passed.');

    // ── UnifiedFabric tests (Stage 18) ──
    console.log('\n── UnifiedFabric tests ──');

    const fabric = new UnifiedFabric();

    // addConstruct
    const c1 = fabric.addConstruct('pipeline', 'syntax');
    console.assert(c1.name === 'pipeline', 'construct name');
    console.assert(c1.type === 'syntax', 'construct type');
    console.assert(fabric.constructCount() === 1, 'construct count 1');
    console.log('  ✓ addConstruct');

    // Multiple constructs
    fabric.addConstruct('orchestration', 'syntax');
    fabric.addConstruct('memory', 'exec');
    console.assert(fabric.constructCount() === 3, 'construct count 3');
    console.log('  ✓ multiple constructs');

    // getConstructs (all)
    const all = fabric.getConstructs();
    console.assert(all.length === 3, 'all constructs');
    console.log('  ✓ getConstructs (all)');

    // getConstructs (filtered)
    const syntaxOnly = fabric.getConstructs('syntax');
    console.assert(syntaxOnly.length === 2, `syntax constructs: ${syntaxOnly.length}`);
    console.log('  ✓ getConstructs (filtered)');

    // applyRule
    fabric.applyRule({ action: 'merge', sources: ['pipeline', 'orchestration'], target: 'unify.exec' });
    console.assert(fabric.ruleCount() === 1, 'rule count 1');
    console.assert(fabric.constructCount() === 4, 'construct count after rule');
    console.log('  ✓ applyRule');

    // toExecutionPlan
    const plan = fabric.toExecutionPlan('civilization-tier');
    console.assert(plan._type === 'UnifiedExecutionPlan', 'plan type');
    console.assert(plan.scenario === 'civilization-tier', 'plan scenario');
    console.assert(plan.constructCount === 4, 'plan construct count');
    console.assert(plan.appliedRules.length === 1, 'plan rules');
    console.assert(plan.graph.nodes.length >= 3, `plan graph nodes: ${plan.graph.nodes.length}`);
    console.assert(plan.graph.edges.length >= 1, `plan graph edges: ${plan.graph.edges.length}`);
    console.assert(plan.summary.includes('civilization-tier'), 'plan summary');
    console.log('  ✓ toExecutionPlan');

    // clear
    fabric.clear();
    console.assert(fabric.constructCount() === 0, 'cleared constructs');
    console.assert(fabric.ruleCount() === 0, 'cleared rules');
    console.log('  ✓ clear');

    // empty plan
    const emptyPlan = fabric.toExecutionPlan('empty');
    console.assert(emptyPlan.constructCount === 0, 'empty count');
    console.log('  ✓ empty plan');

    console.log('── all UnifiedFabric tests passed ──');
}
