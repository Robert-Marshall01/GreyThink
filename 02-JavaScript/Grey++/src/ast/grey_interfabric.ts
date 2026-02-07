// ─── Grey++ Interop Fabric Layer ────────────────────────────────────────────
// Represents cross-backend execution graphs and applies stub bridging rules
// for unified interop execution plans.  Pure in-memory — no external
// dependencies, no real cross-process execution.
//
// Self-test:  npx tsx src/ast/grey_interfabric.ts --test
//
// Extend by plugging real execution graph planners in a future stage.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface InteropEdge {
    source: string;
    target: string;
    kind: string;
    createdAt: string;
}

export interface InteropRule {
    kind: string;
    modules: string[];
    strategy: string;
    createdAt: string;
}

export interface InteropPlan {
    _type: 'InteropPlan';
    edgeCount: number;
    ruleCount: number;
    edges: InteropEdge[];
    rules: InteropRule[];
    executionOrder: string[];
    meta: { stub: true };
}

// ─── InteropFabric ─────────────────────────────────────────────────────────

export class InteropFabric {
    private edges: InteropEdge[] = [];
    private rules: InteropRule[] = [];

    /** Add an edge to the cross-backend execution graph. */
    addEdge(source: string, target: string, kind: string): InteropEdge {
        const edge: InteropEdge = {
            source,
            target,
            kind,
            createdAt: new Date().toISOString(),
        };
        this.edges.push(edge);
        return edge;
    }

    /** Apply a stub bridging rule (e.g. "JS output → C++ input"). */
    applyRule(kind: string, modules: string[], strategy: string): InteropRule {
        const rule: InteropRule = {
            kind,
            modules,
            strategy,
            createdAt: new Date().toISOString(),
        };
        this.rules.push(rule);
        return rule;
    }

    /** Get all edges, optionally filtered by kind. */
    getEdges(kind?: string): InteropEdge[] {
        if (kind) return this.edges.filter(e => e.kind === kind);
        return [...this.edges];
    }

    /** Get all rules, optionally filtered by kind. */
    getRules(kind?: string): InteropRule[] {
        if (kind) return this.rules.filter(r => r.kind === kind);
        return [...this.rules];
    }

    /** Number of edges in the graph. */
    edgeCount(): number {
        return this.edges.length;
    }

    /** Number of applied rules. */
    ruleCount(): number {
        return this.rules.length;
    }

    /** Generate a unified interop execution plan. */
    generatePlan(): InteropPlan {
        // Build a simple topological-ish execution order from edges.
        const seen = new Set<string>();
        const order: string[] = [];
        for (const edge of this.edges) {
            if (!seen.has(edge.source)) { seen.add(edge.source); order.push(edge.source); }
            if (!seen.has(edge.target)) { seen.add(edge.target); order.push(edge.target); }
        }
        // Also include modules from rules that aren't in edges.
        for (const rule of this.rules) {
            for (const m of rule.modules) {
                if (!seen.has(m)) { seen.add(m); order.push(m); }
            }
        }

        return {
            _type: 'InteropPlan',
            edgeCount: this.edges.length,
            ruleCount: this.rules.length,
            edges: [...this.edges],
            rules: [...this.rules],
            executionOrder: order,
            meta: { stub: true },
        };
    }

    /** Clear all edges and rules. */
    clear(): void {
        this.edges = [];
        this.rules = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_interfabric') && process.argv.includes('--test')) {
    console.log('── grey_interfabric self-test ──');

    const fabric = new InteropFabric();

    // addEdge
    const e1 = fabric.addEdge('js', 'cpp', 'data');
    console.assert(e1.source === 'js', 'edge source');
    console.assert(e1.target === 'cpp', 'edge target');
    console.assert(e1.kind === 'data', 'edge kind');
    console.assert(fabric.edgeCount() === 1, 'edge count 1');
    console.log('  ✓ addEdge');

    // multiple edges
    fabric.addEdge('cpp', 'riscv', 'call');
    fabric.addEdge('js', 'riscv', 'orch');
    console.assert(fabric.edgeCount() === 3, 'edge count 3');
    console.log('  ✓ multiple edges');

    // getEdges (all)
    console.assert(fabric.getEdges().length === 3, 'getEdges all');
    console.log('  ✓ getEdges (all)');

    // getEdges (filtered)
    console.assert(fabric.getEdges('data').length === 1, 'getEdges data');
    console.assert(fabric.getEdges('call').length === 1, 'getEdges call');
    console.assert(fabric.getEdges('unknown').length === 0, 'getEdges unknown');
    console.log('  ✓ getEdges (filtered)');

    // applyRule
    const r1 = fabric.applyRule('orch', ['js', 'cpp', 'riscv'], 'round-robin');
    console.assert(r1.kind === 'orch', 'rule kind');
    console.assert(r1.modules.length === 3, 'rule modules');
    console.assert(r1.strategy === 'round-robin', 'rule strategy');
    console.assert(fabric.ruleCount() === 1, 'rule count 1');
    console.log('  ✓ applyRule');

    // multiple rules
    fabric.applyRule('data', ['js', 'cpp'], 'direct');
    console.assert(fabric.ruleCount() === 2, 'rule count 2');
    console.log('  ✓ multiple rules');

    // getRules (all)
    console.assert(fabric.getRules().length === 2, 'getRules all');
    console.log('  ✓ getRules (all)');

    // getRules (filtered)
    console.assert(fabric.getRules('orch').length === 1, 'getRules orch');
    console.assert(fabric.getRules('data').length === 1, 'getRules data');
    console.assert(fabric.getRules('unknown').length === 0, 'getRules unknown');
    console.log('  ✓ getRules (filtered)');

    // generatePlan
    const plan = fabric.generatePlan();
    console.assert(plan._type === 'InteropPlan', 'plan type');
    console.assert(plan.edgeCount === 3, 'plan edge count');
    console.assert(plan.ruleCount === 2, 'plan rule count');
    console.assert(plan.edges.length === 3, 'plan edges array');
    console.assert(plan.rules.length === 2, 'plan rules array');
    console.assert(plan.executionOrder.length === 3, `plan execution order has 3 nodes, got ${plan.executionOrder.length}`);
    console.assert(plan.executionOrder.includes('js'), 'plan includes js');
    console.assert(plan.executionOrder.includes('cpp'), 'plan includes cpp');
    console.assert(plan.executionOrder.includes('riscv'), 'plan includes riscv');
    console.assert(plan.meta.stub === true, 'plan meta stub');
    console.log('  ✓ generatePlan');

    // clear
    fabric.clear();
    console.assert(fabric.edgeCount() === 0, 'clear edges');
    console.assert(fabric.ruleCount() === 0, 'clear rules');
    console.log('  ✓ clear');

    // empty plan
    const emptyPlan = fabric.generatePlan();
    console.assert(emptyPlan.edgeCount === 0, 'empty plan edges');
    console.assert(emptyPlan.ruleCount === 0, 'empty plan rules');
    console.assert(emptyPlan.executionOrder.length === 0, 'empty plan order');
    console.log('  ✓ empty plan');

    console.log('── all grey_interfabric tests passed ──');
}
