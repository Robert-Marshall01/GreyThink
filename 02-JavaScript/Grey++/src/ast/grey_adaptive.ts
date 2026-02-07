// ─── Grey++ Adaptive Fabric ──────────────────────────────────────────────────
// Builds adaptive execution graphs, applies stub optimization rules, and
// produces refined execution plans.  Pure in-memory — no external
// dependencies, no real AI, no real I/O.
//
// Self-test:  npx tsx src/ast/grey_adaptive.ts --test
//
// Extend by plugging real graph optimizers in a future stage.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface AdaptiveGraphNode {
    id: string;
    label: string;
    weight: number;
    dependencies: string[];
    optimized: boolean;
}

export interface AdaptiveGraph {
    id: string;
    nodes: AdaptiveGraphNode[];
    options: Record<string, unknown>;
    createdAt: string;
}

export interface AdaptiveRule {
    id: string;
    name: string;
    condition: string;
    action: string;
    priority: number;
}

export interface OptimizedPlan {
    _type: 'OptimizedPlan';
    graphId: string;
    phases: OptimizedPhase[];
    criticalPath: string[];
    totalWeight: number;
    estimatedGain: string;
    timestamp: string;
}

export interface OptimizedPhase {
    phase: number;
    nodes: string[];
    parallelizable: boolean;
}

// ═══════════════════════════════════════════════════════════════════════════
//  AdaptiveFabric
// ═══════════════════════════════════════════════════════════════════════════

export class AdaptiveFabric {
    private graphs: AdaptiveGraph[] = [];
    private rules: AdaptiveRule[] = [];
    private graphSeq = 0;
    private ruleSeq = 0;

    // ── Graph Building ──────────────────────────────────────────────────

    /**
     * Build an adaptive execution graph from a list of node labels.
     * Each node is given a sequential id and a default weight of 1.
     * Dependencies are inferred linearly (each node depends on the previous).
     */
    buildAdaptiveGraph(labels: string[], options: Record<string, unknown> = {}): AdaptiveGraph {
        const graphId = `adaptive-graph-${++this.graphSeq}`;

        const nodes: AdaptiveGraphNode[] = labels.map((label, i) => ({
            id: `${graphId}-node-${i}`,
            label,
            weight: 1,
            dependencies: i > 0 ? [`${graphId}-node-${i - 1}`] : [],
            optimized: false,
        }));

        const graph: AdaptiveGraph = {
            id: graphId,
            nodes,
            options,
            createdAt: new Date().toISOString(),
        };

        this.graphs.push(graph);
        return graph;
    }

    /**
     * Get a graph by id.
     */
    getGraph(id: string): AdaptiveGraph | null {
        return this.graphs.find(g => g.id === id) ?? null;
    }

    /**
     * List all graph ids.
     */
    listGraphs(): string[] {
        return this.graphs.map(g => g.id);
    }

    // ── Rule Management ─────────────────────────────────────────────────

    /**
     * Register an optimization rule.
     */
    applyRule(name: string, condition: string, action: string, priority: number = 0): AdaptiveRule {
        const rule: AdaptiveRule = {
            id: `adaptive-rule-${++this.ruleSeq}`,
            name,
            condition,
            action,
            priority,
        };
        this.rules.push(rule);
        // Sort by priority descending.
        this.rules.sort((a, b) => b.priority - a.priority);
        return rule;
    }

    /**
     * Get all registered rules.
     */
    getRules(): AdaptiveRule[] {
        return [...this.rules];
    }

    /**
     * Remove a rule by id.
     */
    removeRule(id: string): boolean {
        const idx = this.rules.findIndex(r => r.id === id);
        if (idx < 0) return false;
        this.rules.splice(idx, 1);
        return true;
    }

    // ── Optimization ────────────────────────────────────────────────────

    /**
     * Generate an optimized execution plan for a specific graph.
     * Applies registered rules (stub) and computes phases.
     */
    getOptimizedPlan(graphId: string): OptimizedPlan {
        const graph = this.getGraph(graphId);
        if (!graph) {
            throw new Error(`AdaptiveFabric: no graph with id "${graphId}"`);
        }

        // Apply rules to mark nodes as optimized (stub).
        for (const node of graph.nodes) {
            for (const rule of this.rules) {
                if (node.label.includes(rule.condition) || rule.condition === '*') {
                    node.optimized = true;
                    break;
                }
            }
        }

        // Compute phases via topological ordering.
        const phases = this.computePhases(graph);

        // Critical path = longest chain of dependencies.
        const criticalPath = this.computeCriticalPath(graph);

        // Total weight.
        const totalWeight = graph.nodes.reduce((sum, n) => sum + n.weight, 0);

        // Estimated gain (stub).
        const optimizedCount = graph.nodes.filter(n => n.optimized).length;
        const estimatedGain = optimizedCount > 0
            ? `~${Math.round((optimizedCount / graph.nodes.length) * 30)}% improvement from ${optimizedCount} optimized node(s)`
            : 'No optimizations applied';

        return {
            _type: 'OptimizedPlan',
            graphId,
            phases,
            criticalPath,
            totalWeight,
            estimatedGain,
            timestamp: new Date().toISOString(),
        };
    }

    /**
     * Compute execution phases (topological ordering grouped by depth).
     */
    private computePhases(graph: AdaptiveGraph): OptimizedPhase[] {
        const nodeMap = new Map(graph.nodes.map(n => [n.id, n]));
        const depth = new Map<string, number>();

        function getDepth(id: string): number {
            if (depth.has(id)) return depth.get(id)!;
            const node = nodeMap.get(id);
            if (!node || node.dependencies.length === 0) {
                depth.set(id, 0);
                return 0;
            }
            const maxDep = Math.max(...node.dependencies.map(d => getDepth(d)));
            const d = maxDep + 1;
            depth.set(id, d);
            return d;
        }

        for (const node of graph.nodes) getDepth(node.id);

        // Group by depth.
        const phaseMap = new Map<number, string[]>();
        for (const [id, d] of depth) {
            if (!phaseMap.has(d)) phaseMap.set(d, []);
            phaseMap.get(d)!.push(nodeMap.get(id)!.label);
        }

        const phases: OptimizedPhase[] = [];
        const sortedDepths = [...phaseMap.keys()].sort((a, b) => a - b);
        for (const d of sortedDepths) {
            const nodes = phaseMap.get(d)!;
            phases.push({
                phase: d,
                nodes,
                parallelizable: nodes.length > 1,
            });
        }

        return phases;
    }

    /**
     * Compute the critical path (longest dependency chain by label).
     */
    private computeCriticalPath(graph: AdaptiveGraph): string[] {
        if (graph.nodes.length === 0) return [];

        const nodeMap = new Map(graph.nodes.map(n => [n.id, n]));
        let longestPath: string[] = [];

        function dfs(id: string, path: string[]): void {
            const node = nodeMap.get(id);
            if (!node) return;
            const current = [...path, node.label];
            if (current.length > longestPath.length) longestPath = current;
            // Find nodes that depend on this one.
            for (const n of graph.nodes) {
                if (n.dependencies.includes(id)) {
                    dfs(n.id, current);
                }
            }
        }

        // Start from root nodes (no dependencies).
        for (const node of graph.nodes) {
            if (node.dependencies.length === 0) {
                dfs(node.id, []);
            }
        }

        return longestPath;
    }

    // ── History ─────────────────────────────────────────────────────────

    /**
     * Get all graphs and rules.
     */
    getHistory(): { graphs: AdaptiveGraph[]; rules: AdaptiveRule[] } {
        return {
            graphs: [...this.graphs],
            rules: [...this.rules],
        };
    }

    // ── Clear ───────────────────────────────────────────────────────────

    /**
     * Clear all graphs and rules.
     */
    clear(): void {
        this.graphs = [];
        this.rules = [];
        this.graphSeq = 0;
        this.ruleSeq = 0;
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_adaptive') && process.argv.includes('--test')) {
    console.log('── grey_adaptive self-test ──');

    const fabric = new AdaptiveFabric();

    // Build a graph
    const g1 = fabric.buildAdaptiveGraph(['fetch', 'transform', 'store'], { strategy: 'latency' });
    console.assert(g1.id === 'adaptive-graph-1', `graph id: ${g1.id}`);
    console.assert(g1.nodes.length === 3, 'graph nodes');
    console.assert(g1.nodes[0].dependencies.length === 0, 'root has no deps');
    console.assert(g1.nodes[1].dependencies.length === 1, 'second depends on first');
    console.assert(g1.nodes[2].dependencies.length === 1, 'third depends on second');
    console.assert(g1.options.strategy === 'latency', 'graph options');
    console.log('  ✓ build graph');

    // Get graph
    console.assert(fabric.getGraph('adaptive-graph-1') !== null, 'get by id');
    console.assert(fabric.getGraph('nonexistent') === null, 'get missing');
    console.assert(fabric.listGraphs().length === 1, 'list graphs');
    console.log('  ✓ get graph');

    // Apply rules
    const r1 = fabric.applyRule('cache-transform', 'transform', 'cache', 10);
    console.assert(r1.id === 'adaptive-rule-1', 'rule id');
    console.assert(r1.priority === 10, 'rule priority');
    console.log('  ✓ apply rule');

    const r2 = fabric.applyRule('parallelize-all', '*', 'parallelize', 5);
    console.assert(fabric.getRules().length === 2, 'two rules');
    console.assert(fabric.getRules()[0].priority === 10, 'sorted by priority');
    console.log('  ✓ rule ordering');

    // Remove rule
    console.assert(fabric.removeRule('adaptive-rule-2'), 'remove existing');
    console.assert(!fabric.removeRule('nonexistent'), 'remove missing');
    console.assert(fabric.getRules().length === 1, 'one rule left');
    console.log('  ✓ remove rule');

    // Optimized plan
    const plan = fabric.getOptimizedPlan('adaptive-graph-1');
    console.assert(plan._type === 'OptimizedPlan', 'plan type');
    console.assert(plan.graphId === 'adaptive-graph-1', 'plan graph id');
    console.assert(plan.phases.length === 3, `plan phases: ${plan.phases.length}`);
    console.assert(plan.phases[0].nodes[0] === 'fetch', 'phase 0 node');
    console.assert(plan.phases[0].parallelizable === false, 'phase 0 not parallelizable');
    console.assert(plan.criticalPath.length === 3, 'critical path length');
    console.assert(plan.criticalPath[0] === 'fetch', 'critical path start');
    console.assert(plan.criticalPath[2] === 'store', 'critical path end');
    console.assert(plan.totalWeight === 3, 'total weight');
    console.assert(plan.estimatedGain.includes('1 optimized'), `gain: ${plan.estimatedGain}`);
    console.log('  ✓ optimized plan');

    // Plan for missing graph throws
    try {
        fabric.getOptimizedPlan('nonexistent');
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('no graph'), 'missing graph error');
        console.log('  ✓ plan for missing graph throws');
    }

    // Multiple graphs
    const g2 = fabric.buildAdaptiveGraph(['a', 'b'], { mode: 'aggressive' });
    console.assert(g2.id === 'adaptive-graph-2', 'second graph id');
    console.assert(fabric.listGraphs().length === 2, 'two graphs');
    console.log('  ✓ multiple graphs');

    // Parallelizable phase (graph with independent roots)
    const g3 = fabric.buildAdaptiveGraph(['x'], {});
    const plan3 = fabric.getOptimizedPlan(g3.id);
    console.assert(plan3.phases.length === 1, 'single phase');
    console.assert(plan3.criticalPath.length === 1, 'single critical path');
    console.log('  ✓ single-node graph');

    // Empty graph
    const g4 = fabric.buildAdaptiveGraph([], {});
    const plan4 = fabric.getOptimizedPlan(g4.id);
    console.assert(plan4.phases.length === 0, 'empty phases');
    console.assert(plan4.criticalPath.length === 0, 'empty critical path');
    console.assert(plan4.totalWeight === 0, 'empty weight');
    console.log('  ✓ empty graph');

    // History
    const history = fabric.getHistory();
    console.assert(history.graphs.length === 4, 'history graphs');
    console.assert(history.rules.length === 1, 'history rules');
    console.log('  ✓ history');

    // Clear
    fabric.clear();
    console.assert(fabric.listGraphs().length === 0, 'cleared graphs');
    console.assert(fabric.getRules().length === 0, 'cleared rules');
    console.log('  ✓ clear');

    // Post-clear IDs reset
    const g5 = fabric.buildAdaptiveGraph(['test'], {});
    console.assert(g5.id === 'adaptive-graph-1', 'id reset after clear');
    console.log('  ✓ id reset after clear');

    console.log('── all grey_adaptive tests passed ──');
}
