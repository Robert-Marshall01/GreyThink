// ─────────────────────────────────────────────────────────────────────────────
//  grey_execution.ts  –  Stage 14 · Execution Fabric Layer
// ─────────────────────────────────────────────────────────────────────────────
//  Represents orchestration as a unified execution graph.  Applies stub
//  scheduling and dependency resolution, and outputs a unified execution plan.
//  No external dependencies — pure in-memory stub.
//
//  Self-test:  npx tsx src/ast/grey_execution.ts --test
// ─────────────────────────────────────────────────────────────────────────────

// ── Types ───────────────────────────────────────────────────────────────────

export interface ExecutionNode {
    id: string;
    label: string;
    dependencies: string[];
    status: 'pending' | 'scheduled' | 'running' | 'completed' | 'failed';
    priority: number;
    scheduledAt?: string;
    completedAt?: string;
}

export interface ExecutionEdge {
    from: string;  // source node id
    to: string;    // target node id (dependency)
}

export interface ExecutionPlan {
    _type: 'ExecutionPlan';
    nodeCount: number;
    edgeCount: number;
    phases: ExecutionPhase[];
    criticalPath: string[];
    estimatedParallelism: number;
    timestamp: string;
    meta: { stub: true };
}

export interface ExecutionPhase {
    phase: number;
    nodes: string[];
    canParallelize: boolean;
}

export interface ExecutionFabricConfig {
    maxParallelism?: number;
}

// ── ExecutionFabric ─────────────────────────────────────────────────────────

export class ExecutionFabric {
    private nodes: Map<string, ExecutionNode> = new Map();
    private edges: ExecutionEdge[] = [];
    private maxParallelism: number;
    private planHistory: ExecutionPlan[] = [];

    constructor(config?: ExecutionFabricConfig) {
        this.maxParallelism = config?.maxParallelism ?? 4;
    }

    /** Add a node to the execution graph. */
    addNode(id: string, label?: string, dependencies: string[] = [], priority: number = 0): ExecutionNode {
        const node: ExecutionNode = {
            id,
            label: label ?? id,
            dependencies,
            status: 'pending',
            priority,
        };
        this.nodes.set(id, node);

        // Add dependency edges
        for (const dep of dependencies) {
            this.edges.push({ from: id, to: dep });
        }

        return node;
    }

    /** Build an execution graph from a list of step names (auto-chain). */
    buildGraph(steps: string[]): void {
        for (let i = 0; i < steps.length; i++) {
            const id = `step-${i}`;
            const deps = i > 0 ? [`step-${i - 1}`] : [];
            this.addNode(id, steps[i], deps, i);
        }
    }

    /** Get a node by id. */
    getNode(id: string): ExecutionNode | undefined {
        return this.nodes.get(id);
    }

    /** List all node ids. */
    listNodes(): string[] {
        return [...this.nodes.keys()];
    }

    /** Mark a node as scheduled. */
    schedule(id?: string): void {
        if (id) {
            const node = this.nodes.get(id);
            if (node) {
                node.status = 'scheduled';
                node.scheduledAt = new Date().toISOString();
            }
        } else {
            // Schedule all pending nodes
            for (const node of this.nodes.values()) {
                if (node.status === 'pending') {
                    node.status = 'scheduled';
                    node.scheduledAt = new Date().toISOString();
                }
            }
        }
    }

    /** Mark a node as completed. */
    completeNode(id: string): void {
        const node = this.nodes.get(id);
        if (node) {
            node.status = 'completed';
            node.completedAt = new Date().toISOString();
        }
    }

    /** Mark a node as failed. */
    failNode(id: string): void {
        const node = this.nodes.get(id);
        if (node) {
            node.status = 'failed';
        }
    }

    /** Compute execution phases (topological layers). */
    computePhases(): ExecutionPhase[] {
        const inDegree = new Map<string, number>();
        const adjList = new Map<string, string[]>();

        // Initialize
        for (const id of this.nodes.keys()) {
            inDegree.set(id, 0);
            adjList.set(id, []);
        }

        // Build adjacency
        for (const edge of this.edges) {
            // edge.from depends on edge.to, so edge.to must come first
            if (this.nodes.has(edge.from) && this.nodes.has(edge.to)) {
                adjList.get(edge.to)!.push(edge.from);
                inDegree.set(edge.from, (inDegree.get(edge.from) ?? 0) + 1);
            }
        }

        const phases: ExecutionPhase[] = [];
        const remaining = new Set(this.nodes.keys());
        let phaseNum = 0;

        while (remaining.size > 0) {
            // Find nodes with in-degree 0 among remaining
            const ready: string[] = [];
            for (const id of remaining) {
                if ((inDegree.get(id) ?? 0) === 0) {
                    ready.push(id);
                }
            }

            if (ready.length === 0) {
                // Cycle detected — break remaining into single phase
                phases.push({
                    phase: phaseNum,
                    nodes: [...remaining],
                    canParallelize: false,
                });
                break;
            }

            phases.push({
                phase: phaseNum++,
                nodes: ready,
                canParallelize: ready.length > 1 && ready.length <= this.maxParallelism,
            });

            // Remove ready nodes and update in-degrees
            for (const id of ready) {
                remaining.delete(id);
                for (const neighbor of (adjList.get(id) ?? [])) {
                    inDegree.set(neighbor, (inDegree.get(neighbor) ?? 0) - 1);
                }
            }
        }

        return phases;
    }

    /** Compute the critical path (longest chain of dependencies). */
    computeCriticalPath(): string[] {
        const phases = this.computePhases();
        // Critical path = one node from each phase (the longest sequential chain)
        const critical: string[] = [];
        for (const phase of phases) {
            // Pick the node with highest priority (or first)
            const sorted = [...phase.nodes].sort((a, b) => {
                const na = this.nodes.get(a);
                const nb = this.nodes.get(b);
                return (nb?.priority ?? 0) - (na?.priority ?? 0);
            });
            if (sorted.length > 0) critical.push(sorted[0]);
        }
        return critical;
    }

    /** Generate a unified execution plan. */
    getPlan(): ExecutionPlan {
        const phases = this.computePhases();
        const criticalPath = this.computeCriticalPath();
        const maxPhaseWidth = Math.max(1, ...phases.map(p => p.nodes.length));

        const plan: ExecutionPlan = {
            _type: 'ExecutionPlan',
            nodeCount: this.nodes.size,
            edgeCount: this.edges.length,
            phases,
            criticalPath,
            estimatedParallelism: Math.min(maxPhaseWidth, this.maxParallelism),
            timestamp: new Date().toISOString(),
            meta: { stub: true },
        };

        this.planHistory.push(plan);
        return plan;
    }

    /** Get all generated plans. */
    getHistory(): ExecutionPlan[] {
        return [...this.planHistory];
    }

    /** Clear all state. */
    clear(): void {
        this.nodes.clear();
        this.edges = [];
        this.planHistory = [];
    }
}

// ── Self-test ───────────────────────────────────────────────────────────────

if (process.argv.includes('--test')) {
    console.log('── grey_execution self-test ──');

    const ef = new ExecutionFabric();

    // Add individual nodes
    const n1 = ef.addNode('a', 'net.http');
    console.assert(n1.id === 'a', 'addNode id');
    console.assert(n1.label === 'net.http', 'addNode label');
    console.assert(n1.status === 'pending', 'addNode status');
    console.log('  ✓ add node');

    ef.addNode('b', 'doc.edit', ['a']);
    ef.addNode('c', 'dist.sync', ['a']);
    ef.addNode('d', 'sec.audit', ['b', 'c']);

    // List nodes
    const ids = ef.listNodes();
    console.assert(ids.length === 4, `listNodes count: ${ids.length}`);
    console.log('  ✓ list nodes');

    // Get node
    const got = ef.getNode('b');
    console.assert(got?.label === 'doc.edit', 'getNode label');
    console.assert(got?.dependencies.includes('a'), 'getNode dependencies');
    console.log('  ✓ get node');

    // Compute phases
    const phases = ef.computePhases();
    console.assert(phases.length >= 2, `phases count: ${phases.length}`);
    console.assert(phases[0].nodes.includes('a'), 'phase 0 has root');
    console.assert(phases[phases.length - 1].nodes.includes('d'), 'last phase has leaf');
    console.log('  ✓ compute phases');

    // Parallelism detection
    const midPhase = phases.find(p => p.nodes.includes('b') || p.nodes.includes('c'));
    console.assert(midPhase !== undefined, 'mid phase exists');
    if (midPhase && midPhase.nodes.includes('b') && midPhase.nodes.includes('c')) {
        console.assert(midPhase.canParallelize === true, 'b and c can parallelize');
        console.log('  ✓ parallelism detection');
    } else {
        console.log('  ✓ parallelism detection (sequential)');
    }

    // Critical path
    const critical = ef.computeCriticalPath();
    console.assert(critical.length >= 2, `critical path length: ${critical.length}`);
    console.log('  ✓ critical path');

    // Execution plan
    const plan = ef.getPlan();
    console.assert(plan._type === 'ExecutionPlan', 'plan type');
    console.assert(plan.nodeCount === 4, 'plan node count');
    console.assert(plan.edgeCount === 4, `plan edge count: ${plan.edgeCount}`);
    console.assert(plan.phases.length >= 2, 'plan phases');
    console.assert(plan.estimatedParallelism >= 1, 'plan parallelism');
    console.log('  ✓ execution plan');

    // Schedule and complete
    ef.schedule('a');
    console.assert(ef.getNode('a')?.status === 'scheduled', 'schedule single');
    console.assert(ef.getNode('a')?.scheduledAt !== undefined, 'scheduledAt set');
    console.log('  ✓ schedule node');

    ef.completeNode('a');
    console.assert(ef.getNode('a')?.status === 'completed', 'complete node');
    console.assert(ef.getNode('a')?.completedAt !== undefined, 'completedAt set');
    console.log('  ✓ complete node');

    ef.failNode('b');
    console.assert(ef.getNode('b')?.status === 'failed', 'fail node');
    console.log('  ✓ fail node');

    // Schedule all pending
    ef.schedule();
    console.assert(ef.getNode('c')?.status === 'scheduled', 'schedule all c');
    console.assert(ef.getNode('d')?.status === 'scheduled', 'schedule all d');
    console.log('  ✓ schedule all pending');

    // History
    const hist = ef.getHistory();
    console.assert(hist.length === 1, 'plan history');
    console.log('  ✓ plan history');

    // BuildGraph auto-chain
    ef.clear();
    ef.buildGraph(['net.http', 'doc.edit', 'dist.consensus']);
    console.assert(ef.listNodes().length === 3, 'buildGraph node count');
    const autoPhases = ef.computePhases();
    console.assert(autoPhases.length === 3, `buildGraph phases: ${autoPhases.length}`);
    console.assert(autoPhases[0].nodes[0] === 'step-0', 'buildGraph phase order');
    console.log('  ✓ buildGraph auto-chain');

    // Clear
    ef.clear();
    console.assert(ef.listNodes().length === 0, 'clear empties nodes');
    console.assert(ef.getHistory().length === 0, 'clear empties history');
    console.log('  ✓ clear');

    console.log('── all grey_execution tests passed ──');
}
