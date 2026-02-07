// ─────────────────────────────────────────────────────────────────────────────
//  grey_fabric.ts  –  Stage 14 · Fabric Coordinator
// ─────────────────────────────────────────────────────────────────────────────
//  Registers orchestration pipelines spanning multiple modules, manages
//  execution order and dependencies, and outputs consolidated orchestration
//  reports.  No external dependencies — pure in-memory stub.
//
//  Self-test:  npx tsx src/ast/grey_fabric.ts --test
// ─────────────────────────────────────────────────────────────────────────────

// ── Types ───────────────────────────────────────────────────────────────────

export interface FabricPipeline {
    id: string;
    steps: string[];
    dependencies: string[];
    status: 'registered' | 'running' | 'completed' | 'failed';
    registeredAt: string;
    completedAt?: string;
}

export interface FabricDependency {
    from: string;  // pipeline id
    to: string;    // pipeline id that must complete first
}

export interface FabricReport {
    _type: 'FabricReport';
    totalPipelines: number;
    registered: number;
    running: number;
    completed: number;
    failed: number;
    dependencies: FabricDependency[];
    executionOrder: string[];
    timestamp: string;
    meta: { stub: true };
}

export interface FabricConfig {
    maxConcurrent?: number;
}

// ── FabricCoordinator ───────────────────────────────────────────────────────

export class FabricCoordinator {
    private pipelines: Map<string, FabricPipeline> = new Map();
    private dependencies: FabricDependency[] = [];
    private maxConcurrent: number;

    constructor(config?: FabricConfig) {
        this.maxConcurrent = config?.maxConcurrent ?? 4;
    }

    /** Register a new orchestration pipeline. */
    register(id: string, steps: string[], dependencies: string[] = []): FabricPipeline {
        const pipeline: FabricPipeline = {
            id,
            steps,
            dependencies,
            status: 'registered',
            registeredAt: new Date().toISOString(),
        };
        this.pipelines.set(id, pipeline);

        // Register dependency edges
        for (const dep of dependencies) {
            this.dependencies.push({ from: id, to: dep });
        }

        return pipeline;
    }

    /** Mark a pipeline as running. */
    start(id: string): void {
        const p = this.pipelines.get(id);
        if (p) p.status = 'running';
    }

    /** Mark a pipeline as completed. */
    complete(id: string): void {
        const p = this.pipelines.get(id);
        if (p) {
            p.status = 'completed';
            p.completedAt = new Date().toISOString();
        }
    }

    /** Mark a pipeline as failed. */
    fail(id: string): void {
        const p = this.pipelines.get(id);
        if (p) p.status = 'failed';
    }

    /** Get a pipeline by id. */
    get(id: string): FabricPipeline | undefined {
        return this.pipelines.get(id);
    }

    /** List all registered pipeline ids. */
    list(): string[] {
        return [...this.pipelines.keys()];
    }

    /** Compute execution order using topological sort (stub: simple dependency resolution). */
    computeExecutionOrder(): string[] {
        const visited = new Set<string>();
        const order: string[] = [];
        const depMap = new Map<string, string[]>();

        // Build dependency adjacency
        for (const dep of this.dependencies) {
            if (!depMap.has(dep.from)) depMap.set(dep.from, []);
            depMap.get(dep.from)!.push(dep.to);
        }

        const visit = (id: string): void => {
            if (visited.has(id)) return;
            visited.add(id);
            const deps = depMap.get(id) ?? [];
            for (const dep of deps) {
                visit(dep);
            }
            order.push(id);
        };

        for (const id of this.pipelines.keys()) {
            visit(id);
        }

        return order;
    }

    /** Generate a consolidated orchestration report. */
    getReport(): FabricReport {
        const all = [...this.pipelines.values()];
        return {
            _type: 'FabricReport',
            totalPipelines: all.length,
            registered: all.filter(p => p.status === 'registered').length,
            running: all.filter(p => p.status === 'running').length,
            completed: all.filter(p => p.status === 'completed').length,
            failed: all.filter(p => p.status === 'failed').length,
            dependencies: [...this.dependencies],
            executionOrder: this.computeExecutionOrder(),
            timestamp: new Date().toISOString(),
            meta: { stub: true },
        };
    }

    /** Clear all state. */
    clear(): void {
        this.pipelines.clear();
        this.dependencies = [];
    }
}

// ── Self-test ───────────────────────────────────────────────────────────────

if (process.argv.includes('--test')) {
    console.log('── grey_fabric self-test ──');

    const fc = new FabricCoordinator();

    // Register pipelines
    const p1 = fc.register('p1', ['net.http', 'doc.edit']);
    console.assert(p1.id === 'p1', 'register id');
    console.assert(p1.steps.length === 2, 'register steps');
    console.assert(p1.status === 'registered', 'register status');
    console.log('  ✓ register pipeline');

    const p2 = fc.register('p2', ['dist.consensus', 'sec.perm'], ['p1']);
    console.assert(p2.dependencies.includes('p1'), 'dependency registered');
    console.log('  ✓ register with dependency');

    const p3 = fc.register('p3', ['artifact.density'], ['p2']);
    console.log('  ✓ register chain');

    // List
    const ids = fc.list();
    console.assert(ids.length === 3, `list has 3 pipelines, got ${ids.length}`);
    console.assert(ids.includes('p1') && ids.includes('p2') && ids.includes('p3'), 'list contents');
    console.log('  ✓ list pipelines');

    // Get
    const got = fc.get('p2');
    console.assert(got?.id === 'p2', 'get by id');
    console.assert(got?.steps[0] === 'dist.consensus', 'get step content');
    console.log('  ✓ get pipeline');

    // Execution order (topological)
    const order = fc.computeExecutionOrder();
    console.assert(order.indexOf('p1') < order.indexOf('p2'), 'p1 before p2');
    console.assert(order.indexOf('p2') < order.indexOf('p3'), 'p2 before p3');
    console.log('  ✓ execution order (topological)');

    // Status transitions
    fc.start('p1');
    console.assert(fc.get('p1')?.status === 'running', 'start → running');
    console.log('  ✓ start pipeline');

    fc.complete('p1');
    console.assert(fc.get('p1')?.status === 'completed', 'complete → completed');
    console.assert(fc.get('p1')?.completedAt !== undefined, 'completedAt set');
    console.log('  ✓ complete pipeline');

    fc.fail('p2');
    console.assert(fc.get('p2')?.status === 'failed', 'fail → failed');
    console.log('  ✓ fail pipeline');

    // Report
    const report = fc.getReport();
    console.assert(report._type === 'FabricReport', 'report type');
    console.assert(report.totalPipelines === 3, 'report total');
    console.assert(report.completed === 1, 'report completed');
    console.assert(report.failed === 1, 'report failed');
    console.assert(report.registered === 1, 'report registered');
    console.assert(report.dependencies.length === 2, 'report dependencies');
    console.assert(report.executionOrder.length === 3, 'report execution order');
    console.log('  ✓ consolidated report');

    // Clear
    fc.clear();
    console.assert(fc.list().length === 0, 'clear empties pipelines');
    console.assert(fc.getReport().totalPipelines === 0, 'clear empties report');
    console.log('  ✓ clear');

    // No dependencies — independent pipelines
    fc.register('a', ['step1']);
    fc.register('b', ['step2']);
    const indOrder = fc.computeExecutionOrder();
    console.assert(indOrder.length === 2, 'independent pipelines ordered');
    console.log('  ✓ independent pipelines');

    console.log('── all grey_fabric tests passed ──');
}
