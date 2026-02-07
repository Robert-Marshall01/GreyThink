// ─── Grey++ Pipeline Orchestration ───────────────────────────────────────────
// Central coordinator for pipeline definitions and execution.  Registers
// named pipelines, executes them step-by-step, and tracks intermediate
// results in memory.  All stubs — no external dependencies, no real AI,
// no real I/O.
//
// Extend by plugging real step executors in a future stage.

import type { PipelineStepKind, OrchestrationNode, ASTNode } from './grey_ast.js';
import type { PipelineStepResult, PipelineResult } from './grey_pipeline.js';
import { dispatchPipelineStep } from './grey_pipeline.js';

// ─── Types ──────────────────────────────────────────────────────────────────

export type PipelineState = 'registered' | 'running' | 'completed' | 'failed';

export interface PipelineStepDef {
    stepKind: PipelineStepKind;
    args: unknown[];
    flags: Record<string, unknown>;
    primitive?: string;
    op?: string;
}

export interface PipelineDefinition {
    name: string;
    steps: PipelineStepDef[];
    state: PipelineState;
    registeredAt: string;
}

export interface PipelineExecution {
    id: string;
    pipelineName: string;
    state: PipelineState;
    stepResults: PipelineStepResult[];
    finalOutput: unknown;
    startedAt: string;
    completedAt: string | null;
}

// ═══════════════════════════════════════════════════════════════════════════
//  PipelineCoordinator
// ═══════════════════════════════════════════════════════════════════════════

export class PipelineCoordinator {
    /** Registered pipeline definitions — keyed by name. */
    private definitions = new Map<string, PipelineDefinition>();

    /** Execution history — keyed by execution id. */
    private executions = new Map<string, PipelineExecution>();

    /** Auto-increment counter for execution ids. */
    private execSeq = 0;

    // ── Pipeline Registration ───────────────────────────────────────────

    /**
     * Register a named pipeline definition.
     * If a pipeline with the same name exists, it is replaced.
     */
    register(name: string, steps: PipelineStepDef[]): PipelineDefinition {
        const def: PipelineDefinition = {
            name,
            steps: [...steps],
            state: 'registered',
            registeredAt: new Date().toISOString(),
        };
        this.definitions.set(name, def);
        return def;
    }

    /**
     * Unregister a pipeline definition.
     * Returns `true` if removed, `false` if not found.
     */
    unregister(name: string): boolean {
        return this.definitions.delete(name);
    }

    /**
     * Check whether a pipeline is registered.
     */
    has(name: string): boolean {
        return this.definitions.has(name);
    }

    /**
     * Get a pipeline definition by name.
     */
    get(name: string): PipelineDefinition | null {
        return this.definitions.get(name) ?? null;
    }

    /**
     * List all registered pipeline names.
     */
    listPipelines(): string[] {
        return [...this.definitions.keys()];
    }

    // ── Pipeline Execution ──────────────────────────────────────────────

    /**
     * Execute a named pipeline step-by-step.
     * Each step's output feeds into the next step's input.
     * Returns a PipelineExecution descriptor with all intermediate results.
     */
    execute(name: string): PipelineExecution {
        const def = this.definitions.get(name);
        if (!def) {
            throw new Error(`PipelineCoordinator: no pipeline registered as "${name}".`);
        }

        const execId = `exec-${++this.execSeq}`;
        const execution: PipelineExecution = {
            id: execId,
            pipelineName: name,
            state: 'running',
            stepResults: [],
            finalOutput: null,
            startedAt: new Date().toISOString(),
            completedAt: null,
        };

        this.executions.set(execId, execution);
        def.state = 'running';

        let prevOutput: unknown = null;

        for (let i = 0; i < def.steps.length; i++) {
            const step = def.steps[i];
            try {
                const result = dispatchPipelineStep(
                    step.stepKind,
                    i,
                    step.args,
                    step.flags,
                    prevOutput,
                    step.primitive,
                    step.op,
                );
                execution.stepResults.push(result);
                prevOutput = result.output;
            } catch (err: any) {
                execution.state = 'failed';
                execution.completedAt = new Date().toISOString();
                def.state = 'failed';
                throw err;
            }
        }

        execution.finalOutput = prevOutput;
        execution.state = 'completed';
        execution.completedAt = new Date().toISOString();
        def.state = 'completed';

        return execution;
    }

    /**
     * Execute an inline (unnamed) pipeline from a list of step definitions.
     * Useful for one-off pipelines parsed directly from the REPL.
     */
    executeInline(steps: PipelineStepDef[], name?: string): PipelineExecution {
        const pipelineName = name ?? `inline-${Date.now().toString(36)}`;

        // Temporarily register, execute, then remove if it was truly inline.
        this.register(pipelineName, steps);
        const result = this.execute(pipelineName);
        if (!name) this.unregister(pipelineName);
        return result;
    }

    // ── Execution History ───────────────────────────────────────────────

    /**
     * Get a specific execution by id.
     */
    getExecution(execId: string): PipelineExecution | null {
        return this.executions.get(execId) ?? null;
    }

    /**
     * List all executions, optionally filtered by pipeline name.
     */
    listExecutions(pipelineName?: string): PipelineExecution[] {
        const all = [...this.executions.values()];
        if (pipelineName) return all.filter(e => e.pipelineName === pipelineName);
        return all;
    }

    /**
     * Get intermediate results for a specific execution.
     */
    getIntermediateResults(execId: string): PipelineStepResult[] {
        return this.executions.get(execId)?.stepResults ?? [];
    }

    /**
     * Clear execution history.
     */
    clearHistory(): void {
        this.executions.clear();
        this.execSeq = 0;
    }

    // ── Diagnostics ─────────────────────────────────────────────────────

    /**
     * Return a snapshot of all registered pipelines and execution history.
     */
    snapshot(): {
        pipelines: PipelineDefinition[];
        executions: PipelineExecution[];
    } {
        return {
            pipelines: [...this.definitions.values()],
            executions: [...this.executions.values()],
        };
    }
}

// ═══════════════════════════════════════════════════════════════════════════
//  Stage 14 — Orchestration Primitives
// ═══════════════════════════════════════════════════════════════════════════

// ── Result types ────────────────────────────────────────────────────────────

export interface OrchPipelineResult {
    kind: 'pipeline';
    steps: string[];
    status: 'orchestrated';
    message: string;
    report: OrchestrationReport;
    timestamp: string;
}

export interface OrchCrossResult {
    kind: 'cross';
    modules: string[];
    status: 'coordinated';
    message: string;
    report: OrchestrationReport;
    timestamp: string;
}

export interface OrchGlobalResult {
    kind: 'global';
    scenario: string;
    status: 'engaged';
    message: string;
    report: OrchestrationReport;
    timestamp: string;
}

export type OrchResult = OrchPipelineResult | OrchCrossResult | OrchGlobalResult;

export interface OrchestrationReport {
    _type: 'OrchestrationReport';
    primitive: string;
    inputSummary: string;
    outputSummary: string;
    meta: { stub: true };
}

// ── Dispatcher ──────────────────────────────────────────────────────────────

export function dispatchOrchestration(
    node: OrchestrationNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        fabricCoordinator?: { register: Function; getReport: Function };
        executionFabric?: { buildGraph: Function; schedule: Function; getPlan: Function };
    } = {},
): OrchResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }

    switch (node.primitive) {
        case 'pipeline': return orchPipeline(flags, ctx);
        case 'cross': return orchCross(flags, ctx);
        case 'global': return orchGlobal(flags, ctx);
        default:
            throw new Error(`[orch] Unknown primitive: ${node.primitive}`);
    }
}

// ── orch.pipeline ───────────────────────────────────────────────────────────

function orchPipeline(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): OrchPipelineResult {
    const stepsRaw = String(flags['steps'] ?? '');
    const steps = stepsRaw.split(',').map(s => s.trim()).filter(Boolean);

    if (ctx.fabricCoordinator) {
        ctx.fabricCoordinator.register(`orch-pipeline-${Date.now()}`, steps);
    }
    if (ctx.executionFabric) {
        ctx.executionFabric.buildGraph(steps);
    }

    const report: OrchestrationReport = {
        _type: 'OrchestrationReport',
        primitive: 'pipeline',
        inputSummary: `steps=[${steps.join(', ')}]`,
        outputSummary: `Pipeline orchestrated across ${steps.length} step(s)`,
        meta: { stub: true },
    };

    return {
        kind: 'pipeline',
        steps,
        status: 'orchestrated',
        message: `Pipeline orchestrated — ${steps.length} step(s): ${steps.join(' → ')}`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ── orch.cross ──────────────────────────────────────────────────────────────

function orchCross(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): OrchCrossResult {
    const modulesRaw = String(flags['modules'] ?? '');
    const modules = modulesRaw.split(',').map(s => s.trim()).filter(Boolean);

    if (ctx.fabricCoordinator) {
        ctx.fabricCoordinator.register(`orch-cross-${Date.now()}`, modules);
    }

    const report: OrchestrationReport = {
        _type: 'OrchestrationReport',
        primitive: 'cross',
        inputSummary: `modules=[${modules.join(', ')}]`,
        outputSummary: `Cross-module coordination achieved across ${modules.length} module(s)`,
        meta: { stub: true },
    };

    return {
        kind: 'cross',
        modules,
        status: 'coordinated',
        message: `Cross-module coordination achieved — ${modules.length} module(s): ${modules.join(', ')}`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ── orch.global ─────────────────────────────────────────────────────────────

function orchGlobal(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): OrchGlobalResult {
    const scenario = String(flags['scenario'] ?? 'default');

    if (ctx.fabricCoordinator) {
        ctx.fabricCoordinator.register(`orch-global-${Date.now()}`, [scenario]);
    }
    if (ctx.executionFabric) {
        ctx.executionFabric.buildGraph([scenario]);
        ctx.executionFabric.schedule();
    }

    const report: OrchestrationReport = {
        _type: 'OrchestrationReport',
        primitive: 'global',
        inputSummary: `scenario="${scenario}"`,
        outputSummary: `Global execution fabric engaged for "${scenario}"`,
        meta: { stub: true },
    };

    return {
        kind: 'global',
        scenario,
        status: 'engaged',
        message: `Global execution fabric engaged — scenario: "${scenario}"`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_orchestration') && process.argv.includes('--test')) {
    console.log('═══ grey_orchestration.ts Self-Test ═══\n');

    const coord = new PipelineCoordinator();

    // Register a pipeline.
    const def = coord.register('demo-pipeline', [
        { stepKind: 'infer', args: [], flags: { model: 'basic', dataset: 'demo' } },
        { stepKind: 'store', args: [], flags: { backend: 'pdf', file: 'demo.pdf' } },
        { stepKind: 'net', args: [], flags: { url: 'https://api.example.com' }, primitive: 'http', op: 'POST' },
    ]);
    console.log('Registered:', def.name, '—', def.steps.length, 'steps');
    console.log('Pipelines:', coord.listPipelines());

    // Execute.
    console.log('\n── execute demo-pipeline ──');
    const exec = coord.execute('demo-pipeline');
    console.log('Execution id:', exec.id);
    console.log('State:', exec.state);
    console.log('Steps completed:', exec.stepResults.length);
    console.log('Final output:', JSON.stringify(exec.finalOutput, null, 2));

    // Intermediate results.
    console.log('\n── intermediate results ──');
    const intermediates = coord.getIntermediateResults(exec.id);
    for (const r of intermediates) {
        console.log(`  Step ${r.stepIndex} (${r.stepKind}): ${(r.output as any)?.status ?? (r.output as any)?.prediction ?? 'ok'}`);
    }

    // Execute inline.
    console.log('\n── execute inline pipeline ──');
    const inline = coord.executeInline([
        { stepKind: 'infer', args: [], flags: { model: 'gpt4', prompt: 'hello' } },
        { stepKind: 'store', args: [], flags: { backend: 'file', file: 'output.json' } },
    ]);
    console.log('Inline execution:', inline.id, '— state:', inline.state);

    // Snapshot.
    console.log('\n── snapshot ──');
    const snap = coord.snapshot();
    console.log(`Pipelines: ${snap.pipelines.length}, Executions: ${snap.executions.length}`);

    // History.
    console.log('All executions:', coord.listExecutions().map(e => e.id));

    console.log('\n✓ All grey_orchestration tests passed.');
}

// ─── Stage 14 Orchestration Primitives Self-test ────────────────────────────

if (process.argv[1]?.includes('grey_orchestration') && process.argv.includes('--test-orch')) {
    console.log('── grey_orchestration primitives self-test ──');

    const fakeNode = (primitive: string, flags: Record<string, string>): OrchestrationNode => ({
        kind: 'Orchestration' as any,
        primitive,
        args: [],
        flags: Object.fromEntries(
            Object.entries(flags).map(([k, v]) => [k, { kind: 'StringLit' as any, value: v } as any])
        ),
    });

    const resolve = (n: ASTNode) => (n as any).value;

    // orch.pipeline
    const p1 = dispatchOrchestration(fakeNode('pipeline', { steps: 'net.http, doc.edit, dist.consensus' }), resolve);
    console.assert(p1.kind === 'pipeline', 'pipeline kind');
    console.assert(p1.status === 'orchestrated', 'pipeline status');
    console.assert(p1.steps.length === 3, 'pipeline steps');
    console.assert(p1.message.includes('3 step(s)'), 'pipeline message');
    console.assert(p1.report._type === 'OrchestrationReport', 'pipeline report');
    console.log('  ✓ orch.pipeline');

    // orch.cross
    const c1 = dispatchOrchestration(fakeNode('cross', { modules: 'pipeline, security, artifact' }), resolve);
    console.assert(c1.kind === 'cross', 'cross kind');
    console.assert(c1.status === 'coordinated', 'cross status');
    console.assert(c1.modules.length === 3, 'cross modules');
    console.log('  ✓ orch.cross');

    // orch.global
    const g1 = dispatchOrchestration(fakeNode('global', { scenario: 'civilization-tier coordination' }), resolve);
    console.assert(g1.kind === 'global', 'global kind');
    console.assert(g1.status === 'engaged', 'global status');
    console.assert(g1.scenario === 'civilization-tier coordination', 'global scenario');
    console.log('  ✓ orch.global');

    // Unknown primitive
    try {
        dispatchOrchestration(fakeNode('unknown', {}), resolve);
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown error');
        console.log('  ✓ unknown primitive throws');
    }

    // With mock coordinators
    const regLog: any[] = [];
    const mockFabric = { register: (id: string, items: string[]) => regLog.push({ id, items }), getReport: () => ({}) };
    const graphLog: any[] = [];
    const mockExec = { buildGraph: (n: string[]) => graphLog.push(n), schedule: () => { }, getPlan: () => ({}) };

    dispatchOrchestration(fakeNode('pipeline', { steps: 'a, b' }), resolve, { fabricCoordinator: mockFabric, executionFabric: mockExec });
    console.assert(regLog.length === 1, 'fabric registered');
    console.assert(graphLog.length === 1, 'graph built');
    console.log('  ✓ with coordinators');

    console.log('── all grey_orchestration primitives tests passed ──');
}
