// ─── Grey++ Self-Optimization Primitives ─────────────────────────────────────
// Dispatcher for SelfOptNode: routes pipeline, trust, and artifact
// optimization requests through adaptive coordinators.  All stubs — no
// external dependencies, no real optimizers, no real AI.
//
// Self-test:  npx tsx src/ast/grey_selfopt.ts --test
//
// Extend by plugging real optimization backends in a future stage.

import type { SelfOptNode, ASTNode } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface SelfOptReport {
    _type: 'SelfOptReport';
    primitive: string;
    inputSummary: string;
    outputSummary: string;
    recommendations: string[];
    meta: { stub: true };
}

export interface SelfOptPipelineResult {
    kind: 'pipeline';
    target: string;
    strategy: string;
    status: 'optimized';
    message: string;
    report: SelfOptReport;
    timestamp: string;
}

export interface SelfOptTrustResult {
    kind: 'trust';
    target: string;
    threshold: number;
    status: 'tuned';
    message: string;
    report: SelfOptReport;
    timestamp: string;
}

export interface SelfOptArtifactResult {
    kind: 'artifact';
    target: string;
    mode: string;
    status: 'refined';
    message: string;
    report: SelfOptReport;
    timestamp: string;
}

export type SelfOptResult = SelfOptPipelineResult | SelfOptTrustResult | SelfOptArtifactResult;

// ─── Dispatcher ─────────────────────────────────────────────────────────────

export function dispatchSelfOpt(
    node: SelfOptNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        adaptCoordinator?: { collect: Function; getReport: Function };
        adaptiveFabric?: { buildAdaptiveGraph: Function; getOptimizedPlan: Function };
    } = {},
): SelfOptResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }

    switch (node.primitive) {
        case 'pipeline': return selfOptPipeline(flags, ctx);
        case 'trust': return selfOptTrust(flags, ctx);
        case 'artifact': return selfOptArtifact(flags, ctx);
        default:
            throw new Error(`[selfopt] Unknown primitive: ${node.primitive}`);
    }
}

// ─── selfopt.pipeline ───────────────────────────────────────────────────────

function selfOptPipeline(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): SelfOptPipelineResult {
    const target = String(flags['target'] ?? 'default');
    const strategy = String(flags['strategy'] ?? 'latency');

    const recommendations = [
        `Reorder steps in "${target}" for ${strategy} optimization`,
        `Cache intermediate results for pipeline "${target}"`,
        `Consider parallel execution where dependencies allow`,
    ];

    if (ctx.adaptCoordinator) {
        ctx.adaptCoordinator.collect('selfopt', 'pipeline', { target, strategy, recommendations });
    }
    if (ctx.adaptiveFabric) {
        ctx.adaptiveFabric.buildAdaptiveGraph([target], { strategy });
    }

    const report: SelfOptReport = {
        _type: 'SelfOptReport',
        primitive: 'pipeline',
        inputSummary: `target="${target}", strategy="${strategy}"`,
        outputSummary: `Pipeline "${target}" optimized for ${strategy} — ${recommendations.length} recommendation(s)`,
        recommendations,
        meta: { stub: true },
    };

    return {
        kind: 'pipeline',
        target,
        strategy,
        status: 'optimized',
        message: `Pipeline "${target}" optimized for ${strategy} — ${recommendations.length} recommendation(s)`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── selfopt.trust ──────────────────────────────────────────────────────────

function selfOptTrust(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): SelfOptTrustResult {
    const target = String(flags['target'] ?? 'default');
    const threshold = Number(flags['threshold'] ?? 0.8);

    const recommendations = [
        `Adjust trust threshold for "${target}" to ${threshold}`,
        `Review audit trail for low-confidence decisions`,
        `Consider adaptive threshold based on historical accuracy`,
    ];

    if (ctx.adaptCoordinator) {
        ctx.adaptCoordinator.collect('selfopt', 'trust', { target, threshold, recommendations });
    }

    const report: SelfOptReport = {
        _type: 'SelfOptReport',
        primitive: 'trust',
        inputSummary: `target="${target}", threshold=${threshold}`,
        outputSummary: `Trust policy "${target}" tuned — threshold set to ${threshold}`,
        recommendations,
        meta: { stub: true },
    };

    return {
        kind: 'trust',
        target,
        threshold,
        status: 'tuned',
        message: `Trust policy "${target}" tuned — threshold set to ${threshold}`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── selfopt.artifact ───────────────────────────────────────────────────────

function selfOptArtifact(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): SelfOptArtifactResult {
    const target = String(flags['target'] ?? 'default');
    const mode = String(flags['mode'] ?? 'balanced');

    const recommendations = [
        `Refine density calculation for "${target}" using ${mode} mode`,
        `Re-evaluate rarity bands after optimization`,
        `Archive low-density artifacts to reduce noise`,
    ];

    if (ctx.adaptCoordinator) {
        ctx.adaptCoordinator.collect('selfopt', 'artifact', { target, mode, recommendations });
    }
    if (ctx.adaptiveFabric) {
        ctx.adaptiveFabric.buildAdaptiveGraph([target], { mode });
    }

    const report: SelfOptReport = {
        _type: 'SelfOptReport',
        primitive: 'artifact',
        inputSummary: `target="${target}", mode="${mode}"`,
        outputSummary: `Artifact "${target}" refined in ${mode} mode — ${recommendations.length} recommendation(s)`,
        recommendations,
        meta: { stub: true },
    };

    return {
        kind: 'artifact',
        target,
        mode,
        status: 'refined',
        message: `Artifact "${target}" refined in ${mode} mode — ${recommendations.length} recommendation(s)`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_selfopt') && process.argv.includes('--test')) {
    console.log('── grey_selfopt self-test ──');

    const fakeNode = (primitive: string, flags: Record<string, string | number>): SelfOptNode => ({
        kind: 'SelfOpt' as any,
        primitive,
        args: [],
        flags: Object.fromEntries(
            Object.entries(flags).map(([k, v]) => [k, typeof v === 'number'
                ? { kind: 'NumberLit' as any, value: v } as any
                : { kind: 'StringLit' as any, value: v } as any
            ])
        ),
    });

    const resolve = (n: ASTNode) => (n as any).value;

    // selfopt.pipeline
    const p1 = dispatchSelfOpt(fakeNode('pipeline', { target: 'pipeline1', strategy: 'latency' }), resolve);
    console.assert(p1.kind === 'pipeline', 'pipeline kind');
    console.assert(p1.status === 'optimized', 'pipeline status');
    console.assert(p1.target === 'pipeline1', 'pipeline target');
    console.assert(p1.strategy === 'latency', 'pipeline strategy');
    console.assert(p1.report._type === 'SelfOptReport', 'pipeline report type');
    console.assert(p1.report.recommendations.length === 3, 'pipeline recommendations');
    console.log('  ✓ selfopt.pipeline');

    // selfopt.trust
    const t1 = dispatchSelfOpt(fakeNode('trust', { target: 'sec.perm', threshold: 0.9 }), resolve);
    console.assert(t1.kind === 'trust', 'trust kind');
    console.assert(t1.status === 'tuned', 'trust status');
    console.assert(t1.target === 'sec.perm', 'trust target');
    console.assert(t1.threshold === 0.9, 'trust threshold');
    console.assert(t1.report.recommendations.length === 3, 'trust recommendations');
    console.log('  ✓ selfopt.trust');

    // selfopt.artifact
    const a1 = dispatchSelfOpt(fakeNode('artifact', { target: 'density', mode: 'aggressive' }), resolve);
    console.assert(a1.kind === 'artifact', 'artifact kind');
    console.assert(a1.status === 'refined', 'artifact status');
    console.assert(a1.target === 'density', 'artifact target');
    console.assert(a1.mode === 'aggressive', 'artifact mode');
    console.assert(a1.report.recommendations.length === 3, 'artifact recommendations');
    console.log('  ✓ selfopt.artifact');

    // Unknown primitive
    try {
        dispatchSelfOpt(fakeNode('unknown', {}), resolve);
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown error');
        console.log('  ✓ unknown primitive throws');
    }

    // With mock coordinators
    const collectLog: any[] = [];
    const graphLog: any[] = [];
    const mockAdapt = { collect: (domain: string, kind: string, data: any) => collectLog.push({ domain, kind, data }), getReport: () => ({}) };
    const mockFabric = { buildAdaptiveGraph: (nodes: string[], opts: any) => graphLog.push({ nodes, opts }), getOptimizedPlan: () => ({}) };

    dispatchSelfOpt(fakeNode('pipeline', { target: 'p1', strategy: 'throughput' }), resolve, { adaptCoordinator: mockAdapt, adaptiveFabric: mockFabric });
    console.assert(collectLog.length === 1, 'adapt collected');
    console.assert(collectLog[0].domain === 'selfopt', 'adapt domain');
    console.assert(graphLog.length === 1, 'adaptive graph built');
    console.log('  ✓ with coordinators');

    // Default flags
    const d1 = dispatchSelfOpt(fakeNode('pipeline', {}), resolve);
    console.assert(d1.target === 'default', 'default target');
    console.assert(d1.strategy === 'latency', 'default strategy');
    console.log('  ✓ default flags');

    console.log('── all grey_selfopt tests passed ──');
}
