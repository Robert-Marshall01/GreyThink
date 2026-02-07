// ─── Grey++ Meta-Language Unification Primitives ─────────────────────────────
// Dispatcher for UnifyNode: routes syntax, semantic, and exec unification
// requests through the unification layer.  All stubs — no real compiler
// infrastructure, no external dependencies.
//
// Self-test:  npx tsx src/ast/grey_unify.ts --test
//
// Extend by plugging real syntax/semantic analysis backends in a future stage.

import type { UnifyNode, ASTNode } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface UnifyReport {
    _type: 'UnifyReport';
    primitive: string;
    inputSummary: string;
    outputSummary: string;
    unifiedConstructs: string[];
    meta: { stub: true };
}

export interface UnifySyntaxResult {
    kind: 'syntax';
    modules: string[];
    status: 'unified';
    message: string;
    unifiedGrammar: string;
    report: UnifyReport;
    timestamp: string;
}

export interface UnifySemanticResult {
    kind: 'semantic';
    source: string;
    status: 'unified';
    message: string;
    semanticMap: string;
    report: UnifyReport;
    timestamp: string;
}

export interface UnifyExecResult {
    kind: 'exec';
    scenario: string;
    status: 'unified';
    message: string;
    executionPlan: string;
    report: UnifyReport;
    timestamp: string;
}

export type UnifyResult = UnifySyntaxResult | UnifySemanticResult | UnifyExecResult;

// ─── Dispatcher ─────────────────────────────────────────────────────────────

export function dispatchUnify(
    node: UnifyNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        unifyCoordinator?: { mapPrimitive: Function; translate: Function; generateReport?: Function };
        unifiedFabric?: { addConstruct: Function; applyRule: Function; toExecutionPlan?: Function };
    } = {},
): UnifyResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }

    switch (node.primitive) {
        case 'syntax': return unifySyntax(flags, ctx);
        case 'semantic': return unifySemantic(flags, ctx);
        case 'exec': return unifyExec(flags, ctx);
        default:
            throw new Error(`[unify] Unknown primitive: ${node.primitive}`);
    }
}

// ─── unify.syntax ───────────────────────────────────────────────────────────

function unifySyntax(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): UnifySyntaxResult {
    const modulesRaw = String(flags['modules'] ?? 'default');
    const modules = modulesRaw.split(',').map(m => m.trim()).filter(Boolean);

    const unifiedGrammar = `Syntax unified across [${modules.join(', ')}] — all constructs mapped to universal Grey++ grammar.`;

    const unifiedConstructs = modules.map(m => `${m} → unify.syntax`);

    if (ctx.unifyCoordinator?.mapPrimitive) {
        for (const mod of modules) {
            ctx.unifyCoordinator.mapPrimitive(mod, 'syntax');
        }
    }
    if (ctx.unifiedFabric?.addConstruct) {
        for (const mod of modules) {
            ctx.unifiedFabric.addConstruct(mod, 'syntax');
        }
    }

    const report: UnifyReport = {
        _type: 'UnifyReport',
        primitive: 'syntax',
        inputSummary: `modules="${modulesRaw}"`,
        outputSummary: unifiedGrammar,
        unifiedConstructs,
        meta: { stub: true },
    };

    return {
        kind: 'syntax',
        modules,
        status: 'unified',
        message: `Syntax unified across ${modules.length} module(s): ${modules.join(', ')}`,
        unifiedGrammar,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── unify.semantic ─────────────────────────────────────────────────────────

function unifySemantic(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): UnifySemanticResult {
    const source = String(flags['source'] ?? 'default');

    const semanticMap = `Semantic unification of "${source}" — all meaning preserved, cross-primitive references resolved.`;

    const unifiedConstructs = [
        `${source} → semantic-node`,
        `${source}.types → unified-type-map`,
        `${source}.deps → dependency-graph`,
    ];

    if (ctx.unifyCoordinator?.translate) {
        ctx.unifyCoordinator.translate(source, 'semantic');
    }
    if (ctx.unifiedFabric?.addConstruct) {
        ctx.unifiedFabric.addConstruct(source, 'semantic');
    }

    const report: UnifyReport = {
        _type: 'UnifyReport',
        primitive: 'semantic',
        inputSummary: `source="${source}"`,
        outputSummary: semanticMap,
        unifiedConstructs,
        meta: { stub: true },
    };

    return {
        kind: 'semantic',
        source,
        status: 'unified',
        message: `Semantic unification complete for "${source}"`,
        semanticMap,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── unify.exec ─────────────────────────────────────────────────────────────

function unifyExec(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): UnifyExecResult {
    const scenario = String(flags['scenario'] ?? 'default');

    const executionPlan = `Execution unified for "${scenario}" — all primitives composed into single meta-language execution graph.`;

    const unifiedConstructs = [
        `${scenario} → exec-graph`,
        `pipeline + orchestration → unified-exec`,
        `security + trust → unified-policy`,
        `memory + narrative → unified-context`,
    ];

    if (ctx.unifyCoordinator?.mapPrimitive) {
        ctx.unifyCoordinator.mapPrimitive(scenario, 'exec');
    }
    if (ctx.unifiedFabric?.applyRule) {
        ctx.unifiedFabric.applyRule({
            action: 'merge',
            sources: ['pipeline', 'orchestration'],
            target: 'unify.exec',
        });
    }
    if (ctx.unifiedFabric?.toExecutionPlan) {
        ctx.unifiedFabric.toExecutionPlan(scenario);
    }

    const report: UnifyReport = {
        _type: 'UnifyReport',
        primitive: 'exec',
        inputSummary: `scenario="${scenario}"`,
        outputSummary: executionPlan,
        unifiedConstructs,
        meta: { stub: true },
    };

    return {
        kind: 'exec',
        scenario,
        status: 'unified',
        message: `Execution unification complete for "${scenario}"`,
        executionPlan,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_unify') && !process.argv[1]?.includes('grey_unifycoord') && process.argv.includes('--test')) {
    console.log('── grey_unify self-test ──');

    const fakeNode = (primitive: string, flags: Record<string, string | number>): UnifyNode => ({
        kind: 'Unify' as any,
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

    // unify.syntax
    const s1 = dispatchUnify(fakeNode('syntax', { modules: 'pipeline, orchestration, memory' }), resolve);
    console.assert(s1.kind === 'syntax', 'syntax kind');
    console.assert(s1.status === 'unified', 'syntax status');
    console.assert(s1.modules.length === 3, `syntax modules count: ${s1.modules.length}`);
    console.assert(s1.modules[0] === 'pipeline', 'first module');
    console.assert(s1.unifiedGrammar.includes('pipeline'), 'grammar content');
    console.assert(s1.report._type === 'UnifyReport', 'syntax report type');
    console.assert(s1.report.unifiedConstructs.length === 3, 'syntax constructs');
    console.log('  ✓ unify.syntax');

    // unify.semantic
    const m1 = dispatchUnify(fakeNode('semantic', { source: 'artifact.rarity' }), resolve);
    console.assert(m1.kind === 'semantic', 'semantic kind');
    console.assert(m1.status === 'unified', 'semantic status');
    console.assert(m1.source === 'artifact.rarity', 'semantic source');
    console.assert(m1.semanticMap.includes('artifact.rarity'), 'semantic map content');
    console.assert(m1.report.unifiedConstructs.length === 3, 'semantic constructs');
    console.log('  ✓ unify.semantic');

    // unify.exec
    const e1 = dispatchUnify(fakeNode('exec', { scenario: 'civilization-tier orchestration' }), resolve);
    console.assert(e1.kind === 'exec', 'exec kind');
    console.assert(e1.status === 'unified', 'exec status');
    console.assert(e1.scenario === 'civilization-tier orchestration', 'exec scenario');
    console.assert(e1.executionPlan.includes('civilization-tier'), 'exec plan content');
    console.assert(e1.report.unifiedConstructs.length === 4, 'exec constructs');
    console.log('  ✓ unify.exec');

    // Unknown primitive
    try {
        dispatchUnify(fakeNode('unknown', {}), resolve);
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown error');
        console.log('  ✓ unknown primitive throws');
    }

    // With mock coordinators
    const mapLog: any[] = [];
    const translateLog: any[] = [];
    const constructLog: any[] = [];
    const ruleLog: any[] = [];
    const planLog: any[] = [];
    const mockCoord = {
        mapPrimitive: (name: string, type: string) => mapLog.push({ name, type }),
        translate: (source: string, type: string) => translateLog.push({ source, type }),
    };
    const mockFabric = {
        addConstruct: (name: string, type: string) => constructLog.push({ name, type }),
        applyRule: (rule: any) => ruleLog.push(rule),
        toExecutionPlan: (scenario: string) => planLog.push(scenario),
    };

    // syntax with coordinators
    dispatchUnify(fakeNode('syntax', { modules: 'net, sec' }), resolve, { unifyCoordinator: mockCoord, unifiedFabric: mockFabric });
    console.assert(mapLog.length === 2, `syntax mapped ${mapLog.length} primitives`);
    console.assert(constructLog.length === 2, `syntax added ${constructLog.length} constructs`);
    console.log('  ✓ syntax with coordinators');

    // semantic with coordinators
    dispatchUnify(fakeNode('semantic', { source: 'doc.edit' }), resolve, { unifyCoordinator: mockCoord, unifiedFabric: mockFabric });
    console.assert(translateLog.length === 1, 'semantic translated');
    console.assert(constructLog.length === 3, 'semantic construct added');
    console.log('  ✓ semantic with coordinators');

    // exec with coordinators
    dispatchUnify(fakeNode('exec', { scenario: 'global' }), resolve, { unifyCoordinator: mockCoord, unifiedFabric: mockFabric });
    console.assert(ruleLog.length === 1, 'exec rule applied');
    console.assert(planLog.length === 1, 'exec plan generated');
    console.log('  ✓ exec with coordinators');

    // Default flags
    const d1 = dispatchUnify(fakeNode('syntax', {}), resolve);
    console.assert(d1.modules.length === 1 && d1.modules[0] === 'default', 'default modules');
    const d2 = dispatchUnify(fakeNode('semantic', {}), resolve);
    console.assert(d2.source === 'default', 'default source');
    const d3 = dispatchUnify(fakeNode('exec', {}), resolve);
    console.assert(d3.scenario === 'default', 'default scenario');
    console.log('  ✓ default flags');

    console.log('── all grey_unify tests passed ──');
}
