// ─── Grey++ Cross-Backend Interoperability Primitives ───────────────────────
// Dispatcher for InteropNode: routes data exchange, function call bridging,
// and orchestration unification across multi-backend execution targets.
// All stubs — no real cross-backend I/O, no external dependencies.
//
// Self-test:  npx tsx src/ast/grey_interop.ts --test
//
// Extend by plugging real interop bridges in a future stage.

import type { InteropNode, ASTNode } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface InteropReport {
    _type: 'InteropReport';
    primitive: string;
    inputSummary: string;
    outputSummary: string;
    bridgedArtifacts: string[];
    meta: { stub: true };
}

export interface InteropDataResult {
    kind: 'data';
    source: string;
    target: string;
    format: string;
    status: 'exchanged';
    message: string;
    output: string;
    report: InteropReport;
    timestamp: string;
}

export interface InteropCallResult {
    kind: 'call';
    source: string;
    target: string;
    method: string;
    status: 'bridged';
    message: string;
    output: string;
    report: InteropReport;
    timestamp: string;
}

export interface InteropOrchResult {
    kind: 'orch';
    modules: string[];
    strategy: string;
    status: 'unified';
    message: string;
    output: string;
    report: InteropReport;
    timestamp: string;
}

export type InteropResult = InteropDataResult | InteropCallResult | InteropOrchResult;

// ─── Dispatcher ─────────────────────────────────────────────────────────────

export function dispatchInterop(
    node: InteropNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        interopCoordinator?: { registerBridge: Function; addTranslation: Function; generateReport?: Function };
        interopFabric?: { addEdge: Function; applyRule: Function; generatePlan?: Function };
    } = {},
): InteropResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }

    switch (node.primitive) {
        case 'data': return interopData(flags, ctx);
        case 'call': return interopCall(flags, ctx);
        case 'orch': return interopOrch(flags, ctx);
        default:
            throw new Error(`[interop] Unknown primitive: ${node.primitive}`);
    }
}

// ─── interop.data ───────────────────────────────────────────────────────────

function interopData(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): InteropDataResult {
    const source = String(flags['source'] ?? 'execmulti.js');
    const target = String(flags['target'] ?? 'execmulti.cpp');
    const format = String(flags['format'] ?? 'json');

    const output = `Data exchanged: ${source} → ${target} (${format})`;

    const bridgedArtifacts = [
        `${source}.output → serialized`,
        `${format} → transport-encoding`,
        `${target}.input → deserialized`,
    ];

    if (ctx.interopCoordinator?.registerBridge) {
        ctx.interopCoordinator.registerBridge(source, target, 'data');
    }
    if (ctx.interopFabric?.addEdge) {
        ctx.interopFabric.addEdge(source, target, 'data');
    }

    const report: InteropReport = {
        _type: 'InteropReport',
        primitive: 'data',
        inputSummary: `source="${source}" target="${target}" format="${format}"`,
        outputSummary: output,
        bridgedArtifacts,
        meta: { stub: true },
    };

    return {
        kind: 'data',
        source,
        target,
        format,
        status: 'exchanged',
        message: `Data exchange complete: ${source} → ${target}`,
        output,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── interop.call ───────────────────────────────────────────────────────────

function interopCall(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): InteropCallResult {
    const source = String(flags['source'] ?? 'execmulti.js');
    const target = String(flags['target'] ?? 'execmulti.cpp');
    const method = String(flags['method'] ?? 'invoke');

    const output = `Function call bridged: ${source} → ${target}.${method}()`;

    const bridgedArtifacts = [
        `${source}.caller → marshalled`,
        `${target}.${method} → bound`,
        `cross-backend.bridge → established`,
    ];

    if (ctx.interopCoordinator?.registerBridge) {
        ctx.interopCoordinator.registerBridge(source, target, 'call');
    }
    if (ctx.interopCoordinator?.addTranslation) {
        ctx.interopCoordinator.addTranslation(source, target, method);
    }
    if (ctx.interopFabric?.addEdge) {
        ctx.interopFabric.addEdge(source, target, 'call');
    }

    const report: InteropReport = {
        _type: 'InteropReport',
        primitive: 'call',
        inputSummary: `source="${source}" target="${target}" method="${method}"`,
        outputSummary: output,
        bridgedArtifacts,
        meta: { stub: true },
    };

    return {
        kind: 'call',
        source,
        target,
        method,
        status: 'bridged',
        message: `Function call bridged: ${source} → ${target}.${method}()`,
        output,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── interop.orch ───────────────────────────────────────────────────────────

function interopOrch(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): InteropOrchResult {
    const modulesRaw = String(flags['modules'] ?? 'js, cpp, riscv');
    const modules = modulesRaw.split(',').map(m => m.trim()).filter(Boolean);
    const strategy = String(flags['strategy'] ?? 'round-robin');

    const output = `Orchestration unified across [${modules.join(', ')}] using ${strategy}`;

    const bridgedArtifacts = modules.map(m => `${m} → interop.orch.node`);
    bridgedArtifacts.push(`interop.orch → unified-execution-plan`);

    if (ctx.interopCoordinator?.registerBridge) {
        for (let i = 0; i < modules.length - 1; i++) {
            ctx.interopCoordinator.registerBridge(modules[i], modules[i + 1], 'orch');
        }
    }
    if (ctx.interopFabric?.applyRule) {
        ctx.interopFabric.applyRule('orch', modules, strategy);
    }

    const report: InteropReport = {
        _type: 'InteropReport',
        primitive: 'orch',
        inputSummary: `modules="${modulesRaw}" strategy="${strategy}"`,
        outputSummary: output,
        bridgedArtifacts,
        meta: { stub: true },
    };

    return {
        kind: 'orch',
        modules,
        strategy,
        status: 'unified',
        message: `Interop orchestration unified — ${modules.length} module(s): ${modules.join(', ')}`,
        output,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_interop') && !process.argv[1]?.includes('grey_interopcoord') && !process.argv[1]?.includes('grey_interfabric') && process.argv.includes('--test')) {
    console.log('── grey_interop self-test ──');

    const fakeNode = (primitive: string, flags: Record<string, string>): InteropNode => ({
        kind: 'Interop' as any,
        primitive,
        args: [],
        flags: Object.fromEntries(
            Object.entries(flags).map(([k, v]) => [k, { kind: 'StringLit' as any, value: v } as any])
        ),
    });

    const resolve = (n: ASTNode) => (n as any).value;

    // interop.data
    const d1 = dispatchInterop(fakeNode('data', { source: 'execmulti.js', target: 'execmulti.cpp' }), resolve);
    console.assert(d1.kind === 'data', 'data kind');
    console.assert(d1.status === 'exchanged', 'data status');
    console.assert(d1.source === 'execmulti.js', 'data source');
    console.assert(d1.target === 'execmulti.cpp', 'data target');
    console.assert(d1.output.includes('Data exchanged'), 'data output');
    console.assert(d1.report._type === 'InteropReport', 'data report type');
    console.assert(d1.report.bridgedArtifacts.length === 3, 'data artifacts');
    console.log('  ✓ interop.data');

    // interop.call
    const c1 = dispatchInterop(fakeNode('call', { source: 'execmulti.cpp', target: 'execmulti.riscv', method: 'compute' }), resolve);
    console.assert(c1.kind === 'call', 'call kind');
    console.assert(c1.status === 'bridged', 'call status');
    console.assert(c1.source === 'execmulti.cpp', 'call source');
    console.assert(c1.target === 'execmulti.riscv', 'call target');
    console.assert((c1 as InteropCallResult).method === 'compute', 'call method');
    console.assert(c1.output.includes('Function call bridged'), 'call output');
    console.assert(c1.report.bridgedArtifacts.length === 3, 'call artifacts');
    console.log('  ✓ interop.call');

    // interop.orch
    const o1 = dispatchInterop(fakeNode('orch', { modules: 'js, cpp, riscv' }), resolve);
    console.assert(o1.kind === 'orch', 'orch kind');
    console.assert(o1.status === 'unified', 'orch status');
    console.assert((o1 as InteropOrchResult).modules.length === 3, 'orch modules');
    console.assert(o1.output.includes('Orchestration unified'), 'orch output');
    console.assert(o1.report.bridgedArtifacts.length === 4, 'orch artifacts');
    console.log('  ✓ interop.orch');

    // Unknown primitive
    try {
        dispatchInterop(fakeNode('unknown', {}), resolve);
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown error');
        console.log('  ✓ unknown primitive throws');
    }

    // With mock coordinators
    const bridgeLog: any[] = [];
    const translationLog: any[] = [];
    const edgeLog: any[] = [];
    const ruleLog: any[] = [];
    const mockCoord = {
        registerBridge: (s: string, t: string, kind: string) => bridgeLog.push({ s, t, kind }),
        addTranslation: (s: string, t: string, m: string) => translationLog.push({ s, t, m }),
    };
    const mockFabric = {
        addEdge: (s: string, t: string, kind: string) => edgeLog.push({ s, t, kind }),
        applyRule: (kind: string, modules: string[], strategy: string) => ruleLog.push({ kind, modules, strategy }),
    };

    // data with coordinators
    dispatchInterop(fakeNode('data', { source: 'a', target: 'b' }), resolve, { interopCoordinator: mockCoord, interopFabric: mockFabric });
    console.assert(bridgeLog.length === 1, 'data bridge registered');
    console.assert(edgeLog.length === 1, 'data edge added');
    console.log('  ✓ data with coordinators');

    // call with coordinators
    dispatchInterop(fakeNode('call', { source: 'c', target: 'd', method: 'run' }), resolve, { interopCoordinator: mockCoord, interopFabric: mockFabric });
    console.assert(bridgeLog.length === 2, 'call bridge registered');
    console.assert(translationLog.length === 1, 'call translation added');
    console.assert(edgeLog.length === 2, 'call edge added');
    console.log('  ✓ call with coordinators');

    // orch with coordinators
    dispatchInterop(fakeNode('orch', { modules: 'a, b, c' }), resolve, { interopCoordinator: mockCoord, interopFabric: mockFabric });
    console.assert(bridgeLog.length === 4, 'orch bridges registered (2 edges for 3 modules)');
    console.assert(ruleLog.length === 1, 'orch rule applied');
    console.log('  ✓ orch with coordinators');

    // Default flags
    const dd = dispatchInterop(fakeNode('data', {}), resolve);
    console.assert((dd as InteropDataResult).source === 'execmulti.js', 'default data source');
    console.assert((dd as InteropDataResult).target === 'execmulti.cpp', 'default data target');
    const dc = dispatchInterop(fakeNode('call', {}), resolve);
    console.assert((dc as InteropCallResult).method === 'invoke', 'default call method');
    const dor = dispatchInterop(fakeNode('orch', {}), resolve);
    console.assert((dor as InteropOrchResult).modules.length === 3, 'default orch modules');
    console.log('  ✓ default flags');

    console.log('── all grey_interop tests passed ──');
}
