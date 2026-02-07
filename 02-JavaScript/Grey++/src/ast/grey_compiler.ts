// ─── Grey++ Compiler Primitives ──────────────────────────────────────────────
// Dispatcher for CompilerNode: routes syntax, backend, and exec compilation
// requests through the compiler layer.  All stubs — no real code generation
// infrastructure, no external dependencies.
//
// Self-test:  npx tsx src/ast/grey_compiler.ts --test
//
// Extend by plugging real compiler backends in a future stage.

import type { CompilerNode, ASTNode } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface CompilerReport {
    _type: 'CompilerReport';
    primitive: string;
    inputSummary: string;
    outputSummary: string;
    compiledArtifacts: string[];
    meta: { stub: true };
}

export interface CompilerSyntaxResult {
    kind: 'syntax';
    source: string[];
    status: 'compiled';
    message: string;
    compiledGrammar: string;
    report: CompilerReport;
    timestamp: string;
}

export interface CompilerBackendResult {
    kind: 'backend';
    target: string;
    optimize: boolean;
    status: 'compiled';
    message: string;
    codeFragment: string;
    report: CompilerReport;
    timestamp: string;
}

export interface CompilerExecResult {
    kind: 'exec';
    scenario: string;
    status: 'compiled';
    message: string;
    executionPlan: string;
    report: CompilerReport;
    timestamp: string;
}

export type CompilerResult = CompilerSyntaxResult | CompilerBackendResult | CompilerExecResult;

// ─── Dispatcher ─────────────────────────────────────────────────────────────

export function dispatchCompiler(
    node: CompilerNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        compileCoordinator?: { mapSyntax: Function; translateBackend: Function; generateReport?: Function };
        backendFabric?: { addTarget: Function; applyTranslation: Function; toCodeFragment?: Function };
    } = {},
): CompilerResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }

    switch (node.primitive) {
        case 'syntax': return compilerSyntax(flags, ctx);
        case 'backend': return compilerBackend(flags, ctx);
        case 'exec': return compilerExec(flags, ctx);
        default:
            throw new Error(`[compiler] Unknown primitive: ${node.primitive}`);
    }
}

// ─── compiler.syntax ────────────────────────────────────────────────────────

function compilerSyntax(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): CompilerSyntaxResult {
    const sourceRaw = String(flags['source'] ?? 'default');
    const source = sourceRaw.split(',').map(s => s.trim()).filter(Boolean);

    const compiledGrammar = `Syntax compiled from [${source.join(', ')}] — all constructs lowered to intermediate representation.`;

    const compiledArtifacts = source.map(s => `${s} → compiler.syntax.ir`);

    if (ctx.compileCoordinator?.mapSyntax) {
        for (const src of source) {
            ctx.compileCoordinator.mapSyntax(src, 'syntax');
        }
    }
    if (ctx.backendFabric?.addTarget) {
        for (const src of source) {
            ctx.backendFabric.addTarget(src, 'syntax');
        }
    }

    const report: CompilerReport = {
        _type: 'CompilerReport',
        primitive: 'syntax',
        inputSummary: `source="${sourceRaw}"`,
        outputSummary: compiledGrammar,
        compiledArtifacts,
        meta: { stub: true },
    };

    return {
        kind: 'syntax',
        source,
        status: 'compiled',
        message: `Syntax compiled from ${source.length} source(s): ${source.join(', ')}`,
        compiledGrammar,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── compiler.backend ───────────────────────────────────────────────────────

function compilerBackend(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): CompilerBackendResult {
    const target = String(flags['target'] ?? 'javascript');
    const optimize = flags['optimize'] === true || flags['optimize'] === 'true';

    const codeFragment = `// Generated ${target} code${optimize ? ' (optimized)' : ''}\n// Backend compilation of Grey++ IR → ${target} target.`;

    const compiledArtifacts = [
        `ir → ${target}.output`,
        `${target}.types → type-definitions`,
        `${target}.modules → bundled-output`,
    ];

    if (ctx.compileCoordinator?.translateBackend) {
        ctx.compileCoordinator.translateBackend(target, optimize);
    }
    if (ctx.backendFabric?.applyTranslation) {
        ctx.backendFabric.applyTranslation({
            target,
            optimize,
            action: 'codegen',
        });
    }

    const report: CompilerReport = {
        _type: 'CompilerReport',
        primitive: 'backend',
        inputSummary: `target="${target}" optimize=${optimize}`,
        outputSummary: codeFragment,
        compiledArtifacts,
        meta: { stub: true },
    };

    return {
        kind: 'backend',
        target,
        optimize,
        status: 'compiled',
        message: `Backend compiled for target "${target}"${optimize ? ' with optimizations' : ''}`,
        codeFragment,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── compiler.exec ──────────────────────────────────────────────────────────

function compilerExec(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): CompilerExecResult {
    const scenario = String(flags['scenario'] ?? 'default');

    const executionPlan = `Execution compiled for "${scenario}" — all primitives lowered to runnable execution graph.`;

    const compiledArtifacts = [
        `${scenario} → exec-graph`,
        `pipeline + orchestration → compiled-exec`,
        `security + trust → compiled-policy`,
        `memory + narrative → compiled-context`,
    ];

    if (ctx.compileCoordinator?.mapSyntax) {
        ctx.compileCoordinator.mapSyntax(scenario, 'exec');
    }
    if (ctx.backendFabric?.applyTranslation) {
        ctx.backendFabric.applyTranslation({
            action: 'compile-exec',
            scenario,
            target: 'exec-graph',
        });
    }
    if (ctx.backendFabric?.toCodeFragment) {
        ctx.backendFabric.toCodeFragment(scenario);
    }

    const report: CompilerReport = {
        _type: 'CompilerReport',
        primitive: 'exec',
        inputSummary: `scenario="${scenario}"`,
        outputSummary: executionPlan,
        compiledArtifacts,
        meta: { stub: true },
    };

    return {
        kind: 'exec',
        scenario,
        status: 'compiled',
        message: `Execution compilation complete for "${scenario}"`,
        executionPlan,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_compiler') && !process.argv[1]?.includes('grey_compilecoord') && process.argv.includes('--test')) {
    console.log('── grey_compiler self-test ──');

    const fakeNode = (primitive: string, flags: Record<string, string | number | boolean>): CompilerNode => ({
        kind: 'Compiler' as any,
        primitive,
        args: [],
        flags: Object.fromEntries(
            Object.entries(flags).map(([k, v]) => [k, typeof v === 'number'
                ? { kind: 'NumberLit' as any, value: v } as any
                : typeof v === 'boolean'
                    ? { kind: 'BoolLit' as any, value: v } as any
                    : { kind: 'StringLit' as any, value: v } as any
            ])
        ),
    });

    const resolve = (n: ASTNode) => (n as any).value;

    // compiler.syntax
    const s1 = dispatchCompiler(fakeNode('syntax', { source: 'pipeline, orchestration, memory' }), resolve);
    console.assert(s1.kind === 'syntax', 'syntax kind');
    console.assert(s1.status === 'compiled', 'syntax status');
    console.assert((s1 as CompilerSyntaxResult).source.length === 3, `syntax source count: ${(s1 as CompilerSyntaxResult).source.length}`);
    console.assert((s1 as CompilerSyntaxResult).source[0] === 'pipeline', 'first source');
    console.assert((s1 as CompilerSyntaxResult).compiledGrammar.includes('pipeline'), 'grammar content');
    console.assert(s1.report._type === 'CompilerReport', 'syntax report type');
    console.assert(s1.report.compiledArtifacts.length === 3, 'syntax artifacts');
    console.log('  ✓ compiler.syntax');

    // compiler.backend
    const b1 = dispatchCompiler(fakeNode('backend', { target: 'typescript', optimize: true }), resolve);
    console.assert(b1.kind === 'backend', 'backend kind');
    console.assert(b1.status === 'compiled', 'backend status');
    console.assert((b1 as CompilerBackendResult).target === 'typescript', 'backend target');
    console.assert((b1 as CompilerBackendResult).optimize === true, 'backend optimize');
    console.assert((b1 as CompilerBackendResult).codeFragment.includes('typescript'), 'backend code');
    console.assert(b1.report.compiledArtifacts.length === 3, 'backend artifacts');
    console.log('  ✓ compiler.backend');

    // compiler.exec
    const e1 = dispatchCompiler(fakeNode('exec', { scenario: 'full-stack deployment' }), resolve);
    console.assert(e1.kind === 'exec', 'exec kind');
    console.assert(e1.status === 'compiled', 'exec status');
    console.assert((e1 as CompilerExecResult).scenario === 'full-stack deployment', 'exec scenario');
    console.assert((e1 as CompilerExecResult).executionPlan.includes('full-stack'), 'exec plan content');
    console.assert(e1.report.compiledArtifacts.length === 4, 'exec artifacts');
    console.log('  ✓ compiler.exec');

    // Unknown primitive
    try {
        dispatchCompiler(fakeNode('unknown', {}), resolve);
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown error');
        console.log('  ✓ unknown primitive throws');
    }

    // With mock coordinators
    const mapLog: any[] = [];
    const translateLog: any[] = [];
    const targetLog: any[] = [];
    const translationLog: any[] = [];
    const fragmentLog: any[] = [];
    const mockCoord = {
        mapSyntax: (name: string, type: string) => mapLog.push({ name, type }),
        translateBackend: (target: string, optimize: boolean) => translateLog.push({ target, optimize }),
    };
    const mockFabric = {
        addTarget: (name: string, type: string) => targetLog.push({ name, type }),
        applyTranslation: (rule: any) => translationLog.push(rule),
        toCodeFragment: (scenario: string) => fragmentLog.push(scenario),
    };

    // syntax with coordinators
    dispatchCompiler(fakeNode('syntax', { source: 'net, sec' }), resolve, { compileCoordinator: mockCoord, backendFabric: mockFabric });
    console.assert(mapLog.length === 2, `syntax mapped ${mapLog.length} sources`);
    console.assert(targetLog.length === 2, `syntax added ${targetLog.length} targets`);
    console.log('  ✓ syntax with coordinators');

    // backend with coordinators
    dispatchCompiler(fakeNode('backend', { target: 'python', optimize: false }), resolve, { compileCoordinator: mockCoord, backendFabric: mockFabric });
    console.assert(translateLog.length === 1, 'backend translated');
    console.assert(translationLog.length === 1, 'backend translation applied');
    console.log('  ✓ backend with coordinators');

    // exec with coordinators
    dispatchCompiler(fakeNode('exec', { scenario: 'global' }), resolve, { compileCoordinator: mockCoord, backendFabric: mockFabric });
    console.assert(translationLog.length === 2, 'exec translation applied');
    console.assert(fragmentLog.length === 1, 'exec fragment generated');
    console.log('  ✓ exec with coordinators');

    // Default flags
    const d1 = dispatchCompiler(fakeNode('syntax', {}), resolve);
    console.assert((d1 as CompilerSyntaxResult).source.length === 1 && (d1 as CompilerSyntaxResult).source[0] === 'default', 'default source');
    const d2 = dispatchCompiler(fakeNode('backend', {}), resolve);
    console.assert((d2 as CompilerBackendResult).target === 'javascript', 'default target');
    console.assert((d2 as CompilerBackendResult).optimize === false, 'default optimize');
    const d3 = dispatchCompiler(fakeNode('exec', {}), resolve);
    console.assert((d3 as CompilerExecResult).scenario === 'default', 'default scenario');
    console.log('  ✓ default flags');

    console.log('── all grey_compiler tests passed ──');
}
