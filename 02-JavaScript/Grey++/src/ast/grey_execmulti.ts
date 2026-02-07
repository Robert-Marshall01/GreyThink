// ─── Grey++ Multi-Backend Execution Primitives ──────────────────────────────
// Dispatcher for ExecMultiNode: routes JS, C++, and RISC-V execution
// requests through the multi-backend execution layer.  All stubs — no real
// compilation or execution, no external dependencies.
//
// Self-test:  npx tsx src/ast/grey_execmulti.ts --test
//
// Extend by plugging real execution backends in a future stage.

import type { ExecMultiNode, ASTNode } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface ExecMultiReport {
    _type: 'ExecMultiReport';
    primitive: string;
    inputSummary: string;
    outputSummary: string;
    executedArtifacts: string[];
    meta: { stub: true };
}

export interface ExecMultiJsResult {
    kind: 'js';
    program: string;
    status: 'executed';
    message: string;
    output: string;
    report: ExecMultiReport;
    timestamp: string;
}

export interface ExecMultiCppResult {
    kind: 'cpp';
    program: string;
    status: 'executed';
    message: string;
    output: string;
    report: ExecMultiReport;
    timestamp: string;
}

export interface ExecMultiRiscvResult {
    kind: 'riscv';
    program: string;
    status: 'executed';
    message: string;
    output: string;
    report: ExecMultiReport;
    timestamp: string;
}

export type ExecMultiResult = ExecMultiJsResult | ExecMultiCppResult | ExecMultiRiscvResult;

// ─── Dispatcher ─────────────────────────────────────────────────────────────

export function dispatchExecMulti(
    node: ExecMultiNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        multiExecCoordinator?: { mapBackend: Function; dispatchExec: Function; generateReport?: Function };
        backendRunner?: { addContext: Function; applyExec: Function; toResult?: Function };
    } = {},
): ExecMultiResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }

    switch (node.primitive) {
        case 'js': return execMultiJs(flags, ctx);
        case 'cpp': return execMultiCpp(flags, ctx);
        case 'riscv': return execMultiRiscv(flags, ctx);
        default:
            throw new Error(`[execmulti] Unknown primitive: ${node.primitive}`);
    }
}

// ─── execmulti.js ───────────────────────────────────────────────────────────

function execMultiJs(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): ExecMultiJsResult {
    const program = String(flags['program'] ?? 'console.log("hello")');

    const output = `JS executed: ${program}`;

    const executedArtifacts = [
        `js.source → parsed`,
        `js.ast → compiled`,
        `js.runtime → executed`,
    ];

    if (ctx.multiExecCoordinator?.mapBackend) {
        ctx.multiExecCoordinator.mapBackend('js', program);
    }
    if (ctx.backendRunner?.addContext) {
        ctx.backendRunner.addContext('js', program);
    }

    const report: ExecMultiReport = {
        _type: 'ExecMultiReport',
        primitive: 'js',
        inputSummary: `program="${program}"`,
        outputSummary: output,
        executedArtifacts,
        meta: { stub: true },
    };

    return {
        kind: 'js',
        program,
        status: 'executed',
        message: `JavaScript execution complete: ${program.slice(0, 50)}${program.length > 50 ? '…' : ''}`,
        output,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── execmulti.cpp ──────────────────────────────────────────────────────────

function execMultiCpp(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): ExecMultiCppResult {
    const program = String(flags['program'] ?? 'int main(){return 0;}');

    const output = `C++ executed: ${program}`;

    const executedArtifacts = [
        `cpp.source → parsed`,
        `cpp.ast → compiled`,
        `cpp.binary → linked`,
        `cpp.runtime → executed`,
    ];

    if (ctx.multiExecCoordinator?.dispatchExec) {
        ctx.multiExecCoordinator.dispatchExec('cpp', program);
    }
    if (ctx.backendRunner?.applyExec) {
        ctx.backendRunner.applyExec({
            backend: 'cpp',
            program,
            action: 'compile-and-run',
        });
    }

    const report: ExecMultiReport = {
        _type: 'ExecMultiReport',
        primitive: 'cpp',
        inputSummary: `program="${program}"`,
        outputSummary: output,
        executedArtifacts,
        meta: { stub: true },
    };

    return {
        kind: 'cpp',
        program,
        status: 'executed',
        message: `C++ execution complete: ${program.slice(0, 50)}${program.length > 50 ? '…' : ''}`,
        output,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── execmulti.riscv ────────────────────────────────────────────────────────

function execMultiRiscv(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): ExecMultiRiscvResult {
    const program = String(flags['program'] ?? 'addi x0, x0, 0');

    const output = `RISC-V executed: ${program}`;

    const executedArtifacts = [
        `riscv.asm → assembled`,
        `riscv.binary → linked`,
        `riscv.simulator → executed`,
    ];

    if (ctx.multiExecCoordinator?.mapBackend) {
        ctx.multiExecCoordinator.mapBackend('riscv', program);
    }
    if (ctx.backendRunner?.applyExec) {
        ctx.backendRunner.applyExec({
            backend: 'riscv',
            program,
            action: 'assemble-and-simulate',
        });
    }
    if (ctx.backendRunner?.toResult) {
        ctx.backendRunner.toResult('riscv');
    }

    const report: ExecMultiReport = {
        _type: 'ExecMultiReport',
        primitive: 'riscv',
        inputSummary: `program="${program}"`,
        outputSummary: output,
        executedArtifacts,
        meta: { stub: true },
    };

    return {
        kind: 'riscv',
        program,
        status: 'executed',
        message: `RISC-V execution complete: ${program.slice(0, 50)}${program.length > 50 ? '…' : ''}`,
        output,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_execmulti') && !process.argv[1]?.includes('grey_multiexec') && process.argv.includes('--test')) {
    console.log('── grey_execmulti self-test ──');

    const fakeNode = (primitive: string, flags: Record<string, string>): ExecMultiNode => ({
        kind: 'ExecMulti' as any,
        primitive,
        args: [],
        flags: Object.fromEntries(
            Object.entries(flags).map(([k, v]) => [k, { kind: 'StringLit' as any, value: v } as any])
        ),
    });

    const resolve = (n: ASTNode) => (n as any).value;

    // execmulti.js
    const j1 = dispatchExecMulti(fakeNode('js', { program: "console.log('Hello Grey++')" }), resolve);
    console.assert(j1.kind === 'js', 'js kind');
    console.assert(j1.status === 'executed', 'js status');
    console.assert(j1.program === "console.log('Hello Grey++')", 'js program');
    console.assert(j1.output.includes('JS executed'), 'js output');
    console.assert(j1.report._type === 'ExecMultiReport', 'js report type');
    console.assert(j1.report.executedArtifacts.length === 3, 'js artifacts');
    console.log('  ✓ execmulti.js');

    // execmulti.cpp
    const c1 = dispatchExecMulti(fakeNode('cpp', { program: 'int main(){return 0;}' }), resolve);
    console.assert(c1.kind === 'cpp', 'cpp kind');
    console.assert(c1.status === 'executed', 'cpp status');
    console.assert(c1.program === 'int main(){return 0;}', 'cpp program');
    console.assert(c1.output.includes('C++ executed'), 'cpp output');
    console.assert(c1.report.executedArtifacts.length === 4, 'cpp artifacts');
    console.log('  ✓ execmulti.cpp');

    // execmulti.riscv
    const r1 = dispatchExecMulti(fakeNode('riscv', { program: 'addi x1, x0, 5' }), resolve);
    console.assert(r1.kind === 'riscv', 'riscv kind');
    console.assert(r1.status === 'executed', 'riscv status');
    console.assert(r1.program === 'addi x1, x0, 5', 'riscv program');
    console.assert(r1.output.includes('RISC-V executed'), 'riscv output');
    console.assert(r1.report.executedArtifacts.length === 3, 'riscv artifacts');
    console.log('  ✓ execmulti.riscv');

    // Unknown primitive
    try {
        dispatchExecMulti(fakeNode('wasm', {}), resolve);
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown error');
        console.log('  ✓ unknown primitive throws');
    }

    // With mock coordinators
    const mapLog: any[] = [];
    const dispatchLog: any[] = [];
    const contextLog: any[] = [];
    const execLog: any[] = [];
    const resultLog: any[] = [];
    const mockCoord = {
        mapBackend: (backend: string, program: string) => mapLog.push({ backend, program }),
        dispatchExec: (backend: string, program: string) => dispatchLog.push({ backend, program }),
    };
    const mockRunner = {
        addContext: (backend: string, program: string) => contextLog.push({ backend, program }),
        applyExec: (rule: any) => execLog.push(rule),
        toResult: (backend: string) => resultLog.push(backend),
    };

    // js with coordinators
    dispatchExecMulti(fakeNode('js', { program: 'test()' }), resolve, { multiExecCoordinator: mockCoord, backendRunner: mockRunner });
    console.assert(mapLog.length === 1, `js mapped ${mapLog.length} backends`);
    console.assert(contextLog.length === 1, `js added ${contextLog.length} contexts`);
    console.log('  ✓ js with coordinators');

    // cpp with coordinators
    dispatchExecMulti(fakeNode('cpp', { program: 'main()' }), resolve, { multiExecCoordinator: mockCoord, backendRunner: mockRunner });
    console.assert(dispatchLog.length === 1, 'cpp dispatched');
    console.assert(execLog.length === 1, 'cpp exec applied');
    console.log('  ✓ cpp with coordinators');

    // riscv with coordinators
    dispatchExecMulti(fakeNode('riscv', { program: 'nop' }), resolve, { multiExecCoordinator: mockCoord, backendRunner: mockRunner });
    console.assert(mapLog.length === 2, 'riscv mapped');
    console.assert(execLog.length === 2, 'riscv exec applied');
    console.assert(resultLog.length === 1, 'riscv result generated');
    console.log('  ✓ riscv with coordinators');

    // Default flags
    const d1 = dispatchExecMulti(fakeNode('js', {}), resolve);
    console.assert(d1.program === 'console.log("hello")', 'default js program');
    const d2 = dispatchExecMulti(fakeNode('cpp', {}), resolve);
    console.assert(d2.program === 'int main(){return 0;}', 'default cpp program');
    const d3 = dispatchExecMulti(fakeNode('riscv', {}), resolve);
    console.assert(d3.program === 'addi x0, x0, 0', 'default riscv program');
    console.log('  ✓ default flags');

    console.log('── all grey_execmulti tests passed ──');
}
