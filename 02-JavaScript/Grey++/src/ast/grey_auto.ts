// ─────────────────────────────────────────────────────────────────────────────
//  grey_auto.ts  –  Stage 13 · Auto-Approval Primitives Dispatcher
// ─────────────────────────────────────────────────────────────────────────────
//  Dispatches auto.safe | auto.sandbox | auto.prompt commands to dedicated
//  executor functions.  The runtime delegates to dispatchAuto() which routes
//  to the correct handler based on the primitive string.
//
//  Self-test:  npx tsx src/ast/grey_auto.ts --test
// ─────────────────────────────────────────────────────────────────────────────

import type { AutoNode, ASTNode } from './grey_ast.js';

// ── Result types ────────────────────────────────────────────────────────────

export interface AutoSafeResult {
    kind: 'safe';
    command: string;
    decision: 'auto-approved';
    message: string;
    timestamp: string;
}

export interface AutoSandboxResult {
    kind: 'sandbox';
    command: string;
    decision: 'sandboxed';
    message: string;
    isolated: boolean;
    timestamp: string;
}

export interface AutoPromptResult {
    kind: 'prompt';
    command: string;
    decision: 'approval-requested';
    message: string;
    requiresHuman: boolean;
    timestamp: string;
}

export type AutoResult = AutoSafeResult | AutoSandboxResult | AutoPromptResult;

// ── Dispatcher ──────────────────────────────────────────────────────────────

export function dispatchAuto(
    node: AutoNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        trustBoundary?: { classify: Function; autoApprove: Function; routeCommand: Function };
        policyLayer?: { evaluate: Function; getAllowList: Function; getDenyList: Function };
    }
): AutoResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }
    const command = (flags['command'] as string) ?? String(node.args[0] ?? 'unknown');

    switch (node.primitive) {
        case 'safe': return autoSafe(command, flags, ctx);
        case 'sandbox': return autoSandbox(command, flags, ctx);
        case 'prompt': return autoPrompt(command, flags, ctx);
        default:
            throw new Error(`[auto] Unknown primitive: ${node.primitive}`);
    }
}

// ── auto.safe ───────────────────────────────────────────────────────────────

function autoSafe(
    command: string,
    flags: Record<string, unknown>,
    ctx: Record<string, any>
): AutoSafeResult {
    const boundary = ctx.trustBoundary;
    if (boundary) {
        const classification = boundary.classify(command);
        if (classification === 'safe') {
            boundary.autoApprove(command);
        }
    }

    const policy = ctx.policyLayer;
    let policyNote = '';
    if (policy) {
        const evaluation = policy.evaluate(command);
        policyNote = ` (policy: ${evaluation.action})`;
    }

    return {
        kind: 'safe',
        command,
        decision: 'auto-approved',
        message: `Safe command auto-approved: "${command}"${policyNote}`,
        timestamp: new Date().toISOString(),
    };
}

// ── auto.sandbox ────────────────────────────────────────────────────────────

function autoSandbox(
    command: string,
    flags: Record<string, unknown>,
    ctx: Record<string, any>
): AutoSandboxResult {
    const boundary = ctx.trustBoundary;
    if (boundary) {
        boundary.routeCommand(command, 'sandbox');
    }

    const policy = ctx.policyLayer;
    let policyNote = '';
    if (policy) {
        const evaluation = policy.evaluate(command);
        policyNote = ` (policy: ${evaluation.action})`;
    }

    return {
        kind: 'sandbox',
        command,
        decision: 'sandboxed',
        message: `Sandboxed execution: "${command}"${policyNote}`,
        isolated: true,
        timestamp: new Date().toISOString(),
    };
}

// ── auto.prompt ─────────────────────────────────────────────────────────────

function autoPrompt(
    command: string,
    flags: Record<string, unknown>,
    ctx: Record<string, any>
): AutoPromptResult {
    const boundary = ctx.trustBoundary;
    if (boundary) {
        boundary.routeCommand(command, 'prompt');
    }

    const reason = (flags['reason'] as string) ?? 'Potentially destructive operation';
    const policy = ctx.policyLayer;
    let policyNote = '';
    if (policy) {
        const evaluation = policy.evaluate(command);
        policyNote = ` (policy: ${evaluation.action})`;
    }

    return {
        kind: 'prompt',
        command,
        decision: 'approval-requested',
        message: `Approval requested: "${command}" — ${reason}${policyNote}`,
        requiresHuman: true,
        timestamp: new Date().toISOString(),
    };
}

// ── Self-test ───────────────────────────────────────────────────────────────

if (process.argv.includes('--test')) {
    console.log('── grey_auto self-test ──');

    const fakeNode = (primitive: string, cmd: string): AutoNode => ({
        kind: 'Auto' as any,
        primitive,
        args: [],
        flags: {
            command: { kind: 'StringLit' as any, value: cmd } as any,
        },
    });

    const resolve = (n: ASTNode) => (n as any).value;

    // Test without coordinators (fallback paths)
    const s = dispatchAuto(fakeNode('safe', 'ls'), resolve, {});
    console.assert(s.kind === 'safe', 'safe kind');
    console.assert(s.decision === 'auto-approved', 'safe decision');
    console.assert(s.command === 'ls', 'safe command');
    console.assert(s.message.includes('auto-approved'), 'safe message');
    console.log('  ✓ auto.safe (fallback)');

    const sb = dispatchAuto(fakeNode('sandbox', 'rm -rf testdir'), resolve, {});
    console.assert(sb.kind === 'sandbox', 'sandbox kind');
    console.assert(sb.decision === 'sandboxed', 'sandbox decision');
    console.assert(sb.isolated === true, 'sandbox isolated');
    console.log('  ✓ auto.sandbox (fallback)');

    const pr = dispatchAuto(fakeNode('prompt', 'git reset --hard'), resolve, {});
    console.assert(pr.kind === 'prompt', 'prompt kind');
    console.assert(pr.decision === 'approval-requested', 'prompt decision');
    console.assert(pr.requiresHuman === true, 'prompt requiresHuman');
    console.log('  ✓ auto.prompt (fallback)');

    // Test with mock coordinators
    const classifyLog: string[] = [];
    const routeLog: { cmd: string; mode: string }[] = [];
    const mockBoundary = {
        classify: (cmd: string) => { classifyLog.push(cmd); return 'safe'; },
        autoApprove: (cmd: string) => { /* noop */ },
        routeCommand: (cmd: string, mode: string) => { routeLog.push({ cmd, mode }); },
    };
    const evalLog: string[] = [];
    const mockPolicy = {
        evaluate: (cmd: string) => { evalLog.push(cmd); return { action: 'allow', reason: 'test' }; },
        getAllowList: () => [],
        getDenyList: () => [],
    };

    const s2 = dispatchAuto(fakeNode('safe', 'pwd'), resolve, {
        trustBoundary: mockBoundary, policyLayer: mockPolicy,
    });
    console.assert(s2.message.includes('policy: allow'), 'safe with policy');
    console.assert(classifyLog.includes('pwd'), 'boundary classified pwd');
    console.assert(evalLog.includes('pwd'), 'policy evaluated pwd');
    console.log('  ✓ auto.safe (coordinator + policy)');

    const sb2 = dispatchAuto(fakeNode('sandbox', 'rm -rf /'), resolve, {
        trustBoundary: mockBoundary, policyLayer: mockPolicy,
    });
    console.assert(routeLog.some(r => r.cmd === 'rm -rf /' && r.mode === 'sandbox'), 'boundary routed to sandbox');
    console.log('  ✓ auto.sandbox (coordinator)');

    const pr2 = dispatchAuto(fakeNode('prompt', 'shutdown'), resolve, {
        trustBoundary: mockBoundary, policyLayer: mockPolicy,
    });
    console.assert(routeLog.some(r => r.cmd === 'shutdown' && r.mode === 'prompt'), 'boundary routed to prompt');
    console.log('  ✓ auto.prompt (coordinator)');

    // Unknown primitive
    try {
        dispatchAuto(fakeNode('unknown', 'x'), resolve, {});
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown primitive error');
        console.log('  ✓ unknown primitive throws');
    }

    console.log('── all grey_auto tests passed ──');
}
