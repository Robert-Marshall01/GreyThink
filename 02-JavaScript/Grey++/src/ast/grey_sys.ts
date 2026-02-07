// ─── Grey++ System Primitives ────────────────────────────────────────────────
// Isolated executors for every SysNode operation.  Each primitive is a pure
// function that receives resolved arguments and returns a structured result —
// no shared state, no background services, no external dependencies.
//
// Extend by adding a new `case` in `dispatchSys` and a matching executor.

import type { SysOp } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface SysResult {
    _type: string;
    op: SysOp;
    [key: string]: unknown;
}

// ─── Individual executors ───────────────────────────────────────────────────

/**
 * sys.log — emit a structured log message to stdout.
 * This is the only primitive that produces real side-effects; everything
 * else returns placeholders.
 */
export function sysLog(args: unknown[]): SysResult {
    const message = args.map(a => (typeof a === 'string' ? a : JSON.stringify(a))).join(' ');
    console.log(`[grey++ sys.log] ${message}`);
    return {
        _type: 'SysLog',
        op: 'log',
        message,
        timestamp: new Date().toISOString(),
    };
}

/**
 * sys.thread — lightweight concurrency stub.
 * Returns a descriptor representing a "started" thread.  No real OS thread
 * is created — this is a placeholder for the future threading runtime.
 */
export function sysThread(
    args: unknown[],
    flags: Record<string, unknown>,
): SysResult {
    const threadId = `thread-${Date.now().toString(36)}`;
    const label = (flags['label'] as string) ?? (flags['name'] as string) ?? null;

    return {
        _type: 'SysThread',
        op: 'thread',
        threadId,
        label,
        status: 'started',
        payload: args.length > 0 ? args[0] : null,
        meta: { stub: true, note: 'Thread started (stub — no real OS thread).' },
    };
}

/**
 * sys.message — inter-process / inter-thread message-passing stub.
 * Returns a confirmation descriptor.  No actual delivery occurs.
 */
export function sysMessage(
    args: unknown[],
    flags: Record<string, unknown>,
): SysResult {
    const target = (flags['target'] as string) ?? String(args[0] ?? 'unknown');
    const payload = (flags['payload'] as unknown) ?? args[1] ?? null;

    return {
        _type: 'SysMessage',
        op: 'message',
        target,
        payload,
        status: 'sent',
        meta: { stub: true, note: 'Message sent (stub — no real delivery).' },
    };
}

// ─── Central dispatcher ─────────────────────────────────────────────────────
// Maps a SysOp string to the matching executor.  Unknown ops fall through
// to a generic stub so the pipeline never throws on new operations.

export function dispatchSys(
    op: SysOp,
    args: unknown[],
    flags: Record<string, unknown>,
): SysResult {
    switch (op) {
        case 'log': return sysLog(args);
        case 'thread': return sysThread(args, flags);
        case 'message': return sysMessage(args, flags);

        // ── Existing stubs preserved from the runtime ──
        case 'env':
            return {
                _type: 'SysEnv',
                op: 'env',
                name: String(args[0] ?? ''),
                value: (typeof process !== 'undefined' ? process.env[String(args[0] ?? '')] : null) ?? null,
            };

        case 'spawn':
            return {
                _type: 'SysSpawn',
                op: 'spawn',
                status: 'spawned',
                args,
                meta: { stub: true, note: 'Process spawned (stub).' },
            };

        case 'sleep':
            return {
                _type: 'SysSleep',
                op: 'sleep',
                ms: args[0] ?? 0,
                meta: { stub: true },
            };

        default:
            return {
                _type: 'SysResult',
                op,
                args,
                flags,
                meta: { stub: true, note: `Unknown sys op "${op}" — generic stub.` },
            };
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1] && process.argv[1].includes('grey_sys') && process.argv.includes('--test')) {
    console.log('═══ grey_sys.ts Self-Test ═══\n');

    console.log('── sys.log ──');
    console.log(JSON.stringify(sysLog(['Hello Grey++']), null, 2));

    console.log('\n── sys.thread ──');
    console.log(JSON.stringify(sysThread([], { label: 'compute' }), null, 2));

    console.log('\n── sys.message ──');
    console.log(JSON.stringify(sysMessage([], { target: 'process1', payload: 'Ping' }), null, 2));

    console.log('\n── dispatch (unknown op) ──');
    console.log(JSON.stringify(dispatchSys('custom_op', ['arg1'], {}), null, 2));

    console.log('\n✓ All grey_sys tests passed.');
}
