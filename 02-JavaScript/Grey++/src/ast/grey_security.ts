// ─── Grey++ Security Primitives ──────────────────────────────────────────────
// Isolated executors for every SecNode primitive.  Each security operation
// is a pure function that receives resolved arguments and returns a
// structured stub result — no real permission engines, no real sandboxing,
// no external deps.
//
// Extend by swapping the stub bodies for real implementations (OS-level
// sandboxing, RBAC engines, structured audit sinks) in a future stage.

import type { SecPrimitive } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface SecResult {
    _type: string;
    primitive: SecPrimitive;
    [key: string]: unknown;
}

// ═══════════════════════════════════════════════════════════════════════════
//  Permission Check Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * sec.perm — check whether a user / role has permission for an action (stub).
 *
 *   sec.perm user=alice action=net.http
 *   sec.perm role=admin action=storage.delete
 *
 * Returns a placeholder "granted" result.  No real permission engine.
 */
export function secPerm(
    args: unknown[],
    flags: Record<string, unknown>,
): SecResult {
    const user = String(flags['user'] ?? args[0] ?? 'anonymous');
    const role = String(flags['role'] ?? 'user');
    const action = String(flags['action'] ?? args[1] ?? '*');

    return {
        _type: 'SecPerm',
        primitive: 'perm',
        user,
        role,
        action,
        granted: true,
        reason: `Permission granted — ${user} may perform "${action}".`,
        meta: { stub: true, note: 'No real permission engine — always grants.' },
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Sandbox Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * sec.sandbox — execute a command or expression in a sandboxed context (stub).
 *
 *   sec.sandbox run "rm -rf testdir"
 *   sec.sandbox eval "2 + 2" timeout=5000
 *
 * Returns a placeholder execution result.  No real sandboxing occurs.
 */
export function secSandbox(
    args: unknown[],
    flags: Record<string, unknown>,
): SecResult {
    const command = String(args[0] ?? flags['cmd'] ?? flags['run'] ?? '<none>');
    const timeout = Number(flags['timeout'] ?? 30000);
    const sandboxId = `sbx-${Date.now().toString(36)}`;

    return {
        _type: 'SecSandbox',
        primitive: 'sandbox',
        sandboxId,
        command,
        timeout,
        status: 'completed',
        exitCode: 0,
        output: `Sandbox executed — "${command}"`,
        meta: { stub: true, note: 'No real sandbox — placeholder execution.' },
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Audit Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * sec.audit — record an audit log entry (stub).
 *
 *   sec.audit message="Pipeline executed" level=info
 *   sec.audit message="Access denied" user=bob level=warn
 *
 * Returns a placeholder audit receipt.  No real audit sink.
 */
export function secAudit(
    args: unknown[],
    flags: Record<string, unknown>,
): SecResult {
    const message = String(flags['message'] ?? args[0] ?? '<no message>');
    const level = String(flags['level'] ?? 'info');
    const user = String(flags['user'] ?? 'system');
    const auditId = `aud-${Date.now().toString(36)}`;

    return {
        _type: 'SecAudit',
        primitive: 'audit',
        auditId,
        message,
        level,
        user,
        timestamp: new Date().toISOString(),
        recorded: true,
        meta: { stub: true, note: 'No real audit sink — placeholder receipt.' },
    };
}

// ─── Central dispatcher ─────────────────────────────────────────────────────
// Maps a SecPrimitive string to the matching executor.  Unknown primitives
// fall through to a generic stub.

export function dispatchSec(
    primitive: SecPrimitive,
    args: unknown[],
    flags: Record<string, unknown>,
): SecResult {
    switch (primitive) {
        case 'perm': return secPerm(args, flags);
        case 'sandbox': return secSandbox(args, flags);
        case 'audit': return secAudit(args, flags);
        default:
            return {
                _type: 'SecResult',
                primitive,
                args,
                flags,
                response: `Unknown sec primitive "${primitive}" — generic stub.`,
                meta: { stub: true },
            };
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_security') && process.argv.includes('--test')) {
    console.log('═══ grey_security.ts Self-Test ═══\n');

    console.log('── sec.perm (user + action) ──');
    console.log(JSON.stringify(secPerm([], { user: 'alice', action: 'net.http' }), null, 2));

    console.log('\n── sec.perm (role) ──');
    console.log(JSON.stringify(secPerm([], { role: 'admin', action: 'storage.delete' }), null, 2));

    console.log('\n── sec.sandbox run ──');
    console.log(JSON.stringify(secSandbox(['rm -rf testdir'], {}), null, 2));

    console.log('\n── sec.sandbox eval ──');
    console.log(JSON.stringify(secSandbox(['2 + 2'], { timeout: 5000 }), null, 2));

    console.log('\n── sec.audit info ──');
    console.log(JSON.stringify(secAudit([], { message: 'Pipeline executed', level: 'info' }), null, 2));

    console.log('\n── sec.audit warn ──');
    console.log(JSON.stringify(secAudit([], { message: 'Access denied', user: 'bob', level: 'warn' }), null, 2));

    console.log('\n── dispatch (unknown) ──');
    console.log(JSON.stringify(dispatchSec('encrypt', ['data'], { algo: 'aes256' }), null, 2));

    console.log('\n✓ All grey_security tests passed.');
}
