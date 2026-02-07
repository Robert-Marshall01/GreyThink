// ─── Grey++ Networking Primitives ────────────────────────────────────────────
// Isolated executors for every NetNode primitive.  Each networking operation
// is a pure function that receives resolved arguments and returns a
// structured stub result — no real HTTP, no real sockets, no external deps.
//
// Extend by swapping the stub bodies for real implementations (fetch, ws,
// gRPC client) in a future stage.

import type { NetPrimitive } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface NetResult {
    _type: string;
    primitive: NetPrimitive;
    op: string;
    [key: string]: unknown;
}

// ═══════════════════════════════════════════════════════════════════════════
//  HTTP Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * net.http — execute an HTTP request (stub).
 *
 *   net.http GET "https://example.com"
 *   net.http POST "https://api.example.com/data" body="payload"
 *
 * Returns a placeholder response.  No real network call is made.
 */
export function netHttp(
    op: string,
    args: unknown[],
    flags: Record<string, unknown>,
): NetResult {
    const method = op.toUpperCase();
    const url = String(args[0] ?? flags['url'] ?? 'https://example.com');
    const body = flags['body'] ?? flags['data'] ?? null;
    const headers = flags['headers'] ?? {};

    return {
        _type: 'NetHttp',
        primitive: 'http',
        op: method,
        url,
        body,
        headers,
        status: 200,
        response: `HTTP request sent — ${method} ${url}`,
        meta: { stub: true, note: 'No real HTTP call — placeholder response.' },
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Socket Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * net.socket — open / interact with a socket connection (stub).
 *
 *   net.socket connect host=localhost port=8080
 *   net.socket send data="hello"
 *   net.socket close
 *
 * Returns a placeholder connection descriptor.  No real socket is opened.
 */
export function netSocket(
    op: string,
    args: unknown[],
    flags: Record<string, unknown>,
): NetResult {
    const host = String(flags['host'] ?? args[0] ?? 'localhost');
    const port = Number(flags['port'] ?? args[1] ?? 0);
    const data = flags['data'] ?? flags['payload'] ?? null;

    const socketId = `sock-${Date.now().toString(36)}`;

    return {
        _type: 'NetSocket',
        primitive: 'socket',
        op,
        socketId,
        host,
        port,
        data,
        status: op === 'close' ? 'closed' : 'opened',
        response: `Socket ${op} — ${host}:${port}`,
        meta: { stub: true, note: 'No real socket — placeholder descriptor.' },
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  RPC Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * net.rpc — invoke a remote procedure call (stub).
 *
 *   net.rpc call service=math method=add args=[1,2]
 *   net.rpc call service=users method=getById args=[42]
 *
 * Returns a placeholder RPC result.  No real transport is used.
 */
export function netRpc(
    op: string,
    args: unknown[],
    flags: Record<string, unknown>,
): NetResult {
    const service = String(flags['service'] ?? args[0] ?? 'default');
    const method = String(flags['method'] ?? args[1] ?? 'call');
    const rpcArgs = flags['args'] ?? args.slice(2);

    const callId = `rpc-${Date.now().toString(36)}`;

    return {
        _type: 'NetRpc',
        primitive: 'rpc',
        op,
        callId,
        service,
        method,
        args: rpcArgs,
        status: 'executed',
        response: `RPC call executed — ${service}.${method}`,
        meta: { stub: true, note: 'No real RPC transport — placeholder result.' },
    };
}

// ─── Central dispatcher ─────────────────────────────────────────────────────
// Maps a NetPrimitive string to the matching executor.  Unknown primitives
// fall through to a generic stub.

export function dispatchNet(
    primitive: NetPrimitive,
    op: string,
    args: unknown[],
    flags: Record<string, unknown>,
): NetResult {
    switch (primitive) {
        case 'http': return netHttp(op, args, flags);
        case 'socket': return netSocket(op, args, flags);
        case 'rpc': return netRpc(op, args, flags);
        default:
            return {
                _type: 'NetResult',
                primitive,
                op,
                args,
                flags,
                response: `Unknown net primitive "${primitive}" — generic stub.`,
                meta: { stub: true },
            };
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_net') && process.argv.includes('--test')) {
    console.log('═══ grey_net.ts Self-Test ═══\n');

    console.log('── net.http GET ──');
    console.log(JSON.stringify(netHttp('GET', ['https://example.com'], {}), null, 2));

    console.log('\n── net.http POST ──');
    console.log(JSON.stringify(netHttp('POST', ['https://api.example.com/data'], { body: '{"key":"value"}' }), null, 2));

    console.log('\n── net.socket connect ──');
    console.log(JSON.stringify(netSocket('connect', [], { host: 'localhost', port: 8080 }), null, 2));

    console.log('\n── net.socket close ──');
    console.log(JSON.stringify(netSocket('close', [], { host: 'localhost', port: 8080 }), null, 2));

    console.log('\n── net.rpc call ──');
    console.log(JSON.stringify(netRpc('call', [], { service: 'math', method: 'add', args: [1, 2] }), null, 2));

    console.log('\n── dispatch (unknown) ──');
    console.log(JSON.stringify(dispatchNet('mqtt', 'publish', ['topic/test'], { payload: 'hello' }), null, 2));

    console.log('\n✓ All grey_net tests passed.');
}
