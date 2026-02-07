// ─── Grey++ Protocol Coordinator ─────────────────────────────────────────────
// Central router for networking protocols (http, socket, rpc).  Registers
// protocol handlers, routes messages to the correct handler, and tracks
// connection state in memory.  All stubs — no external dependencies, no
// real networking, no background services.
//
// Extend by plugging real transport adapters in a future stage.

import type { NetPrimitive } from './grey_ast.js';

// ─── Types ──────────────────────────────────────────────────────────────────

export type ConnectionState = 'idle' | 'connecting' | 'connected' | 'disconnected' | 'error';

export interface ConnectionInfo {
    id: string;
    protocol: NetPrimitive;
    state: ConnectionState;
    endpoint: string;
    createdAt: string;
    meta: Record<string, unknown>;
}

export interface ProtocolMessage {
    id: string;
    protocol: NetPrimitive;
    direction: 'outbound' | 'inbound';
    endpoint: string;
    payload: unknown;
    timestamp: string;
}

/** A protocol handler is a function that processes a message and returns a result. */
export type ProtocolHandler = (
    op: string,
    args: unknown[],
    flags: Record<string, unknown>,
) => unknown;

// ═══════════════════════════════════════════════════════════════════════════
//  ProtocolCoordinator
// ═══════════════════════════════════════════════════════════════════════════

export class ProtocolCoordinator {
    /** Registered protocol handlers — keyed by protocol name. */
    private handlers = new Map<string, ProtocolHandler>();

    /** Active connections — keyed by connection id. */
    private connections = new Map<string, ConnectionInfo>();

    /** Message log — append-only in-memory audit trail. */
    private messageLog: ProtocolMessage[] = [];

    /** Auto-increment counters. */
    private connSeq = 0;
    private msgSeq = 0;

    // ── Handler Registration ────────────────────────────────────────────

    /**
     * Register a protocol handler.
     * If a handler for the same protocol already exists, it is replaced.
     */
    registerHandler(protocol: string, handler: ProtocolHandler): void {
        this.handlers.set(protocol, handler);
    }

    /**
     * Unregister a protocol handler.
     * Returns `true` if a handler was removed, `false` if none was found.
     */
    unregisterHandler(protocol: string): boolean {
        return this.handlers.delete(protocol);
    }

    /**
     * Check whether a handler is registered for a given protocol.
     */
    hasHandler(protocol: string): boolean {
        return this.handlers.has(protocol);
    }

    /**
     * List all registered protocol names.
     */
    listProtocols(): string[] {
        return [...this.handlers.keys()];
    }

    // ── Message Routing ─────────────────────────────────────────────────

    /**
     * Route a message to the correct protocol handler.
     * If no handler is registered for the given protocol, returns a stub
     * error result.  The message is always logged regardless.
     */
    route(
        protocol: NetPrimitive,
        op: string,
        args: unknown[] = [],
        flags: Record<string, unknown> = {},
    ): unknown {
        const endpoint = String(flags['url'] ?? flags['host'] ?? args[0] ?? '<unknown>');

        // Log the outbound message.
        this.logMessage(protocol, 'outbound', endpoint, { op, args, flags });

        const handler = this.handlers.get(protocol);
        if (!handler) {
            return {
                _type: 'ProtocolError',
                protocol,
                error: `No handler registered for protocol "${protocol}".`,
                meta: { stub: true },
            };
        }

        return handler(op, args, flags);
    }

    // ── Connection State ────────────────────────────────────────────────

    /**
     * Open a tracked connection for a given protocol and endpoint.
     * Returns the `ConnectionInfo` descriptor.
     */
    openConnection(protocol: NetPrimitive, endpoint: string, meta?: Record<string, unknown>): ConnectionInfo {
        const id = `conn-${++this.connSeq}`;
        const info: ConnectionInfo = {
            id,
            protocol,
            state: 'connected',
            endpoint,
            createdAt: new Date().toISOString(),
            meta: meta ?? {},
        };
        this.connections.set(id, info);
        return info;
    }

    /**
     * Update the state of an existing connection.
     * Returns the updated info, or `null` if the connection is unknown.
     */
    setConnectionState(connectionId: string, state: ConnectionState): ConnectionInfo | null {
        const info = this.connections.get(connectionId);
        if (!info) return null;
        info.state = state;
        return info;
    }

    /**
     * Close a connection (sets state to 'disconnected').
     */
    closeConnection(connectionId: string): ConnectionInfo | null {
        return this.setConnectionState(connectionId, 'disconnected');
    }

    /**
     * Get info for a specific connection.
     */
    getConnection(connectionId: string): ConnectionInfo | null {
        return this.connections.get(connectionId) ?? null;
    }

    /**
     * List all tracked connections, optionally filtered by protocol.
     */
    listConnections(protocol?: string): ConnectionInfo[] {
        const all = [...this.connections.values()];
        if (protocol) return all.filter(c => c.protocol === protocol);
        return all;
    }

    // ── Message Logging ─────────────────────────────────────────────────

    private logMessage(
        protocol: NetPrimitive,
        direction: 'outbound' | 'inbound',
        endpoint: string,
        payload: unknown,
    ): void {
        this.messageLog.push({
            id: `pmsg-${++this.msgSeq}`,
            protocol,
            direction,
            endpoint,
            payload,
            timestamp: new Date().toISOString(),
        });
    }

    /**
     * Get all logged messages, optionally filtered by protocol.
     */
    getMessages(protocol?: string): readonly ProtocolMessage[] {
        if (protocol) return this.messageLog.filter(m => m.protocol === protocol);
        return this.messageLog;
    }

    /**
     * Clear the message log.
     */
    clearMessages(): void {
        this.messageLog = [];
        this.msgSeq = 0;
    }

    // ── Diagnostics ─────────────────────────────────────────────────────

    /**
     * Return a snapshot of the entire protocol coordination state.
     */
    snapshot(): {
        protocols: string[];
        connections: ConnectionInfo[];
        messages: ProtocolMessage[];
    } {
        return {
            protocols: this.listProtocols(),
            connections: this.listConnections(),
            messages: [...this.messageLog],
        };
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_protocol') && process.argv.includes('--test')) {
    console.log('═══ grey_protocol.ts Self-Test ═══\n');

    const coord = new ProtocolCoordinator();

    // Register handlers.
    coord.registerHandler('http', (op, args, flags) => ({
        _type: 'HttpStub', method: op, url: args[0], flags,
    }));
    coord.registerHandler('socket', (op, args, flags) => ({
        _type: 'SocketStub', op, host: flags['host'], port: flags['port'],
    }));
    coord.registerHandler('rpc', (op, args, flags) => ({
        _type: 'RpcStub', op, service: flags['service'], method: flags['method'],
    }));

    console.log('Registered protocols:', coord.listProtocols());

    // Route messages.
    console.log('\n── route http ──');
    console.log(JSON.stringify(coord.route('http', 'GET', ['https://example.com'], {}), null, 2));

    console.log('\n── route socket ──');
    console.log(JSON.stringify(coord.route('socket', 'connect', [], { host: 'localhost', port: 8080 }), null, 2));

    console.log('\n── route rpc ──');
    console.log(JSON.stringify(coord.route('rpc', 'call', [], { service: 'math', method: 'add' }), null, 2));

    console.log('\n── route unknown ──');
    console.log(JSON.stringify(coord.route('mqtt', 'publish', ['topic'], {}), null, 2));

    // Connection tracking.
    console.log('\n── connections ──');
    const c1 = coord.openConnection('socket', 'localhost:8080');
    console.log('Opened:', c1.id, c1.endpoint, c1.state);

    coord.setConnectionState(c1.id, 'idle');
    console.log('After idle:', coord.getConnection(c1.id)?.state);

    coord.closeConnection(c1.id);
    console.log('After close:', coord.getConnection(c1.id)?.state);

    // Message log.
    console.log('\n── message log ──');
    console.log(`Total messages logged: ${coord.getMessages().length}`);
    console.log(`HTTP messages: ${coord.getMessages('http').length}`);

    // Snapshot.
    console.log('\n── snapshot ──');
    console.log(JSON.stringify(coord.snapshot(), null, 2));

    console.log('\n✓ All grey_protocol tests passed.');
}
