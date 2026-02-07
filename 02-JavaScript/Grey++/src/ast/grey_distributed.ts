// ─── Grey++ Distributed Primitives ───────────────────────────────────────────
// Isolated executors for every DistNode primitive.  Each distributed operation
// is a pure function that receives resolved arguments and returns a
// structured stub result — no real cluster management, no real consensus
// protocols, no external deps.
//
// Extend by swapping the stub bodies for real implementations (Raft, Paxos,
// distributed barriers) in a future stage.

import type { DistPrimitive } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface DistResult {
    _type: string;
    primitive: DistPrimitive;
    [key: string]: unknown;
}

// ═══════════════════════════════════════════════════════════════════════════
//  Node Registration Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * dist.register — register a node in the distributed cluster (stub).
 *
 *   dist.register node=alpha
 *   dist.register node=beta role=worker
 *
 * Returns a placeholder "Node registered" result.  No real cluster.
 */
export function distRegister(
    args: unknown[],
    flags: Record<string, unknown>,
): DistResult {
    const node = String(flags['node'] ?? args[0] ?? 'unknown');
    const role = String(flags['role'] ?? 'peer');
    const nodeId = `node-${Date.now().toString(36)}`;

    return {
        _type: 'DistRegister',
        primitive: 'register',
        nodeId,
        node,
        role,
        status: 'registered',
        response: `Node registered — "${node}" as ${role}.`,
        meta: { stub: true, note: 'No real cluster — placeholder registration.' },
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Consensus Vote Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * dist.consensus — propose and vote on a consensus decision (stub).
 *
 *   dist.consensus proposal="upgrade pipeline"
 *   dist.consensus proposal="scale to 5 nodes" quorum=3
 *
 * Returns a placeholder "Consensus achieved" result.  No real voting.
 */
export function distConsensus(
    args: unknown[],
    flags: Record<string, unknown>,
): DistResult {
    const proposal = String(flags['proposal'] ?? args[0] ?? '<no proposal>');
    const quorum = Number(flags['quorum'] ?? 3);
    const voteId = `vote-${Date.now().toString(36)}`;

    return {
        _type: 'DistConsensus',
        primitive: 'consensus',
        voteId,
        proposal,
        quorum,
        votes: quorum,
        accepted: true,
        status: 'achieved',
        response: `Consensus achieved — "${proposal}" (${quorum}/${quorum} votes).`,
        meta: { stub: true, note: 'No real consensus protocol — always accepts.' },
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  Synchronization Barrier Stub
// ═══════════════════════════════════════════════════════════════════════════

/**
 * dist.sync — create or join a synchronization barrier (stub).
 *
 *   dist.sync barrier="stage8"
 *   dist.sync barrier="checkpoint" nodes=4
 *
 * Returns a placeholder "Sync barrier passed" result.  No real barrier.
 */
export function distSync(
    args: unknown[],
    flags: Record<string, unknown>,
): DistResult {
    const barrier = String(flags['barrier'] ?? args[0] ?? 'default');
    const nodes = Number(flags['nodes'] ?? 1);
    const barrierId = `barrier-${Date.now().toString(36)}`;

    return {
        _type: 'DistSync',
        primitive: 'sync',
        barrierId,
        barrier,
        expectedNodes: nodes,
        arrivedNodes: nodes,
        status: 'passed',
        response: `Sync barrier passed — "${barrier}" (${nodes}/${nodes} nodes arrived).`,
        meta: { stub: true, note: 'No real sync barrier — always passes.' },
    };
}

// ─── Central dispatcher ─────────────────────────────────────────────────────
// Maps a DistPrimitive string to the matching executor.  Unknown primitives
// fall through to a generic stub.

export function dispatchDist(
    primitive: DistPrimitive,
    args: unknown[],
    flags: Record<string, unknown>,
): DistResult {
    switch (primitive) {
        case 'register': return distRegister(args, flags);
        case 'consensus': return distConsensus(args, flags);
        case 'sync': return distSync(args, flags);
        default:
            return {
                _type: 'DistResult',
                primitive,
                args,
                flags,
                response: `Unknown dist primitive "${primitive}" — generic stub.`,
                meta: { stub: true },
            };
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_distributed') && process.argv.includes('--test')) {
    console.log('═══ grey_distributed.ts Self-Test ═══\n');

    console.log('── dist.register (node) ──');
    console.log(JSON.stringify(distRegister([], { node: 'alpha' }), null, 2));

    console.log('\n── dist.register (node + role) ──');
    console.log(JSON.stringify(distRegister([], { node: 'beta', role: 'worker' }), null, 2));

    console.log('\n── dist.consensus (proposal) ──');
    console.log(JSON.stringify(distConsensus([], { proposal: 'upgrade pipeline' }), null, 2));

    console.log('\n── dist.consensus (proposal + quorum) ──');
    console.log(JSON.stringify(distConsensus([], { proposal: 'scale to 5 nodes', quorum: 5 }), null, 2));

    console.log('\n── dist.sync (barrier) ──');
    console.log(JSON.stringify(distSync([], { barrier: 'stage8' }), null, 2));

    console.log('\n── dist.sync (barrier + nodes) ──');
    console.log(JSON.stringify(distSync([], { barrier: 'checkpoint', nodes: 4 }), null, 2));

    console.log('\n── dispatch (unknown) ──');
    console.log(JSON.stringify(dispatchDist('elect', ['leader'], { timeout: 5000 }), null, 2));

    console.log('\n✓ All grey_distributed tests passed.');
}
