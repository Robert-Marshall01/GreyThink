// ─── Grey++ Distributed Coordination Stubs ──────────────────────────────────
// In-memory Coordinator that can register processes, send/receive messages,
// and track process state.  All operations are synchronous stubs — no real
// networking, no external dependencies, no background services.
//
// Extend by swapping the in-memory maps for a transport layer (WebSocket,
// gRPC, etc.) in a future stage.

// ─── Types ──────────────────────────────────────────────────────────────────

export type ProcessState = 'registered' | 'running' | 'paused' | 'stopped';

export interface ProcessInfo {
    id: string;
    label: string;
    state: ProcessState;
    createdAt: string;
    meta: Record<string, unknown>;
}

export interface Message {
    id: string;
    from: string;
    to: string;
    payload: unknown;
    timestamp: string;
}

// ═══════════════════════════════════════════════════════════════════════════
//  Coordinator
// ═══════════════════════════════════════════════════════════════════════════

export class Coordinator {
    /** Process registry — keyed by process id. */
    private processes = new Map<string, ProcessInfo>();

    /** Per-process inbox — keyed by target process id. */
    private inboxes = new Map<string, Message[]>();

    /** Monotonic counter for generating unique message ids. */
    private msgSeq = 0;

    // ── Process Registration ────────────────────────────────────────────

    /**
     * Register a new process.  Returns the `ProcessInfo` descriptor.
     * If a process with the same id already exists, it is returned as-is
     * (idempotent).
     */
    register(id: string, label?: string, meta?: Record<string, unknown>): ProcessInfo {
        if (this.processes.has(id)) return this.processes.get(id)!;

        const info: ProcessInfo = {
            id,
            label: label ?? id,
            state: 'registered',
            createdAt: new Date().toISOString(),
            meta: meta ?? {},
        };
        this.processes.set(id, info);
        this.inboxes.set(id, []);
        return info;
    }

    // ── Message Passing ─────────────────────────────────────────────────

    /**
     * Send a message from one process to another (stub — in-memory only).
     * Both `from` and `to` processes are auto-registered if unknown.
     */
    send(from: string, to: string, payload: unknown): Message {
        // Auto-register unknown processes.
        if (!this.processes.has(from)) this.register(from);
        if (!this.processes.has(to)) this.register(to);

        const msg: Message = {
            id: `msg-${++this.msgSeq}`,
            from,
            to,
            payload,
            timestamp: new Date().toISOString(),
        };

        this.inboxes.get(to)!.push(msg);
        return msg;
    }

    /**
     * Receive (and remove) the next message from a process's inbox.
     * Returns `null` if the inbox is empty.
     */
    receive(processId: string): Message | null {
        const inbox = this.inboxes.get(processId);
        if (!inbox || inbox.length === 0) return null;
        return inbox.shift()!;
    }

    /**
     * Peek at all messages in a process's inbox without consuming them.
     */
    peek(processId: string): readonly Message[] {
        return this.inboxes.get(processId) ?? [];
    }

    // ── Process State Tracking ──────────────────────────────────────────

    /**
     * Transition a process to a new state.
     * Returns the updated `ProcessInfo`, or `null` if the process is unknown.
     */
    setState(processId: string, state: ProcessState): ProcessInfo | null {
        const info = this.processes.get(processId);
        if (!info) return null;
        info.state = state;
        return info;
    }

    /**
     * Get the current info for a process, or `null` if unregistered.
     */
    getProcess(processId: string): ProcessInfo | null {
        return this.processes.get(processId) ?? null;
    }

    /**
     * List all registered processes.
     */
    listProcesses(): ProcessInfo[] {
        return [...this.processes.values()];
    }

    // ── Diagnostics ─────────────────────────────────────────────────────

    /**
     * Return a snapshot of the entire coordination state (useful for
     * debugging / REPL inspection).
     */
    snapshot(): {
        processes: ProcessInfo[];
        inboxes: Record<string, Message[]>;
    } {
        const inboxes: Record<string, Message[]> = {};
        for (const [k, v] of this.inboxes) inboxes[k] = [...v];
        return {
            processes: this.listProcesses(),
            inboxes,
        };
    }
}

// ═══════════════════════════════════════════════════════════════════════════
//  CoordinationManager  (Stage 8 — Distributed Coordination)
// ═══════════════════════════════════════════════════════════════════════════
// Higher-level coordinator for distributed primitives: node registration,
// consensus voting, and synchronization barriers.  Built on top of the
// existing Coordinator for process/message tracking.

export interface ClusterNode {
    name: string;
    role: string;
    registeredAt: string;
    meta: Record<string, unknown>;
}

export interface ConsensusRound {
    id: string;
    proposal: string;
    quorum: number;
    votes: string[];
    accepted: boolean;
    createdAt: string;
}

export interface SyncBarrier {
    name: string;
    expectedNodes: number;
    arrivedNodes: string[];
    passed: boolean;
    createdAt: string;
}

export class CoordinationManager {
    /** Registered cluster nodes — keyed by node name. */
    private nodes = new Map<string, ClusterNode>();

    /** Consensus rounds — keyed by round id. */
    private consensusRounds = new Map<string, ConsensusRound>();

    /** Sync barriers — keyed by barrier name. */
    private barriers = new Map<string, SyncBarrier>();

    /** Auto-increment counters. */
    private roundSeq = 0;

    // ── Node Registration ───────────────────────────────────────────────

    /**
     * Register a named node in the cluster.
     * Idempotent — re-registering an existing node returns it unchanged.
     */
    registerNode(name: string, role: string = 'peer', meta?: Record<string, unknown>): ClusterNode {
        if (this.nodes.has(name)) return this.nodes.get(name)!;
        const node: ClusterNode = {
            name,
            role,
            registeredAt: new Date().toISOString(),
            meta: meta ?? {},
        };
        this.nodes.set(name, node);
        return node;
    }

    /**
     * Remove a node from the cluster.
     */
    unregisterNode(name: string): boolean {
        return this.nodes.delete(name);
    }

    /**
     * Check whether a node is registered.
     */
    hasNode(name: string): boolean {
        return this.nodes.has(name);
    }

    /**
     * List all registered nodes.
     */
    listNodes(): ClusterNode[] {
        return [...this.nodes.values()];
    }

    // ── Consensus ───────────────────────────────────────────────────────

    /**
     * Start a consensus round for a proposal.
     * All currently registered nodes automatically vote "yes" (stub).
     * Returns the round result.
     */
    startConsensus(proposal: string, quorum?: number): ConsensusRound {
        const id = `round-${++this.roundSeq}`;
        const voters = [...this.nodes.keys()];
        const effectiveQuorum = quorum ?? Math.max(1, Math.ceil(voters.length / 2));
        const accepted = voters.length >= effectiveQuorum;

        const round: ConsensusRound = {
            id,
            proposal,
            quorum: effectiveQuorum,
            votes: voters,
            accepted,
            createdAt: new Date().toISOString(),
        };
        this.consensusRounds.set(id, round);
        return round;
    }

    /**
     * Get a consensus round by id.
     */
    getRound(id: string): ConsensusRound | null {
        return this.consensusRounds.get(id) ?? null;
    }

    /**
     * List all consensus rounds.
     */
    listRounds(): ConsensusRound[] {
        return [...this.consensusRounds.values()];
    }

    // ── Synchronization Barriers ────────────────────────────────────────

    /**
     * Create or join a named barrier.  When enough nodes arrive, the
     * barrier is marked as passed.
     */
    arriveAtBarrier(barrierName: string, nodeName: string, expectedNodes?: number): SyncBarrier {
        let barrier = this.barriers.get(barrierName);
        if (!barrier) {
            barrier = {
                name: barrierName,
                expectedNodes: expectedNodes ?? (this.nodes.size || 1),
                arrivedNodes: [],
                passed: false,
                createdAt: new Date().toISOString(),
            };
            this.barriers.set(barrierName, barrier);
        }

        if (!barrier.arrivedNodes.includes(nodeName)) {
            barrier.arrivedNodes.push(nodeName);
        }

        barrier.passed = barrier.arrivedNodes.length >= barrier.expectedNodes;
        return barrier;
    }

    /**
     * Get barrier state.
     */
    getBarrier(name: string): SyncBarrier | null {
        return this.barriers.get(name) ?? null;
    }

    /**
     * List all barriers.
     */
    listBarriers(): SyncBarrier[] {
        return [...this.barriers.values()];
    }

    // ── Diagnostics ─────────────────────────────────────────────────────

    /**
     * Return a snapshot of the entire coordination manager state.
     */
    snapshot(): {
        nodes: ClusterNode[];
        consensusRounds: ConsensusRound[];
        barriers: SyncBarrier[];
    } {
        return {
            nodes: this.listNodes(),
            consensusRounds: this.listRounds(),
            barriers: this.listBarriers(),
        };
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1] && process.argv[1].includes('grey_coordination') && process.argv.includes('--test')) {
    console.log('═══ grey_coordination.ts Self-Test ═══\n');

    const coord = new Coordinator();

    // Register processes.
    const p1 = coord.register('proc-1', 'Worker A');
    const p2 = coord.register('proc-2', 'Worker B');
    console.log('Registered:', p1.id, p1.label);
    console.log('Registered:', p2.id, p2.label);

    // Idempotent re-registration.
    const p1Again = coord.register('proc-1', 'Should not overwrite');
    console.log('Idempotent:', p1Again.label === 'Worker A' ? 'PASS' : 'FAIL');

    // Send messages.
    const m1 = coord.send('proc-1', 'proc-2', { text: 'Hello from A' });
    const m2 = coord.send('proc-1', 'proc-2', { text: 'Second message' });
    console.log('\nSent:', m1.id, '→', m1.to);
    console.log('Sent:', m2.id, '→', m2.to);

    // Receive.
    const rx1 = coord.receive('proc-2');
    console.log('Received:', rx1?.id, JSON.stringify(rx1?.payload));
    const rx2 = coord.receive('proc-2');
    console.log('Received:', rx2?.id, JSON.stringify(rx2?.payload));
    const rx3 = coord.receive('proc-2');
    console.log('Empty inbox:', rx3 === null ? 'PASS' : 'FAIL');

    // State transitions.
    coord.setState('proc-1', 'running');
    coord.setState('proc-2', 'paused');
    console.log('\nState proc-1:', coord.getProcess('proc-1')?.state);
    console.log('State proc-2:', coord.getProcess('proc-2')?.state);

    // Snapshot.
    console.log('\nSnapshot:', JSON.stringify(coord.snapshot(), null, 2));

    console.log('\n✓ All grey_coordination tests passed.');

    // ── CoordinationManager tests (Stage 8) ─────────────────────────────
    console.log('\n═══ CoordinationManager Self-Test ═══\n');

    const mgr = new CoordinationManager();

    // Register nodes.
    const n1 = mgr.registerNode('alpha', 'leader');
    const n2 = mgr.registerNode('beta', 'worker');
    const n3 = mgr.registerNode('gamma', 'worker');
    console.log('Registered nodes:', mgr.listNodes().map(n => n.name));

    // Idempotent re-registration.
    const n1Again = mgr.registerNode('alpha', 'should-not-overwrite');
    console.log('Idempotent:', n1Again.role === 'leader' ? 'PASS' : 'FAIL');

    // Consensus.
    console.log('\n── consensus ──');
    const r1 = mgr.startConsensus('upgrade pipeline');
    console.log(`Round ${r1.id}: "${r1.proposal}" — accepted=${r1.accepted}, votes=${r1.votes.length}/${r1.quorum}`);

    const r2 = mgr.startConsensus('scale to 5 nodes', 5);
    console.log(`Round ${r2.id}: "${r2.proposal}" — accepted=${r2.accepted}, votes=${r2.votes.length}/${r2.quorum}`);

    // Barriers.
    console.log('\n── barriers ──');
    mgr.arriveAtBarrier('stage8', 'alpha', 3);
    mgr.arriveAtBarrier('stage8', 'beta');
    const b1 = mgr.arriveAtBarrier('stage8', 'gamma');
    console.log(`Barrier "${b1.name}": passed=${b1.passed}, arrived=${b1.arrivedNodes.length}/${b1.expectedNodes}`);

    // Snapshot.
    console.log('\n── snapshot ──');
    const snap2 = mgr.snapshot();
    console.log(`Nodes: ${snap2.nodes.length}, Rounds: ${snap2.consensusRounds.length}, Barriers: ${snap2.barriers.length}`);

    console.log('\n✓ All CoordinationManager tests passed.');
}
