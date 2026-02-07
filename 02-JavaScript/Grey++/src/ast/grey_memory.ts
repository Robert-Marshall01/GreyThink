// ─── Grey++ Memory Primitives ────────────────────────────────────────────────
// Dispatcher for MemoryNode: routes store, recall, and evolve requests
// through persistent coordinators.  All stubs — no external dependencies,
// no real persistence backends, no real AI.
//
// Self-test:  npx tsx src/ast/grey_memory.ts --test
//
// Extend by plugging real persistence backends in a future stage.

import type { MemoryNode, ASTNode } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface MemoryReport {
    _type: 'MemoryReport';
    primitive: string;
    inputSummary: string;
    outputSummary: string;
    entries: number;
    meta: { stub: true };
}

export interface MemoryStoreResult {
    kind: 'store';
    key: string;
    value: string;
    status: 'stored';
    message: string;
    report: MemoryReport;
    timestamp: string;
}

export interface MemoryRecallResult {
    kind: 'recall';
    key: string;
    value: string | null;
    found: boolean;
    status: 'recalled';
    message: string;
    report: MemoryReport;
    timestamp: string;
}

export interface MemoryEvolveResult {
    kind: 'evolve';
    key: string;
    strategy: string;
    status: 'evolved';
    message: string;
    report: MemoryReport;
    timestamp: string;
}

export type MemoryResult = MemoryStoreResult | MemoryRecallResult | MemoryEvolveResult;

// ─── Dispatcher ─────────────────────────────────────────────────────────────

export function dispatchMemory(
    node: MemoryNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        persistCoordinator?: { store: Function; recall: Function; evolve: Function; getMemories: Function };
        persistenceLayer?: { addNode: Function; getNode: Function; applyRule: Function };
    } = {},
): MemoryResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }

    switch (node.primitive) {
        case 'store': return memoryStore(flags, ctx);
        case 'recall': return memoryRecall(flags, ctx);
        case 'evolve': return memoryEvolve(flags, ctx);
        default:
            throw new Error(`[memory] Unknown primitive: ${node.primitive}`);
    }
}

// ─── memory.store ───────────────────────────────────────────────────────────

function memoryStore(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): MemoryStoreResult {
    const key = String(flags['key'] ?? 'default');
    const value = String(flags['value'] ?? '');

    if (ctx.persistCoordinator) {
        ctx.persistCoordinator.store(key, value);
    }
    if (ctx.persistenceLayer) {
        ctx.persistenceLayer.addNode(key, { value, storedAt: new Date().toISOString() });
    }

    const report: MemoryReport = {
        _type: 'MemoryReport',
        primitive: 'store',
        inputSummary: `key="${key}", value="${value}"`,
        outputSummary: `Stored "${key}" → "${value}"`,
        entries: 1,
        meta: { stub: true },
    };

    return {
        kind: 'store',
        key,
        value,
        status: 'stored',
        message: `Stored "${key}" → "${value}"`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── memory.recall ──────────────────────────────────────────────────────────

function memoryRecall(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): MemoryRecallResult {
    const key = String(flags['key'] ?? 'default');

    let value: string | null = null;
    let found = false;

    if (ctx.persistCoordinator) {
        const recalled = ctx.persistCoordinator.recall(key);
        if (recalled !== undefined && recalled !== null) {
            value = String(recalled);
            found = true;
        }
    }

    const report: MemoryReport = {
        _type: 'MemoryReport',
        primitive: 'recall',
        inputSummary: `key="${key}"`,
        outputSummary: found ? `Recalled "${key}" → "${value}"` : `Key "${key}" not found`,
        entries: found ? 1 : 0,
        meta: { stub: true },
    };

    return {
        kind: 'recall',
        key,
        value,
        found,
        status: 'recalled',
        message: found ? `Recalled "${key}" → "${value}"` : `Key "${key}" not found`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── memory.evolve ──────────────────────────────────────────────────────────

function memoryEvolve(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): MemoryEvolveResult {
    const key = String(flags['key'] ?? 'default');
    const strategy = String(flags['strategy'] ?? 'consolidate');

    if (ctx.persistCoordinator) {
        ctx.persistCoordinator.evolve(key, strategy);
    }
    if (ctx.persistenceLayer) {
        ctx.persistenceLayer.applyRule(key, { strategy, evolvedAt: new Date().toISOString() });
    }

    const report: MemoryReport = {
        _type: 'MemoryReport',
        primitive: 'evolve',
        inputSummary: `key="${key}", strategy="${strategy}"`,
        outputSummary: `Evolved "${key}" using ${strategy} strategy`,
        entries: 1,
        meta: { stub: true },
    };

    return {
        kind: 'evolve',
        key,
        strategy,
        status: 'evolved',
        message: `Evolved "${key}" using ${strategy} strategy`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_memory') && process.argv.includes('--test')) {
    console.log('── grey_memory self-test ──');

    const fakeNode = (primitive: string, flags: Record<string, string | number>): MemoryNode => ({
        kind: 'Memory' as any,
        primitive,
        args: [],
        flags: Object.fromEntries(
            Object.entries(flags).map(([k, v]) => [k, typeof v === 'number'
                ? { kind: 'NumberLit' as any, value: v } as any
                : { kind: 'StringLit' as any, value: v } as any
            ])
        ),
    });

    const resolve = (n: ASTNode) => (n as any).value;

    // memory.store
    const s1 = dispatchMemory(fakeNode('store', { key: 'pipeline1', value: 'optimized' }), resolve);
    console.assert(s1.kind === 'store', 'store kind');
    console.assert(s1.status === 'stored', 'store status');
    console.assert(s1.key === 'pipeline1', 'store key');
    console.assert(s1.value === 'optimized', 'store value');
    console.assert(s1.report._type === 'MemoryReport', 'store report type');
    console.assert(s1.report.entries === 1, 'store entries');
    console.log('  ✓ memory.store');

    // memory.recall (no coordinator — key not found)
    const r1 = dispatchMemory(fakeNode('recall', { key: 'doc.edit' }), resolve);
    console.assert(r1.kind === 'recall', 'recall kind');
    console.assert(r1.status === 'recalled', 'recall status');
    console.assert(r1.key === 'doc.edit', 'recall key');
    console.assert(r1.found === false, 'recall not found without coordinator');
    console.assert(r1.value === null, 'recall value null');
    console.log('  ✓ memory.recall (no coordinator)');

    // memory.evolve
    const e1 = dispatchMemory(fakeNode('evolve', { key: 'artifact.rarity', strategy: 'prune' }), resolve);
    console.assert(e1.kind === 'evolve', 'evolve kind');
    console.assert(e1.status === 'evolved', 'evolve status');
    console.assert(e1.key === 'artifact.rarity', 'evolve key');
    console.assert(e1.strategy === 'prune', 'evolve strategy');
    console.assert(e1.report.entries === 1, 'evolve entries');
    console.log('  ✓ memory.evolve');

    // Unknown primitive
    try {
        dispatchMemory(fakeNode('unknown', {}), resolve);
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown error');
        console.log('  ✓ unknown primitive throws');
    }

    // With mock coordinators
    const storeLog: any[] = [];
    const recallMap = new Map<string, string>([['myKey', 'myValue']]);
    const evolveLog: any[] = [];
    const nodeLog: any[] = [];
    const ruleLog: any[] = [];

    const mockPersist = {
        store: (k: string, v: string) => storeLog.push({ k, v }),
        recall: (k: string) => recallMap.get(k) ?? null,
        evolve: (k: string, s: string) => evolveLog.push({ k, s }),
        getMemories: () => ({}),
    };
    const mockLayer = {
        addNode: (k: string, data: any) => nodeLog.push({ k, data }),
        getNode: (k: string) => null,
        applyRule: (k: string, rule: any) => ruleLog.push({ k, rule }),
    };

    // Store with coordinators
    dispatchMemory(fakeNode('store', { key: 'k1', value: 'v1' }), resolve, { persistCoordinator: mockPersist, persistenceLayer: mockLayer });
    console.assert(storeLog.length === 1, 'persist store called');
    console.assert(storeLog[0].k === 'k1', 'persist store key');
    console.assert(nodeLog.length === 1, 'layer addNode called');
    console.log('  ✓ store with coordinators');

    // Recall with coordinator (found)
    const r2 = dispatchMemory(fakeNode('recall', { key: 'myKey' }), resolve, { persistCoordinator: mockPersist });
    console.assert(r2.found === true, 'recall found with coordinator');
    console.assert(r2.value === 'myValue', 'recall value from coordinator');
    console.log('  ✓ recall with coordinator (found)');

    // Recall with coordinator (not found)
    const r3 = dispatchMemory(fakeNode('recall', { key: 'missing' }), resolve, { persistCoordinator: mockPersist });
    console.assert(r3.found === false, 'recall not found');
    console.assert(r3.value === null, 'recall value null');
    console.log('  ✓ recall with coordinator (not found)');

    // Evolve with coordinators
    dispatchMemory(fakeNode('evolve', { key: 'e1', strategy: 'merge' }), resolve, { persistCoordinator: mockPersist, persistenceLayer: mockLayer });
    console.assert(evolveLog.length === 1, 'persist evolve called');
    console.assert(ruleLog.length === 1, 'layer applyRule called');
    console.log('  ✓ evolve with coordinators');

    // Default flags
    const d1 = dispatchMemory(fakeNode('store', {}), resolve);
    console.assert(d1.key === 'default', 'default key');
    console.assert(d1.value === '', 'default value');
    console.log('  ✓ default flags');

    console.log('── all grey_memory tests passed ──');
}
