// ─── Grey++ Persist Coordinator ──────────────────────────────────────────────
// Registers stored semantic memories, handles recall requests, and applies
// stub evolution rules.  All in-memory — no external persistence, no real AI.
//
// Self-test:  npx tsx src/ast/grey_persist.ts --test
//
// Extend by plugging real persistence backends in a future stage.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface MemoryEntry {
    key: string;
    value: string;
    version: number;
    createdAt: string;
    updatedAt: string;
    history: EvolutionRecord[];
}

export interface EvolutionRecord {
    strategy: string;
    previousValue: string;
    newValue: string;
    evolvedAt: string;
}

// ─── PersistCoordinator ────────────────────────────────────────────────────

export class PersistCoordinator {
    private memories: Map<string, MemoryEntry> = new Map();
    private globalHistory: { action: string; key: string; timestamp: string }[] = [];

    /** Store a key-value pair in memory. Updates existing entries. */
    store(key: string, value: string): MemoryEntry {
        const now = new Date().toISOString();
        const existing = this.memories.get(key);

        if (existing) {
            existing.value = value;
            existing.version++;
            existing.updatedAt = now;
            this.globalHistory.push({ action: 'update', key, timestamp: now });
            return existing;
        }

        const entry: MemoryEntry = {
            key,
            value,
            version: 1,
            createdAt: now,
            updatedAt: now,
            history: [],
        };
        this.memories.set(key, entry);
        this.globalHistory.push({ action: 'store', key, timestamp: now });
        return entry;
    }

    /** Recall a value by key. Returns the value or null if not found. */
    recall(key: string): string | null {
        const entry = this.memories.get(key);
        if (!entry) return null;
        this.globalHistory.push({ action: 'recall', key, timestamp: new Date().toISOString() });
        return entry.value;
    }

    /** Evolve a memory entry using a given strategy. */
    evolve(key: string, strategy: string): MemoryEntry | null {
        const entry = this.memories.get(key);
        if (!entry) return null;

        const now = new Date().toISOString();
        const previousValue = entry.value;

        // Stub evolution: append strategy marker to value
        let newValue: string;
        switch (strategy) {
            case 'consolidate':
                newValue = `[consolidated] ${previousValue}`;
                break;
            case 'prune':
                newValue = `[pruned] ${previousValue}`;
                break;
            case 'merge':
                newValue = `[merged] ${previousValue}`;
                break;
            default:
                newValue = `[${strategy}] ${previousValue}`;
                break;
        }

        entry.history.push({
            strategy,
            previousValue,
            newValue,
            evolvedAt: now,
        });

        entry.value = newValue;
        entry.version++;
        entry.updatedAt = now;

        this.globalHistory.push({ action: 'evolve', key, timestamp: now });
        return entry;
    }

    /** Get all stored memories as a plain object. */
    getMemories(): Record<string, MemoryEntry> {
        const result: Record<string, MemoryEntry> = {};
        for (const [k, v] of this.memories) {
            result[k] = v;
        }
        return result;
    }

    /** Get a specific memory entry. */
    getEntry(key: string): MemoryEntry | null {
        return this.memories.get(key) ?? null;
    }

    /** Get global action history. */
    getHistory(): { action: string; key: string; timestamp: string }[] {
        return [...this.globalHistory];
    }

    /** Get total number of stored entries. */
    size(): number {
        return this.memories.size;
    }

    /** Clear all memories and history. */
    clear(): void {
        this.memories.clear();
        this.globalHistory = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_persist') && process.argv.includes('--test')) {
    console.log('── grey_persist self-test ──');

    const coord = new PersistCoordinator();

    // Store
    const e1 = coord.store('pipeline1', 'optimized');
    console.assert(e1.key === 'pipeline1', 'store key');
    console.assert(e1.value === 'optimized', 'store value');
    console.assert(e1.version === 1, 'store version');
    console.assert(coord.size() === 1, 'size after store');
    console.log('  ✓ store');

    // Recall
    const v1 = coord.recall('pipeline1');
    console.assert(v1 === 'optimized', 'recall value');
    console.log('  ✓ recall');

    // Recall missing key
    const v2 = coord.recall('missing');
    console.assert(v2 === null, 'recall missing returns null');
    console.log('  ✓ recall missing');

    // Update existing entry
    const e2 = coord.store('pipeline1', 'highly-optimized');
    console.assert(e2.version === 2, 'update version');
    console.assert(e2.value === 'highly-optimized', 'update value');
    console.assert(coord.size() === 1, 'size unchanged after update');
    console.log('  ✓ update');

    // Evolve — consolidate
    const evolved1 = coord.evolve('pipeline1', 'consolidate');
    console.assert(evolved1 !== null, 'evolve returns entry');
    console.assert(evolved1!.value.includes('[consolidated]'), 'consolidate strategy applied');
    console.assert(evolved1!.version === 3, 'evolve bumps version');
    console.assert(evolved1!.history.length === 1, 'evolve records history');
    console.assert(evolved1!.history[0].strategy === 'consolidate', 'history strategy');
    console.log('  ✓ evolve consolidate');

    // Evolve — prune
    const evolved2 = coord.evolve('pipeline1', 'prune');
    console.assert(evolved2!.value.includes('[pruned]'), 'prune strategy applied');
    console.assert(evolved2!.history.length === 2, 'history grows');
    console.log('  ✓ evolve prune');

    // Evolve — merge
    const evolved3 = coord.evolve('pipeline1', 'merge');
    console.assert(evolved3!.value.includes('[merged]'), 'merge strategy applied');
    console.log('  ✓ evolve merge');

    // Evolve — custom strategy
    const evolved4 = coord.evolve('pipeline1', 'custom');
    console.assert(evolved4!.value.includes('[custom]'), 'custom strategy applied');
    console.log('  ✓ evolve custom');

    // Evolve missing key
    const evolved5 = coord.evolve('missing', 'consolidate');
    console.assert(evolved5 === null, 'evolve missing returns null');
    console.log('  ✓ evolve missing');

    // getEntry
    const entry = coord.getEntry('pipeline1');
    console.assert(entry !== null, 'getEntry returns entry');
    console.assert(entry!.key === 'pipeline1', 'getEntry key');
    console.log('  ✓ getEntry');

    // getMemories
    const mems = coord.getMemories();
    console.assert(Object.keys(mems).length === 1, 'getMemories count');
    console.assert(mems['pipeline1'] !== undefined, 'getMemories has key');
    console.log('  ✓ getMemories');

    // getHistory
    const hist = coord.getHistory();
    // store + recall + update + evolve*4 + recall(missing doesn't record... actually recall does record)
    // store(1) + recall(1) + recall-missing(no history for missing) + update(1) + evolve*4 = 7
    // Wait: recall of missing key returns null and doesn't push history.
    // Actually looking at the code: recall only pushes history if entry exists.
    // So: store(1) + recall(1) + update(1) + evolve(4) + getEntry(no) = 7
    console.assert(hist.length === 7, `history has 7 entries, got ${hist.length}`);
    console.log('  ✓ getHistory');

    // Multiple keys
    coord.store('doc.edit', 'pattern-A');
    coord.store('artifact.rarity', 'score-high');
    console.assert(coord.size() === 3, 'multiple keys');
    console.log('  ✓ multiple keys');

    // Clear
    coord.clear();
    console.assert(coord.size() === 0, 'clear empties memories');
    console.assert(coord.getHistory().length === 0, 'clear empties history');
    console.log('  ✓ clear');

    console.log('── all grey_persist tests passed ──');
}
