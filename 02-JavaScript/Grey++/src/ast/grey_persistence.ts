// ─── Grey++ Persistence Layer ────────────────────────────────────────────────
// Persistent graph, stub persistence rules, and consolidated memory state.
// All in-memory — no external storage, no real AI, no disk I/O.
//
// Self-test:  npx tsx src/ast/grey_persistence.ts --test
//
// Extend by plugging real graph databases or file persistence in a future stage.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface PersistenceNode {
    id: string;
    data: Record<string, unknown>;
    edges: string[];
    createdAt: string;
    updatedAt: string;
}

export interface PersistenceRule {
    strategy: string;
    appliedAt: string;
    input: Record<string, unknown>;
    output: Record<string, unknown>;
}

export interface ConsolidatedState {
    nodeCount: number;
    ruleCount: number;
    nodes: Record<string, PersistenceNode>;
    recentRules: PersistenceRule[];
    timestamp: string;
}

// ─── PersistenceLayer ──────────────────────────────────────────────────────

export class PersistenceLayer {
    private nodes: Map<string, PersistenceNode> = new Map();
    private rules: PersistenceRule[] = [];
    private history: { action: string; nodeId: string; timestamp: string }[] = [];

    /** Add a node to the persistence graph. Updates existing nodes. */
    addNode(id: string, data: Record<string, unknown>): PersistenceNode {
        const now = new Date().toISOString();
        const existing = this.nodes.get(id);

        if (existing) {
            existing.data = { ...existing.data, ...data };
            existing.updatedAt = now;
            this.history.push({ action: 'update', nodeId: id, timestamp: now });
            return existing;
        }

        const node: PersistenceNode = {
            id,
            data,
            edges: [],
            createdAt: now,
            updatedAt: now,
        };
        this.nodes.set(id, node);
        this.history.push({ action: 'add', nodeId: id, timestamp: now });
        return node;
    }

    /** Get a node by its ID. */
    getNode(id: string): PersistenceNode | null {
        return this.nodes.get(id) ?? null;
    }

    /** Add an edge between two nodes. Creates target if it doesn't exist. */
    addEdge(fromId: string, toId: string): boolean {
        const fromNode = this.nodes.get(fromId);
        if (!fromNode) return false;

        if (!this.nodes.has(toId)) {
            this.addNode(toId, { _autoCreated: true });
        }

        if (!fromNode.edges.includes(toId)) {
            fromNode.edges.push(toId);
        }
        return true;
    }

    /** Apply a persistence rule to a specific node. */
    applyRule(nodeId: string, ruleInput: Record<string, unknown>): PersistenceRule {
        const now = new Date().toISOString();
        const strategy = String(ruleInput['strategy'] ?? 'default');
        const node = this.nodes.get(nodeId);

        // Stub rule application: produce output based on strategy
        let output: Record<string, unknown>;
        switch (strategy) {
            case 'consolidate':
                output = {
                    action: 'consolidated',
                    nodeId,
                    nodeExists: !!node,
                    message: `Node "${nodeId}" consolidated`,
                };
                break;
            case 'prune':
                output = {
                    action: 'pruned',
                    nodeId,
                    nodeExists: !!node,
                    message: `Node "${nodeId}" pruned`,
                };
                break;
            case 'merge':
                output = {
                    action: 'merged',
                    nodeId,
                    nodeExists: !!node,
                    message: `Node "${nodeId}" merged`,
                };
                break;
            default:
                output = {
                    action: strategy,
                    nodeId,
                    nodeExists: !!node,
                    message: `Rule "${strategy}" applied to "${nodeId}"`,
                };
                break;
        }

        const rule: PersistenceRule = {
            strategy,
            appliedAt: now,
            input: ruleInput,
            output,
        };

        this.rules.push(rule);
        this.history.push({ action: `rule:${strategy}`, nodeId, timestamp: now });

        // If node exists, mark it as updated
        if (node) {
            node.data['_lastRule'] = strategy;
            node.updatedAt = now;
        }

        return rule;
    }

    /** Get consolidated state of the entire persistence layer. */
    getConsolidatedState(): ConsolidatedState {
        const nodesObj: Record<string, PersistenceNode> = {};
        for (const [k, v] of this.nodes) {
            nodesObj[k] = v;
        }

        return {
            nodeCount: this.nodes.size,
            ruleCount: this.rules.length,
            nodes: nodesObj,
            recentRules: this.rules.slice(-10), // last 10 rules
            timestamp: new Date().toISOString(),
        };
    }

    /** Get all applied rules. */
    getRules(): PersistenceRule[] {
        return [...this.rules];
    }

    /** Get action history. */
    getHistory(): { action: string; nodeId: string; timestamp: string }[] {
        return [...this.history];
    }

    /** Get total number of nodes. */
    size(): number {
        return this.nodes.size;
    }

    /** Clear all nodes, rules, and history. */
    clear(): void {
        this.nodes.clear();
        this.rules = [];
        this.history = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_persistence') && process.argv.includes('--test')) {
    console.log('── grey_persistence self-test ──');

    const layer = new PersistenceLayer();

    // Add node
    const n1 = layer.addNode('pipeline1', { value: 'optimized', score: 0.95 });
    console.assert(n1.id === 'pipeline1', 'node id');
    console.assert(n1.data['value'] === 'optimized', 'node data value');
    console.assert(n1.edges.length === 0, 'node edges empty');
    console.assert(layer.size() === 1, 'size after add');
    console.log('  ✓ addNode');

    // Get node
    const g1 = layer.getNode('pipeline1');
    console.assert(g1 !== null, 'getNode returns node');
    console.assert(g1!.id === 'pipeline1', 'getNode id');
    console.log('  ✓ getNode');

    // Get missing node
    const g2 = layer.getNode('missing');
    console.assert(g2 === null, 'getNode missing returns null');
    console.log('  ✓ getNode missing');

    // Update existing node
    const n2 = layer.addNode('pipeline1', { score: 0.99, newField: 'test' });
    console.assert(n2.data['score'] === 0.99, 'update merges data');
    console.assert(n2.data['value'] === 'optimized', 'update preserves existing data');
    console.assert(n2.data['newField'] === 'test', 'update adds new field');
    console.assert(layer.size() === 1, 'size unchanged after update');
    console.log('  ✓ update node');

    // Add edge
    layer.addNode('doc.edit', { type: 'document' });
    const edgeOk = layer.addEdge('pipeline1', 'doc.edit');
    console.assert(edgeOk === true, 'addEdge returns true');
    const n1After = layer.getNode('pipeline1')!;
    console.assert(n1After.edges.includes('doc.edit'), 'edge added');
    console.log('  ✓ addEdge');

    // Add edge to non-existent target (auto-creates)
    const edgeOk2 = layer.addEdge('pipeline1', 'auto-created');
    console.assert(edgeOk2 === true, 'addEdge auto-creates target');
    console.assert(layer.getNode('auto-created') !== null, 'auto-created node exists');
    console.log('  ✓ addEdge auto-create');

    // Add edge from non-existent source
    const edgeFail = layer.addEdge('nonexistent', 'pipeline1');
    console.assert(edgeFail === false, 'addEdge from nonexistent fails');
    console.log('  ✓ addEdge fail');

    // Duplicate edge (no duplicates)
    layer.addEdge('pipeline1', 'doc.edit');
    console.assert(n1After.edges.filter(e => e === 'doc.edit').length === 1, 'no duplicate edges');
    console.log('  ✓ no duplicate edges');

    // Apply rule — consolidate
    const r1 = layer.applyRule('pipeline1', { strategy: 'consolidate' });
    console.assert(r1.strategy === 'consolidate', 'rule strategy');
    console.assert(r1.output['action'] === 'consolidated', 'rule output action');
    console.assert(r1.output['nodeExists'] === true, 'rule nodeExists');
    const n1Ruled = layer.getNode('pipeline1')!;
    console.assert(n1Ruled.data['_lastRule'] === 'consolidate', 'node marked with last rule');
    console.log('  ✓ applyRule consolidate');

    // Apply rule — prune
    const r2 = layer.applyRule('pipeline1', { strategy: 'prune' });
    console.assert(r2.output['action'] === 'pruned', 'prune action');
    console.log('  ✓ applyRule prune');

    // Apply rule — merge
    const r3 = layer.applyRule('pipeline1', { strategy: 'merge' });
    console.assert(r3.output['action'] === 'merged', 'merge action');
    console.log('  ✓ applyRule merge');

    // Apply rule — custom
    const r4 = layer.applyRule('pipeline1', { strategy: 'custom' });
    console.assert(r4.output['action'] === 'custom', 'custom action');
    console.log('  ✓ applyRule custom');

    // Apply rule on missing node
    const r5 = layer.applyRule('missing', { strategy: 'consolidate' });
    console.assert(r5.output['nodeExists'] === false, 'rule on missing node');
    console.log('  ✓ applyRule missing node');

    // getRules
    const rules = layer.getRules();
    console.assert(rules.length === 5, `getRules count is ${rules.length}, expected 5`);
    console.log('  ✓ getRules');

    // getConsolidatedState
    const state = layer.getConsolidatedState();
    console.assert(state.nodeCount === 3, 'consolidated nodeCount');
    console.assert(state.ruleCount === 5, 'consolidated ruleCount');
    console.assert(Object.keys(state.nodes).length === 3, 'consolidated nodes obj');
    console.assert(state.recentRules.length === 5, 'consolidated recentRules');
    console.log('  ✓ getConsolidatedState');

    // getHistory
    const hist = layer.getHistory();
    console.assert(hist.length > 0, 'history has entries');
    console.log('  ✓ getHistory');

    // Clear
    layer.clear();
    console.assert(layer.size() === 0, 'clear empties nodes');
    console.assert(layer.getRules().length === 0, 'clear empties rules');
    console.assert(layer.getHistory().length === 0, 'clear empties history');
    console.log('  ✓ clear');

    console.log('── all grey_persistence tests passed ──');
}
