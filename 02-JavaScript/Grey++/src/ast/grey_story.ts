// ─── Grey++ Story Fabric Layer ───────────────────────────────────────────────
// Represents Grey++ evolution as a narrative graph.  Applies stub storytelling
// rules (compress achievement, map rarity to proof) and outputs recruiter-ready
// narrative fragments.  All in-memory — no external NLP, no real AI.
//
// Self-test:  npx tsx src/ast/grey_story.ts --test
//
// Extend by plugging real NLP/generation backends in a future stage.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface StoryFragment {
    id: string;
    theme: string;
    content: string;
    createdAt: string;
    tags: string[];
}

export interface NarrativeGraphNode {
    id: string;
    label: string;
    edges: string[];
    metadata: Record<string, unknown>;
}

export interface RecruiterNarrative {
    title: string;
    fragments: StoryFragment[];
    graph: NarrativeGraphNode[];
    proofSummary: string;
    timestamp: string;
}

export interface StoryRule {
    name: string;
    action: 'compress' | 'expand' | 'map' | (string & {});
    source: string;
    target: string;
    appliedAt?: string;
}

// ─── StoryFabric ────────────────────────────────────────────────────────────

export class StoryFabric {
    private fragments: Map<string, StoryFragment> = new Map();
    private graph: Map<string, NarrativeGraphNode> = new Map();
    private rules: StoryRule[] = [];
    private history: { action: string; id: string; timestamp: string }[] = [];

    /** Add a narrative fragment keyed by theme or id. */
    addFragment(theme: string, content: string, tags: string[] = []): StoryFragment {
        const id = `frag-${this.fragments.size + 1}`;
        const now = new Date().toISOString();
        const fragment: StoryFragment = {
            id,
            theme,
            content,
            createdAt: now,
            tags: [...tags, theme],
        };
        this.fragments.set(id, fragment);
        this.history.push({ action: 'addFragment', id, timestamp: now });
        return fragment;
    }

    /** Get all fragments, optionally filtered by theme. */
    getFragments(theme?: string): StoryFragment[] {
        const all = [...this.fragments.values()];
        return theme ? all.filter(f => f.theme === theme) : all;
    }

    /** Build a narrative graph from a list of node labels. */
    buildNarrativeGraph(labels: string[], opts: Record<string, unknown> = {}): NarrativeGraphNode[] {
        const nodes: NarrativeGraphNode[] = [];
        const now = new Date().toISOString();

        for (let i = 0; i < labels.length; i++) {
            const id = `node-${this.graph.size + 1}`;
            const edges = i + 1 < labels.length ? [`node-${this.graph.size + 2}`] : [];
            const node: NarrativeGraphNode = {
                id,
                label: labels[i],
                edges,
                metadata: { ...opts, position: i },
            };
            this.graph.set(id, node);
            nodes.push(node);
            this.history.push({ action: 'addGraphNode', id, timestamp: now });
        }

        return nodes;
    }

    /** Apply a storytelling rule (stub). */
    applyRule(rule: Omit<StoryRule, 'appliedAt'>): StoryRule {
        const now = new Date().toISOString();
        const applied: StoryRule = { ...rule, appliedAt: now };
        this.rules.push(applied);
        this.history.push({ action: `rule:${rule.action}`, id: rule.name, timestamp: now });

        // Stub rule application — generate a fragment based on the rule
        switch (rule.action) {
            case 'compress':
                this.addFragment(rule.target, `[Compressed] Achievement from "${rule.source}" distilled into "${rule.target}"`, ['compressed']);
                break;
            case 'expand':
                this.addFragment(rule.target, `[Expanded] Detailed narrative of "${rule.source}" for "${rule.target}"`, ['expanded']);
                break;
            case 'map':
                this.addFragment(rule.target, `[Mapped] "${rule.source}" rarity/density mapped to proof for "${rule.target}"`, ['mapped']);
                break;
            default:
                this.addFragment(rule.target, `[${rule.action}] Rule applied: "${rule.source}" → "${rule.target}"`, [rule.action]);
                break;
        }

        return applied;
    }

    /** Generate a recruiter-ready narrative from all collected fragments and graph. */
    toRecruiterNarrative(title?: string): RecruiterNarrative {
        const fragments = [...this.fragments.values()];
        const graphNodes = [...this.graph.values()];

        const proofLines = fragments.map(f => `• ${f.content}`);
        const proofSummary = proofLines.length > 0
            ? `${fragments.length} narrative fragment(s):\n${proofLines.join('\n')}`
            : 'No narrative fragments collected yet.';

        return {
            title: title ?? `Grey++ Narrative — ${fragments.length} fragment(s), ${graphNodes.length} graph node(s)`,
            fragments,
            graph: graphNodes,
            proofSummary,
            timestamp: new Date().toISOString(),
        };
    }

    /** Get all applied rules. */
    getRules(): StoryRule[] {
        return [...this.rules];
    }

    /** Get action history. */
    getHistory(): { action: string; id: string; timestamp: string }[] {
        return [...this.history];
    }

    /** Get graph node count. */
    graphSize(): number {
        return this.graph.size;
    }

    /** Get fragment count. */
    fragmentCount(): number {
        return this.fragments.size;
    }

    /** Clear all state. */
    clear(): void {
        this.fragments.clear();
        this.graph.clear();
        this.rules = [];
        this.history = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_story') && process.argv.includes('--test')) {
    console.log('── grey_story self-test ──');

    const fabric = new StoryFabric();

    // addFragment
    const f1 = fabric.addFragment('pipeline', 'Built modular pipeline system', ['engineering']);
    console.assert(f1.id === 'frag-1', 'fragment id');
    console.assert(f1.theme === 'pipeline', 'fragment theme');
    console.assert(f1.content.includes('pipeline'), 'fragment content');
    console.assert(f1.tags.includes('engineering'), 'custom tag');
    console.assert(f1.tags.includes('pipeline'), 'theme auto-tag');
    console.assert(fabric.fragmentCount() === 1, 'fragment count');
    console.log('  ✓ addFragment');

    // getFragments — all
    fabric.addFragment('security', 'Implemented trust boundaries');
    const all = fabric.getFragments();
    console.assert(all.length === 2, 'all fragments');
    console.log('  ✓ getFragments (all)');

    // getFragments — filtered
    const secFrags = fabric.getFragments('security');
    console.assert(secFrags.length === 1, 'filtered fragments');
    console.assert(secFrags[0].theme === 'security', 'filtered theme');
    console.log('  ✓ getFragments (filtered)');

    // buildNarrativeGraph
    const nodes = fabric.buildNarrativeGraph(['stage-1', 'stage-2', 'stage-3'], { scope: 'evolution' });
    console.assert(nodes.length === 3, 'graph nodes created');
    console.assert(nodes[0].label === 'stage-1', 'first node label');
    console.assert(nodes[0].edges.length === 1, 'first node has edge');
    console.assert(nodes[2].edges.length === 0, 'last node no edges');
    console.assert(fabric.graphSize() === 3, 'graph size');
    console.log('  ✓ buildNarrativeGraph');

    // applyRule — compress
    const r1 = fabric.applyRule({ name: 'compress-pipeline', action: 'compress', source: 'pipeline-details', target: 'pipeline-summary' });
    console.assert(r1.appliedAt !== undefined, 'rule appliedAt');
    console.assert(r1.action === 'compress', 'rule action');
    console.assert(fabric.fragmentCount() === 3, 'compress generated fragment');
    const compFrag = fabric.getFragments('pipeline-summary');
    console.assert(compFrag.length === 1, 'compressed fragment exists');
    console.assert(compFrag[0].content.includes('[Compressed]'), 'compressed content');
    console.log('  ✓ applyRule compress');

    // applyRule — expand
    fabric.applyRule({ name: 'expand-security', action: 'expand', source: 'trust-overview', target: 'trust-detailed' });
    const expFrag = fabric.getFragments('trust-detailed');
    console.assert(expFrag[0].content.includes('[Expanded]'), 'expanded content');
    console.log('  ✓ applyRule expand');

    // applyRule — map
    fabric.applyRule({ name: 'map-rarity', action: 'map', source: 'artifact.rarity', target: 'recruiter-proof' });
    const mapFrag = fabric.getFragments('recruiter-proof');
    console.assert(mapFrag[0].content.includes('[Mapped]'), 'mapped content');
    console.log('  ✓ applyRule map');

    // applyRule — custom
    fabric.applyRule({ name: 'custom-rule', action: 'transform', source: 'a', target: 'b' });
    console.assert(fabric.getRules().length === 4, 'rules count');
    console.log('  ✓ applyRule custom');

    // toRecruiterNarrative
    const narrative = fabric.toRecruiterNarrative();
    console.assert(narrative.fragments.length === fabric.fragmentCount(), 'narrative fragment count');
    console.assert(narrative.graph.length === 3, 'narrative graph count');
    console.assert(narrative.proofSummary.includes('narrative fragment'), 'proof summary content');
    console.assert(narrative.title.includes('fragment'), 'narrative title');
    console.log('  ✓ toRecruiterNarrative');

    // toRecruiterNarrative — custom title
    const narrative2 = fabric.toRecruiterNarrative('My Custom Title');
    console.assert(narrative2.title === 'My Custom Title', 'custom title');
    console.log('  ✓ toRecruiterNarrative custom title');

    // getHistory
    const hist = fabric.getHistory();
    console.assert(hist.length > 0, 'history has entries');
    console.log('  ✓ getHistory');

    // clear
    fabric.clear();
    console.assert(fabric.fragmentCount() === 0, 'clear fragments');
    console.assert(fabric.graphSize() === 0, 'clear graph');
    console.assert(fabric.getRules().length === 0, 'clear rules');
    console.assert(fabric.getHistory().length === 0, 'clear history');
    console.log('  ✓ clear');

    // Empty narrative
    const emptyNarrative = fabric.toRecruiterNarrative();
    console.assert(emptyNarrative.fragments.length === 0, 'empty fragments');
    console.assert(emptyNarrative.proofSummary.includes('No narrative'), 'empty proof summary');
    console.log('  ✓ empty narrative');

    console.log('── all grey_story tests passed ──');
}
