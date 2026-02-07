// ─── Grey++ Narrative Primitives ─────────────────────────────────────────────
// Dispatcher for NarrativeNode: routes proof, story, and log requests
// through narrative coordinators.  All stubs — no external NLP, no real AI.
//
// Self-test:  npx tsx src/ast/grey_narrative.ts --test
//
// Extend by plugging real narrative generation backends in a future stage.

import type { NarrativeNode, ASTNode } from './grey_ast.js';

// ─── Result types ───────────────────────────────────────────────────────────

export interface NarrativeReport {
    _type: 'NarrativeReport';
    primitive: string;
    inputSummary: string;
    outputSummary: string;
    fragments: string[];
    meta: { stub: true };
}

export interface NarrativeProofResult {
    kind: 'proof';
    source: string;
    format: string;
    status: 'generated';
    proofPoint: string;
    message: string;
    report: NarrativeReport;
    timestamp: string;
}

export interface NarrativeStoryResult {
    kind: 'story';
    theme: string;
    scope: string;
    status: 'drafted';
    storyFragment: string;
    message: string;
    report: NarrativeReport;
    timestamp: string;
}

export interface NarrativeLogResult {
    kind: 'log';
    stage: string;
    detail: string;
    status: 'logged';
    logEntry: string;
    message: string;
    report: NarrativeReport;
    timestamp: string;
}

export type NarrativeResult = NarrativeProofResult | NarrativeStoryResult | NarrativeLogResult;

// ─── Dispatcher ─────────────────────────────────────────────────────────────

export function dispatchNarrative(
    node: NarrativeNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        metaCoordinator?: { getEntries: Function; generateReport: Function; toProofPoints?: Function };
        storyFabric?: { addFragment: Function; buildNarrativeGraph: Function; toRecruiterNarrative?: Function };
    } = {},
): NarrativeResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }

    switch (node.primitive) {
        case 'proof': return narrativeProof(flags, ctx);
        case 'story': return narrativeStory(flags, ctx);
        case 'log': return narrativeLog(flags, ctx);
        default:
            throw new Error(`[narrative] Unknown primitive: ${node.primitive}`);
    }
}

// ─── narrative.proof ────────────────────────────────────────────────────────

function narrativeProof(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): NarrativeProofResult {
    const source = String(flags['source'] ?? 'default');
    const format = String(flags['format'] ?? 'bullet');

    const proofPoint = `Proof point generated from "${source}": Demonstrated capability in ${source} — measurable, verifiable, recruiter-ready.`;

    const fragments = [
        `Source: ${source}`,
        `Built and validated ${source} primitives end-to-end`,
        `Achieved zero-dependency modular architecture`,
        `Proof format: ${format}`,
    ];

    if (ctx.metaCoordinator?.toProofPoints) {
        const points = ctx.metaCoordinator.toProofPoints(source);
        fragments.push(...(Array.isArray(points) ? points : []));
    }
    if (ctx.metaCoordinator?.getEntries) {
        const entries = ctx.metaCoordinator.getEntries(source);
        if (Array.isArray(entries) && entries.length > 0) {
            fragments.push(`Meta entries from "${source}": ${entries.length} collected`);
        }
    }

    const report: NarrativeReport = {
        _type: 'NarrativeReport',
        primitive: 'proof',
        inputSummary: `source="${source}", format="${format}"`,
        outputSummary: proofPoint,
        fragments,
        meta: { stub: true },
    };

    return {
        kind: 'proof',
        source,
        format,
        status: 'generated',
        proofPoint,
        message: `Proof point generated from "${source}"`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── narrative.story ────────────────────────────────────────────────────────

function narrativeStory(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): NarrativeStoryResult {
    const theme = String(flags['theme'] ?? 'default');
    const scope = String(flags['scope'] ?? 'civilization');

    const storyFragment = `Civilization story drafted: "${theme}" — Grey++ evolved through ${scope}-scale orchestration, composing primitives into a self-describing, self-optimizing meta-language.`;

    const fragments = [
        `Theme: ${theme}`,
        `Scope: ${scope}`,
        `Grey++ composes 17+ primitive kinds into unified execution`,
        `Each stage extends the narrative graph with new capabilities`,
    ];

    if (ctx.storyFabric?.buildNarrativeGraph) {
        ctx.storyFabric.buildNarrativeGraph([theme], { scope });
    }
    if (ctx.storyFabric?.addFragment) {
        ctx.storyFabric.addFragment(theme, storyFragment);
    }

    const report: NarrativeReport = {
        _type: 'NarrativeReport',
        primitive: 'story',
        inputSummary: `theme="${theme}", scope="${scope}"`,
        outputSummary: storyFragment,
        fragments,
        meta: { stub: true },
    };

    return {
        kind: 'story',
        theme,
        scope,
        status: 'drafted',
        storyFragment,
        message: `Civilization story drafted: "${theme}"`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── narrative.log ──────────────────────────────────────────────────────────

function narrativeLog(
    flags: Record<string, unknown>,
    ctx: Record<string, any>,
): NarrativeLogResult {
    const stage = String(flags['stage'] ?? 'unknown');
    const detail = String(flags['detail'] ?? 'evolution checkpoint');

    const logEntry = `Evolution logged: ${stage} — ${detail}`;

    const fragments = [
        `Stage: ${stage}`,
        `Detail: ${detail}`,
        `Timestamp: ${new Date().toISOString()}`,
    ];

    if (ctx.metaCoordinator?.getEntries) {
        const allEntries = ctx.metaCoordinator.getEntries();
        if (Array.isArray(allEntries)) {
            fragments.push(`Total meta entries at ${stage}: ${allEntries.length}`);
        }
    }
    if (ctx.storyFabric?.addFragment) {
        ctx.storyFabric.addFragment(`log:${stage}`, logEntry);
    }

    const report: NarrativeReport = {
        _type: 'NarrativeReport',
        primitive: 'log',
        inputSummary: `stage="${stage}", detail="${detail}"`,
        outputSummary: logEntry,
        fragments,
        meta: { stub: true },
    };

    return {
        kind: 'log',
        stage,
        detail,
        status: 'logged',
        logEntry,
        message: `Evolution logged: ${stage}`,
        report,
        timestamp: new Date().toISOString(),
    };
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_narrative') && process.argv.includes('--test')) {
    console.log('── grey_narrative self-test ──');

    const fakeNode = (primitive: string, flags: Record<string, string | number>): NarrativeNode => ({
        kind: 'Narrative' as any,
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

    // narrative.proof
    const p1 = dispatchNarrative(fakeNode('proof', { source: 'artifact.rarity', format: 'bullet' }), resolve);
    console.assert(p1.kind === 'proof', 'proof kind');
    console.assert(p1.status === 'generated', 'proof status');
    console.assert(p1.source === 'artifact.rarity', 'proof source');
    console.assert(p1.format === 'bullet', 'proof format');
    console.assert(p1.proofPoint.includes('artifact.rarity'), 'proof point content');
    console.assert(p1.report._type === 'NarrativeReport', 'proof report type');
    console.assert(p1.report.fragments.length >= 3, 'proof fragments');
    console.log('  ✓ narrative.proof');

    // narrative.story
    const s1 = dispatchNarrative(fakeNode('story', { theme: 'civilization-tier orchestration', scope: 'global' }), resolve);
    console.assert(s1.kind === 'story', 'story kind');
    console.assert(s1.status === 'drafted', 'story status');
    console.assert(s1.theme === 'civilization-tier orchestration', 'story theme');
    console.assert(s1.scope === 'global', 'story scope');
    console.assert(s1.storyFragment.includes('civilization-tier orchestration'), 'story fragment content');
    console.assert(s1.report.fragments.length >= 3, 'story fragments');
    console.log('  ✓ narrative.story');

    // narrative.log
    const l1 = dispatchNarrative(fakeNode('log', { stage: 'Stage 16', detail: 'Memory primitives complete' }), resolve);
    console.assert(l1.kind === 'log', 'log kind');
    console.assert(l1.status === 'logged', 'log status');
    console.assert(l1.stage === 'Stage 16', 'log stage');
    console.assert(l1.detail === 'Memory primitives complete', 'log detail');
    console.assert(l1.logEntry.includes('Stage 16'), 'log entry content');
    console.assert(l1.report.fragments.length >= 3, 'log fragments');
    console.log('  ✓ narrative.log');

    // Unknown primitive
    try {
        dispatchNarrative(fakeNode('unknown', {}), resolve);
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown error');
        console.log('  ✓ unknown primitive throws');
    }

    // With mock coordinators
    const collectLog: any[] = [];
    const fragmentLog: any[] = [];
    const graphLog: any[] = [];
    const mockMeta = {
        getEntries: (sub?: string) => sub ? [{ subsystem: sub }] : [{ subsystem: 'a' }, { subsystem: 'b' }],
        generateReport: () => ({}),
        toProofPoints: (source: string) => [`Proven: ${source} works`],
    };
    const mockStory = {
        addFragment: (key: string, frag: string) => fragmentLog.push({ key, frag }),
        buildNarrativeGraph: (nodes: string[], opts: any) => graphLog.push({ nodes, opts }),
    };

    // proof with coordinators
    const p2 = dispatchNarrative(fakeNode('proof', { source: 'pipeline' }), resolve, { metaCoordinator: mockMeta });
    console.assert(p2.report.fragments.some((f: string) => f.includes('Proven')), 'proof enriched by meta');
    console.assert(p2.report.fragments.some((f: string) => f.includes('Meta entries')), 'proof meta entries');
    console.log('  ✓ proof with meta coordinator');

    // story with coordinators
    dispatchNarrative(fakeNode('story', { theme: 't1' }), resolve, { storyFabric: mockStory });
    console.assert(graphLog.length === 1, 'story graph built');
    console.assert(fragmentLog.length === 1, 'story fragment added');
    console.log('  ✓ story with story fabric');

    // log with coordinators
    const prevFragCount = fragmentLog.length;
    dispatchNarrative(fakeNode('log', { stage: 'S17' }), resolve, { metaCoordinator: mockMeta, storyFabric: mockStory });
    console.assert(fragmentLog.length === prevFragCount + 1, 'log fragment added');
    console.log('  ✓ log with coordinators');

    // Default flags
    const d1 = dispatchNarrative(fakeNode('proof', {}), resolve);
    console.assert(d1.source === 'default', 'default source');
    console.assert(d1.format === 'bullet', 'default format');
    console.log('  ✓ default flags');

    console.log('── all grey_narrative tests passed ──');
}
