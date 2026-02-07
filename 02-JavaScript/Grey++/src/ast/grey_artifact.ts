// ─────────────────────────────────────────────────────────────────────────────
//  grey_artifact.ts  –  Stage 12 · Artifact Primitives Dispatcher
// ─────────────────────────────────────────────────────────────────────────────
//  Dispatches artifact.density | artifact.rarity | artifact.proof commands
//  to dedicated executor functions. The runtime delegates to dispatchArtifact()
//  which routes to the correct handler based on the primitive string.
//
//  Self-test:  npx tsx src/ast/grey_artifact.ts --test
// ─────────────────────────────────────────────────────────────────────────────

import type { ArtifactNode, ASTNode } from './grey_ast.js';

// ── Result types ────────────────────────────────────────────────────────────

export interface DensityResult {
    kind: 'density';
    target: string;
    density: number;
    totalTokens: number;
    uniqueTokens: number;
    ratio: number;
    timestamp: string;
}

export interface RarityResult {
    kind: 'rarity';
    target: string;
    band: string;
    score: number;
    factors: Record<string, number>;
    timestamp: string;
}

export interface ProofResult {
    kind: 'proof';
    target: string;
    hash: string;
    snapshot: Record<string, unknown>;
    verified: boolean;
    timestamp: string;
}

export type ArtifactResult = DensityResult | RarityResult | ProofResult;

// ── Dispatcher ──────────────────────────────────────────────────────────────

export function dispatchArtifact(
    node: ArtifactNode,
    resolve: (n: ASTNode) => unknown,
    ctx: {
        artifactCoordinator?: { track: Function; calculateDensity: Function; mapRarityBands: Function; snapshot: Function };
        rarityLayer?: { assignBand: Function; generateScore: Function; generateReport: Function };
    }
): ArtifactResult {
    const flags: Record<string, unknown> = {};
    if (node.flags) {
        for (const [k, v] of Object.entries(node.flags)) {
            flags[k] = resolve(v);
        }
    }
    const target = (flags['target'] as string) ?? 'default';

    switch (node.primitive) {
        case 'density': return artifactDensity(target, flags, ctx);
        case 'rarity': return artifactRarity(target, flags, ctx);
        case 'proof': return artifactProof(target, flags, ctx);
        default:
            throw new Error(`[artifact] Unknown primitive: ${node.primitive}`);
    }
}

// ── density ─────────────────────────────────────────────────────────────────

function artifactDensity(
    target: string,
    flags: Record<string, unknown>,
    ctx: Record<string, any>
): DensityResult {
    const coordinator = ctx.artifactCoordinator;
    if (coordinator) {
        const d = coordinator.calculateDensity(target);
        return {
            kind: 'density',
            target,
            density: d.density,
            totalTokens: d.totalTokens,
            uniqueTokens: d.uniqueTokens,
            ratio: d.ratio,
            timestamp: new Date().toISOString(),
        };
    }
    // Fallback: return zeroed result
    return {
        kind: 'density',
        target,
        density: 0,
        totalTokens: 0,
        uniqueTokens: 0,
        ratio: 0,
        timestamp: new Date().toISOString(),
    };
}

// ── rarity ──────────────────────────────────────────────────────────────────

function artifactRarity(
    target: string,
    flags: Record<string, unknown>,
    ctx: Record<string, any>
): RarityResult {
    const rarity = ctx.rarityLayer;
    if (rarity) {
        const report = rarity.generateReport(target);
        return {
            kind: 'rarity',
            target,
            band: report.band,
            score: report.score,
            factors: report.factors ?? {},
            timestamp: new Date().toISOString(),
        };
    }
    return {
        kind: 'rarity',
        target,
        band: 'common',
        score: 0,
        factors: {},
        timestamp: new Date().toISOString(),
    };
}

// ── proof ───────────────────────────────────────────────────────────────────

function artifactProof(
    target: string,
    flags: Record<string, unknown>,
    ctx: Record<string, any>
): ProofResult {
    const coordinator = ctx.artifactCoordinator;
    if (coordinator) {
        const snap = coordinator.snapshot(target);
        // Simple hash: JSON → base-36 checksum
        const raw = JSON.stringify(snap);
        let hash = 0;
        for (let i = 0; i < raw.length; i++) {
            hash = ((hash << 5) - hash + raw.charCodeAt(i)) | 0;
        }
        return {
            kind: 'proof',
            target,
            hash: Math.abs(hash).toString(36),
            snapshot: snap,
            verified: true,
            timestamp: new Date().toISOString(),
        };
    }
    return {
        kind: 'proof',
        target,
        hash: '0',
        snapshot: {},
        verified: false,
        timestamp: new Date().toISOString(),
    };
}

// ── Self-test ───────────────────────────────────────────────────────────────

if (process.argv.includes('--test')) {
    console.log('── grey_artifact self-test ──');

    const fakeNode = (primitive: string, targetVal: string): ArtifactNode => ({
        kind: 'Artifact' as any,
        primitive,
        args: [],
        flags: {
            target: { kind: 'StringLit' as any, value: targetVal } as any,
        },
    });

    const resolve = (n: ASTNode) => (n as any).value;

    // Test without coordinators (fallback paths)
    const d = dispatchArtifact(fakeNode('density', 'pipe1'), resolve, {});
    console.assert(d.kind === 'density', 'density kind');
    console.assert(d.target === 'pipe1', 'density target');
    console.assert(d.density === 0, 'density fallback');
    console.log('  ✓ artifact.density (fallback)');

    const r = dispatchArtifact(fakeNode('rarity', 'doc.edit'), resolve, {});
    console.assert(r.kind === 'rarity', 'rarity kind');
    console.assert(r.band === 'common', 'rarity fallback band');
    console.log('  ✓ artifact.rarity (fallback)');

    const p = dispatchArtifact(fakeNode('proof', 'dist.consensus'), resolve, {});
    console.assert(p.kind === 'proof', 'proof kind');
    console.assert(p.verified === false, 'proof fallback unverified');
    console.log('  ✓ artifact.proof (fallback)');

    // Test with mock coordinators
    const mockCoord = {
        track: () => { },
        calculateDensity: (t: string) => ({ density: 0.75, totalTokens: 100, uniqueTokens: 75, ratio: 0.75 }),
        mapRarityBands: () => ({}),
        snapshot: (t: string) => ({ target: t, nodes: 5, edges: 3 }),
    };
    const mockRarity = {
        assignBand: () => 'rare',
        generateScore: () => 42,
        generateReport: (t: string) => ({ band: 'legendary', score: 88, factors: { complexity: 0.9, novelty: 0.8 } }),
    };

    const d2 = dispatchArtifact(fakeNode('density', 'pipeline1'), resolve, { artifactCoordinator: mockCoord });
    console.assert(d2.density === 0.75, 'density with coordinator');
    console.assert(d2.totalTokens === 100, 'density totalTokens');
    console.log('  ✓ artifact.density (coordinator)');

    const r2 = dispatchArtifact(fakeNode('rarity', 'doc.edit'), resolve, { rarityLayer: mockRarity });
    console.assert(r2.band === 'legendary', 'rarity with layer');
    console.assert(r2.score === 88, 'rarity score');
    console.log('  ✓ artifact.rarity (layer)');

    const p2 = dispatchArtifact(fakeNode('proof', 'dist.consensus'), resolve, { artifactCoordinator: mockCoord });
    console.assert(p2.verified === true, 'proof verified with coordinator');
    console.assert(typeof p2.hash === 'string' && p2.hash !== '0', 'proof hash generated');
    console.log('  ✓ artifact.proof (coordinator)');

    // Unknown primitive
    try {
        dispatchArtifact(fakeNode('unknown', 'x'), resolve, {});
        console.assert(false, 'should throw');
    } catch (e: any) {
        console.assert(e.message.includes('Unknown primitive'), 'unknown primitive error');
        console.log('  ✓ unknown primitive throws');
    }

    console.log('── all grey_artifact tests passed ──');
}
