// ─────────────────────────────────────────────────────────────────────────────
//  grey_density.ts  –  Stage 12 · ArtifactCoordinator
// ─────────────────────────────────────────────────────────────────────────────
//  Tracks artifact outputs and calculates density metrics. Density measures
//  how information-rich a pipeline's or command's output is by comparing
//  unique tokens against total tokens.
//
//  Self-test:  npx tsx src/ast/grey_density.ts --test
// ─────────────────────────────────────────────────────────────────────────────

// ── Types ───────────────────────────────────────────────────────────────────

export interface ArtifactEntry {
    target: string;
    tokens: string[];
    metadata?: Record<string, unknown>;
    timestamp: string;
}

export interface DensityMetrics {
    density: number;
    totalTokens: number;
    uniqueTokens: number;
    ratio: number;
}

export interface RarityBandMap {
    [target: string]: {
        band: string;
        density: number;
    };
}

export interface ArtifactSnapshot {
    target: string;
    entries: number;
    totalTokens: number;
    uniqueTokens: number;
    density: number;
    metadata: Record<string, unknown>;
}

// ── ArtifactCoordinator ─────────────────────────────────────────────────────

export class ArtifactCoordinator {
    private store: Map<string, ArtifactEntry[]> = new Map();

    /** Track an artifact output by target name. */
    track(target: string, tokens: string[], metadata?: Record<string, unknown>): void {
        const entry: ArtifactEntry = {
            target,
            tokens,
            metadata: metadata ?? {},
            timestamp: new Date().toISOString(),
        };
        const existing = this.store.get(target) ?? [];
        existing.push(entry);
        this.store.set(target, existing);
    }

    /** Calculate density metrics for a target. Density = uniqueTokens / totalTokens. */
    calculateDensity(target: string): DensityMetrics {
        const entries = this.store.get(target);
        if (!entries || entries.length === 0) {
            return { density: 0, totalTokens: 0, uniqueTokens: 0, ratio: 0 };
        }

        const allTokens: string[] = [];
        for (const e of entries) {
            allTokens.push(...e.tokens);
        }
        const total = allTokens.length;
        const unique = new Set(allTokens).size;
        const ratio = total === 0 ? 0 : unique / total;

        return {
            density: ratio,
            totalTokens: total,
            uniqueTokens: unique,
            ratio,
        };
    }

    /** Map all tracked targets to rarity bands based on density thresholds. */
    mapRarityBands(): RarityBandMap {
        const bands: RarityBandMap = {};
        for (const target of this.store.keys()) {
            const d = this.calculateDensity(target);
            let band: string;
            if (d.density >= 0.9) band = 'myth';
            else if (d.density >= 0.75) band = 'legendary';
            else if (d.density >= 0.5) band = 'rare';
            else band = 'common';
            bands[target] = { band, density: d.density };
        }
        return bands;
    }

    /** Take a snapshot of a target's artifact state. */
    snapshot(target: string): ArtifactSnapshot {
        const entries = this.store.get(target);
        if (!entries || entries.length === 0) {
            return {
                target,
                entries: 0,
                totalTokens: 0,
                uniqueTokens: 0,
                density: 0,
                metadata: {},
            };
        }

        const d = this.calculateDensity(target);
        // Merge all metadata
        const merged: Record<string, unknown> = {};
        for (const e of entries) {
            if (e.metadata) Object.assign(merged, e.metadata);
        }

        return {
            target,
            entries: entries.length,
            totalTokens: d.totalTokens,
            uniqueTokens: d.uniqueTokens,
            density: d.density,
            metadata: merged,
        };
    }

    /** List all tracked targets. */
    targets(): string[] {
        return [...this.store.keys()];
    }

    /** Clear all tracked data. */
    clear(): void {
        this.store.clear();
    }
}

// ── Self-test ───────────────────────────────────────────────────────────────

if (process.argv.includes('--test')) {
    console.log('── grey_density self-test ──');

    const coord = new ArtifactCoordinator();

    // Empty target
    const empty = coord.calculateDensity('nonexist');
    console.assert(empty.density === 0, 'empty density is 0');
    console.assert(empty.totalTokens === 0, 'empty totalTokens is 0');
    console.log('  ✓ empty target returns zero density');

    // Track some tokens
    coord.track('pipeline1', ['fn', 'query', 'fn', 'infer', 'query'], { stage: 7 });
    const d1 = coord.calculateDensity('pipeline1');
    console.assert(d1.totalTokens === 5, 'totalTokens = 5');
    console.assert(d1.uniqueTokens === 3, 'uniqueTokens = 3');
    console.assert(d1.density === 3 / 5, `density = ${3 / 5}`);
    console.log('  ✓ single track density calculation');

    // Track more for same target
    coord.track('pipeline1', ['fn', 'sys'], { stage: 8 });
    const d2 = coord.calculateDensity('pipeline1');
    console.assert(d2.totalTokens === 7, 'totalTokens = 7 after second track');
    console.assert(d2.uniqueTokens === 4, 'uniqueTokens = 4 (fn, query, infer, sys)');
    console.log('  ✓ cumulative density calculation');

    // Track another target with all unique tokens
    coord.track('doc.edit', ['a', 'b', 'c', 'd'], { op: 'edit' });
    const d3 = coord.calculateDensity('doc.edit');
    console.assert(d3.density === 1.0, 'all unique -> density 1.0');
    console.log('  ✓ all-unique density = 1.0');

    // Rarity bands
    const bands = coord.mapRarityBands();
    console.assert(bands['doc.edit'].band === 'myth', 'density 1.0 -> myth');
    console.assert(bands['pipeline1'].band === 'rare' || bands['pipeline1'].band === 'common',
        `pipeline1 band: ${bands['pipeline1'].band}`);
    console.log('  ✓ rarity band mapping');

    // Snapshot
    const snap = coord.snapshot('pipeline1');
    console.assert(snap.entries === 2, 'snapshot entries = 2');
    console.assert(snap.totalTokens === 7, 'snapshot totalTokens');
    console.assert(snap.metadata.stage === 8, 'snapshot merged metadata');
    console.log('  ✓ snapshot generation');

    // Empty snapshot
    const emptySnap = coord.snapshot('ghost');
    console.assert(emptySnap.entries === 0, 'empty snapshot entries = 0');
    console.log('  ✓ empty snapshot');

    // Targets list
    const tgts = coord.targets();
    console.assert(tgts.includes('pipeline1'), 'targets includes pipeline1');
    console.assert(tgts.includes('doc.edit'), 'targets includes doc.edit');
    console.log('  ✓ targets listing');

    // Clear
    coord.clear();
    console.assert(coord.targets().length === 0, 'clear empties store');
    console.log('  ✓ clear');

    console.log('── all grey_density tests passed ──');
}
