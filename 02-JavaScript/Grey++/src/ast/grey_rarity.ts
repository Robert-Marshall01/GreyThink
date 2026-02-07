// ─────────────────────────────────────────────────────────────────────────────
//  grey_rarity.ts  –  Stage 12 · RarityLayer
// ─────────────────────────────────────────────────────────────────────────────
//  Assigns rarity bands (common, rare, legendary, myth) to artifact outputs
//  based on configurable scoring factors (complexity, novelty, frequency,
//  depth). Generates composite scores and detailed reports.
//
//  Self-test:  npx tsx src/ast/grey_rarity.ts --test
// ─────────────────────────────────────────────────────────────────────────────

// ── Types ───────────────────────────────────────────────────────────────────

export type RarityBand = 'common' | 'rare' | 'legendary' | 'myth';

export interface RarityFactors {
    complexity: number;   // 0-1  How complex is the output structure
    novelty: number;      // 0-1  How novel/unique are the tokens
    frequency: number;    // 0-1  How frequently the target is accessed (inverted)
    depth: number;        // 0-1  How deep is the call chain or AST depth
}

export interface RarityReport {
    target: string;
    band: RarityBand;
    score: number;
    factors: RarityFactors;
    threshold: { common: number; rare: number; legendary: number; myth: number };
    timestamp: string;
}

export interface RarityConfig {
    weights?: Partial<Record<keyof RarityFactors, number>>;
    thresholds?: { rare?: number; legendary?: number; myth?: number };
}

// ── Defaults ────────────────────────────────────────────────────────────────

const DEFAULT_WEIGHTS: Record<keyof RarityFactors, number> = {
    complexity: 0.3,
    novelty: 0.3,
    frequency: 0.2,
    depth: 0.2,
};

const DEFAULT_THRESHOLDS = {
    common: 0,
    rare: 40,
    legendary: 70,
    myth: 90,
};

// ── RarityLayer ─────────────────────────────────────────────────────────────

export class RarityLayer {
    private weights: Record<keyof RarityFactors, number>;
    private thresholds: { common: number; rare: number; legendary: number; myth: number };
    private history: Map<string, RarityReport[]> = new Map();

    constructor(config?: RarityConfig) {
        this.weights = { ...DEFAULT_WEIGHTS, ...(config?.weights ?? {}) };
        this.thresholds = {
            ...DEFAULT_THRESHOLDS,
            ...(config?.thresholds ?? {}),
        };
    }

    /** Compute a composite score (0-100) from factors using weighted sum. */
    generateScore(factors: Partial<RarityFactors>): number {
        const f: RarityFactors = {
            complexity: factors.complexity ?? 0,
            novelty: factors.novelty ?? 0,
            frequency: factors.frequency ?? 0,
            depth: factors.depth ?? 0,
        };

        let score = 0;
        let totalWeight = 0;
        for (const key of Object.keys(this.weights) as (keyof RarityFactors)[]) {
            score += f[key] * this.weights[key];
            totalWeight += this.weights[key];
        }
        // Normalize to 0-100
        return totalWeight > 0 ? Math.round((score / totalWeight) * 100) : 0;
    }

    /** Assign a rarity band based on a numeric score. */
    assignBand(score: number): RarityBand {
        if (score >= this.thresholds.myth) return 'myth';
        if (score >= this.thresholds.legendary) return 'legendary';
        if (score >= this.thresholds.rare) return 'rare';
        return 'common';
    }

    /** Generate a full rarity report for a target with given factors. */
    generateReport(target: string, factors?: Partial<RarityFactors>): RarityReport {
        const f: RarityFactors = {
            complexity: factors?.complexity ?? 0,
            novelty: factors?.novelty ?? 0,
            frequency: factors?.frequency ?? 0,
            depth: factors?.depth ?? 0,
        };

        const score = this.generateScore(f);
        const band = this.assignBand(score);

        const report: RarityReport = {
            target,
            band,
            score,
            factors: f,
            threshold: { ...this.thresholds },
            timestamp: new Date().toISOString(),
        };

        // Cache in history
        const hist = this.history.get(target) ?? [];
        hist.push(report);
        this.history.set(target, hist);

        return report;
    }

    /** Get historical reports for a target. */
    getHistory(target: string): RarityReport[] {
        return this.history.get(target) ?? [];
    }

    /** List all targets that have been evaluated. */
    targets(): string[] {
        return [...this.history.keys()];
    }

    /** Clear all history. */
    clear(): void {
        this.history.clear();
    }
}

// ── Self-test ───────────────────────────────────────────────────────────────

if (process.argv.includes('--test')) {
    console.log('── grey_rarity self-test ──');

    const layer = new RarityLayer();

    // Score calculation
    const s1 = layer.generateScore({ complexity: 1, novelty: 1, frequency: 1, depth: 1 });
    console.assert(s1 === 100, `max score = 100, got ${s1}`);
    console.log('  ✓ max factors → score 100');

    const s2 = layer.generateScore({ complexity: 0, novelty: 0, frequency: 0, depth: 0 });
    console.assert(s2 === 0, `min score = 0, got ${s2}`);
    console.log('  ✓ zero factors → score 0');

    const s3 = layer.generateScore({ complexity: 0.5, novelty: 0.5, frequency: 0.5, depth: 0.5 });
    console.assert(s3 === 50, `half factors → score 50, got ${s3}`);
    console.log('  ✓ half factors → score 50');

    // Band assignment
    console.assert(layer.assignBand(0) === 'common', 'score 0 → common');
    console.assert(layer.assignBand(39) === 'common', 'score 39 → common');
    console.assert(layer.assignBand(40) === 'rare', 'score 40 → rare');
    console.assert(layer.assignBand(69) === 'rare', 'score 69 → rare');
    console.assert(layer.assignBand(70) === 'legendary', 'score 70 → legendary');
    console.assert(layer.assignBand(89) === 'legendary', 'score 89 → legendary');
    console.assert(layer.assignBand(90) === 'myth', 'score 90 → myth');
    console.assert(layer.assignBand(100) === 'myth', 'score 100 → myth');
    console.log('  ✓ band assignment thresholds');

    // Report generation
    const r = layer.generateReport('pipeline1', { complexity: 0.9, novelty: 0.8, frequency: 0.7, depth: 0.6 });
    console.assert(r.target === 'pipeline1', 'report target');
    console.assert(r.band === 'legendary' || r.band === 'myth', `report band: ${r.band}`);
    console.assert(r.score > 0, 'report score > 0');
    console.assert(r.factors.complexity === 0.9, 'report factors preserved');
    console.log(`  ✓ report: band=${r.band} score=${r.score}`);

    // Fallback report (no factors)
    const r2 = layer.generateReport('empty');
    console.assert(r2.band === 'common', 'no factors → common');
    console.assert(r2.score === 0, 'no factors → score 0');
    console.log('  ✓ report with no factors → common');

    // History
    const hist = layer.getHistory('pipeline1');
    console.assert(hist.length === 1, 'history has 1 entry');
    console.log('  ✓ history tracking');

    // Targets
    const tgts = layer.targets();
    console.assert(tgts.includes('pipeline1'), 'targets includes pipeline1');
    console.assert(tgts.includes('empty'), 'targets includes empty');
    console.log('  ✓ targets listing');

    // Custom config
    const custom = new RarityLayer({
        weights: { complexity: 1, novelty: 0, frequency: 0, depth: 0 },
        thresholds: { rare: 30, legendary: 60, myth: 80 },
    });
    const cs = custom.generateScore({ complexity: 0.5 });
    console.assert(cs === 50, `custom weighted score = 50, got ${cs}`);
    console.assert(custom.assignBand(cs) === 'rare', 'custom threshold: 50 → rare');
    console.log('  ✓ custom weights and thresholds');

    // Clear
    layer.clear();
    console.assert(layer.targets().length === 0, 'clear empties history');
    console.log('  ✓ clear');

    console.log('── all grey_rarity tests passed ──');
}
