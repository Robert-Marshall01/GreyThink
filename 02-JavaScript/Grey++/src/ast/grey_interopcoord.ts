// ─── Grey++ Interop Coordinator ─────────────────────────────────────────────
// Manages cross-backend interoperability bridges, translation rules, and
// consolidated interop reports.  Pure in-memory stub — no external
// dependencies, no real cross-process communication.
//
// Self-test:  npx tsx src/ast/grey_interopcoord.ts --test
//
// Extend by plugging real bridge connectors in a future stage.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface InteropBridge {
    source: string;
    target: string;
    kind: string;
    createdAt: string;
}

export interface TranslationRule {
    source: string;
    target: string;
    method: string;
    createdAt: string;
}

export interface InteropCoordReport {
    _type: 'InteropCoordReport';
    bridgeCount: number;
    translationCount: number;
    bridges: InteropBridge[];
    translations: TranslationRule[];
    meta: { stub: true };
}

// ─── InteropCoordinator ────────────────────────────────────────────────────

export class InteropCoordinator {
    private bridges: InteropBridge[] = [];
    private translations: TranslationRule[] = [];

    /** Register an interoperability bridge between two backends. */
    registerBridge(source: string, target: string, kind: string): InteropBridge {
        const bridge: InteropBridge = {
            source,
            target,
            kind,
            createdAt: new Date().toISOString(),
        };
        this.bridges.push(bridge);
        return bridge;
    }

    /** Add a translation rule for cross-backend method bridging. */
    addTranslation(source: string, target: string, method: string): TranslationRule {
        const rule: TranslationRule = {
            source,
            target,
            method,
            createdAt: new Date().toISOString(),
        };
        this.translations.push(rule);
        return rule;
    }

    /** Get all registered bridges, optionally filtered by kind. */
    getBridges(kind?: string): InteropBridge[] {
        if (kind) return this.bridges.filter(b => b.kind === kind);
        return [...this.bridges];
    }

    /** Get all translation rules, optionally filtered by source. */
    getTranslations(source?: string): TranslationRule[] {
        if (source) return this.translations.filter(t => t.source === source);
        return [...this.translations];
    }

    /** Number of registered bridges. */
    bridgeCount(): number {
        return this.bridges.length;
    }

    /** Number of translation rules. */
    translationCount(): number {
        return this.translations.length;
    }

    /** Generate a consolidated interop report. */
    generateReport(): InteropCoordReport {
        return {
            _type: 'InteropCoordReport',
            bridgeCount: this.bridges.length,
            translationCount: this.translations.length,
            bridges: [...this.bridges],
            translations: [...this.translations],
            meta: { stub: true },
        };
    }

    /** Clear all bridges and translations. */
    clear(): void {
        this.bridges = [];
        this.translations = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_interopcoord') && process.argv.includes('--test')) {
    console.log('── grey_interopcoord self-test ──');

    const coord = new InteropCoordinator();

    // registerBridge
    const b1 = coord.registerBridge('js', 'cpp', 'data');
    console.assert(b1.source === 'js', 'bridge source');
    console.assert(b1.target === 'cpp', 'bridge target');
    console.assert(b1.kind === 'data', 'bridge kind');
    console.assert(coord.bridgeCount() === 1, 'bridge count 1');
    console.log('  ✓ registerBridge');

    // multiple bridges
    coord.registerBridge('cpp', 'riscv', 'call');
    coord.registerBridge('js', 'riscv', 'orch');
    console.assert(coord.bridgeCount() === 3, 'bridge count 3');
    console.log('  ✓ multiple bridges');

    // getBridges (all)
    console.assert(coord.getBridges().length === 3, 'getBridges all');
    console.log('  ✓ getBridges (all)');

    // getBridges (filtered)
    console.assert(coord.getBridges('data').length === 1, 'getBridges data');
    console.assert(coord.getBridges('call').length === 1, 'getBridges call');
    console.assert(coord.getBridges('orch').length === 1, 'getBridges orch');
    console.assert(coord.getBridges('unknown').length === 0, 'getBridges unknown');
    console.log('  ✓ getBridges (filtered)');

    // addTranslation
    const t1 = coord.addTranslation('js', 'cpp', 'compute');
    console.assert(t1.source === 'js', 'translation source');
    console.assert(t1.target === 'cpp', 'translation target');
    console.assert(t1.method === 'compute', 'translation method');
    console.assert(coord.translationCount() === 1, 'translation count 1');
    console.log('  ✓ addTranslation');

    // multiple translations
    coord.addTranslation('cpp', 'riscv', 'execute');
    console.assert(coord.translationCount() === 2, 'translation count 2');
    console.log('  ✓ multiple translations');

    // getTranslations (all)
    console.assert(coord.getTranslations().length === 2, 'getTranslations all');
    console.log('  ✓ getTranslations (all)');

    // getTranslations (filtered)
    console.assert(coord.getTranslations('js').length === 1, 'getTranslations js');
    console.assert(coord.getTranslations('cpp').length === 1, 'getTranslations cpp');
    console.assert(coord.getTranslations('unknown').length === 0, 'getTranslations unknown');
    console.log('  ✓ getTranslations (filtered)');

    // generateReport
    const report = coord.generateReport();
    console.assert(report._type === 'InteropCoordReport', 'report type');
    console.assert(report.bridgeCount === 3, 'report bridge count');
    console.assert(report.translationCount === 2, 'report translation count');
    console.assert(report.bridges.length === 3, 'report bridges array');
    console.assert(report.translations.length === 2, 'report translations array');
    console.assert(report.meta.stub === true, 'report meta stub');
    console.log('  ✓ generateReport');

    // clear
    coord.clear();
    console.assert(coord.bridgeCount() === 0, 'clear bridges');
    console.assert(coord.translationCount() === 0, 'clear translations');
    console.log('  ✓ clear');

    // empty report
    const emptyReport = coord.generateReport();
    console.assert(emptyReport.bridgeCount === 0, 'empty report bridges');
    console.assert(emptyReport.translationCount === 0, 'empty report translations');
    console.log('  ✓ empty report');

    console.log('── all grey_interopcoord tests passed ──');
}
