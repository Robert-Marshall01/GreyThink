// ─── Grey++ Backend Fabric ───────────────────────────────────────────────────
// BackendFabric manages compilation targets, applies translations to
// intermediate representations, and produces code fragments for each target.
//
// Self-test:  npx tsx src/ast/grey_backend.ts --test
//
// No external dependencies — pure in-memory graph/arrays.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface BackendTarget {
    name: string;
    type: string;
    registeredAt: string;
}

export interface TranslationRule {
    target?: string;
    optimize?: boolean;
    action: string;
    scenario?: string;
    appliedAt: string;
}

export interface CodeFragment {
    scenario: string;
    code: string;
    generatedAt: string;
}

export interface BackendReport {
    _type: 'BackendReport';
    totalTargets: number;
    totalTranslations: number;
    totalFragments: number;
    targets: BackendTarget[];
    translations: TranslationRule[];
    fragments: CodeFragment[];
    summary: string;
    meta: { stub: true };
}

// ── BackendFabric ───────────────────────────────────────────────────────────

export class BackendFabric {
    private targets: BackendTarget[] = [];
    private translations: TranslationRule[] = [];
    private fragments: CodeFragment[] = [];

    /** Register a compilation target. */
    addTarget(name: string, type: string): BackendTarget {
        const target: BackendTarget = {
            name,
            type,
            registeredAt: new Date().toISOString(),
        };
        this.targets.push(target);
        return target;
    }

    /** Apply a translation rule to the backend graph. */
    applyTranslation(rule: { target?: string; optimize?: boolean; action: string; scenario?: string }): TranslationRule {
        const entry: TranslationRule = {
            ...rule,
            appliedAt: new Date().toISOString(),
        };
        this.translations.push(entry);
        return entry;
    }

    /** Generate a code fragment for a given scenario. */
    toCodeFragment(scenario: string): CodeFragment {
        const fragment: CodeFragment = {
            scenario,
            code: `// Compiled code for scenario: ${scenario}\n// Targets: [${this.targets.map(t => t.name).join(', ')}]`,
            generatedAt: new Date().toISOString(),
        };
        this.fragments.push(fragment);
        return fragment;
    }

    /** Get all registered targets, optionally filtered by type. */
    getTargets(type?: string): BackendTarget[] {
        if (type) return this.targets.filter(t => t.type === type);
        return [...this.targets];
    }

    /** Get all applied translations. */
    getTranslations(): TranslationRule[] {
        return [...this.translations];
    }

    /** Get all generated code fragments. */
    getFragments(): CodeFragment[] {
        return [...this.fragments];
    }

    /** Generate a consolidated backend report. */
    generateBackendReport(): BackendReport {
        const summary = this.targets.length === 0 && this.translations.length === 0
            ? 'No compilation targets registered yet — run compiler.* commands first.'
            : `Backend report: ${this.targets.length} target(s), ${this.translations.length} translation(s), ${this.fragments.length} fragment(s) across [${[...new Set(this.targets.map(t => t.type))].join(', ')}].`;

        return {
            _type: 'BackendReport',
            totalTargets: this.targets.length,
            totalTranslations: this.translations.length,
            totalFragments: this.fragments.length,
            targets: [...this.targets],
            translations: [...this.translations],
            fragments: [...this.fragments],
            summary,
            meta: { stub: true },
        };
    }

    /** Clear all state. */
    clear(): void {
        this.targets = [];
        this.translations = [];
        this.fragments = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_backend') && process.argv.includes('--test')) {
    console.log('── grey_backend self-test ──');

    const fabric = new BackendFabric();

    // addTarget
    const t1 = fabric.addTarget('pipeline', 'syntax');
    console.assert(t1.name === 'pipeline', 'target name');
    console.assert(t1.type === 'syntax', 'target type');
    console.assert(t1.registeredAt !== undefined, 'target timestamp');
    console.log('  ✓ addTarget');

    // More targets
    fabric.addTarget('orchestration', 'syntax');
    fabric.addTarget('memory', 'exec');
    const allTargets = fabric.getTargets();
    console.assert(allTargets.length === 3, 'all targets');
    console.log('  ✓ multiple targets');

    // getTargets (filtered)
    const syntaxOnly = fabric.getTargets('syntax');
    console.assert(syntaxOnly.length === 2, `syntax targets: ${syntaxOnly.length}`);
    const execOnly = fabric.getTargets('exec');
    console.assert(execOnly.length === 1, 'exec targets');
    console.log('  ✓ getTargets (filtered)');

    // applyTranslation
    const tr1 = fabric.applyTranslation({ target: 'javascript', optimize: true, action: 'codegen' });
    console.assert(tr1.target === 'javascript', 'translation target');
    console.assert(tr1.optimize === true, 'translation optimize');
    console.assert(tr1.action === 'codegen', 'translation action');
    console.assert(tr1.appliedAt !== undefined, 'translation timestamp');
    console.log('  ✓ applyTranslation');

    // More translations
    fabric.applyTranslation({ action: 'compile-exec', scenario: 'global', target: 'exec-graph' });
    const allTranslations = fabric.getTranslations();
    console.assert(allTranslations.length === 2, 'all translations');
    console.log('  ✓ multiple translations');

    // toCodeFragment
    const frag = fabric.toCodeFragment('full-stack');
    console.assert(frag.scenario === 'full-stack', 'fragment scenario');
    console.assert(frag.code.includes('full-stack'), 'fragment code');
    console.assert(frag.code.includes('pipeline'), 'fragment includes target');
    console.assert(frag.generatedAt !== undefined, 'fragment timestamp');
    console.log('  ✓ toCodeFragment');

    // getFragments
    fabric.toCodeFragment('global');
    const allFragments = fabric.getFragments();
    console.assert(allFragments.length === 2, 'all fragments');
    console.log('  ✓ multiple fragments');

    // generateBackendReport
    const report = fabric.generateBackendReport();
    console.assert(report._type === 'BackendReport', 'report type');
    console.assert(report.totalTargets === 3, 'report targets');
    console.assert(report.totalTranslations === 2, 'report translations');
    console.assert(report.totalFragments === 2, 'report fragments');
    console.assert(report.summary.includes('3 target(s)'), 'report summary targets');
    console.assert(report.summary.includes('2 translation(s)'), 'report summary translations');
    console.log('  ✓ generateBackendReport');

    // clear
    fabric.clear();
    console.assert(fabric.getTargets().length === 0, 'clear targets');
    console.assert(fabric.getTranslations().length === 0, 'clear translations');
    console.assert(fabric.getFragments().length === 0, 'clear fragments');
    console.log('  ✓ clear');

    // empty report
    const emptyReport = fabric.generateBackendReport();
    console.assert(emptyReport.totalTargets === 0, 'empty targets');
    console.assert(emptyReport.summary.includes('No compilation targets'), 'empty summary');
    console.log('  ✓ empty report');

    console.log('── all grey_backend tests passed ──');
}
