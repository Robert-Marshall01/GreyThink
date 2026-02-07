// ─── Grey++ Compile Coordinator ──────────────────────────────────────────────
// CompileCoordinator maps syntax sources to intermediate representations,
// handles backend translation, and outputs consolidated compilation reports.
//
// Self-test:  npx tsx src/ast/grey_compilecoord.ts --test
//
// No external dependencies — pure in-memory arrays/maps.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface SyntaxMapping {
    name: string;
    type: string;
    irForm: string;
    timestamp: string;
}

export interface BackendTranslation {
    target: string;
    optimize: boolean;
    outputForm: string;
    timestamp: string;
}

export interface CompilationReport {
    _type: 'CompilationReport';
    totalMappings: number;
    totalTranslations: number;
    mappings: SyntaxMapping[];
    translations: BackendTranslation[];
    irConstructs: string[];
    summary: string;
    meta: { stub: true };
}

// ── CompileCoordinator ──────────────────────────────────────────────────────

export class CompileCoordinator {
    private mappings: SyntaxMapping[] = [];
    private translations: BackendTranslation[] = [];

    /** Map a syntax source into an intermediate representation. */
    mapSyntax(name: string, type: string): SyntaxMapping {
        const mapping: SyntaxMapping = {
            name,
            type,
            irForm: `compiler.ir(${name}:${type})`,
            timestamp: new Date().toISOString(),
        };
        this.mappings.push(mapping);
        return mapping;
    }

    /** Translate to a backend target. */
    translateBackend(target: string, optimize: boolean): BackendTranslation {
        const entry: BackendTranslation = {
            target,
            optimize,
            outputForm: `compiler.backend(${target}${optimize ? ':optimized' : ''})`,
            timestamp: new Date().toISOString(),
        };
        this.translations.push(entry);
        return entry;
    }

    /** Get all syntax mappings, optionally filtered by type. */
    getMappings(type?: string): SyntaxMapping[] {
        if (type) return this.mappings.filter(m => m.type === type);
        return [...this.mappings];
    }

    /** Get all backend translations, optionally filtered by target. */
    getTranslations(target?: string): BackendTranslation[] {
        if (target) return this.translations.filter(t => t.target === target);
        return [...this.translations];
    }

    /** Generate a consolidated compilation report. */
    generateReport(): CompilationReport {
        const irConstructs = [
            ...new Set(this.mappings.map(m => m.irForm)),
        ];

        const summary = this.mappings.length === 0 && this.translations.length === 0
            ? 'No sources compiled yet — run compiler.* commands first.'
            : `Compilation report: ${this.mappings.length} mapping(s), ${this.translations.length} translation(s) across [${[...new Set(this.mappings.map(m => m.type))].join(', ')}].`;

        return {
            _type: 'CompilationReport',
            totalMappings: this.mappings.length,
            totalTranslations: this.translations.length,
            mappings: [...this.mappings],
            translations: [...this.translations],
            irConstructs,
            summary,
            meta: { stub: true },
        };
    }

    /** Total number of syntax mappings registered. */
    mappingCount(): number {
        return this.mappings.length;
    }

    /** Total number of backend translations registered. */
    translationCount(): number {
        return this.translations.length;
    }

    /** Clear all state. */
    clear(): void {
        this.mappings = [];
        this.translations = [];
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_compilecoord') && process.argv.includes('--test')) {
    console.log('── grey_compilecoord self-test ──');

    const coord = new CompileCoordinator();

    // mapSyntax
    const m1 = coord.mapSyntax('pipeline', 'syntax');
    console.assert(m1.name === 'pipeline', 'map name');
    console.assert(m1.type === 'syntax', 'map type');
    console.assert(m1.irForm === 'compiler.ir(pipeline:syntax)', 'map ir form');
    console.assert(coord.mappingCount() === 1, 'mapping count 1');
    console.log('  ✓ mapSyntax');

    // More mappings
    coord.mapSyntax('orchestration', 'syntax');
    coord.mapSyntax('memory', 'exec');
    console.assert(coord.mappingCount() === 3, 'mapping count 3');
    console.log('  ✓ multiple mappings');

    // getMappings (all)
    const allMappings = coord.getMappings();
    console.assert(allMappings.length === 3, 'all mappings');
    console.log('  ✓ getMappings (all)');

    // getMappings (filtered)
    const syntaxOnly = coord.getMappings('syntax');
    console.assert(syntaxOnly.length === 2, `syntax mappings: ${syntaxOnly.length}`);
    const execOnly = coord.getMappings('exec');
    console.assert(execOnly.length === 1, 'exec mappings');
    console.log('  ✓ getMappings (filtered)');

    // translateBackend
    const t1 = coord.translateBackend('javascript', true);
    console.assert(t1.target === 'javascript', 'translate target');
    console.assert(t1.optimize === true, 'translate optimize');
    console.assert(t1.outputForm === 'compiler.backend(javascript:optimized)', 'translate output form');
    console.assert(coord.translationCount() === 1, 'translation count');
    console.log('  ✓ translateBackend');

    // More translations
    coord.translateBackend('python', false);
    console.assert(coord.translationCount() === 2, 'translation count 2');
    console.log('  ✓ multiple translations');

    // getTranslations (all)
    const allTranslations = coord.getTranslations();
    console.assert(allTranslations.length === 2, 'all translations');
    console.log('  ✓ getTranslations (all)');

    // getTranslations (filtered)
    const jsOnly = coord.getTranslations('javascript');
    console.assert(jsOnly.length === 1, 'js translations');
    console.log('  ✓ getTranslations (filtered)');

    // generateReport
    const report = coord.generateReport();
    console.assert(report._type === 'CompilationReport', 'report type');
    console.assert(report.totalMappings === 3, 'report mappings');
    console.assert(report.totalTranslations === 2, 'report translations');
    console.assert(report.irConstructs.length === 3, `ir entries: ${report.irConstructs.length}`);
    console.assert(report.summary.includes('3 mapping(s)'), 'report summary');
    console.log('  ✓ generateReport');

    // clear
    coord.clear();
    console.assert(coord.mappingCount() === 0, 'clear mappings');
    console.assert(coord.translationCount() === 0, 'clear translations');
    console.log('  ✓ clear');

    // empty report
    const emptyReport = coord.generateReport();
    console.assert(emptyReport.totalMappings === 0, 'empty mappings');
    console.assert(emptyReport.summary.includes('No sources compiled'), 'empty summary');
    console.log('  ✓ empty report');

    console.log('── all grey_compilecoord tests passed ──');
}
