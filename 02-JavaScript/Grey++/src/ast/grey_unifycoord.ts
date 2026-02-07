// ─── Grey++ Meta-Language Coordinator ────────────────────────────────────────
// UnifyCoordinator maps all primitives into a single universal syntax,
// handles translation between legacy and unified constructs, and outputs
// consolidated unified language reports.
//
// Self-test:  npx tsx src/ast/grey_unifycoord.ts --test
//
// No external dependencies — pure in-memory arrays/maps.

// ─── Types ──────────────────────────────────────────────────────────────────

export interface PrimitiveMapping {
    name: string;
    type: string;
    unifiedForm: string;
    timestamp: string;
}

export interface TranslationEntry {
    source: string;
    sourceType: string;
    target: string;
    targetType: string;
    timestamp: string;
}

export interface UnifiedLanguageReport {
    _type: 'UnifiedLanguageReport';
    totalMappings: number;
    totalTranslations: number;
    mappings: PrimitiveMapping[];
    translations: TranslationEntry[];
    universalGrammar: string[];
    summary: string;
    meta: { stub: true };
}

// ── UnifyCoordinator ────────────────────────────────────────────────────────

export class UnifyCoordinator {
    private mappings: PrimitiveMapping[] = [];
    private translations: TranslationEntry[] = [];

    /** Map a primitive into the universal syntax. */
    mapPrimitive(name: string, type: string): PrimitiveMapping {
        const mapping: PrimitiveMapping = {
            name,
            type,
            unifiedForm: `unify.${type}(${name})`,
            timestamp: new Date().toISOString(),
        };
        this.mappings.push(mapping);
        return mapping;
    }

    /** Translate between legacy construct and unified construct. */
    translate(source: string, targetType: string): TranslationEntry {
        const entry: TranslationEntry = {
            source,
            sourceType: 'legacy',
            target: `unify.${targetType}(${source})`,
            targetType,
            timestamp: new Date().toISOString(),
        };
        this.translations.push(entry);
        return entry;
    }

    /** Get all mappings, optionally filtered by type. */
    getMappings(type?: string): PrimitiveMapping[] {
        if (type) return this.mappings.filter(m => m.type === type);
        return [...this.mappings];
    }

    /** Get all translations, optionally filtered by target type. */
    getTranslations(targetType?: string): TranslationEntry[] {
        if (targetType) return this.translations.filter(t => t.targetType === targetType);
        return [...this.translations];
    }

    /** Generate a consolidated unified language report. */
    generateReport(): UnifiedLanguageReport {
        const universalGrammar = [
            ...new Set(this.mappings.map(m => m.unifiedForm)),
        ];

        const summary = this.mappings.length === 0 && this.translations.length === 0
            ? 'No primitives mapped yet — run unify.* commands first.'
            : `Unified language report: ${this.mappings.length} mapping(s), ${this.translations.length} translation(s) across [${[...new Set(this.mappings.map(m => m.type))].join(', ')}].`;

        return {
            _type: 'UnifiedLanguageReport',
            totalMappings: this.mappings.length,
            totalTranslations: this.translations.length,
            mappings: [...this.mappings],
            translations: [...this.translations],
            universalGrammar,
            summary,
            meta: { stub: true },
        };
    }

    /** Total number of mappings registered. */
    mappingCount(): number {
        return this.mappings.length;
    }

    /** Total number of translations registered. */
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

if (process.argv[1]?.includes('grey_unifycoord') && process.argv.includes('--test')) {
    console.log('── grey_unifycoord self-test ──');

    const coord = new UnifyCoordinator();

    // mapPrimitive
    const m1 = coord.mapPrimitive('pipeline', 'syntax');
    console.assert(m1.name === 'pipeline', 'map name');
    console.assert(m1.type === 'syntax', 'map type');
    console.assert(m1.unifiedForm === 'unify.syntax(pipeline)', 'map unified form');
    console.assert(coord.mappingCount() === 1, 'mapping count 1');
    console.log('  ✓ mapPrimitive');

    // More mappings
    coord.mapPrimitive('orchestration', 'syntax');
    coord.mapPrimitive('memory', 'exec');
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

    // translate
    const t1 = coord.translate('artifact.rarity', 'semantic');
    console.assert(t1.source === 'artifact.rarity', 'translate source');
    console.assert(t1.targetType === 'semantic', 'translate target type');
    console.assert(t1.target === 'unify.semantic(artifact.rarity)', 'translate target');
    console.assert(coord.translationCount() === 1, 'translation count');
    console.log('  ✓ translate');

    // More translations
    coord.translate('doc.edit', 'syntax');
    console.assert(coord.translationCount() === 2, 'translation count 2');
    console.log('  ✓ multiple translations');

    // getTranslations (all)
    const allTranslations = coord.getTranslations();
    console.assert(allTranslations.length === 2, 'all translations');
    console.log('  ✓ getTranslations (all)');

    // getTranslations (filtered)
    const semOnly = coord.getTranslations('semantic');
    console.assert(semOnly.length === 1, 'semantic translations');
    console.log('  ✓ getTranslations (filtered)');

    // generateReport
    const report = coord.generateReport();
    console.assert(report._type === 'UnifiedLanguageReport', 'report type');
    console.assert(report.totalMappings === 3, 'report mappings');
    console.assert(report.totalTranslations === 2, 'report translations');
    console.assert(report.universalGrammar.length === 3, `grammar entries: ${report.universalGrammar.length}`);
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
    console.assert(emptyReport.summary.includes('No primitives mapped'), 'empty summary');
    console.log('  ✓ empty report');

    console.log('── all grey_unifycoord tests passed ──');
}
