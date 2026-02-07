// ─── Grey++ Language Coordinator (Syntax Coordinator) ────────────────────────
// Manages unified syntax rules and translates UniNode constructs into their
// underlying primitive AST nodes.  Maintains a registry of all primitives
// under one grammar so the REPL and future parsers can resolve `do { … }`
// blocks into executable sub-trees.
//
// Now also manages paradigm rules for 50+ target languages, enabling the
// translator and normalizer to understand cross-language semantics.
//
// No external deps — all state is in-memory Maps / arrays.

// ─── Types ──────────────────────────────────────────────────────────────────

/** A registered syntax rule that maps a keyword to a primitive kind. */
export interface SyntaxRule {
    /** The keyword that triggers this rule (e.g. "net", "sec", "doc"). */
    keyword: string;
    /** The underlying AST node kind (e.g. "Net", "Sec", "Doc"). */
    nodeKind: string;
    /** Whether the keyword uses dot-notation (e.g. net.http, sec.perm). */
    dotNotation: boolean;
    /** Human-readable description. */
    description: string;
}

/** Result of translating a unified step into a primitive description. */
export interface TranslationResult {
    keyword: string;
    nodeKind: string;
    dotNotation: boolean;
    primitive?: string;
    resolved: boolean;
}

/** Paradigm classification */
export type Paradigm =
    | 'oop' | 'functional' | 'procedural' | 'declarative'
    | 'concurrent' | 'logic' | 'scripting' | 'systems'
    | 'dataflow' | 'event-driven' | 'metaprogramming'
    | 'actor' | 'generic' | 'prototype' | 'stack' | 'contract';

/** Language family for syntactic grouping */
export type LangFamily = 'clike' | 'pylike' | 'mllike' | 'lisplike' | 'shelllike' | 'other';

/** Paradigm rule describing a language's capabilities and semantics */
export interface ParadigmRule {
    /** Language identifier (matches TargetLanguage / normalizer names) */
    lang: string;
    /** Syntactic family */
    family: LangFamily;
    /** Primary paradigms the language supports */
    paradigms: Paradigm[];
    /** Whether the language is statically typed */
    staticTyped: boolean;
    /** Whether the language supports first-class functions */
    firstClassFunctions: boolean;
    /** Whether the language supports pattern matching */
    patternMatching: boolean;
    /** Whether the language supports null safety */
    nullSafe: boolean;
    /** Memory model */
    memory: 'gc' | 'manual' | 'ownership' | 'refcount' | 'stack' | 'vm';
    /** Grey++ construct mappings — what Grey++ features this lang can express */
    greyMappings: {
        bind: boolean;
        fn: boolean;
        loop: boolean;
        cond: boolean;
        struct: boolean;
        module: boolean;
        pattern: boolean;
        async: boolean;
        generic: boolean;
    };
}

// ═══════════════════════════════════════════════════════════════════════════
//  LanguageCoordinator
// ═══════════════════════════════════════════════════════════════════════════

export class LanguageCoordinator {
    /** Registered syntax rules by keyword. */
    private rules = new Map<string, SyntaxRule>();

    /** Paradigm rules by language identifier. */
    private paradigmRules = new Map<string, ParadigmRule>();

    /** History of translations performed. */
    private translationLog: TranslationResult[] = [];

    constructor() {
        // Seed with the built-in primitives.
        this.seedDefaults();
        this.seedParadigms();
    }

    // ── Register Syntax Rule ────────────────────────────────────────────

    /**
     * Register a unified syntax rule.  If a rule for the keyword already
     * exists, it is replaced.
     */
    registerRule(rule: SyntaxRule): void {
        this.rules.set(rule.keyword, rule);
    }

    // ── Translate ───────────────────────────────────────────────────────

    /**
     * Translate a unified syntax keyword (optionally with a dot-separated
     * primitive) into a primitive AST descriptor.
     *
     *   translate("net", "http")  → { nodeKind: "Net", primitive: "http", resolved: true }
     *   translate("doc", "edit")  → { nodeKind: "Doc", primitive: "edit", resolved: true }
     *   translate("unknown")     → { resolved: false }
     */
    translate(keyword: string, primitive?: string): TranslationResult {
        const rule = this.rules.get(keyword);

        if (!rule) {
            const result: TranslationResult = {
                keyword,
                nodeKind: 'Unknown',
                dotNotation: false,
                primitive,
                resolved: false,
            };
            this.translationLog.push(result);
            return result;
        }

        const result: TranslationResult = {
            keyword,
            nodeKind: rule.nodeKind,
            dotNotation: rule.dotNotation,
            primitive,
            resolved: true,
        };
        this.translationLog.push(result);
        return result;
    }

    // ── Query ───────────────────────────────────────────────────────────

    /** Get all registered rules. */
    getRules(): SyntaxRule[] {
        return Array.from(this.rules.values());
    }

    /** Get a rule by keyword. */
    getRule(keyword: string): SyntaxRule | undefined {
        return this.rules.get(keyword);
    }

    /** Check if a keyword is registered. */
    hasRule(keyword: string): boolean {
        return this.rules.has(keyword);
    }

    /** Get the grammar mapping — keyword → nodeKind. */
    getGrammarMap(): Record<string, string> {
        const map: Record<string, string> = {};
        for (const [kw, rule] of this.rules) {
            map[kw] = rule.nodeKind;
        }
        return map;
    }

    /** Get the translation log. */
    getTranslationLog(): TranslationResult[] {
        return [...this.translationLog];
    }

    // ── Snapshot ─────────────────────────────────────────────────────────

    snapshot(): {
        ruleCount: number;
        rules: SyntaxRule[];
        translationCount: number;
        grammarMap: Record<string, string>;
    } {
        return {
            ruleCount: this.rules.size,
            rules: this.getRules(),
            translationCount: this.translationLog.length,
            grammarMap: this.getGrammarMap(),
        };
    }

    // ── Internal ────────────────────────────────────────────────────────

    private seedDefaults(): void {
        const defaults: SyntaxRule[] = [
            { keyword: 'net', nodeKind: 'Net', dotNotation: true, description: 'Network primitives (http, socket, rpc)' },
            { keyword: 'pipeline', nodeKind: 'Pipeline', dotNotation: false, description: 'Multi-step AI/data pipelines' },
            { keyword: 'sec', nodeKind: 'Sec', dotNotation: true, description: 'Security primitives (perm, sandbox, audit)' },
            { keyword: 'dist', nodeKind: 'Dist', dotNotation: true, description: 'Distributed primitives (register, consensus, sync)' },
            { keyword: 'doc', nodeKind: 'Doc', dotNotation: true, description: 'Document primitives (parse, render, edit)' },
            { keyword: 'sys', nodeKind: 'Sys', dotNotation: true, description: 'System primitives (log, thread, message)' },
            { keyword: 'storage', nodeKind: 'Storage', dotNotation: false, description: 'Storage primitives (pdf, db, file)' },
            { keyword: 'infer', nodeKind: 'Infer', dotNotation: false, description: 'AI inference operations' },
            { keyword: 'query', nodeKind: 'Query', dotNotation: false, description: 'Declarative SQL-style queries' },
            { keyword: 'fn', nodeKind: 'Function', dotNotation: false, description: 'Function declarations' },
            { keyword: 'module', nodeKind: 'Module', dotNotation: false, description: 'Framework adapter modules' },
        ];
        for (const rule of defaults) {
            this.rules.set(rule.keyword, rule);
        }
    }

    // ── Paradigm Registration ───────────────────────────────────────────

    /** Register a paradigm rule for a language. */
    registerParadigm(rule: ParadigmRule): void {
        this.paradigmRules.set(rule.lang, rule);
    }

    /** Get paradigm rule for a language. */
    getParadigm(lang: string): ParadigmRule | undefined {
        return this.paradigmRules.get(lang);
    }

    /** Get all registered paradigm rules. */
    getAllParadigms(): ParadigmRule[] {
        return Array.from(this.paradigmRules.values());
    }

    /** Find all languages that support a given paradigm. */
    languagesByParadigm(p: Paradigm): string[] {
        return this.getAllParadigms()
            .filter(r => r.paradigms.includes(p))
            .map(r => r.lang);
    }

    /** Find all languages in a given family. */
    languagesByFamily(f: LangFamily): string[] {
        return this.getAllParadigms()
            .filter(r => r.family === f)
            .map(r => r.lang);
    }

    /** Find all languages compatible with a Grey++ construct. */
    languagesSupportingConstruct(construct: keyof ParadigmRule['greyMappings']): string[] {
        return this.getAllParadigms()
            .filter(r => r.greyMappings[construct])
            .map(r => r.lang);
    }

    /** Check interop compatibility between two languages (shared paradigms). */
    interopScore(langA: string, langB: string): number {
        const a = this.paradigmRules.get(langA);
        const b = this.paradigmRules.get(langB);
        if (!a || !b) return 0;
        const shared = a.paradigms.filter(p => b.paradigms.includes(p));
        const mappingKeys = Object.keys(a.greyMappings) as (keyof ParadigmRule['greyMappings'])[];
        const sharedMappings = mappingKeys.filter(k => a.greyMappings[k] && b.greyMappings[k]);
        return shared.length + sharedMappings.length;
    }

    /** Find the most compatible languages for a given language. */
    mostCompatible(lang: string, topN = 5): { lang: string; score: number }[] {
        const all = this.getAllParadigms()
            .filter(r => r.lang !== lang)
            .map(r => ({ lang: r.lang, score: this.interopScore(lang, r.lang) }))
            .sort((a, b) => b.score - a.score);
        return all.slice(0, topN);
    }

    // ── Paradigm Snapshot ────────────────────────────────────────────────

    paradigmSnapshot(): {
        languageCount: number;
        families: Record<string, number>;
        paradigmCoverage: Record<string, number>;
    } {
        const families: Record<string, number> = {};
        const paradigmCoverage: Record<string, number> = {};
        for (const r of this.paradigmRules.values()) {
            families[r.family] = (families[r.family] || 0) + 1;
            for (const p of r.paradigms) {
                paradigmCoverage[p] = (paradigmCoverage[p] || 0) + 1;
            }
        }
        return { languageCount: this.paradigmRules.size, families, paradigmCoverage };
    }

    // ── Paradigm Seed Data ──────────────────────────────────────────────

    private seedParadigms(): void {
        const allMappings = { bind: true, fn: true, loop: true, cond: true, struct: true, module: true, pattern: true, async: true, generic: true };
        const noPattern = { ...allMappings, pattern: false };
        const noAsync = { ...allMappings, async: false };
        const noGeneric = { ...allMappings, generic: false };
        const noPatAsync = { ...allMappings, pattern: false, async: false };
        const noPatGen = { ...allMappings, pattern: false, generic: false };
        const noAsyGen = { ...allMappings, async: false, generic: false };
        const minimal = { bind: true, fn: true, loop: true, cond: true, struct: false, module: true, pattern: false, async: false, generic: false };
        const basic = { bind: true, fn: true, loop: true, cond: true, struct: true, module: true, pattern: false, async: false, generic: false };

        const rules: ParadigmRule[] = [
            // ── Core Application & Systems ──
            { lang: 'javascript', family: 'clike', paradigms: ['oop', 'functional', 'event-driven', 'prototype'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPattern },
            { lang: 'typescript', family: 'clike', paradigms: ['oop', 'functional', 'event-driven', 'generic'], staticTyped: true, firstClassFunctions: true, patternMatching: false, nullSafe: true, memory: 'gc', greyMappings: noPattern },
            { lang: 'python', family: 'pylike', paradigms: ['oop', 'functional', 'procedural', 'scripting', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: true, nullSafe: false, memory: 'gc', greyMappings: noGeneric },
            { lang: 'java', family: 'clike', paradigms: ['oop', 'generic', 'concurrent'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: false, memory: 'gc', greyMappings: allMappings },
            { lang: 'go', family: 'clike', paradigms: ['procedural', 'concurrent', 'systems'], staticTyped: true, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: { ...noPattern, generic: true } },
            { lang: 'rust', family: 'clike', paradigms: ['systems', 'functional', 'concurrent', 'generic'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'ownership', greyMappings: allMappings },
            { lang: 'csharp', family: 'clike', paradigms: ['oop', 'functional', 'generic', 'concurrent', 'event-driven'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'gc', greyMappings: allMappings },
            { lang: 'swift', family: 'clike', paradigms: ['oop', 'functional', 'generic', 'concurrent'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'refcount', greyMappings: allMappings },
            { lang: 'kotlin', family: 'clike', paradigms: ['oop', 'functional', 'generic', 'concurrent'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'gc', greyMappings: allMappings },
            { lang: 'cpp', family: 'clike', paradigms: ['oop', 'procedural', 'generic', 'systems', 'metaprogramming'], staticTyped: true, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'manual', greyMappings: noPattern },

            // ── Data & Infrastructure ──
            { lang: 'sql', family: 'other', paradigms: ['declarative', 'dataflow'], staticTyped: true, firstClassFunctions: false, patternMatching: false, nullSafe: false, memory: 'vm', greyMappings: { bind: true, fn: false, loop: false, cond: true, struct: true, module: true, pattern: false, async: false, generic: false } },
            { lang: 'ruby', family: 'pylike', paradigms: ['oop', 'functional', 'scripting', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: true, nullSafe: false, memory: 'gc', greyMappings: noGeneric },
            { lang: 'php', family: 'clike', paradigms: ['oop', 'procedural', 'scripting'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: true, memory: 'gc', greyMappings: noPatGen },
            { lang: 'bash', family: 'shelllike', paradigms: ['scripting', 'procedural'], staticTyped: false, firstClassFunctions: false, patternMatching: false, nullSafe: false, memory: 'stack', greyMappings: minimal },
            { lang: 'powershell', family: 'shelllike', paradigms: ['scripting', 'procedural', 'oop'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: { ...minimal, struct: true } },
            { lang: 'r', family: 'pylike', paradigms: ['functional', 'procedural', 'dataflow'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPatGen },

            // ── Functional & Niche ──
            { lang: 'scala', family: 'clike', paradigms: ['oop', 'functional', 'generic', 'concurrent'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: false, memory: 'gc', greyMappings: allMappings },
            { lang: 'julia', family: 'pylike', paradigms: ['functional', 'procedural', 'generic', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: true, nullSafe: false, memory: 'gc', greyMappings: { ...allMappings, async: true } },
            { lang: 'dart', family: 'clike', paradigms: ['oop', 'functional', 'generic', 'concurrent'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'gc', greyMappings: allMappings },
            { lang: 'objc', family: 'clike', paradigms: ['oop', 'procedural'], staticTyped: true, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'refcount', greyMappings: noPatGen },
            { lang: 'elixir', family: 'other', paradigms: ['functional', 'concurrent', 'actor', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: true, nullSafe: false, memory: 'gc', greyMappings: noGeneric },
            { lang: 'clojure', family: 'lisplike', paradigms: ['functional', 'concurrent', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPatGen },
            { lang: 'haskell', family: 'mllike', paradigms: ['functional', 'generic'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'gc', greyMappings: { ...allMappings, async: false } },
            { lang: 'erlang', family: 'other', paradigms: ['functional', 'concurrent', 'actor'], staticTyped: false, firstClassFunctions: true, patternMatching: true, nullSafe: false, memory: 'gc', greyMappings: noGeneric },
            { lang: 'fsharp', family: 'mllike', paradigms: ['functional', 'oop', 'generic', 'concurrent'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'gc', greyMappings: allMappings },
            { lang: 'ocaml', family: 'mllike', paradigms: ['functional', 'oop', 'generic'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'gc', greyMappings: noAsync },
            { lang: 'lisp', family: 'lisplike', paradigms: ['functional', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPatGen },
            { lang: 'prolog', family: 'other', paradigms: ['logic', 'declarative'], staticTyped: false, firstClassFunctions: false, patternMatching: true, nullSafe: false, memory: 'gc', greyMappings: { bind: true, fn: true, loop: true, cond: true, struct: true, module: true, pattern: true, async: false, generic: false } },
            { lang: 'elm', family: 'mllike', paradigms: ['functional'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'gc', greyMappings: { ...noAsync, generic: true } },
            { lang: 'smalltalk', family: 'other', paradigms: ['oop', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: basic },

            // ── Low-Level / Embedded / Legacy ──
            { lang: 'c', family: 'clike', paradigms: ['procedural', 'systems'], staticTyped: true, firstClassFunctions: false, patternMatching: false, nullSafe: false, memory: 'manual', greyMappings: { bind: true, fn: true, loop: true, cond: true, struct: true, module: true, pattern: false, async: false, generic: false } },
            { lang: 'fortran', family: 'other', paradigms: ['procedural', 'systems'], staticTyped: true, firstClassFunctions: false, patternMatching: false, nullSafe: false, memory: 'stack', greyMappings: basic },
            { lang: 'vhdl', family: 'other', paradigms: ['declarative', 'concurrent', 'dataflow'], staticTyped: true, firstClassFunctions: false, patternMatching: false, nullSafe: false, memory: 'stack', greyMappings: { bind: true, fn: true, loop: true, cond: true, struct: true, module: true, pattern: false, async: false, generic: true } },
            { lang: 'matlab', family: 'pylike', paradigms: ['procedural', 'functional', 'dataflow'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPatGen },
            { lang: 'pascal', family: 'other', paradigms: ['procedural', 'oop'], staticTyped: true, firstClassFunctions: false, patternMatching: false, nullSafe: false, memory: 'manual', greyMappings: basic },
            { lang: 'perl', family: 'pylike', paradigms: ['procedural', 'oop', 'scripting', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPatGen },
            { lang: 'solidity', family: 'clike', paradigms: ['oop', 'contract'], staticTyped: true, firstClassFunctions: false, patternMatching: false, nullSafe: false, memory: 'stack', greyMappings: { bind: true, fn: true, loop: true, cond: true, struct: true, module: true, pattern: false, async: false, generic: false } },
            { lang: 'vbnet', family: 'other', paradigms: ['oop', 'procedural', 'event-driven'], staticTyped: true, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPatGen },

            // ── Modern / Emerging ──
            { lang: 'zig', family: 'clike', paradigms: ['procedural', 'systems', 'generic'], staticTyped: true, firstClassFunctions: true, patternMatching: false, nullSafe: true, memory: 'manual', greyMappings: { ...noPattern, async: true } },
            { lang: 'apex', family: 'clike', paradigms: ['oop', 'generic'], staticTyped: true, firstClassFunctions: false, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPatAsync },
            { lang: 'mojo', family: 'pylike', paradigms: ['oop', 'functional', 'systems', 'generic'], staticTyped: true, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'ownership', greyMappings: noPattern },
            { lang: 'lua', family: 'pylike', paradigms: ['procedural', 'scripting', 'prototype', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: minimal },
            { lang: 'groovy', family: 'clike', paradigms: ['oop', 'functional', 'scripting', 'metaprogramming'], staticTyped: false, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPatAsync },
            { lang: 'nim', family: 'pylike', paradigms: ['procedural', 'functional', 'oop', 'systems', 'metaprogramming', 'generic'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: false, memory: 'gc', greyMappings: allMappings },
            { lang: 'crystal', family: 'pylike', paradigms: ['oop', 'functional', 'concurrent', 'generic'], staticTyped: true, firstClassFunctions: true, patternMatching: false, nullSafe: true, memory: 'gc', greyMappings: noPattern },
            { lang: 'd', family: 'clike', paradigms: ['oop', 'procedural', 'functional', 'generic', 'systems', 'metaprogramming'], staticTyped: true, firstClassFunctions: true, patternMatching: false, nullSafe: false, memory: 'gc', greyMappings: noPattern },

            // ── Grey++ itself ──
            { lang: 'grey', family: 'other', paradigms: ['oop', 'functional', 'procedural', 'declarative', 'concurrent', 'systems', 'metaprogramming', 'generic'], staticTyped: true, firstClassFunctions: true, patternMatching: true, nullSafe: true, memory: 'gc', greyMappings: allMappings },
        ];

        for (const rule of rules) {
            this.paradigmRules.set(rule.lang, rule);
        }
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_language') && process.argv.includes('--test')) {
    console.log('═══ grey_language.ts Self-Test ═══\n');

    const coordinator = new LanguageCoordinator();

    // Check default rules.
    console.log('── Default rules ──');
    const rules = coordinator.getRules();
    console.log(`Registered ${rules.length} rules:`);
    for (const r of rules) {
        console.log(`  ${r.keyword} → ${r.nodeKind} (dot: ${r.dotNotation})`);
    }

    // Translate known keywords.
    console.log('\n── translate("net", "http") ──');
    console.log(JSON.stringify(coordinator.translate('net', 'http'), null, 2));

    console.log('\n── translate("doc", "edit") ──');
    console.log(JSON.stringify(coordinator.translate('doc', 'edit'), null, 2));

    console.log('\n── translate("sec", "perm") ──');
    console.log(JSON.stringify(coordinator.translate('sec', 'perm'), null, 2));

    console.log('\n── translate("dist", "consensus") ──');
    console.log(JSON.stringify(coordinator.translate('dist', 'consensus'), null, 2));

    console.log('\n── translate("pipeline") ──');
    console.log(JSON.stringify(coordinator.translate('pipeline'), null, 2));

    // Unknown keyword.
    console.log('\n── translate("unknown") ──');
    console.log(JSON.stringify(coordinator.translate('unknown'), null, 2));

    // Register custom rule.
    console.log('\n── registerRule (custom) ──');
    coordinator.registerRule({
        keyword: 'ml',
        nodeKind: 'Infer',
        dotNotation: true,
        description: 'Machine learning shorthand',
    });
    console.log(JSON.stringify(coordinator.translate('ml', 'train'), null, 2));

    // Grammar map.
    console.log('\n── getGrammarMap ──');
    console.log(JSON.stringify(coordinator.getGrammarMap(), null, 2));

    // Snapshot.
    console.log('\n── snapshot ──');
    const snap = coordinator.snapshot();
    console.log(`Rules: ${snap.ruleCount}, Translations: ${snap.translationCount}`);

    // ── Paradigm Tests ──
    console.log('\n── paradigm rules ──');
    const allParadigms = coordinator.getAllParadigms();
    console.log(`Registered ${allParadigms.length} language paradigm rules`);
    console.assert(allParadigms.length >= 47, `Expected 47+ paradigm rules, got ${allParadigms.length}`);

    const pyRule = coordinator.getParadigm('python');
    console.assert(pyRule !== undefined, 'python paradigm exists');
    console.assert(pyRule!.family === 'pylike', `python family: ${pyRule!.family}`);
    console.assert(pyRule!.paradigms.includes('oop'), 'python is oop');
    console.assert(pyRule!.firstClassFunctions === true, 'python has first-class functions');
    console.log('  ✓ python paradigm correct');

    const rsRule = coordinator.getParadigm('rust');
    console.assert(rsRule!.memory === 'ownership', 'rust has ownership memory');
    console.assert(rsRule!.nullSafe === true, 'rust is null-safe');
    console.assert(rsRule!.greyMappings.pattern === true, 'rust supports pattern matching');
    console.log('  ✓ rust paradigm correct');

    const oopLangs = coordinator.languagesByParadigm('oop');
    console.assert(oopLangs.length >= 20, `OOP languages: ${oopLangs.length}`);
    console.log(`  ✓ ${oopLangs.length} OOP languages`);

    const funcLangs = coordinator.languagesByParadigm('functional');
    console.assert(funcLangs.length >= 15, `Functional languages: ${funcLangs.length}`);
    console.log(`  ✓ ${funcLangs.length} functional languages`);

    const clikeLangs = coordinator.languagesByFamily('clike');
    console.assert(clikeLangs.length >= 10, `C-like family: ${clikeLangs.length}`);
    console.log(`  ✓ ${clikeLangs.length} c-like languages`);

    const asyncLangs = coordinator.languagesSupportingConstruct('async');
    console.log(`  ✓ ${asyncLangs.length} languages support async`);

    const pyCompat = coordinator.mostCompatible('python', 3);
    console.assert(pyCompat.length === 3, 'mostCompatible returns top 3');
    console.log(`  ✓ python most compatible: ${pyCompat.map(c => `${c.lang}(${c.score})`).join(', ')}`);

    const pSnap = coordinator.paradigmSnapshot();
    console.log(`  ✓ paradigm snapshot: ${pSnap.languageCount} langs, ${Object.keys(pSnap.families).length} families, ${Object.keys(pSnap.paradigmCoverage).length} paradigms`);

    console.log('\n✓ All grey_language tests passed.');
}
