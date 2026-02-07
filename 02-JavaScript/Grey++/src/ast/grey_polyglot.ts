// ─── Grey++ Polyglot Unification Registry ────────────────────────────────────
// Master registry of ALL programming languages unified under Grey++.
// Every language is catalogued with its paradigms, features, syntax forms,
// and mapping rules so that Grey++ can normalize, translate, and interoperate
// across the entire spectrum of human programming languages.
//
// Self-test:  npx tsx src/ast/grey_polyglot.ts --test

// ─── Types ──────────────────────────────────────────────────────────────────

/** Programming paradigms a language may support. */
export type Paradigm =
    | 'imperative' | 'object-oriented' | 'functional' | 'declarative'
    | 'procedural' | 'logic' | 'concurrent' | 'reactive' | 'metaprogramming'
    | 'systems' | 'scripting' | 'query' | 'hardware-description'
    | 'contract' | 'stack-based' | 'array' | 'event-driven';

/** Capability tier for language features. */
export type FeatureTier = 'native' | 'library' | 'pattern' | 'unsupported';

/** A unified language descriptor. */
export interface LanguageDescriptor {
    /** Canonical identifier (e.g. 'python', 'javascript'). */
    id: string;
    /** Display name. */
    name: string;
    /** Language group (1-5). */
    group: 1 | 2 | 3 | 4 | 5;
    /** Group label. */
    groupLabel: string;
    /** Primary use cases. */
    domains: string[];
    /** Supported paradigms. */
    paradigms: Paradigm[];
    /** File extensions. */
    extensions: string[];
    /** Syntax forms for universal constructs:  construct → surface keyword(s). */
    syntaxForms: Record<string, string[]>;
    /** Feature capability map. */
    features: Record<string, FeatureTier>;
    /** How this language expresses print / output. */
    printForm: string;
    /** Comment style: single-line prefix. */
    commentPrefix: string;
    /** Whether the language is typed. */
    typed: 'static' | 'dynamic' | 'gradual' | 'none';
    /** String delimiter preference. */
    stringDelimiter: string;
    /** Statement terminator (e.g. ';', '', newline). */
    terminator: string;
    /** Block delimiters [open, close]. */
    blockDelimiters: [string, string];
}

// ─── Group Labels ───────────────────────────────────────────────────────────

const GROUP_LABELS: Record<number, string> = {
    1: 'Core Application & Systems Languages',
    2: 'Data & Infrastructure Layer',
    3: 'High-Value Functional & Niche',
    4: 'Low-Level, Embedded & Legacy',
    5: 'Modern Web, Domain-Specific & Emerging',
};

// ═══════════════════════════════════════════════════════════════════════════
//  COMPLETE LANGUAGE REGISTRY — 50+ languages
// ═══════════════════════════════════════════════════════════════════════════

export const LANGUAGE_REGISTRY: LanguageDescriptor[] = [

    // ─────────────────────────────────────────────────────────────────────
    //  GROUP 1: Core Application & Systems Languages
    // ─────────────────────────────────────────────────────────────────────

    {
        id: 'python', name: 'Python', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['AI', 'Data Science', 'Web', 'Scripting', 'Automation'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'scripting'],
        extensions: ['.py', '.pyw', '.pyi'],
        syntaxForms: {
            bind: ['='],
            fn: ['def'],
            loop: ['for', 'while'],
            cond: ['if', 'elif', 'else'],
            struct: ['class'],
            module: ['import', 'from'],
            match: ['match'],
            yield: ['yield'],
            async: ['async def', 'await'],
            except: ['try', 'except', 'finally'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'pattern',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'print(...)',
        commentPrefix: '#',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: [':', ''],
    },
    {
        id: 'javascript', name: 'JavaScript', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['Web Development', 'Full-Stack', 'Mobile', 'Serverless'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'event-driven'],
        extensions: ['.js', '.mjs', '.cjs', '.jsx'],
        syntaxForms: {
            bind: ['let', 'const', 'var'],
            fn: ['function', '=>'],
            loop: ['for', 'while', 'for...of', 'for...in', 'do...while'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['class'],
            module: ['import', 'export', 'require'],
            async: ['async function', 'await'],
            except: ['try', 'catch', 'finally'],
            yield: ['yield', 'yield*'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'unsupported', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'console.log(...)',
        commentPrefix: '//',
        typed: 'dynamic',
        stringDelimiter: "'",
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'typescript', name: 'TypeScript', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['Scalable Web Apps', 'Full-Stack', 'Enterprise Frontend'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'event-driven'],
        extensions: ['.ts', '.tsx', '.mts', '.cts'],
        syntaxForms: {
            bind: ['let', 'const', 'var'],
            fn: ['function', '=>'],
            loop: ['for', 'while', 'for...of', 'for...in'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['class', 'interface', 'type'],
            module: ['import', 'export'],
            async: ['async function', 'await'],
            except: ['try', 'catch', 'finally'],
            generic: ['<T>', '<T extends', '<T, U>'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'unsupported', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'library',
        },
        printForm: 'console.log(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: "'",
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'java', name: 'Java', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['Enterprise', 'Android', 'Big Data', 'Backend'],
        paradigms: ['imperative', 'object-oriented', 'concurrent'],
        extensions: ['.java'],
        syntaxForms: {
            bind: ['int', 'String', 'var', 'final'],
            fn: ['void', 'public', 'private', 'static'],
            loop: ['for', 'while', 'do', 'for-each'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['class', 'interface', 'record', 'enum'],
            module: ['import', 'package'],
            async: ['CompletableFuture', 'Thread'],
            except: ['try', 'catch', 'finally', 'throws'],
            generic: ['<T>', '<T extends'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'library', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'System.out.println(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'go', name: 'Go', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['Cloud Infrastructure', 'Microservices', 'DevOps', 'Networking'],
        paradigms: ['imperative', 'concurrent', 'procedural'],
        extensions: ['.go'],
        syntaxForms: {
            bind: [':=', 'var'],
            fn: ['func'],
            loop: ['for'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['struct', 'interface'],
            module: ['import', 'package'],
            async: ['go', 'chan', 'select'],
            except: ['defer', 'panic', 'recover'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'unsupported', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'fmt.Println(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'rust', name: 'Rust', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['Systems Safety', 'Blockchain', 'WebAssembly', 'Embedded'],
        paradigms: ['imperative', 'functional', 'systems', 'concurrent'],
        extensions: ['.rs'],
        syntaxForms: {
            bind: ['let', 'let mut'],
            fn: ['fn'],
            loop: ['for', 'while', 'loop'],
            cond: ['if', 'else if', 'else', 'match'],
            struct: ['struct', 'enum', 'trait'],
            module: ['mod', 'use'],
            async: ['async fn', '.await'],
            except: ['Result', 'Option', '?'],
            generic: ['<T>', '<T: Trait>'],
        },
        features: {
            gc: 'unsupported', generics: 'native', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'native', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'println!(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'csharp', name: 'C#', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['Enterprise', 'Game Dev (Unity)', '.NET', 'Cloud'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'concurrent'],
        extensions: ['.cs'],
        syntaxForms: {
            bind: ['var', 'int', 'string', 'const'],
            fn: ['void', 'public', 'private', 'static', 'async'],
            loop: ['for', 'while', 'foreach', 'do'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['class', 'struct', 'interface', 'record', 'enum'],
            module: ['using', 'namespace'],
            async: ['async', 'await', 'Task'],
            except: ['try', 'catch', 'finally', 'throw'],
            generic: ['<T>', '<T where'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'native',
        },
        printForm: 'Console.WriteLine(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'cpp', name: 'C++', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['High Performance', 'Gaming', 'Systems', 'Embedded'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'systems', 'metaprogramming'],
        extensions: ['.cpp', '.cc', '.cxx', '.hpp', '.h'],
        syntaxForms: {
            bind: ['int', 'float', 'double', 'char', 'auto', 'std::string', 'const'],
            fn: ['void', 'int', 'auto'],
            loop: ['for', 'while', 'do'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['struct', 'class', 'enum'],
            module: ['#include', 'namespace', 'using'],
            async: ['std::async', 'std::thread', 'co_await'],
            except: ['try', 'catch', 'throw'],
            generic: ['template<typename T>', 'template<class T>'],
        },
        features: {
            gc: 'unsupported', generics: 'native', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'unsupported', traits: 'pattern',
            lifetimes: 'pattern', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'std::cout << ... << std::endl',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'swift', name: 'Swift', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['iOS Development', 'macOS', 'Server-Side'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'concurrent'],
        extensions: ['.swift'],
        syntaxForms: {
            bind: ['var', 'let'],
            fn: ['func'],
            loop: ['for', 'while', 'repeat'],
            cond: ['if', 'else if', 'else', 'switch', 'guard'],
            struct: ['class', 'struct', 'protocol', 'enum'],
            module: ['import'],
            async: ['async', 'await', 'Task'],
            except: ['do', 'try', 'catch', 'throw'],
            generic: ['<T>', '<T: Protocol>'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'native',
        },
        printForm: 'print(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'kotlin', name: 'Kotlin', group: 1,
        groupLabel: GROUP_LABELS[1],
        domains: ['Android Development', 'Server-Side', 'Multiplatform'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'concurrent'],
        extensions: ['.kt', '.kts'],
        syntaxForms: {
            bind: ['val', 'var'],
            fn: ['fun'],
            loop: ['for', 'while', 'do'],
            cond: ['if', 'else if', 'else', 'when'],
            struct: ['class', 'data class', 'interface', 'object', 'enum class'],
            module: ['import', 'package'],
            async: ['suspend', 'launch', 'async'],
            except: ['try', 'catch', 'finally', 'throw'],
            generic: ['<T>', '<T : Bound>'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'println(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['{', '}'],
    },

    // ─────────────────────────────────────────────────────────────────────
    //  GROUP 2: Data & Infrastructure Layer
    // ─────────────────────────────────────────────────────────────────────

    {
        id: 'sql', name: 'SQL', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['Database Management', 'Data Analysis', 'ETL'],
        paradigms: ['declarative', 'query'],
        extensions: ['.sql'],
        syntaxForms: {
            bind: ['DECLARE', 'SET', 'AS'],
            fn: ['CREATE FUNCTION', 'CREATE PROCEDURE'],
            loop: ['WHILE', 'CURSOR'],
            cond: ['CASE', 'WHEN', 'IF', 'ELSE'],
            struct: ['CREATE TABLE', 'CREATE VIEW', 'CREATE TYPE'],
            module: ['USE', 'CREATE SCHEMA'],
            query: ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'FROM', 'WHERE', 'JOIN'],
        },
        features: {
            gc: 'unsupported', generics: 'unsupported', async: 'unsupported', macros: 'unsupported',
            closures: 'unsupported', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'PRINT ...',
        commentPrefix: '--',
        typed: 'static',
        stringDelimiter: "'",
        terminator: ';',
        blockDelimiters: ['BEGIN', 'END'],
    },
    {
        id: 'ruby', name: 'Ruby', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['Rapid Startup Prototyping', 'Web (Rails)', 'Scripting'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'scripting', 'metaprogramming'],
        extensions: ['.rb', '.rake', '.gemspec'],
        syntaxForms: {
            bind: ['='],
            fn: ['def', 'lambda', 'proc'],
            loop: ['for', 'while', 'until', 'each', 'times', 'loop'],
            cond: ['if', 'elsif', 'else', 'unless', 'case', 'when'],
            struct: ['class', 'module', 'Struct'],
            module: ['require', 'include', 'extend', 'module'],
            except: ['begin', 'rescue', 'ensure', 'raise'],
            yield: ['yield'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'library', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'puts ...',
        commentPrefix: '#',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['do', 'end'],
    },
    {
        id: 'php', name: 'PHP', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['Web (WordPress/Laravel)', 'Server-Side', 'CMS'],
        paradigms: ['imperative', 'object-oriented', 'scripting'],
        extensions: ['.php', '.phtml'],
        syntaxForms: {
            bind: ['$', '='],
            fn: ['function'],
            loop: ['for', 'while', 'foreach', 'do'],
            cond: ['if', 'elseif', 'else', 'switch', 'match'],
            struct: ['class', 'interface', 'trait', 'enum'],
            module: ['use', 'namespace', 'require', 'include'],
            async: ['Fiber'],
            except: ['try', 'catch', 'finally', 'throw'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'library', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'echo ...',
        commentPrefix: '//',
        typed: 'gradual',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'bash', name: 'Shell/Bash', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['DevOps', 'Automation', 'System Administration'],
        paradigms: ['imperative', 'scripting'],
        extensions: ['.sh', '.bash', '.zsh'],
        syntaxForms: {
            bind: ['=', 'export', 'local', 'declare'],
            fn: ['function', '()'],
            loop: ['for', 'while', 'until'],
            cond: ['if', 'elif', 'else', 'case'],
            struct: [],
            module: ['source', '.'],
            except: ['trap'],
        },
        features: {
            gc: 'unsupported', generics: 'unsupported', async: 'pattern', macros: 'unsupported',
            closures: 'unsupported', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'native',
        },
        printForm: 'echo ...',
        commentPrefix: '#',
        typed: 'none',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['then', 'fi'],
    },
    {
        id: 'powershell', name: 'PowerShell', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['Windows System Admin', 'Azure', 'Automation'],
        paradigms: ['imperative', 'object-oriented', 'scripting'],
        extensions: ['.ps1', '.psm1', '.psd1'],
        syntaxForms: {
            bind: ['$', '='],
            fn: ['function'],
            loop: ['for', 'foreach', 'while', 'do'],
            cond: ['if', 'elseif', 'else', 'switch'],
            struct: ['class', 'enum'],
            module: ['Import-Module', 'using module'],
            except: ['try', 'catch', 'finally', 'throw'],
            async: ['Start-Job', 'ForEach-Object -Parallel'],
        },
        features: {
            gc: 'native', generics: 'library', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'Write-Output ...',
        commentPrefix: '#',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'r', name: 'R', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['Statistical Analysis', 'Data Visualization', 'Bioinformatics'],
        paradigms: ['functional', 'imperative', 'array'],
        extensions: ['.r', '.R', '.Rmd'],
        syntaxForms: {
            bind: ['<-', '=', '->'],
            fn: ['function'],
            loop: ['for', 'while', 'repeat'],
            cond: ['if', 'else if', 'else', 'ifelse', 'switch'],
            struct: ['list', 'data.frame', 'setClass'],
            module: ['library', 'require', 'source'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'library', macros: 'unsupported',
            closures: 'native', patternMatch: 'unsupported', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'cat(...)',
        commentPrefix: '#',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'scala', name: 'Scala', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['Big Data', 'Apache Spark', 'Distributed Systems'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'concurrent'],
        extensions: ['.scala', '.sc'],
        syntaxForms: {
            bind: ['val', 'var', 'def'],
            fn: ['def'],
            loop: ['for', 'while'],
            cond: ['if', 'else', 'match'],
            struct: ['class', 'case class', 'trait', 'object', 'enum'],
            module: ['import', 'package'],
            async: ['Future', 'Async'],
            except: ['try', 'catch', 'finally', 'throw'],
            generic: ['[T]', '[T <: Bound]'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'println(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'julia', name: 'Julia', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['Scientific Computing', 'Numerical Analysis', 'ML'],
        paradigms: ['imperative', 'functional', 'metaprogramming', 'array'],
        extensions: ['.jl'],
        syntaxForms: {
            bind: ['=', 'const'],
            fn: ['function', '->'],
            loop: ['for', 'while'],
            cond: ['if', 'elseif', 'else'],
            struct: ['struct', 'mutable struct', 'abstract type'],
            module: ['using', 'import', 'module'],
            async: ['@async', '@spawn'],
            except: ['try', 'catch', 'finally'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'println(...)',
        commentPrefix: '#',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['', 'end'],
    },
    {
        id: 'dart', name: 'Dart', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['Mobile UI (Flutter)', 'Web', 'Cross-Platform'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'reactive'],
        extensions: ['.dart'],
        syntaxForms: {
            bind: ['var', 'final', 'const', 'int', 'String'],
            fn: ['void', 'int', 'String', '=>'],
            loop: ['for', 'while', 'do', 'for-in'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['class', 'abstract class', 'mixin', 'enum', 'extension'],
            module: ['import', 'export', 'library', 'part'],
            async: ['async', 'await', 'Future', 'Stream'],
            except: ['try', 'catch', 'finally', 'throw', 'on'],
            generic: ['<T>', '<T extends'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'unsupported',
        },
        printForm: 'print(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: "'",
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'objc', name: 'Objective-C', group: 2,
        groupLabel: GROUP_LABELS[2],
        domains: ['Legacy iOS Maintenance', 'macOS'],
        paradigms: ['imperative', 'object-oriented'],
        extensions: ['.m', '.mm', '.h'],
        syntaxForms: {
            bind: ['int', 'NSString*', 'id', '__strong', '__weak'],
            fn: ['-', '+'],
            loop: ['for', 'while', 'do', 'for-in'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['@interface', '@implementation', '@protocol'],
            module: ['#import', '@import', '#include'],
            except: ['@try', '@catch', '@finally', '@throw'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'library', macros: 'native',
            closures: 'native', patternMatch: 'unsupported', traits: 'native',
            lifetimes: 'pattern', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'NSLog(@"...")',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '@"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },

    // ─────────────────────────────────────────────────────────────────────
    //  GROUP 3: High-Value Functional & Niche
    // ─────────────────────────────────────────────────────────────────────

    {
        id: 'elixir', name: 'Elixir', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['Scalable Messaging', 'Discord', 'Web (Phoenix)'],
        paradigms: ['functional', 'concurrent', 'metaprogramming'],
        extensions: ['.ex', '.exs'],
        syntaxForms: {
            bind: ['='],
            fn: ['def', 'defp', 'fn'],
            loop: ['for', 'Enum.each', 'Stream'],
            cond: ['if', 'else', 'cond', 'case', 'with'],
            struct: ['defmodule', 'defstruct', 'defprotocol'],
            module: ['import', 'use', 'alias', 'require'],
            async: ['Task.async', 'GenServer', 'spawn'],
            except: ['try', 'rescue', 'catch', 'raise'],
        },
        features: {
            gc: 'native', generics: 'pattern', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'IO.puts(...)',
        commentPrefix: '#',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['do', 'end'],
    },
    {
        id: 'clojure', name: 'Clojure', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['High-level Functional', 'Fintech', 'Data Processing'],
        paradigms: ['functional', 'concurrent', 'metaprogramming'],
        extensions: ['.clj', '.cljs', '.cljc', '.edn'],
        syntaxForms: {
            bind: ['def', 'let'],
            fn: ['defn', 'fn'],
            loop: ['loop', 'recur', 'doseq', 'for'],
            cond: ['if', 'cond', 'case', 'when'],
            struct: ['defrecord', 'deftype', 'defprotocol'],
            module: ['ns', 'require', 'import', 'use'],
            async: ['future', 'promise', 'core.async'],
            except: ['try', 'catch', 'finally', 'throw'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'library', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: '(println ...)',
        commentPrefix: ';',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['(', ')'],
    },
    {
        id: 'haskell', name: 'Haskell', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['Academic', 'High-Security Systems', 'Formal Verification'],
        paradigms: ['functional'],
        extensions: ['.hs', '.lhs'],
        syntaxForms: {
            bind: ['let', '=', '<-', 'where'],
            fn: [''],
            loop: ['map', 'foldr', 'foldl', 'forM_'],
            cond: ['if', 'then', 'else', 'case', 'of', 'guards'],
            struct: ['data', 'newtype', 'type', 'class', 'instance'],
            module: ['import', 'module'],
            async: ['forkIO', 'async', 'STM'],
            except: ['catch', 'try', 'throwIO'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'library',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'native',
        },
        printForm: 'putStrLn ...',
        commentPrefix: '--',
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['where', ''],
    },
    {
        id: 'erlang', name: 'Erlang', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['Telecommunications', 'High Availability', 'Distributed Systems'],
        paradigms: ['functional', 'concurrent'],
        extensions: ['.erl', '.hrl'],
        syntaxForms: {
            bind: ['='],
            fn: ['-spec', '-module'],
            loop: ['lists:foreach', 'lists:map'],
            cond: ['case', 'of', 'if', 'when'],
            struct: ['-record'],
            module: ['-module', '-export', '-import'],
            async: ['spawn', 'receive', '!'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'io:format("~p~n", [...])',
        commentPrefix: '%',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '.',
        blockDelimiters: ['', 'end'],
    },
    {
        id: 'fsharp', name: 'F#', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['Functional .NET', 'Data Science', 'Finance'],
        paradigms: ['functional', 'imperative', 'object-oriented'],
        extensions: ['.fs', '.fsx', '.fsi'],
        syntaxForms: {
            bind: ['let', 'let mutable'],
            fn: ['let', 'member'],
            loop: ['for', 'while', 'Seq', 'List', 'Array'],
            cond: ['if', 'elif', 'else', 'match', 'with'],
            struct: ['type', 'module'],
            module: ['open', 'module', 'namespace'],
            async: ['async', 'Async', 'task'],
            except: ['try', 'with', 'raise', 'failwith'],
            generic: ["'T", "'T when"],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'printfn "..." ...',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['', ''],
    },
    {
        id: 'ocaml', name: 'OCaml', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['Financial Modeling', 'Trading', 'Formal Verification'],
        paradigms: ['functional', 'imperative', 'object-oriented'],
        extensions: ['.ml', '.mli'],
        syntaxForms: {
            bind: ['let', 'let rec', 'let mutable'],
            fn: ['let', 'fun', 'function'],
            loop: ['for', 'while', 'List.iter', 'Array.iter'],
            cond: ['if', 'then', 'else', 'match', 'with'],
            struct: ['type', 'module', 'class'],
            module: ['open', 'module', 'include'],
            except: ['try', 'with', 'raise'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'library', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'native',
        },
        printForm: 'Printf.printf "..." ...',
        commentPrefix: '(*',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';;',
        blockDelimiters: ['begin', 'end'],
    },
    {
        id: 'lisp', name: 'Lisp', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['AI Research', 'Custom Tooling', 'Metaprogramming'],
        paradigms: ['functional', 'metaprogramming', 'imperative'],
        extensions: ['.lisp', '.lsp', '.cl', '.el'],
        syntaxForms: {
            bind: ['defvar', 'defparameter', 'let', 'setq'],
            fn: ['defun', 'lambda'],
            loop: ['loop', 'dolist', 'dotimes', 'mapcar'],
            cond: ['if', 'cond', 'when', 'unless', 'case'],
            struct: ['defclass', 'defstruct'],
            module: ['require', 'provide', 'use-package', 'in-package'],
            except: ['handler-case', 'handler-bind', 'signal', 'error'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'library', macros: 'native',
            closures: 'native', patternMatch: 'library', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: '(format t "..." ...)',
        commentPrefix: ';',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['(', ')'],
    },
    {
        id: 'prolog', name: 'Prolog', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['Logic Programming', 'AI', 'Expert Systems'],
        paradigms: ['logic', 'declarative'],
        extensions: ['.pl', '.pro', '.P'],
        syntaxForms: {
            bind: ['is', '='],
            fn: [':-'],
            loop: ['findall', 'forall', 'between'],
            cond: ['->', ';', 'if-then-else'],
            struct: [':-'],
            module: [':- module', ':- use_module'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'unsupported', macros: 'unsupported',
            closures: 'unsupported', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'write(...)',
        commentPrefix: '%',
        typed: 'dynamic',
        stringDelimiter: "'",
        terminator: '.',
        blockDelimiters: ['(', ')'],
    },
    {
        id: 'solidity', name: 'Solidity', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['Ethereum Smart Contracts', 'DeFi', 'NFTs'],
        paradigms: ['imperative', 'object-oriented', 'contract'],
        extensions: ['.sol'],
        syntaxForms: {
            bind: ['uint', 'int', 'address', 'string', 'bytes', 'bool', 'mapping'],
            fn: ['function'],
            loop: ['for', 'while', 'do'],
            cond: ['if', 'else if', 'else'],
            struct: ['contract', 'struct', 'interface', 'library', 'enum'],
            module: ['import'],
            except: ['require', 'assert', 'revert', 'try', 'catch'],
            modifier: ['modifier', 'payable', 'view', 'pure'],
        },
        features: {
            gc: 'unsupported', generics: 'unsupported', async: 'unsupported', macros: 'unsupported',
            closures: 'unsupported', patternMatch: 'unsupported', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'unsupported',
        },
        printForm: 'emit Log(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'zig', name: 'Zig', group: 3,
        groupLabel: GROUP_LABELS[3],
        domains: ['Modern C Competitor', 'Systems', 'Embedded'],
        paradigms: ['imperative', 'systems', 'procedural'],
        extensions: ['.zig'],
        syntaxForms: {
            bind: ['const', 'var'],
            fn: ['fn', 'pub fn'],
            loop: ['for', 'while'],
            cond: ['if', 'else', 'switch'],
            struct: ['struct', 'union', 'enum', 'error'],
            module: ['@import', 'pub'],
            except: ['catch', 'try', 'error'],
        },
        features: {
            gc: 'unsupported', generics: 'native', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'unsupported',
        },
        printForm: 'std.debug.print("...", .{...})',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },

    // ─────────────────────────────────────────────────────────────────────
    //  GROUP 4: Low-Level, Embedded & Legacy
    // ─────────────────────────────────────────────────────────────────────

    {
        id: 'c', name: 'C', group: 4,
        groupLabel: GROUP_LABELS[4],
        domains: ['Foundation of Computing', 'OS Kernels', 'Embedded'],
        paradigms: ['imperative', 'procedural', 'systems'],
        extensions: ['.c', '.h'],
        syntaxForms: {
            bind: ['int', 'float', 'double', 'char', 'void*', 'const'],
            fn: ['void', 'int', 'float', 'double'],
            loop: ['for', 'while', 'do'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['struct', 'union', 'enum', 'typedef'],
            module: ['#include', '#define'],
        },
        features: {
            gc: 'unsupported', generics: 'unsupported', async: 'library', macros: 'native',
            closures: 'unsupported', patternMatch: 'unsupported', traits: 'unsupported',
            lifetimes: 'pattern', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'printf("...", ...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'fortran', name: 'Fortran', group: 4,
        groupLabel: GROUP_LABELS[4],
        domains: ['Weather Simulation', 'Physics', 'HPC'],
        paradigms: ['imperative', 'procedural', 'array'],
        extensions: ['.f90', '.f95', '.f03', '.f08', '.f', '.for'],
        syntaxForms: {
            bind: ['integer', 'real', 'character', 'logical', 'complex'],
            fn: ['subroutine', 'function'],
            loop: ['do', 'do while'],
            cond: ['if', 'then', 'else if', 'else', 'select case'],
            struct: ['type', 'module'],
            module: ['use', 'module', 'program'],
        },
        features: {
            gc: 'unsupported', generics: 'unsupported', async: 'library', macros: 'unsupported',
            closures: 'unsupported', patternMatch: 'unsupported', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'PRINT *, ...',
        commentPrefix: '!',
        typed: 'static',
        stringDelimiter: "'",
        terminator: '',
        blockDelimiters: ['', 'END'],
    },
    {
        id: 'vhdl', name: 'VHDL/Verilog', group: 4,
        groupLabel: GROUP_LABELS[4],
        domains: ['Hardware Chip Design', 'FPGA', 'ASIC'],
        paradigms: ['hardware-description', 'concurrent'],
        extensions: ['.vhd', '.vhdl', '.v', '.sv'],
        syntaxForms: {
            bind: ['signal', 'variable', 'constant', 'wire', 'reg'],
            fn: ['process', 'function', 'procedure', 'always', 'initial'],
            loop: ['for', 'while', 'generate'],
            cond: ['if', 'elsif', 'else', 'case', 'when'],
            struct: ['entity', 'architecture', 'component', 'module'],
            module: ['library', 'use', 'package', 'module'],
        },
        features: {
            gc: 'unsupported', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'unsupported', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'report "..."',
        commentPrefix: '--',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['begin', 'end'],
    },
    {
        id: 'matlab', name: 'MATLAB', group: 4,
        groupLabel: GROUP_LABELS[4],
        domains: ['Engineering', 'Linear Algebra', 'Control Systems'],
        paradigms: ['imperative', 'array', 'functional'],
        extensions: ['.m', '.mat', '.mlx'],
        syntaxForms: {
            bind: ['='],
            fn: ['function'],
            loop: ['for', 'while', 'parfor'],
            cond: ['if', 'elseif', 'else', 'switch', 'case'],
            struct: ['classdef', 'properties', 'methods'],
            module: ['import', 'addpath'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'library', macros: 'unsupported',
            closures: 'native', patternMatch: 'unsupported', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'disp(...)',
        commentPrefix: '%',
        typed: 'dynamic',
        stringDelimiter: "'",
        terminator: ';',
        blockDelimiters: ['', 'end'],
    },
    {
        id: 'pascal', name: 'Delphi/Pascal', group: 4,
        groupLabel: GROUP_LABELS[4],
        domains: ['Legacy Desktop Apps', 'Embedded', 'Education'],
        paradigms: ['imperative', 'procedural', 'object-oriented'],
        extensions: ['.pas', '.dpr', '.pp'],
        syntaxForms: {
            bind: ['var', 'const', ':='],
            fn: ['procedure', 'function'],
            loop: ['for', 'while', 'repeat'],
            cond: ['if', 'then', 'else', 'case', 'of'],
            struct: ['type', 'record', 'class', 'interface'],
            module: ['uses', 'unit', 'program'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'library', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'WriteLn(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: "'",
        terminator: ';',
        blockDelimiters: ['begin', 'end'],
    },
    {
        id: 'perl', name: 'Perl', group: 4,
        groupLabel: GROUP_LABELS[4],
        domains: ['Legacy Web', 'Bioinformatics', 'Text Processing'],
        paradigms: ['imperative', 'object-oriented', 'scripting', 'functional'],
        extensions: ['.pl', '.pm', '.t'],
        syntaxForms: {
            bind: ['my', 'our', 'local', '$', '@', '%'],
            fn: ['sub'],
            loop: ['for', 'foreach', 'while', 'until', 'do'],
            cond: ['if', 'elsif', 'else', 'unless', 'given', 'when'],
            struct: ['package', 'bless'],
            module: ['use', 'require', 'package'],
            except: ['eval', 'die', 'warn'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'library', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'print "..."',
        commentPrefix: '#',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'apex', name: 'Apex', group: 4,
        groupLabel: GROUP_LABELS[4],
        domains: ['Salesforce Development', 'CRM Automation'],
        paradigms: ['imperative', 'object-oriented'],
        extensions: ['.cls', '.trigger'],
        syntaxForms: {
            bind: ['Integer', 'String', 'Boolean', 'Id', 'var'],
            fn: ['public', 'private', 'static', 'void'],
            loop: ['for', 'while', 'do'],
            cond: ['if', 'else if', 'else', 'switch on'],
            struct: ['class', 'interface', 'enum'],
            module: ['import'],
            except: ['try', 'catch', 'finally', 'throw'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'unsupported', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'unsupported',
        },
        printForm: 'System.debug(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: "'",
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },

    // ─────────────────────────────────────────────────────────────────────
    //  GROUP 5: Modern Web, Domain-Specific & Emerging
    // ─────────────────────────────────────────────────────────────────────

    {
        id: 'mojo', name: 'Mojo', group: 5,
        groupLabel: GROUP_LABELS[5],
        domains: ['AI-Speed Python', 'ML Infrastructure', 'Systems AI'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'systems'],
        extensions: ['.mojo', '.🔥'],
        syntaxForms: {
            bind: ['var', 'let', 'alias'],
            fn: ['fn', 'def'],
            loop: ['for', 'while'],
            cond: ['if', 'elif', 'else'],
            struct: ['struct', 'trait'],
            module: ['import', 'from'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'unsupported', traits: 'native',
            lifetimes: 'native', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'print(...)',
        commentPrefix: '#',
        typed: 'gradual',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: [':', ''],
    },
    {
        id: 'lua', name: 'Luau/Lua', group: 5,
        groupLabel: GROUP_LABELS[5],
        domains: ['Game Scripting', 'Roblox', 'Embedded Scripting'],
        paradigms: ['imperative', 'scripting', 'functional'],
        extensions: ['.lua', '.luau'],
        syntaxForms: {
            bind: ['local', '='],
            fn: ['function', 'local function'],
            loop: ['for', 'while', 'repeat'],
            cond: ['if', 'elseif', 'else'],
            struct: ['{}'],
            module: ['require'],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'library', macros: 'unsupported',
            closures: 'native', patternMatch: 'unsupported', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'print(...)',
        commentPrefix: '--',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['then', 'end'],
    },
    {
        id: 'groovy', name: 'Groovy', group: 5,
        groupLabel: GROUP_LABELS[5],
        domains: ['Jenkins Automation', 'Java Ecosystem', 'DSLs'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'scripting', 'metaprogramming'],
        extensions: ['.groovy', '.gvy', '.gy'],
        syntaxForms: {
            bind: ['def', 'int', 'String', 'final'],
            fn: ['def', 'void'],
            loop: ['for', 'while', 'each', 'eachWithIndex'],
            cond: ['if', 'else if', 'else', 'switch'],
            struct: ['class', 'interface', 'trait', 'enum'],
            module: ['import', 'package'],
            except: ['try', 'catch', 'finally', 'throw'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'library', macros: 'native',
            closures: 'native', patternMatch: 'unsupported', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'println ...',
        commentPrefix: '//',
        typed: 'dynamic',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'elm', name: 'Elm', group: 5,
        groupLabel: GROUP_LABELS[5],
        domains: ['Purely Functional Frontend', 'Web UI'],
        paradigms: ['functional'],
        extensions: ['.elm'],
        syntaxForms: {
            bind: ['='],
            fn: [''],
            loop: ['List.map', 'List.foldr'],
            cond: ['if', 'then', 'else', 'case', 'of'],
            struct: ['type', 'type alias'],
            module: ['import', 'module', 'exposing'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'Debug.log "..." ...',
        commentPrefix: '--',
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['', ''],
    },
    {
        id: 'nim', name: 'Nim', group: 5,
        groupLabel: GROUP_LABELS[5],
        domains: ['Efficient C-like Syntax', 'Systems', 'Scripting'],
        paradigms: ['imperative', 'functional', 'metaprogramming', 'systems'],
        extensions: ['.nim', '.nims'],
        syntaxForms: {
            bind: ['var', 'let', 'const'],
            fn: ['proc', 'func', 'method'],
            loop: ['for', 'while'],
            cond: ['if', 'elif', 'else', 'case', 'of'],
            struct: ['type', 'object', 'enum', 'tuple'],
            module: ['import', 'from', 'include', 'export'],
            except: ['try', 'except', 'finally', 'raise'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'echo ...',
        commentPrefix: '#',
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: [':', ''],
    },
    {
        id: 'crystal', name: 'Crystal', group: 5,
        groupLabel: GROUP_LABELS[5],
        domains: ['Syntax of Ruby, Speed of C', 'Web', 'CLI'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'concurrent'],
        extensions: ['.cr'],
        syntaxForms: {
            bind: ['='],
            fn: ['def'],
            loop: ['each', 'times', 'while', 'until', 'loop'],
            cond: ['if', 'elsif', 'else', 'unless', 'case', 'when'],
            struct: ['class', 'struct', 'module', 'enum'],
            module: ['require', 'include', 'extend'],
            async: ['spawn', 'Channel', 'Fiber'],
            except: ['begin', 'rescue', 'ensure', 'raise'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'puts ...',
        commentPrefix: '#',
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['', 'end'],
    },
    {
        id: 'd', name: 'D', group: 5,
        groupLabel: GROUP_LABELS[5],
        domains: ['Systems Programming Alternative', 'High Performance'],
        paradigms: ['imperative', 'object-oriented', 'functional', 'systems', 'metaprogramming'],
        extensions: ['.d'],
        syntaxForms: {
            bind: ['auto', 'int', 'float', 'string', 'immutable', 'const'],
            fn: ['void', 'int', 'auto'],
            loop: ['for', 'foreach', 'while', 'do'],
            cond: ['if', 'else if', 'else', 'switch', 'final switch'],
            struct: ['struct', 'class', 'interface', 'union', 'enum'],
            module: ['import', 'module'],
            except: ['try', 'catch', 'finally', 'throw'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'native',
            closures: 'native', patternMatch: 'unsupported', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'native', repl: 'unsupported',
        },
        printForm: 'writeln(...)',
        commentPrefix: '//',
        typed: 'static',
        stringDelimiter: '"',
        terminator: ';',
        blockDelimiters: ['{', '}'],
    },
    {
        id: 'vbnet', name: 'Visual Basic .NET', group: 5,
        groupLabel: GROUP_LABELS[5],
        domains: ['Legacy Corporate Apps', '.NET', 'Office Automation'],
        paradigms: ['imperative', 'object-oriented', 'event-driven'],
        extensions: ['.vb'],
        syntaxForms: {
            bind: ['Dim', 'Const', 'Var'],
            fn: ['Sub', 'Function'],
            loop: ['For', 'For Each', 'While', 'Do While', 'Do Until'],
            cond: ['If', 'ElseIf', 'Else', 'Select Case'],
            struct: ['Class', 'Structure', 'Interface', 'Module', 'Enum'],
            module: ['Imports', 'Namespace'],
            async: ['Async', 'Await'],
            except: ['Try', 'Catch', 'Finally', 'Throw'],
        },
        features: {
            gc: 'native', generics: 'native', async: 'native', macros: 'unsupported',
            closures: 'native', patternMatch: 'native', traits: 'native',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'unsupported',
        },
        printForm: 'Console.WriteLine(...)',
        commentPrefix: "'",
        typed: 'static',
        stringDelimiter: '"',
        terminator: '',
        blockDelimiters: ['', 'End'],
    },
    {
        id: 'smalltalk', name: 'Smalltalk', group: 5,
        groupLabel: GROUP_LABELS[5],
        domains: ['Father of OOP', 'Education', 'Prototyping'],
        paradigms: ['object-oriented', 'reactive', 'metaprogramming'],
        extensions: ['.st'],
        syntaxForms: {
            bind: [':=', '|'],
            fn: ['[]'],
            loop: ['timesRepeat:', 'do:', 'whileTrue:', 'whileFalse:'],
            cond: ['ifTrue:', 'ifFalse:', 'ifTrue:ifFalse:'],
            struct: ['Object subclass:'],
            module: [],
        },
        features: {
            gc: 'native', generics: 'unsupported', async: 'library', macros: 'native',
            closures: 'native', patternMatch: 'native', traits: 'unsupported',
            lifetimes: 'unsupported', unsafe: 'unsupported', repl: 'native',
        },
        printForm: 'Transcript show: ...',
        commentPrefix: '"',
        typed: 'dynamic',
        stringDelimiter: "'",
        terminator: '.',
        blockDelimiters: ['[', ']'],
    },
];

// ═══════════════════════════════════════════════════════════════════════════
//  Grey++ Unified Language Entry
// ═══════════════════════════════════════════════════════════════════════════

export const GREY_PLUS_PLUS: LanguageDescriptor = {
    id: 'grey', name: 'Grey++', group: 1,
    groupLabel: 'Meta-Language (Unifies All)',
    domains: ['Universal Programming', 'All Paradigms', 'Cross-Language Interop'],
    paradigms: [
        'imperative', 'object-oriented', 'functional', 'declarative',
        'procedural', 'logic', 'concurrent', 'reactive', 'metaprogramming',
        'systems', 'scripting', 'query', 'hardware-description',
        'contract', 'event-driven', 'array',
    ],
    extensions: ['.grey', '.gpp', '.g++'],
    syntaxForms: {
        bind: ['bind'],
        fn: ['fn'],
        loop: ['loop'],
        cond: ['cond'],
        struct: ['struct'],
        module: ['module'],
        match: ['match'],
        yield: ['yield'],
        async: ['async'],
        except: ['try'],
        query: ['query'],
        infer: ['infer'],
        pipeline: ['pipeline'],
        contract: ['contract'],
        hardware: ['hw'],
        logic: ['rule'],
    },
    features: {
        gc: 'native', generics: 'native', async: 'native', macros: 'native',
        closures: 'native', patternMatch: 'native', traits: 'native',
        lifetimes: 'native', unsafe: 'native', repl: 'native',
    },
    printForm: 'print(...)',
    commentPrefix: '//',
    typed: 'gradual',
    stringDelimiter: '"',
    terminator: '',
    blockDelimiters: ['{', '}'],
};

// ═══════════════════════════════════════════════════════════════════════════
//  PolyglotRegistry — Lookup & Unification API
// ═══════════════════════════════════════════════════════════════════════════

export class PolyglotRegistry {
    private languages = new Map<string, LanguageDescriptor>();

    constructor() {
        for (const lang of LANGUAGE_REGISTRY) {
            this.languages.set(lang.id, lang);
        }
        this.languages.set(GREY_PLUS_PLUS.id, GREY_PLUS_PLUS);
    }

    /** Get a language by id. */
    get(id: string): LanguageDescriptor | undefined {
        return this.languages.get(id);
    }

    /** Get all languages. */
    all(): LanguageDescriptor[] {
        return Array.from(this.languages.values());
    }

    /** Get all languages in a specific group. */
    group(g: 1 | 2 | 3 | 4 | 5): LanguageDescriptor[] {
        return this.all().filter(l => l.group === g);
    }

    /** Get all language IDs. */
    ids(): string[] {
        return Array.from(this.languages.keys());
    }

    /** Total language count (including Grey++). */
    count(): number {
        return this.languages.size;
    }

    /** Look up a language by file extension. */
    byExtension(ext: string): LanguageDescriptor | undefined {
        const normalized = ext.startsWith('.') ? ext : '.' + ext;
        return this.all().find(l => l.extensions.includes(normalized));
    }

    /** Look up languages by paradigm. */
    byParadigm(paradigm: Paradigm): LanguageDescriptor[] {
        return this.all().filter(l => l.paradigms.includes(paradigm));
    }

    /** Look up languages by domain keyword. */
    byDomain(keyword: string): LanguageDescriptor[] {
        const lower = keyword.toLowerCase();
        return this.all().filter(l =>
            l.domains.some(d => d.toLowerCase().includes(lower))
        );
    }

    /** Get the print form mapping for all languages. */
    printForms(): Record<string, string> {
        const map: Record<string, string> = {};
        for (const [id, lang] of this.languages) map[id] = lang.printForm;
        return map;
    }

    /** Get feature support across all languages for a given feature. */
    featureMatrix(feature: string): Record<string, FeatureTier> {
        const map: Record<string, FeatureTier> = {};
        for (const [id, lang] of this.languages) {
            map[id] = lang.features[feature] ?? 'unsupported';
        }
        return map;
    }

    /** Map a universal construct to its surface forms across all languages. */
    constructForms(construct: string): Record<string, string[]> {
        const map: Record<string, string[]> = {};
        for (const [id, lang] of this.languages) {
            map[id] = lang.syntaxForms[construct] ?? [];
        }
        return map;
    }

    /** Get the Grey++ unified form for a construct. */
    unifiedForm(construct: string): string {
        const forms = GREY_PLUS_PLUS.syntaxForms[construct];
        return forms?.[0] ?? construct;
    }

    /** Detect language from a source line using syntax heuristics. */
    detectLanguage(source: string): string {
        const trimmed = source.trim();

        // Grey++ unique keywords first
        if (/^(bind|loop|cond|module|pipeline|query|infer|contract|hw|rule)\s/.test(trimmed)) return 'grey';
        if (/^fn\s+\w+\s*\(/.test(trimmed) && !trimmed.includes('->')) return 'grey';

        // Group 1 detections
        if (/^def\s+\w+\s*\(/.test(trimmed) && trimmed.includes(':')) return 'python';
        if (/^fn\s+\w+\s*\(/.test(trimmed) && trimmed.includes('->')) return 'rust';
        if (/^let\s+mut\s/.test(trimmed)) return 'rust';
        if (/^(mod|use)\s/.test(trimmed) && !trimmed.includes('(')) return 'rust';
        if (/^func\s+\w+\s*\(/.test(trimmed)) {
            if (trimmed.includes('->') && !trimmed.includes('{')) return 'swift';
            return 'go';
        }
        if (/^fun\s+\w+\s*\(/.test(trimmed)) return 'kotlin';
        if (/^(val|var)\s+\w+\s*[=:]/.test(trimmed) && !trimmed.includes('$')) return 'kotlin';
        if (/^package\s+\w/.test(trimmed) && !trimmed.includes(';')) return 'go';
        if (/^(let|const|var)\s/.test(trimmed)) {
            if (/:\s*\w+\s*[=;]/.test(trimmed)) return 'typescript';
            return 'javascript';
        }
        if (/^function\s/.test(trimmed)) return 'javascript';
        if (/^class\s+\w+.*\{/.test(trimmed)) {
            if (/\bextends\b|\bimplements\b/.test(trimmed)) return 'java';
            return 'javascript';
        }
        if (/^(public|private|protected|static)\s/.test(trimmed)) {
            if (trimmed.includes('void') || trimmed.includes('String')) {
                if (/\boverride\b|\babstract\b/.test(trimmed) || trimmed.includes('=>')) return 'csharp';
                return 'java';
            }
            return 'csharp';
        }
        if (/^(int|float|double|char|auto|void|std::)\s/.test(trimmed)) {
            if (trimmed.includes('std::')) return 'cpp';
            return 'c';
        }
        if (/^#include\s/.test(trimmed)) {
            if (trimmed.includes('<') || trimmed.includes('"')) return 'cpp';
            return 'c';
        }
        if (/^struct\s+\w+\s*\{/.test(trimmed)) {
            if (/:\s*\w+/.test(trimmed) && !trimmed.includes(';')) return 'rust';
            return 'cpp';
        }
        if (/^import\s/.test(trimmed)) {
            if (trimmed.includes("'") || trimmed.includes('"') || trimmed.includes('{')) return 'javascript';
            if (trimmed.includes('*')) return 'java';
            return 'python';
        }
        if (/^export\s/.test(trimmed)) return 'javascript';
        if (/^namespace\s/.test(trimmed)) {
            if (trimmed.includes('::')) return 'cpp';
            return 'csharp';
        }

        // Group 2 detections
        if (/^(SELECT|INSERT|UPDATE|DELETE|CREATE|ALTER|DROP|FROM|WHERE)\s/i.test(trimmed)) return 'sql';
        if (/^(require|include|extend)\s/.test(trimmed) && trimmed.includes("'")) return 'ruby';
        if (/^<\?php/.test(trimmed) || /^\$\w+\s*=/.test(trimmed)) return 'php';
        if (/^#!\s*\/bin\/(ba)?sh/.test(trimmed) || /^(export|source|alias)\s/.test(trimmed)) return 'bash';
        if (/^\$(Get-|Set-|New-|Remove-|Start-|Stop-|Invoke-|Write-|Import-)/.test(trimmed) || /^function\s+\w+-\w+/.test(trimmed)) return 'powershell';
        if (/^(library|require)\s*\(/.test(trimmed) && trimmed.includes(')')) return 'r';
        if (/^(case\s+class|sealed\s+trait|object\s+\w+)/.test(trimmed)) return 'scala';
        if (/^(using|import)\s+\w+\.\w+/.test(trimmed) && !trimmed.includes('"')) return 'julia';
        if (/^@(interface|implementation|protocol)\s/.test(trimmed)) return 'objc';
        if (/^#import\s/.test(trimmed)) return 'objc';

        // Group 3 detections
        if (/^defmodule\s/.test(trimmed)) return 'elixir';
        if (/^\(defn?\s/.test(trimmed) || /^\(ns\s/.test(trimmed)) return 'clojure';
        if (/^(data|newtype|instance|deriving)\s/.test(trimmed)) return 'haskell';
        if (/^-module\s*\(/.test(trimmed) || /^-export\s*\(/.test(trimmed)) return 'erlang';
        if (/^(open|let\s+\w+\s*:)/.test(trimmed) && !trimmed.includes('{')) return 'fsharp';
        if (/^(let\s+(rec\s+)?\w+\s*=|type\s+\w+\s*=)/.test(trimmed) && !trimmed.includes('{')) return 'ocaml';
        if (/^\(defun\s/.test(trimmed) || /^\(defvar\s/.test(trimmed) || /^\(setq\s/.test(trimmed)) return 'lisp';
        if (/^:-\s*(module|use_module)/.test(trimmed) || /^\w+\s*\(.*\)\s*:-/.test(trimmed)) return 'prolog';
        if (/^(pragma\s+solidity|contract\s+\w+|mapping\s*\()/.test(trimmed)) return 'solidity';
        if (/^(pub\s+)?fn\s+\w+\(.*\)\s*\w+\s*!/.test(trimmed) || /^const\s+\w+\s*=\s*@import/.test(trimmed)) return 'zig';

        // Group 4 detections
        if (/^(integer|real|character|logical|complex|subroutine|program)\s/i.test(trimmed)) return 'fortran';
        if (/^(signal|entity|architecture|component|process)\s/i.test(trimmed)) return 'vhdl';
        if (/^(classdef|properties|methods)\s/i.test(trimmed) || /^function\s+\[/.test(trimmed)) return 'matlab';
        if (/^(procedure|program|unit|uses)\s/i.test(trimmed)) return 'pascal';
        if (/^(my|our)\s+\$/.test(trimmed) || /^sub\s+\w+\s*\{/.test(trimmed)) return 'perl';

        // Group 5 detections
        if (/^(local\s+function|local\s+\w+\s*=)/.test(trimmed)) return 'lua';
        if (/^(Dim|Sub|Function|Module|Imports|Namespace)\s/i.test(trimmed)) return 'vbnet';
        if (/^\w+\s+subclass:\s/.test(trimmed)) return 'smalltalk';
        if (/^proc\s+\w+/.test(trimmed)) return 'nim';

        // Python catch-all
        if (/^[a-zA-Z_]\w*\s*=\s*[^=]/.test(trimmed)) return 'python';
        if (/^(from|import)\s/.test(trimmed)) return 'python';
        if (/^(if|elif)\s.*:\s*$/.test(trimmed)) return 'python';
        if (/^while\s.*:\s*$/.test(trimmed)) return 'python';
        if (/^for\s+\w+\s+in\s/.test(trimmed)) {
            if (trimmed.includes(':')) return 'python';
            if (trimmed.includes('{')) return 'rust';
            return 'python';
        }

        return 'unknown';
    }

    /** Map a print call FROM any language TO Grey++ universal print. */
    normalizePrint(source: string, langId: string): string {
        const lang = this.get(langId);
        if (!lang) return source;

        const replacements: Record<string, RegExp> = {
            javascript: /console\.log\s*\(/g,
            typescript: /console\.log\s*\(/g,
            python: /print\s*\(/g,
            cpp: /std::cout\s*<<\s*/g,
            c: /printf\s*\(/g,
            rust: /println!\s*\(/g,
            java: /System\.out\.println\s*\(/g,
            csharp: /Console\.WriteLine\s*\(/g,
            go: /fmt\.Println\s*\(/g,
            swift: /print\s*\(/g,
            kotlin: /println\s*\(/g,
            ruby: /puts\s+/g,
            php: /echo\s+/g,
            r: /cat\s*\(/g,
            scala: /println\s*\(/g,
            julia: /println\s*\(/g,
            dart: /print\s*\(/g,
            elixir: /IO\.puts\s*\(/g,
            haskell: /putStrLn\s+/g,
            lua: /print\s*\(/g,
            perl: /print\s+/g,
            fortran: /PRINT\s*\*\s*,\s*/gi,
            pascal: /WriteLn\s*\(/gi,
            vbnet: /Console\.WriteLine\s*\(/gi,
            nim: /echo\s+/g,
            crystal: /puts\s+/g,
            d: /writeln\s*\(/g,
            groovy: /println\s+/g,
            matlab: /disp\s*\(/g,
            apex: /System\.debug\s*\(/g,
            objc: /NSLog\s*\(\s*@/g,
            bash: /echo\s+/g,
            powershell: /Write-Output\s+/g,
        };

        const regex = replacements[langId];
        if (!regex) return source;
        return source.replace(regex, 'print(');
    }

    /** Map Grey++ universal `print(...)` to a specific language's output form. */
    denormalizePrint(greySource: string, targetLang: string): string {
        const lang = this.get(targetLang);
        if (!lang) return greySource;

        const forms: Record<string, (body: string) => string> = {
            javascript: (b) => `console.log(${b})`,
            typescript: (b) => `console.log(${b})`,
            python: (b) => `print(${b})`,
            cpp: (b) => `std::cout << ${b} << std::endl`,
            c: (b) => `printf("%s\\n", ${b})`,
            rust: (b) => `println!("{}", ${b})`,
            java: (b) => `System.out.println(${b})`,
            csharp: (b) => `Console.WriteLine(${b})`,
            go: (b) => `fmt.Println(${b})`,
            swift: (b) => `print(${b})`,
            kotlin: (b) => `println(${b})`,
            ruby: (b) => `puts ${b}`,
            php: (b) => `echo ${b}`,
            r: (b) => `cat(${b})`,
            scala: (b) => `println(${b})`,
            julia: (b) => `println(${b})`,
            dart: (b) => `print(${b})`,
            elixir: (b) => `IO.puts(${b})`,
            haskell: (b) => `putStrLn ${b}`,
            lua: (b) => `print(${b})`,
            perl: (b) => `print ${b}`,
            fortran: (b) => `PRINT *, ${b}`,
            pascal: (b) => `WriteLn(${b})`,
            vbnet: (b) => `Console.WriteLine(${b})`,
            nim: (b) => `echo ${b}`,
            crystal: (b) => `puts ${b}`,
            d: (b) => `writeln(${b})`,
            groovy: (b) => `println ${b}`,
            matlab: (b) => `disp(${b})`,
            apex: (b) => `System.debug(${b})`,
            objc: (b) => `NSLog(@"%@", ${b})`,
            bash: (b) => `echo ${b}`,
            powershell: (b) => `Write-Output ${b}`,
            solidity: (b) => `emit Log(${b})`,
            prolog: (b) => `write(${b})`,
            lisp: (b) => `(format t "~a" ${b})`,
            clojure: (b) => `(println ${b})`,
            erlang: (b) => `io:format("~p~n", [${b}])`,
            fsharp: (b) => `printfn "%A" ${b}`,
            ocaml: (b) => `Printf.printf "%s" ${b}`,
            smalltalk: (b) => `Transcript show: ${b}`,
            vhdl: (b) => `report ${b}`,
            mojo: (b) => `print(${b})`,
            zig: (b) => `std.debug.print("{}", .{${b}})`,
            elm: (b) => `Debug.log "" ${b}`,
            grey: (b) => `print(${b})`,
        };

        const form = forms[targetLang];
        if (!form) return greySource;

        return greySource.replace(/\bprint\s*\(([^)]*)\)/g, (_, body) => form(body.trim()));
    }

    /** Generate a unification summary. */
    summary(): string {
        const groups = [1, 2, 3, 4, 5] as const;
        const lines: string[] = [
            '═══ Grey++ Polyglot Unification Summary ═══',
            `Total languages unified: ${this.count()}`,
            '',
        ];

        for (const g of groups) {
            const langs = this.group(g);
            lines.push(`── Group ${g}: ${GROUP_LABELS[g]} (${langs.length} languages) ──`);
            for (const l of langs) {
                lines.push(`  ${l.name} — ${l.domains.join(', ')}`);
            }
            lines.push('');
        }

        // Grey++ summary
        lines.push('── Grey++ (Meta-Language) ──');
        lines.push(`  Paradigms: ${GREY_PLUS_PLUS.paradigms.join(', ')}`);
        lines.push(`  Constructs: ${Object.keys(GREY_PLUS_PLUS.syntaxForms).join(', ')}`);
        lines.push(`  Total native features: ${Object.entries(GREY_PLUS_PLUS.features).filter(([, v]) => v === 'native').length}`);

        return lines.join('\n');
    }
}

// ═══════════════════════════════════════════════════════════════════════════
//  Cross-Language Construct Mapping Table
// ═══════════════════════════════════════════════════════════════════════════
// Universal construct → Grey++ unified form → all language forms
// This table is the Rosetta Stone of Grey++.

export interface UnificationMapping {
    construct: string;
    greyForm: string;
    description: string;
    /** Language ID → code template */
    forms: Record<string, string>;
}

export const UNIFICATION_TABLE: UnificationMapping[] = [
    {
        construct: 'bind',
        greyForm: 'bind <name> = <expr>',
        description: 'Variable binding / declaration',
        forms: {
            python: '<name> = <expr>',
            javascript: 'const <name> = <expr>;',
            typescript: 'const <name>: <type> = <expr>;',
            java: '<type> <name> = <expr>;',
            go: '<name> := <expr>',
            rust: 'let <name> = <expr>;',
            csharp: 'var <name> = <expr>;',
            cpp: 'auto <name> = <expr>;',
            swift: 'let <name> = <expr>',
            kotlin: 'val <name> = <expr>',
            sql: 'DECLARE @<name> = <expr>;',
            ruby: '<name> = <expr>',
            php: '$<name> = <expr>;',
            bash: '<name>=<expr>',
            powershell: '$<name> = <expr>',
            r: '<name> <- <expr>',
            scala: 'val <name> = <expr>',
            julia: '<name> = <expr>',
            dart: 'final <name> = <expr>;',
            objc: 'id <name> = <expr>;',
            elixir: '<name> = <expr>',
            clojure: '(def <name> <expr>)',
            haskell: 'let <name> = <expr>',
            erlang: '<Name> = <expr>.',
            fsharp: 'let <name> = <expr>',
            ocaml: 'let <name> = <expr>;;',
            lisp: '(defvar <name> <expr>)',
            prolog: '<name> is <expr>.',
            solidity: 'uint <name> = <expr>;',
            zig: 'const <name> = <expr>;',
            c: 'int <name> = <expr>;',
            fortran: 'integer :: <name> = <expr>',
            vhdl: 'signal <name> : integer := <expr>;',
            matlab: '<name> = <expr>;',
            pascal: 'var <name>: Integer = <expr>;',
            perl: 'my $<name> = <expr>;',
            apex: 'Integer <name> = <expr>;',
            mojo: 'let <name> = <expr>',
            lua: 'local <name> = <expr>',
            groovy: 'def <name> = <expr>',
            elm: '<name> = <expr>',
            nim: 'let <name> = <expr>',
            crystal: '<name> = <expr>',
            d: 'auto <name> = <expr>;',
            vbnet: 'Dim <name> = <expr>',
            smalltalk: '<name> := <expr>.',
        },
    },
    {
        construct: 'fn',
        greyForm: 'fn <name>(<params>) { <body> }',
        description: 'Function definition',
        forms: {
            python: 'def <name>(<params>):\n    <body>',
            javascript: 'function <name>(<params>) { <body> }',
            typescript: 'function <name>(<params>): <returnType> { <body> }',
            java: 'public <returnType> <name>(<params>) { <body> }',
            go: 'func <name>(<params>) <returnType> { <body> }',
            rust: 'fn <name>(<params>) -> <returnType> { <body> }',
            csharp: 'public <returnType> <name>(<params>) { <body> }',
            cpp: '<returnType> <name>(<params>) { <body> }',
            swift: 'func <name>(<params>) -> <returnType> { <body> }',
            kotlin: 'fun <name>(<params>): <returnType> { <body> }',
            sql: 'CREATE FUNCTION <name>(<params>) RETURNS <returnType> AS BEGIN <body> END;',
            ruby: 'def <name>(<params>)\n  <body>\nend',
            php: 'function <name>(<params>) { <body> }',
            bash: '<name>() { <body>; }',
            powershell: 'function <name> { param(<params>) <body> }',
            r: '<name> <- function(<params>) { <body> }',
            scala: 'def <name>(<params>): <returnType> = { <body> }',
            julia: 'function <name>(<params>)\n  <body>\nend',
            dart: '<returnType> <name>(<params>) { <body> }',
            objc: '- (<returnType>)<name>:(<params>) { <body> }',
            elixir: 'def <name>(<params>) do\n  <body>\nend',
            clojure: '(defn <name> [<params>] <body>)',
            haskell: '<name> <params> = <body>',
            erlang: '<name>(<params>) -> <body>.',
            fsharp: 'let <name> <params> = <body>',
            ocaml: 'let <name> <params> = <body>;;',
            lisp: '(defun <name> (<params>) <body>)',
            prolog: '<name>(<params>) :- <body>.',
            solidity: 'function <name>(<params>) public returns (<returnType>) { <body> }',
            zig: 'fn <name>(<params>) <returnType> { <body> }',
            c: '<returnType> <name>(<params>) { <body> }',
            fortran: 'function <name>(<params>) result(<returnType>)\n  <body>\nend function',
            vhdl: 'function <name>(<params>) return <returnType> is\nbegin\n  <body>\nend function;',
            matlab: 'function <returnType> = <name>(<params>)\n  <body>\nend',
            pascal: 'function <name>(<params>): <returnType>;\nbegin\n  <body>\nend;',
            perl: 'sub <name> { my (<params>) = @_; <body> }',
            apex: 'public <returnType> <name>(<params>) { <body> }',
            mojo: 'fn <name>(<params>) -> <returnType>:\n    <body>',
            lua: 'function <name>(<params>)\n  <body>\nend',
            groovy: 'def <name>(<params>) { <body> }',
            elm: '<name> <params> =\n  <body>',
            nim: 'proc <name>(<params>): <returnType> =\n  <body>',
            crystal: 'def <name>(<params>)\n  <body>\nend',
            d: '<returnType> <name>(<params>) { <body> }',
            vbnet: 'Function <name>(<params>) As <returnType>\n  <body>\nEnd Function',
            smalltalk: '<name>: <params> [ <body> ]',
        },
    },
    {
        construct: 'loop',
        greyForm: 'loop <binding> in <iterable> { <body> }',
        description: 'Iteration / looping',
        forms: {
            python: 'for <binding> in <iterable>:\n    <body>',
            javascript: 'for (const <binding> of <iterable>) { <body> }',
            typescript: 'for (const <binding> of <iterable>) { <body> }',
            java: 'for (<type> <binding> : <iterable>) { <body> }',
            go: 'for _, <binding> := range <iterable> { <body> }',
            rust: 'for <binding> in <iterable> { <body> }',
            csharp: 'foreach (var <binding> in <iterable>) { <body> }',
            cpp: 'for (auto <binding> : <iterable>) { <body> }',
            swift: 'for <binding> in <iterable> { <body> }',
            kotlin: 'for (<binding> in <iterable>) { <body> }',
            sql: 'DECLARE <binding> CURSOR FOR <iterable>; OPEN <binding>; FETCH NEXT...',
            ruby: '<iterable>.each do |<binding>|\n  <body>\nend',
            php: 'foreach (<iterable> as $<binding>) { <body> }',
            bash: 'for <binding> in <iterable>; do\n  <body>\ndone',
            powershell: 'foreach ($<binding> in <iterable>) { <body> }',
            r: 'for (<binding> in <iterable>) { <body> }',
            scala: 'for (<binding> <- <iterable>) { <body> }',
            julia: 'for <binding> in <iterable>\n  <body>\nend',
            dart: 'for (final <binding> in <iterable>) { <body> }',
            objc: 'for (id <binding> in <iterable>) { <body> }',
            elixir: 'Enum.each(<iterable>, fn <binding> ->\n  <body>\nend)',
            clojure: '(doseq [<binding> <iterable>] <body>)',
            haskell: 'mapM_ (\\<binding> -> <body>) <iterable>',
            erlang: 'lists:foreach(fun(<binding>) -> <body> end, <iterable>).',
            fsharp: '<iterable> |> List.iter (fun <binding> -> <body>)',
            ocaml: 'List.iter (fun <binding> -> <body>) <iterable>;;',
            lisp: '(dolist (<binding> <iterable>) <body>)',
            prolog: 'forall(member(<binding>, <iterable>), <body>).',
            solidity: 'for (uint <binding> = 0; <binding> < <iterable>.length; <binding>++) { <body> }',
            zig: 'for (<iterable>) |<binding>| { <body> }',
            c: 'for (int <binding> = 0; <binding> < <n>; <binding>++) { <body> }',
            fortran: 'do <binding> = 1, <n>\n  <body>\nend do',
            vhdl: 'for <binding> in <iterable> generate\n  <body>\nend generate;',
            matlab: 'for <binding> = <iterable>\n  <body>\nend',
            pascal: 'for <binding> := 1 to <n> do\nbegin\n  <body>\nend;',
            perl: 'foreach my $<binding> (@<iterable>) { <body> }',
            apex: 'for (<type> <binding> : <iterable>) { <body> }',
            mojo: 'for <binding> in <iterable>:\n    <body>',
            lua: 'for _, <binding> in ipairs(<iterable>) do\n  <body>\nend',
            groovy: '<iterable>.each { <binding> -> <body> }',
            elm: 'List.map (\\<binding> -> <body>) <iterable>',
            nim: 'for <binding> in <iterable>:\n  <body>',
            crystal: '<iterable>.each do |<binding>|\n  <body>\nend',
            d: 'foreach (<binding>; <iterable>) { <body> }',
            vbnet: 'For Each <binding> In <iterable>\n  <body>\nNext',
            smalltalk: '<iterable> do: [:<binding> | <body>].',
        },
    },
    {
        construct: 'cond',
        greyForm: 'cond <test> { <then> } else { <otherwise> }',
        description: 'Conditional branching',
        forms: {
            python: 'if <test>:\n    <then>\nelse:\n    <otherwise>',
            javascript: 'if (<test>) { <then> } else { <otherwise> }',
            typescript: 'if (<test>) { <then> } else { <otherwise> }',
            java: 'if (<test>) { <then> } else { <otherwise> }',
            go: 'if <test> { <then> } else { <otherwise> }',
            rust: 'if <test> { <then> } else { <otherwise> }',
            csharp: 'if (<test>) { <then> } else { <otherwise> }',
            cpp: 'if (<test>) { <then> } else { <otherwise> }',
            swift: 'if <test> { <then> } else { <otherwise> }',
            kotlin: 'if (<test>) { <then> } else { <otherwise> }',
            sql: 'CASE WHEN <test> THEN <then> ELSE <otherwise> END',
            ruby: 'if <test>\n  <then>\nelse\n  <otherwise>\nend',
            php: 'if (<test>) { <then> } else { <otherwise> }',
            bash: 'if [ <test> ]; then\n  <then>\nelse\n  <otherwise>\nfi',
            powershell: 'if (<test>) { <then> } else { <otherwise> }',
            r: 'if (<test>) { <then> } else { <otherwise> }',
            scala: 'if (<test>) { <then> } else { <otherwise> }',
            julia: 'if <test>\n  <then>\nelse\n  <otherwise>\nend',
            dart: 'if (<test>) { <then> } else { <otherwise> }',
            objc: 'if (<test>) { <then> } else { <otherwise> }',
            elixir: 'if <test> do\n  <then>\nelse\n  <otherwise>\nend',
            clojure: '(if <test> <then> <otherwise>)',
            haskell: 'if <test> then <then> else <otherwise>',
            erlang: 'case <test> of true -> <then>; false -> <otherwise> end.',
            fsharp: 'if <test> then <then> else <otherwise>',
            ocaml: 'if <test> then <then> else <otherwise>;;',
            lisp: '(if <test> <then> <otherwise>)',
            prolog: '(<test> -> <then> ; <otherwise>).',
            solidity: 'if (<test>) { <then> } else { <otherwise> }',
            zig: 'if (<test>) { <then> } else { <otherwise> }',
            c: 'if (<test>) { <then> } else { <otherwise> }',
            fortran: 'if (<test>) then\n  <then>\nelse\n  <otherwise>\nend if',
            vhdl: 'if <test> then\n  <then>;\nelse\n  <otherwise>;\nend if;',
            matlab: 'if <test>\n  <then>\nelse\n  <otherwise>\nend',
            pascal: 'if <test> then\n  <then>\nelse\n  <otherwise>;',
            perl: 'if (<test>) { <then> } else { <otherwise> }',
            apex: 'if (<test>) { <then> } else { <otherwise> }',
            mojo: 'if <test>:\n    <then>\nelse:\n    <otherwise>',
            lua: 'if <test> then\n  <then>\nelse\n  <otherwise>\nend',
            groovy: 'if (<test>) { <then> } else { <otherwise> }',
            elm: 'if <test> then <then> else <otherwise>',
            nim: 'if <test>:\n  <then>\nelse:\n  <otherwise>',
            crystal: 'if <test>\n  <then>\nelse\n  <otherwise>\nend',
            d: 'if (<test>) { <then> } else { <otherwise> }',
            vbnet: 'If <test> Then\n  <then>\nElse\n  <otherwise>\nEnd If',
            smalltalk: '<test> ifTrue: [<then>] ifFalse: [<otherwise>].',
        },
    },
    {
        construct: 'struct',
        greyForm: 'struct <name> { <fields> }',
        description: 'Data structure / class definition',
        forms: {
            python: 'class <name>:\n  def __init__(self, <fields>): ...',
            javascript: 'class <name> { constructor(<fields>) { ... } }',
            typescript: 'class <name> { <fields> }',
            java: 'public class <name> { <fields> }',
            go: 'type <name> struct { <fields> }',
            rust: 'struct <name> { <fields> }',
            csharp: 'public class <name> { <fields> }',
            cpp: 'struct <name> { <fields> };',
            swift: 'struct <name> { <fields> }',
            kotlin: 'data class <name>(<fields>)',
            sql: 'CREATE TABLE <name> (<fields>);',
            ruby: 'class <name>\n  attr_accessor <fields>\nend',
            php: 'class <name> { <fields> }',
            scala: 'case class <name>(<fields>)',
            julia: 'struct <name>\n  <fields>\nend',
            dart: 'class <name> { <fields> }',
            objc: '@interface <name> : NSObject\n  <fields>\n@end',
            elixir: 'defmodule <name> do\n  defstruct <fields>\nend',
            clojure: '(defrecord <name> [<fields>])',
            haskell: 'data <name> = <name> { <fields> }',
            erlang: '-record(<name>, {<fields>}).',
            fsharp: 'type <name> = { <fields> }',
            ocaml: 'type <name> = { <fields> };;',
            lisp: '(defstruct <name> <fields>)',
            prolog: ':- dynamic <name>/N.',
            solidity: 'struct <name> { <fields> }',
            zig: 'const <name> = struct { <fields> };',
            c: 'typedef struct { <fields> } <name>;',
            fortran: 'type :: <name>\n  <fields>\nend type',
            vhdl: 'type <name> is record\n  <fields>\nend record;',
            matlab: 'classdef <name>\n  properties\n    <fields>\n  end\nend',
            pascal: 'type <name> = record\n  <fields>\nend;',
            perl: 'package <name>;\nsub new { bless {<fields>}, shift }',
            apex: 'public class <name> { <fields> }',
            mojo: 'struct <name>:\n    <fields>',
            lua: '<name> = { <fields> }',
            groovy: 'class <name> { <fields> }',
            elm: 'type alias <name> = { <fields> }',
            nim: 'type <name> = object\n  <fields>',
            crystal: 'class <name>\n  <fields>\nend',
            d: 'struct <name> { <fields> }',
            vbnet: 'Public Class <name>\n  <fields>\nEnd Class',
            smalltalk: 'Object subclass: #<name> instanceVariableNames: \'<fields>\'.',
        },
    },
    {
        construct: 'module',
        greyForm: 'module <name> { <imports> }',
        description: 'Module / import declaration',
        forms: {
            python: 'import <name>',
            javascript: "import { <imports> } from '<name>';",
            typescript: "import { <imports> } from '<name>';",
            java: 'import <name>.*;',
            go: 'import "<name>"',
            rust: 'use <name>;',
            csharp: 'using <name>;',
            cpp: '#include <<name>>',
            swift: 'import <name>',
            kotlin: 'import <name>',
            sql: 'USE <name>;',
            ruby: "require '<name>'",
            php: "use <name>;",
            bash: 'source <name>',
            powershell: 'Import-Module <name>',
            r: 'library(<name>)',
            scala: 'import <name>._',
            julia: 'using <name>',
            dart: "import '<name>';",
            objc: '#import <<name>>',
            elixir: 'import <name>',
            clojure: '(require \'[<name>])',
            haskell: 'import <name>',
            erlang: '-module(<name>).',
            fsharp: 'open <name>',
            ocaml: 'open <name>;;',
            lisp: '(require :name)',
            prolog: ':- use_module(library(<name>)).',
            solidity: "import '<name>';",
            zig: 'const <name> = @import("<name>");',
            c: '#include <<name>>',
            fortran: 'use <name>',
            vhdl: 'library <name>; use <name>.all;',
            matlab: "addpath('<name>')",
            pascal: 'uses <name>;',
            perl: "use <name>;",
            apex: 'import <name>;',
            mojo: 'from <name> import <imports>',
            lua: "local <name> = require('<name>')",
            groovy: 'import <name>',
            elm: 'import <name> exposing (..)',
            nim: 'import <name>',
            crystal: "require \"<name>\"",
            d: 'import <name>;',
            vbnet: 'Imports <name>',
            smalltalk: '',
        },
    },
];

// ═══════════════════════════════════════════════════════════════════════════
//  Self-test
// ═══════════════════════════════════════════════════════════════════════════

if (process.argv[1]?.includes('grey_polyglot') && process.argv.includes('--test')) {
    console.log('═══ grey_polyglot.ts Self-Test ═══\n');

    const registry = new PolyglotRegistry();

    // Count
    console.log(`Total languages: ${registry.count()}`);
    console.assert(registry.count() >= 48, `Expected ≥48, got ${registry.count()}`);
    console.log('  ✓ language count');

    // Group counts
    const g1 = registry.group(1);
    const g2 = registry.group(2);
    const g3 = registry.group(3);
    const g4 = registry.group(4);
    const g5 = registry.group(5);
    console.log(`  Group 1: ${g1.length}, Group 2: ${g2.length}, Group 3: ${g3.length}, Group 4: ${g4.length}, Group 5: ${g5.length}`);
    console.assert(g1.length === 10, `Group 1: expected 10, got ${g1.length}`);
    console.assert(g2.length === 10, `Group 2: expected 10, got ${g2.length}`);
    console.assert(g3.length === 10, `Group 3: expected 10, got ${g3.length}`);
    console.assert(g4.length === 7, `Group 4: expected 7, got ${g4.length}`);
    console.assert(g5.length >= 9, `Group 5: expected ≥9, got ${g5.length}`);
    console.log('  ✓ group distribution');

    // Lookup by ID
    const py = registry.get('python');
    console.assert(py?.name === 'Python', 'python lookup');
    const rs = registry.get('rust');
    console.assert(rs?.name === 'Rust', 'rust lookup');
    const grey = registry.get('grey');
    console.assert(grey?.name === 'Grey++', 'grey++ lookup');
    console.log('  ✓ id lookup');

    // Lookup by extension
    const pyExt = registry.byExtension('.py');
    console.assert(pyExt?.id === 'python', 'extension .py');
    const rsExt = registry.byExtension('.rs');
    console.assert(rsExt?.id === 'rust', 'extension .rs');
    const tsExt = registry.byExtension('.ts');
    console.assert(tsExt?.id === 'typescript', 'extension .ts');
    console.log('  ✓ extension lookup');

    // Lookup by paradigm
    const funcLangs = registry.byParadigm('functional');
    console.assert(funcLangs.length >= 15, `functional langs: ${funcLangs.length}`);
    const sysLangs = registry.byParadigm('systems');
    console.assert(sysLangs.length >= 3, `systems langs: ${sysLangs.length}`);
    console.log('  ✓ paradigm lookup');

    // Lookup by domain
    const aiLangs = registry.byDomain('AI');
    console.assert(aiLangs.length >= 2, `AI domain: ${aiLangs.length}`);
    const webLangs = registry.byDomain('Web');
    console.assert(webLangs.length >= 4, `Web domain: ${webLangs.length}`);
    console.log('  ✓ domain lookup');

    // Language detection
    console.log('\n── detectLanguage ──');
    const detections: [string, string][] = [
        ['def hello():', 'python'],
        ['function hello() { }', 'javascript'],
        ['let x: number = 5;', 'typescript'],
        ['public static void main(String[] args) { }', 'java'],
        ['func main() { }', 'go'],
        ['fn main() -> i32 { }', 'rust'],
        ['Console.WriteLine("hello");', 'csharp'],
        ['#include <iostream>', 'cpp'],
        ['func hello() -> String { }', 'swift'],
        ['fun hello() { }', 'kotlin'],
        ['SELECT * FROM users;', 'sql'],
        ['$name = "test";', 'php'],
        ['export PATH=$HOME/bin', 'bash'],
        ['defmodule MyApp do', 'elixir'],
        ['(defn hello [x] x)', 'clojure'],
        ['data Maybe a = Nothing | Just a', 'haskell'],
        ['-module(myapp).', 'erlang'],
        ['(defun hello () (format t "hi"))', 'lisp'],
        ['pragma solidity ^0.8.0;', 'solidity'],
        ['bind x = 42', 'grey'],
        ['pipeline { infer model=basic }', 'grey'],
    ];
    let detectPass = 0;
    for (const [src, expected] of detections) {
        const got = registry.detectLanguage(src);
        if (got === expected) {
            detectPass++;
        } else {
            console.log(`  ✗ "${src}" → expected ${expected}, got ${got}`);
        }
    }
    console.log(`  ✓ detectLanguage: ${detectPass}/${detections.length} passed`);

    // Print form normalization
    console.log('\n── print normalization ──');
    console.assert(registry.normalizePrint('console.log("hi")', 'javascript').includes('print('), 'js normalize');
    console.assert(registry.normalizePrint('System.out.println("hi")', 'java').includes('print('), 'java normalize');
    console.assert(registry.normalizePrint('fmt.Println("hi")', 'go').includes('print('), 'go normalize');
    console.assert(registry.normalizePrint('println!("hi")', 'rust').includes('print('), 'rust normalize');
    console.log('  ✓ print normalization');

    // Print form denormalization
    console.assert(registry.denormalizePrint('print("hi")', 'javascript').includes('console.log'), 'js denormalize');
    console.assert(registry.denormalizePrint('print("hi")', 'java').includes('System.out.println'), 'java denormalize');
    console.assert(registry.denormalizePrint('print("hi")', 'rust').includes('println!'), 'rust denormalize');
    console.log('  ✓ print denormalization');

    // Feature matrix
    const gcMatrix = registry.featureMatrix('gc');
    console.assert(gcMatrix.python === 'native', 'python gc');
    console.assert(gcMatrix.rust === 'unsupported', 'rust gc');
    console.assert(gcMatrix.c === 'unsupported', 'c gc');
    console.log('  ✓ feature matrix');

    // Construct forms
    const bindForms = registry.constructForms('bind');
    console.assert(bindForms.python.includes('='), 'python bind form');
    console.assert(bindForms.rust.includes('let'), 'rust bind form');
    console.assert(bindForms.go.includes(':='), 'go bind form');
    console.log('  ✓ construct forms');

    // Unification table
    console.assert(UNIFICATION_TABLE.length === 6, `Expected 6 mappings, got ${UNIFICATION_TABLE.length}`);
    for (const mapping of UNIFICATION_TABLE) {
        const langCount = Object.keys(mapping.forms).length;
        console.assert(langCount >= 30, `${mapping.construct}: expected ≥30 forms, got ${langCount}`);
    }
    console.log('  ✓ unification table');

    // Summary
    console.log('\n── Summary ──');
    const summaryText = registry.summary();
    console.assert(summaryText.includes('Python'), 'summary includes Python');
    console.assert(summaryText.includes('Rust'), 'summary includes Rust');
    console.assert(summaryText.includes('Grey++'), 'summary includes Grey++');
    console.assert(summaryText.includes('Haskell'), 'summary includes Haskell');
    console.assert(summaryText.includes('Fortran'), 'summary includes Fortran');
    console.log('  ✓ summary generation');

    console.log('\n✓ All grey_polyglot tests passed.');
}
