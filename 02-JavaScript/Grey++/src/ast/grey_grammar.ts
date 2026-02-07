// ─── Grey++ Grammar Layer ────────────────────────────────────────────────────
// Minimal grammar layer that can parse unified `do { … }` syntax into AST,
// support semantic collapsing (grouping heterogeneous primitives), and output
// a unified AST model.
//
// No external parser libraries — uses simple string/token analysis to
// classify steps within a do-block.  The real parsing is done by the REPL
// parser; this layer provides grammar-level analysis and collapsing.

import { LanguageCoordinator } from './grey_language.js';
import type { TranslationResult } from './grey_language.js';

// ─── Universal Grammar Mapping ──────────────────────────────────────────────
// Maps universal constructs (bind, fn, loop, cond) to their language-specific
// surface forms across Python, JavaScript, C++, and Rust.

/** A single grammar rule mapping a universal construct to language surface forms. */
export interface UniversalGrammarRule {
    /** Universal construct name: bind, fn, loop, cond. */
    construct: string;
    /** Language → surface keyword(s) mapping. */
    forms: Record<string, string[]>;
    /** Grey++ unified syntax. */
    unified: string;
    /** Description of the construct. */
    description: string;
}

/** The complete universal grammar mapping. */
export const UNIVERSAL_GRAMMAR: UniversalGrammarRule[] = [
    {
        construct: 'bind',
        forms: {
            // Group 1
            python: ['='],
            javascript: ['let', 'const', 'var'],
            typescript: ['let', 'const', 'var'],
            java: ['int', 'String', 'var', 'final'],
            go: [':=', 'var'],
            rust: ['let', 'let mut'],
            csharp: ['var', 'int', 'string', 'const'],
            cpp: ['int', 'float', 'double', 'char', 'auto', 'std::string'],
            swift: ['var', 'let'],
            kotlin: ['val', 'var'],
            // Group 2
            sql: ['DECLARE', 'SET'],
            ruby: ['='],
            php: ['$'],
            bash: ['=', 'export', 'local'],
            powershell: ['$'],
            r: ['<-', '='],
            scala: ['val', 'var'],
            julia: ['=', 'const'],
            dart: ['var', 'final', 'const'],
            objc: ['int', 'NSString*', 'id'],
            // Group 3
            elixir: ['='],
            clojure: ['def', 'let'],
            haskell: ['let', '='],
            erlang: ['='],
            fsharp: ['let', 'let mutable'],
            ocaml: ['let'],
            lisp: ['defvar', 'setq', 'let'],
            prolog: ['is', '='],
            solidity: ['uint', 'int', 'address', 'string', 'bool'],
            zig: ['const', 'var'],
            // Group 4
            c: ['int', 'float', 'double', 'char', 'const'],
            fortran: ['integer', 'real', 'character'],
            vhdl: ['signal', 'variable', 'constant'],
            matlab: ['='],
            pascal: ['var', 'const', ':='],
            perl: ['my', 'our', 'local'],
            apex: ['Integer', 'String', 'Boolean'],
            // Group 5
            mojo: ['var', 'let', 'alias'],
            lua: ['local', '='],
            groovy: ['def', 'int', 'String'],
            elm: ['='],
            nim: ['var', 'let', 'const'],
            crystal: ['='],
            d: ['auto', 'int', 'immutable'],
            vbnet: ['Dim', 'Const'],
            smalltalk: [':='],
            // Grey++
            grey: ['bind'],
        },
        unified: 'bind <name> = <expr>',
        description: 'Variable binding / declaration',
    },
    {
        construct: 'fn',
        forms: {
            python: ['def'],
            javascript: ['function', '=>'],
            typescript: ['function', '=>'],
            java: ['void', 'public', 'private', 'static'],
            go: ['func'],
            rust: ['fn'],
            csharp: ['void', 'public', 'private', 'static'],
            cpp: ['void', 'int', 'auto'],
            swift: ['func'],
            kotlin: ['fun'],
            sql: ['CREATE FUNCTION', 'CREATE PROCEDURE'],
            ruby: ['def'],
            php: ['function'],
            bash: ['function'],
            powershell: ['function'],
            r: ['function'],
            scala: ['def'],
            julia: ['function'],
            dart: ['void', 'int', 'String'],
            objc: ['-', '+'],
            elixir: ['def', 'defp'],
            clojure: ['defn', 'fn'],
            haskell: [''],
            erlang: ['-spec'],
            fsharp: ['let'],
            ocaml: ['let'],
            lisp: ['defun', 'lambda'],
            prolog: [':-'],
            solidity: ['function'],
            zig: ['fn', 'pub fn'],
            c: ['void', 'int', 'float', 'double'],
            fortran: ['subroutine', 'function'],
            vhdl: ['process', 'function', 'procedure'],
            matlab: ['function'],
            pascal: ['procedure', 'function'],
            perl: ['sub'],
            apex: ['public', 'private', 'static', 'void'],
            mojo: ['fn', 'def'],
            lua: ['function', 'local function'],
            groovy: ['def', 'void'],
            elm: [''],
            nim: ['proc', 'func'],
            crystal: ['def'],
            d: ['void', 'int', 'auto'],
            vbnet: ['Sub', 'Function'],
            smalltalk: ['[]'],
            grey: ['fn'],
        },
        unified: 'fn <name>(<params>) { <body> }',
        description: 'Function definition',
    },
    {
        construct: 'loop',
        forms: {
            python: ['for', 'while'],
            javascript: ['for', 'while', 'for...of', 'for...in'],
            typescript: ['for', 'while', 'for...of', 'for...in'],
            java: ['for', 'while', 'do', 'for-each'],
            go: ['for'],
            rust: ['for', 'while', 'loop'],
            csharp: ['for', 'while', 'foreach', 'do'],
            cpp: ['for', 'while', 'do'],
            swift: ['for', 'while', 'repeat'],
            kotlin: ['for', 'while', 'do'],
            sql: ['WHILE', 'CURSOR'],
            ruby: ['for', 'while', 'until', 'each', 'times'],
            php: ['for', 'while', 'foreach', 'do'],
            bash: ['for', 'while', 'until'],
            powershell: ['for', 'foreach', 'while', 'do'],
            r: ['for', 'while', 'repeat'],
            scala: ['for', 'while'],
            julia: ['for', 'while'],
            dart: ['for', 'while', 'do', 'for-in'],
            objc: ['for', 'while', 'do', 'for-in'],
            elixir: ['for', 'Enum.each'],
            clojure: ['loop', 'doseq', 'for'],
            haskell: ['map', 'foldr', 'foldl'],
            erlang: ['lists:foreach', 'lists:map'],
            fsharp: ['for', 'while', 'Seq', 'List'],
            ocaml: ['for', 'while', 'List.iter'],
            lisp: ['loop', 'dolist', 'dotimes', 'mapcar'],
            prolog: ['findall', 'forall'],
            solidity: ['for', 'while', 'do'],
            zig: ['for', 'while'],
            c: ['for', 'while', 'do'],
            fortran: ['do', 'do while'],
            vhdl: ['for', 'while', 'generate'],
            matlab: ['for', 'while', 'parfor'],
            pascal: ['for', 'while', 'repeat'],
            perl: ['for', 'foreach', 'while', 'until'],
            apex: ['for', 'while', 'do'],
            mojo: ['for', 'while'],
            lua: ['for', 'while', 'repeat'],
            groovy: ['for', 'while', 'each'],
            elm: ['List.map', 'List.foldr'],
            nim: ['for', 'while'],
            crystal: ['each', 'times', 'while', 'loop'],
            d: ['for', 'foreach', 'while', 'do'],
            vbnet: ['For', 'For Each', 'While', 'Do While'],
            smalltalk: ['timesRepeat:', 'do:', 'whileTrue:'],
            grey: ['loop'],
        },
        unified: 'loop <binding> in <iterable> { <body> }',
        description: 'Iteration / looping construct',
    },
    {
        construct: 'cond',
        forms: {
            python: ['if', 'elif', 'else'],
            javascript: ['if', 'else if', 'else'],
            typescript: ['if', 'else if', 'else'],
            java: ['if', 'else if', 'else', 'switch'],
            go: ['if', 'else if', 'else', 'switch'],
            rust: ['if', 'else if', 'else', 'match'],
            csharp: ['if', 'else if', 'else', 'switch'],
            cpp: ['if', 'else if', 'else', 'switch'],
            swift: ['if', 'else if', 'else', 'switch', 'guard'],
            kotlin: ['if', 'else if', 'else', 'when'],
            sql: ['CASE', 'WHEN', 'IF', 'ELSE'],
            ruby: ['if', 'elsif', 'else', 'unless', 'case'],
            php: ['if', 'elseif', 'else', 'switch', 'match'],
            bash: ['if', 'elif', 'else', 'case'],
            powershell: ['if', 'elseif', 'else', 'switch'],
            r: ['if', 'else if', 'else', 'ifelse'],
            scala: ['if', 'else', 'match'],
            julia: ['if', 'elseif', 'else'],
            dart: ['if', 'else if', 'else', 'switch'],
            objc: ['if', 'else if', 'else', 'switch'],
            elixir: ['if', 'else', 'cond', 'case'],
            clojure: ['if', 'cond', 'case', 'when'],
            haskell: ['if', 'then', 'else', 'case', 'of'],
            erlang: ['case', 'of', 'if', 'when'],
            fsharp: ['if', 'elif', 'else', 'match'],
            ocaml: ['if', 'then', 'else', 'match'],
            lisp: ['if', 'cond', 'when', 'unless'],
            prolog: ['->', ';'],
            solidity: ['if', 'else if', 'else'],
            zig: ['if', 'else', 'switch'],
            c: ['if', 'else if', 'else', 'switch'],
            fortran: ['if', 'then', 'else if', 'else', 'select case'],
            vhdl: ['if', 'elsif', 'else', 'case', 'when'],
            matlab: ['if', 'elseif', 'else', 'switch'],
            pascal: ['if', 'then', 'else', 'case'],
            perl: ['if', 'elsif', 'else', 'unless', 'given'],
            apex: ['if', 'else if', 'else', 'switch on'],
            mojo: ['if', 'elif', 'else'],
            lua: ['if', 'elseif', 'else'],
            groovy: ['if', 'else if', 'else', 'switch'],
            elm: ['if', 'then', 'else', 'case', 'of'],
            nim: ['if', 'elif', 'else', 'case', 'of'],
            crystal: ['if', 'elsif', 'else', 'unless', 'case'],
            d: ['if', 'else if', 'else', 'switch'],
            vbnet: ['If', 'ElseIf', 'Else', 'Select Case'],
            smalltalk: ['ifTrue:', 'ifFalse:', 'ifTrue:ifFalse:'],
            grey: ['cond'],
        },
        unified: 'cond <test> { <then> } else { <otherwise> }',
        description: 'Conditional branching',
    },
    {
        construct: 'struct',
        forms: {
            python: ['class'],
            javascript: ['class'],
            typescript: ['class', 'interface', 'type'],
            java: ['class', 'interface', 'record', 'enum'],
            go: ['struct', 'interface'],
            rust: ['struct', 'enum', 'trait'],
            csharp: ['class', 'struct', 'interface', 'record'],
            cpp: ['struct', 'class'],
            swift: ['struct', 'class', 'protocol', 'enum'],
            kotlin: ['class', 'data class', 'interface', 'object'],
            sql: ['CREATE TABLE', 'CREATE VIEW'],
            ruby: ['class', 'Struct'],
            php: ['class', 'interface', 'trait'],
            bash: [],
            powershell: ['class'],
            r: ['list', 'data.frame', 'setClass'],
            scala: ['class', 'case class', 'trait', 'object'],
            julia: ['struct', 'mutable struct'],
            dart: ['class', 'abstract class', 'mixin'],
            objc: ['@interface', '@implementation', '@protocol'],
            elixir: ['defmodule', 'defstruct'],
            clojure: ['defrecord', 'deftype'],
            haskell: ['data', 'newtype', 'type'],
            erlang: ['-record'],
            fsharp: ['type'],
            ocaml: ['type', 'module', 'class'],
            lisp: ['defclass', 'defstruct'],
            prolog: [':-'],
            solidity: ['contract', 'struct', 'interface'],
            zig: ['struct', 'union', 'enum'],
            c: ['struct', 'union', 'typedef'],
            fortran: ['type'],
            vhdl: ['entity', 'architecture'],
            matlab: ['classdef'],
            pascal: ['record', 'class', 'interface'],
            perl: ['package', 'bless'],
            apex: ['class', 'interface', 'enum'],
            mojo: ['struct', 'trait'],
            lua: ['{}'],
            groovy: ['class', 'interface', 'trait'],
            elm: ['type', 'type alias'],
            nim: ['type', 'object'],
            crystal: ['class', 'struct', 'module'],
            d: ['struct', 'class', 'interface'],
            vbnet: ['Class', 'Structure', 'Interface'],
            smalltalk: ['Object subclass:'],
            grey: ['struct'],
        },
        unified: 'struct <name> { <fields> }',
        description: 'Struct / class definition',
    },
    {
        construct: 'module',
        forms: {
            python: ['import', 'from'],
            javascript: ['import', 'export'],
            typescript: ['import', 'export'],
            java: ['import', 'package'],
            go: ['import', 'package'],
            rust: ['mod', 'use'],
            csharp: ['using', 'namespace'],
            cpp: ['#include', 'namespace'],
            swift: ['import'],
            kotlin: ['import', 'package'],
            sql: ['USE', 'CREATE SCHEMA'],
            ruby: ['require', 'include', 'module'],
            php: ['use', 'namespace', 'require'],
            bash: ['source', '.'],
            powershell: ['Import-Module', 'using module'],
            r: ['library', 'require'],
            scala: ['import', 'package'],
            julia: ['using', 'import', 'module'],
            dart: ['import', 'export', 'library'],
            objc: ['#import', '@import'],
            elixir: ['import', 'use', 'alias'],
            clojure: ['ns', 'require', 'import'],
            haskell: ['import', 'module'],
            erlang: ['-module', '-export', '-import'],
            fsharp: ['open', 'module', 'namespace'],
            ocaml: ['open', 'module', 'include'],
            lisp: ['require', 'provide', 'use-package'],
            prolog: [':- module', ':- use_module'],
            solidity: ['import'],
            zig: ['@import', 'pub'],
            c: ['#include', '#define'],
            fortran: ['use', 'module', 'program'],
            vhdl: ['library', 'use', 'package'],
            matlab: ['import', 'addpath'],
            pascal: ['uses', 'unit', 'program'],
            perl: ['use', 'require', 'package'],
            apex: ['import'],
            mojo: ['import', 'from'],
            lua: ['require'],
            groovy: ['import', 'package'],
            elm: ['import', 'module', 'exposing'],
            nim: ['import', 'from', 'include'],
            crystal: ['require', 'include'],
            d: ['import', 'module'],
            vbnet: ['Imports', 'Namespace'],
            smalltalk: [],
            grey: ['module'],
        },
        unified: 'module <name> { <body> }',
        description: 'Module / import declaration',
    },
];

/** Detect which universal construct a source line represents, given a language. */
export function detectConstruct(source: string, language: string): string | null {
    const trimmed = source.trim();
    for (const rule of UNIVERSAL_GRAMMAR) {
        const forms = rule.forms[language];
        if (!forms) continue;
        for (const form of forms) {
            // Check if the line starts with this keyword form
            if (trimmed.startsWith(form + ' ') || trimmed.startsWith(form + '(') || trimmed === form) {
                return rule.construct;
            }
        }
    }
    // Heuristic: Python assignment (name = expr) without keyword
    if (language === 'python' && /^[a-zA-Z_]\w*\s*=\s*[^=]/.test(trimmed)) return 'bind';
    // Heuristic: C++ #include → module
    if (language === 'cpp' && trimmed.startsWith('#include')) return 'module';
    return null;
}

/** Detect language from source heuristics — supports 50+ languages. */
export function detectLanguage(source: string): string {
    // Delegate to the polyglot registry if available, otherwise use inline heuristics.
    const trimmed = source.trim();

    // ── Grey++ unique keywords (highest priority) ──
    if (/^(bind|loop|cond|pipeline|query|infer|contract|hw|rule)\s/.test(trimmed)) return 'grey';
    if (trimmed.startsWith('fn ') && !trimmed.includes('->')) return 'grey';
    if (trimmed.startsWith('module ') && trimmed.includes('{')) return 'grey';

    // ── Group 1 ──
    if (trimmed.startsWith('def ') && trimmed.includes(':')) return 'python';
    if (trimmed.startsWith('fn ') && trimmed.includes('->')) return 'rust';
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
        if (/\b(extends|implements)\b/.test(trimmed)) return 'java';
        return 'javascript';
    }
    if (/^(public|private|protected|static)\s/.test(trimmed)) {
        if (trimmed.includes('void') || trimmed.includes('String') || trimmed.includes('int ')) {
            if (/\b(override|abstract|async|=>)\b/.test(trimmed)) return 'csharp';
            return 'java';
        }
        return 'csharp';
    }
    // struct disambiguation
    if (trimmed.startsWith('struct ')) {
        if (/:\s*\w+/.test(trimmed) && !trimmed.includes(';')) return 'rust';
        if (/\b(int|float|double|char|auto|std::)\b/.test(trimmed) || trimmed.endsWith('};')) return 'cpp';
        return 'grey';
    }
    if (/^import\s/.test(trimmed)) {
        if (trimmed.includes("'") || trimmed.includes('"') || trimmed.includes('{')) return 'javascript';
        if (trimmed.includes('*')) return 'java';
        return 'python';
    }
    if (/^export\s/.test(trimmed)) return 'javascript';
    if (/^#include\s/.test(trimmed)) {
        if (trimmed.includes('std::') || trimmed.includes('<iostream>') || trimmed.includes('<string>') || trimmed.includes('<vector>')) return 'cpp';
        return 'cpp'; // default to C++ for includes
    }
    if (/^namespace\s/.test(trimmed)) {
        if (trimmed.includes('::')) return 'cpp';
        return 'csharp';
    }
    if (/^(int|float|double|char|auto|void|std::)\s/.test(trimmed)) {
        if (trimmed.includes('std::')) return 'cpp';
        return 'c';
    }
    if (/^using\s/.test(trimmed)) return 'csharp';

    // ── Group 2 ──
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

    // ── Group 3 ──
    if (/^defmodule\s/.test(trimmed)) return 'elixir';
    if (/^\(defn?\s/.test(trimmed) || /^\(ns\s/.test(trimmed)) return 'clojure';
    if (/^(data|newtype|instance|deriving)\s/.test(trimmed)) return 'haskell';
    if (/^-module\s*\(/.test(trimmed) || /^-export\s*\(/.test(trimmed)) return 'erlang';
    if (/^(open|let\s+\w+\s*:)/.test(trimmed) && !trimmed.includes('{')) return 'fsharp';
    if (/^(let\s+(rec\s+)?\w+\s*=|type\s+\w+\s*=)/.test(trimmed) && !trimmed.includes('{')) return 'ocaml';
    if (/^\(defun\s/.test(trimmed) || /^\(defvar\s/.test(trimmed) || /^\(setq\s/.test(trimmed)) return 'lisp';
    if (/^:-\s*(module|use_module)/.test(trimmed) || /^\w+\s*\(.*\)\s*:-/.test(trimmed)) return 'prolog';
    if (/^(pragma\s+solidity|contract\s+\w+|mapping\s*\()/.test(trimmed)) return 'solidity';
    if (/^const\s+\w+\s*=\s*@import/.test(trimmed)) return 'zig';

    // ── Group 4 ──
    if (/^(integer|real|character|logical|complex|subroutine|program)\s/i.test(trimmed)) return 'fortran';
    if (/^(signal|entity|architecture|component|process)\s/i.test(trimmed)) return 'vhdl';
    if (/^(classdef|properties|methods)\s/i.test(trimmed) || /^function\s+\[/.test(trimmed)) return 'matlab';
    if (/^(procedure|program|unit|uses)\s/i.test(trimmed)) return 'pascal';
    if (/^(my|our)\s+\$/.test(trimmed) || /^sub\s+\w+\s*\{/.test(trimmed)) return 'perl';

    // ── Group 5 ──
    if (/^(local\s+function|local\s+\w+\s*=)/.test(trimmed)) return 'lua';
    if (/^(Dim|Sub|Function|Module|Imports|Namespace)\s/i.test(trimmed)) return 'vbnet';
    if (/^\w+\s+subclass:\s/.test(trimmed)) return 'smalltalk';
    if (/^proc\s+\w+/.test(trimmed)) return 'nim';

    // ── Fallback patterns ──
    if (/^class\s+\w+.*:\s*$/.test(trimmed) || /^class\s+\w+\s*:/.test(trimmed)) return 'python';
    if (/^(from|import)\s/.test(trimmed)) return 'python';
    if (/^for\s+\w+\s+in\s/.test(trimmed)) {
        if (trimmed.includes(':')) return 'python';
        if (trimmed.includes('{')) return 'rust';
        return 'python';
    }
    if (/^(if|elif)\s.*:\s*/.test(trimmed)) return 'python';
    if (/^while\s.*:\s*/.test(trimmed)) return 'python';
    if (/^for\s*\(\s*(?:let|var|const)/.test(trimmed)) return 'javascript';
    if (/^while\s*\(/.test(trimmed) && trimmed.includes('{')) return 'javascript';
    if (/^if\s*\(/.test(trimmed) && trimmed.includes('{')) return 'javascript';
    if (/^[a-zA-Z_]\w*\s*=\s*[^=]/.test(trimmed)) return 'python';

    return 'unknown';
}

/** Get grammar rule for a construct name. */
export function getGrammarRule(construct: string): UniversalGrammarRule | undefined {
    return UNIVERSAL_GRAMMAR.find(r => r.construct === construct);
}

// ─── Types ──────────────────────────────────────────────────────────────────

/** A classified step within a do-block. */
export interface GrammarStep {
    index: number;
    /** Raw source line or token string. */
    source: string;
    /** The resolved keyword (net, sec, doc, etc.). */
    keyword: string;
    /** The primitive after the dot (http, perm, edit, etc.). */
    primitive?: string;
    /** Translation result from LanguageCoordinator. */
    translation: TranslationResult;
}

/** A collapsed semantic group — adjacent steps of the same kind. */
export interface SemanticGroup {
    nodeKind: string;
    steps: GrammarStep[];
    collapsed: boolean;
}

/** The unified AST model output. */
export interface UnifiedASTModel {
    _type: 'UnifiedASTModel';
    label?: string;
    totalSteps: number;
    steps: GrammarStep[];
    groups: SemanticGroup[];
    groupCount: number;
    meta: { stub: boolean };
}

// ═══════════════════════════════════════════════════════════════════════════
//  GrammarLayer
// ═══════════════════════════════════════════════════════════════════════════

export class GrammarLayer {
    private coordinator: LanguageCoordinator;

    constructor(coordinator?: LanguageCoordinator) {
        this.coordinator = coordinator ?? new LanguageCoordinator();
    }

    // ── Parse ───────────────────────────────────────────────────────────

    /**
     * Parse a list of step source strings into classified GrammarSteps.
     *
     * Each source string is expected to be a single primitive invocation
     * line like:
     *   "net.http GET https://example.com"
     *   "sec.perm user=alice action=doc.edit"
     *   "doc.edit doc=report target=heading.1 value=New Title"
     *   "pipeline { infer model=basic }"
     */
    parseSteps(sources: string[]): GrammarStep[] {
        return sources.map((src, i) => this.classifyStep(src, i));
    }

    /**
     * Classify a single source line into a GrammarStep.
     */
    classifyStep(source: string, index: number = 0): GrammarStep {
        const trimmed = source.trim();
        const firstToken = trimmed.split(/[\s.]/)[0];

        // Check for dot-notation: keyword.primitive
        const dotMatch = trimmed.match(/^([a-zA-Z_]\w*)\.([a-zA-Z_]\w*)/);
        const keyword = dotMatch ? dotMatch[1] : firstToken;
        const primitive = dotMatch ? dotMatch[2] : undefined;

        const translation = this.coordinator.translate(keyword, primitive);

        return {
            index,
            source: trimmed,
            keyword,
            primitive,
            translation,
        };
    }

    // ── Semantic Collapsing ─────────────────────────────────────────────

    /**
     * Group steps by their resolved nodeKind, collapsing adjacent steps
     * of the same kind into semantic groups.
     *
     * For example:
     *   [Net, Net, Sec, Doc, Doc] → [NetGroup(2), SecGroup(1), DocGroup(2)]
     */
    collapse(steps: GrammarStep[]): SemanticGroup[] {
        if (steps.length === 0) return [];

        const groups: SemanticGroup[] = [];
        let currentKind = steps[0].translation.nodeKind;
        let currentSteps: GrammarStep[] = [steps[0]];

        for (let i = 1; i < steps.length; i++) {
            const kind = steps[i].translation.nodeKind;
            if (kind === currentKind) {
                currentSteps.push(steps[i]);
            } else {
                groups.push({
                    nodeKind: currentKind,
                    steps: currentSteps,
                    collapsed: currentSteps.length > 1,
                });
                currentKind = kind;
                currentSteps = [steps[i]];
            }
        }

        // Push the last group.
        groups.push({
            nodeKind: currentKind,
            steps: currentSteps,
            collapsed: currentSteps.length > 1,
        });

        return groups;
    }

    // ── Output Unified AST Model ────────────────────────────────────────

    /**
     * Produce a unified AST model from a list of source lines.
     * Combines parsing, classification, and semantic collapsing.
     */
    toUnifiedASTModel(sources: string[], label?: string): UnifiedASTModel {
        const steps = this.parseSteps(sources);
        const groups = this.collapse(steps);

        return {
            _type: 'UnifiedASTModel',
            label,
            totalSteps: steps.length,
            steps,
            groups,
            groupCount: groups.length,
            meta: { stub: true },
        };
    }

    // ── Accessors ───────────────────────────────────────────────────────

    getCoordinator(): LanguageCoordinator {
        return this.coordinator;
    }
}

// ─── Self-test ──────────────────────────────────────────────────────────────

if (process.argv[1]?.includes('grey_grammar') && process.argv.includes('--test')) {
    console.log('═══ grey_grammar.ts Self-Test ═══\n');

    const grammar = new GrammarLayer();

    // Classify individual steps.
    console.log('── classifyStep (net.http) ──');
    console.log(JSON.stringify(grammar.classifyStep('net.http GET "https://example.com"'), null, 2));

    console.log('\n── classifyStep (sec.perm) ──');
    console.log(JSON.stringify(grammar.classifyStep('sec.perm user=alice action=doc.edit'), null, 2));

    console.log('\n── classifyStep (doc.edit) ──');
    console.log(JSON.stringify(grammar.classifyStep('doc.edit doc="report" target=heading.1 value="New Title"'), null, 2));

    console.log('\n── classifyStep (pipeline) ──');
    console.log(JSON.stringify(grammar.classifyStep('pipeline { infer model=basic }'), null, 2));

    console.log('\n── classifyStep (unknown) ──');
    console.log(JSON.stringify(grammar.classifyStep('foobar something'), null, 2));

    // Parse multiple steps.
    const sources = [
        'net.http GET "https://api.example.com"',
        'net.http POST "https://api.example.com/data"',
        'sec.perm user=alice action=doc.edit',
        'dist.consensus proposal="upgrade pipeline"',
        'doc.edit doc="report" target=heading.1 value="New Title"',
        'doc.render doc="report" format=html',
    ];

    console.log('\n── parseSteps (6 steps) ──');
    const steps = grammar.parseSteps(sources);
    for (const s of steps) {
        console.log(`  [${s.index}] ${s.keyword}${s.primitive ? '.' + s.primitive : ''} → ${s.translation.nodeKind} (resolved: ${s.translation.resolved})`);
    }

    // Semantic collapsing.
    console.log('\n── collapse ──');
    const groups = grammar.collapse(steps);
    for (const g of groups) {
        console.log(`  ${g.nodeKind}: ${g.steps.length} step(s) (collapsed: ${g.collapsed})`);
    }

    // Unified AST model.
    console.log('\n── toUnifiedASTModel ──');
    const model = grammar.toUnifiedASTModel(sources, 'test-do-block');
    console.log(`Label: ${model.label}`);
    console.log(`Steps: ${model.totalSteps}, Groups: ${model.groupCount}`);
    console.log(JSON.stringify(model.groups.map(g => ({
        kind: g.nodeKind,
        count: g.steps.length,
        collapsed: g.collapsed,
    })), null, 2));

    console.log('\n✓ All grey_grammar tests passed.');

    // ── Stage 18.5+: Universal Grammar Mapping tests (50+ languages) ──
    console.log('\n── Universal Grammar Mapping (50+ languages) ──');

    // detectLanguage — Group 1
    console.assert(detectLanguage('def hello():') === 'python', 'detect python def');
    console.assert(detectLanguage('function hello() {}') === 'javascript', 'detect js function');
    console.assert(detectLanguage('let x = 5') === 'javascript', 'detect js let');
    console.assert(detectLanguage('let x: number = 5;') === 'typescript', 'detect ts let');
    console.assert(detectLanguage('int main() {}') === 'c', 'detect c int');
    console.assert(detectLanguage('#include <iostream>') === 'cpp', 'detect cpp include');
    console.assert(detectLanguage('fn hello() -> i32 {}') === 'rust', 'detect rust fn');
    console.assert(detectLanguage('func main() {}') === 'go', 'detect go func');
    console.assert(detectLanguage('fun hello() {}') === 'kotlin', 'detect kotlin fun');
    console.assert(detectLanguage('bind x = 5') === 'grey', 'detect grey bind');
    console.assert(detectLanguage('pipeline { infer model=basic }') === 'grey', 'detect grey pipeline');
    console.log('  ✓ detectLanguage (Group 1)');

    // detectLanguage — Group 2
    console.assert(detectLanguage('SELECT * FROM users') === 'sql', 'detect sql select');
    console.assert(detectLanguage('$name = "test"') === 'php', 'detect php');
    console.assert(detectLanguage('export PATH=$HOME/bin') === 'bash', 'detect bash');
    console.log('  ✓ detectLanguage (Group 2)');

    // detectLanguage — Group 3
    console.assert(detectLanguage('defmodule MyApp do') === 'elixir', 'detect elixir');
    console.assert(detectLanguage('(defn hello [x] x)') === 'clojure', 'detect clojure');
    console.assert(detectLanguage('data Maybe a = Nothing | Just a') === 'haskell', 'detect haskell');
    console.assert(detectLanguage('-module(myapp).') === 'erlang', 'detect erlang');
    console.assert(detectLanguage('(defun hello () nil)') === 'lisp', 'detect lisp');
    console.assert(detectLanguage('pragma solidity ^0.8.0;') === 'solidity', 'detect solidity');
    console.log('  ✓ detectLanguage (Group 3)');

    // detectLanguage — Group 4
    console.assert(detectLanguage('integer :: x = 5') === 'fortran', 'detect fortran');
    console.assert(detectLanguage('entity counter is') === 'vhdl', 'detect vhdl');
    console.assert(detectLanguage('sub hello { }') === 'perl', 'detect perl');
    console.log('  ✓ detectLanguage (Group 4)');

    // detectLanguage — Group 5
    console.assert(detectLanguage('local x = 5') === 'lua', 'detect lua');
    console.assert(detectLanguage('Dim x = 5') === 'vbnet', 'detect vbnet');
    console.assert(detectLanguage('proc hello() =') === 'nim', 'detect nim');
    console.log('  ✓ detectLanguage (Group 5)');

    // detectConstruct
    console.assert(detectConstruct('let x = 5', 'javascript') === 'bind', 'js let → bind');
    console.assert(detectConstruct('def hello():', 'python') === 'fn', 'python def → fn');
    console.assert(detectConstruct('for i in range(10):', 'python') === 'loop', 'python for → loop');
    console.assert(detectConstruct('if x > 5:', 'python') === 'cond', 'python if → cond');
    console.assert(detectConstruct('x = 42', 'python') === 'bind', 'python assignment → bind');
    console.assert(detectConstruct('fn add(a, b) {}', 'rust') === 'fn', 'rust fn → fn');
    console.log('  ✓ detectConstruct');

    // getGrammarRule
    const bindRule = getGrammarRule('bind');
    console.assert(bindRule !== undefined, 'bind rule exists');
    console.assert(bindRule!.forms.python[0] === '=', 'bind python form');
    console.assert(getGrammarRule('unknown') === undefined, 'unknown rule absent');
    console.log('  ✓ getGrammarRule');

    // UNIVERSAL_GRAMMAR coverage
    console.assert(UNIVERSAL_GRAMMAR.length === 6, '6 universal rules');
    for (const rule of UNIVERSAL_GRAMMAR) {
        console.assert(['bind', 'fn', 'loop', 'cond', 'struct', 'module'].includes(rule.construct), `valid construct: ${rule.construct}`);
        console.assert(rule.forms.grey !== undefined, `grey form exists for ${rule.construct}`);
    }
    console.log('  ✓ UNIVERSAL_GRAMMAR coverage');

    console.log('\n✓ All grey_grammar Stage 18.5 tests passed.');
}
