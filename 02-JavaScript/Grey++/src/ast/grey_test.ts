// ─── Grey++ Multi-Language Unification Engine — Test Suite ───────────────────
// Comprehensive tests for the unification pipeline:
//   grey_grammar → grey_ast (Normalizer) → grey_runtime → grey_translate → grey_repl
//
// Uses Node.js built-in test runner (node:test + node:assert).
//
// Run:   npx tsx src/ast/grey_test.ts
// ─────────────────────────────────────────────────────────────────────────────

import { describe, it } from 'node:test';
import * as assert from 'node:assert/strict';

import { detectLanguage, detectConstruct, getGrammarRule, UNIVERSAL_GRAMMAR } from './grey_grammar.js';
import { Normalizer } from './grey_ast.js';
import type { NormalizedNode, NormFnNode, NormLoopNode, NormCondNode, NormBindNode, NormStructNode, NormModuleNode } from './grey_ast.js';
import { Translation } from './grey_translate.js';
import type { TranslationOutput } from './grey_translate.js';

// ═══════════════════════════════════════════════════════════════════════════
//  1. grey_grammar.ts — Language Detection & Construct Classification
// ═══════════════════════════════════════════════════════════════════════════

describe('grey_grammar — detectLanguage', () => {
    it('detects Python def', () => {
        assert.equal(detectLanguage('def hello(): print("hi")'), 'python');
    });

    it('detects Python for-in with colon', () => {
        assert.equal(detectLanguage('for i in range(10): print(i)'), 'python');
    });

    it('detects Python if with colon', () => {
        assert.equal(detectLanguage('if x > 5: print("big")'), 'python');
    });

    it('detects Python while with colon', () => {
        assert.equal(detectLanguage('while x > 0: x -= 1'), 'python');
    });

    it('detects JavaScript function', () => {
        assert.equal(detectLanguage('function hello(){ console.log("hi"); }'), 'javascript');
    });

    it('detects JavaScript let/const/var', () => {
        assert.equal(detectLanguage('let x = 42'), 'javascript');
        assert.equal(detectLanguage('const y = 10'), 'javascript');
        assert.equal(detectLanguage('var z = 3'), 'javascript');
    });

    it('detects JavaScript classic for-loop', () => {
        assert.equal(detectLanguage('for(let i=0;i<10;i++){console.log(i);}'), 'javascript');
    });

    it('detects JavaScript if/while with parens and braces', () => {
        assert.equal(detectLanguage('if (x > 5) { console.log(x); }'), 'javascript');
        assert.equal(detectLanguage('while (true) { break; }'), 'javascript');
    });

    it('detects C++ void/int/auto functions', () => {
        assert.equal(detectLanguage('void hello(){ std::cout << "hi"; }'), 'cpp');
        assert.equal(detectLanguage('int main() { return 0; }'), 'cpp');
        assert.equal(detectLanguage('auto compute() { return 42; }'), 'cpp');
    });

    it('detects Rust fn with return type', () => {
        assert.equal(detectLanguage('fn hello() -> i32 {}'), 'rust');
    });

    it('detects Rust let mut', () => {
        assert.equal(detectLanguage('let mut x = 5'), 'rust');
    });

    it('detects Grey++ keywords', () => {
        assert.equal(detectLanguage('bind x = 5'), 'grey');
        assert.equal(detectLanguage('fn hello() { print("hi") }'), 'grey');
        assert.equal(detectLanguage('loop i in range(10) { print(i) }'), 'grey');
        assert.equal(detectLanguage('cond x > 5 { print("big") }'), 'grey');
    });
});

describe('grey_grammar — detectConstruct', () => {
    it('classifies JS let as bind', () => {
        assert.equal(detectConstruct('let x = 5', 'javascript'), 'bind');
    });

    it('classifies Python def as fn', () => {
        assert.equal(detectConstruct('def hello():', 'python'), 'fn');
    });

    it('classifies Python for as loop', () => {
        assert.equal(detectConstruct('for i in range(10):', 'python'), 'loop');
    });

    it('classifies Python if as cond', () => {
        assert.equal(detectConstruct('if x > 5:', 'python'), 'cond');
    });

    it('classifies Python assignment as bind', () => {
        assert.equal(detectConstruct('x = 42', 'python'), 'bind');
    });

    it('classifies Rust fn as fn', () => {
        assert.equal(detectConstruct('fn add(a, b) {}', 'rust'), 'fn');
    });

    it('classifies C++ void as fn', () => {
        assert.equal(detectConstruct('void hello() {}', 'cpp'), 'fn');
    });
});

describe('grey_grammar — getGrammarRule & UNIVERSAL_GRAMMAR', () => {
    it('returns rule for bind', () => {
        const rule = getGrammarRule('bind');
        assert.ok(rule);
        assert.equal(rule.construct, 'bind');
        assert.deepEqual(rule.forms.python, ['=']);
    });

    it('returns rule for fn', () => {
        const rule = getGrammarRule('fn');
        assert.ok(rule);
        assert.ok(rule.forms.python.includes('def'));
    });

    it('returns undefined for unknown construct', () => {
        assert.equal(getGrammarRule('unknown'), undefined);
    });

    it('has exactly 6 universal grammar rules', () => {
        assert.equal(UNIVERSAL_GRAMMAR.length, 6);
    });

    it('all rules have grey forms', () => {
        for (const rule of UNIVERSAL_GRAMMAR) {
            assert.ok(rule.forms.grey, `Missing grey form for ${rule.construct}`);
        }
    });
});

// ═══════════════════════════════════════════════════════════════════════════
//  2. grey_ast.ts — Normalizer
// ═══════════════════════════════════════════════════════════════════════════

describe('Normalizer — Python normalization', () => {
    const normalizer = new Normalizer();

    it('normalizes def → NormFn', () => {
        const node = normalizer.normalize('def hello(): print("hi")', 'python');
        assert.equal(node.nkind, 'NormFn');
        const fn = node as NormFnNode;
        assert.equal(fn.name, 'hello');
        assert.deepEqual(fn.params, []);
        assert.equal(fn.body, 'print("hi")');
        assert.equal(fn.sourceLang, 'python');
    });

    it('normalizes def with params', () => {
        const node = normalizer.normalize('def add(a, b): return a + b', 'python');
        assert.equal(node.nkind, 'NormFn');
        const fn = node as NormFnNode;
        assert.equal(fn.name, 'add');
        assert.deepEqual(fn.params, ['a', 'b']);
    });

    it('normalizes for-in-range → NormLoop with range(0,N)', () => {
        const node = normalizer.normalize('for i in range(10): print(i)', 'python');
        assert.equal(node.nkind, 'NormLoop');
        const loop = node as NormLoopNode;
        assert.equal(loop.variant, 'for');
        assert.equal(loop.binding, 'i');
        assert.equal(loop.iterable, 'range(0,10)');
        assert.equal(loop.body, 'print(i)');
    });

    it('normalizes if → NormCond', () => {
        const node = normalizer.normalize('if x > 5: print("big")', 'python');
        assert.equal(node.nkind, 'NormCond');
        const cond = node as NormCondNode;
        assert.equal(cond.condition, 'x > 5');
        assert.equal(cond.thenBranch, 'print("big")');
    });

    it('normalizes if x: print(x) → NormCond', () => {
        const node = normalizer.normalize('if x: print(x)', 'python');
        assert.equal(node.nkind, 'NormCond');
        const cond = node as NormCondNode;
        assert.equal(cond.condition, 'x');
        assert.equal(cond.thenBranch, 'print(x)');
    });

    it('normalizes assignment → NormBind', () => {
        const node = normalizer.normalize('x = 42', 'python');
        assert.equal(node.nkind, 'NormBind');
        const bind = node as NormBindNode;
        assert.equal(bind.name, 'x');
        assert.equal(bind.value, '42');
        assert.equal(bind.mutable, true);
    });

    it('normalizes while → NormLoop (while)', () => {
        const node = normalizer.normalize('while x > 0: x -= 1', 'python');
        assert.equal(node.nkind, 'NormLoop');
        const loop = node as NormLoopNode;
        assert.equal(loop.variant, 'while');
        assert.equal(loop.condition, 'x > 0');
    });
});

describe('Normalizer — JavaScript normalization', () => {
    const normalizer = new Normalizer();

    it('normalizes function → NormFn with print unification', () => {
        const node = normalizer.normalize('function hello(){ console.log("hi"); }', 'javascript');
        assert.equal(node.nkind, 'NormFn');
        const fn = node as NormFnNode;
        assert.equal(fn.name, 'hello');
        assert.equal(fn.body, 'print("hi")');
    });

    it('normalizes let/const/var → NormBind', () => {
        const node = normalizer.normalize('let x = 42', 'javascript');
        assert.equal(node.nkind, 'NormBind');
        const bind = node as NormBindNode;
        assert.equal(bind.name, 'x');
        assert.equal(bind.mutable, true);
        assert.equal(bind.value, '42');

        const constNode = normalizer.normalize('const y = 10', 'javascript');
        assert.equal((constNode as NormBindNode).mutable, false);
    });

    it('normalizes classic for-loop → NormLoop with range', () => {
        const node = normalizer.normalize('for(let i=0;i<10;i++){console.log(i);}', 'javascript');
        assert.equal(node.nkind, 'NormLoop');
        const loop = node as NormLoopNode;
        assert.equal(loop.variant, 'for');
        assert.equal(loop.binding, 'i');
        assert.equal(loop.iterable, 'range(0,10)');
        assert.equal(loop.body, 'print(i)');
    });

    it('normalizes if → NormCond', () => {
        const node = normalizer.normalize('if (x > 5) { console.log("big"); }', 'javascript');
        assert.equal(node.nkind, 'NormCond');
        const cond = node as NormCondNode;
        assert.equal(cond.condition, 'x > 5');
        assert.equal(cond.thenBranch, 'print("big")');
    });
});

describe('Normalizer — C++ normalization', () => {
    const normalizer = new Normalizer();

    it('normalizes void fn → NormFn with print unification', () => {
        const node = normalizer.normalize('void hello(){ std::cout << "hi"; }', 'cpp');
        assert.equal(node.nkind, 'NormFn');
        const fn = node as NormFnNode;
        assert.equal(fn.name, 'hello');
        assert.equal(fn.body, 'print("hi")');
        assert.equal(fn.returnType, 'void');
    });

    it('normalizes auto bind → NormBind', () => {
        const node = normalizer.normalize('auto x = 42;', 'cpp');
        assert.equal(node.nkind, 'NormBind');
        const bind = node as NormBindNode;
        assert.equal(bind.name, 'x');
        assert.equal(bind.typeAnnotation, 'auto');
    });

    it('normalizes int bind → NormBind', () => {
        const node = normalizer.normalize('int count = 0;', 'cpp');
        assert.equal(node.nkind, 'NormBind');
        const bind = node as NormBindNode;
        assert.equal(bind.name, 'count');
        assert.equal(bind.typeAnnotation, 'int');
    });
});

describe('Normalizer — Grey++ normalization', () => {
    const normalizer = new Normalizer();

    it('normalizes bind → NormBind', () => {
        const node = normalizer.normalize('bind x = 42', 'grey');
        assert.equal(node.nkind, 'NormBind');
        assert.equal((node as NormBindNode).name, 'x');
        assert.equal((node as NormBindNode).value, '42');
    });

    it('normalizes fn → NormFn', () => {
        const node = normalizer.normalize('fn add(a, b) { return a + b }', 'grey');
        assert.equal(node.nkind, 'NormFn');
        assert.equal((node as NormFnNode).name, 'add');
        assert.deepEqual((node as NormFnNode).params, ['a', 'b']);
    });

    it('normalizes loop → NormLoop', () => {
        const node = normalizer.normalize('loop i in items { print(i) }', 'grey');
        assert.equal(node.nkind, 'NormLoop');
        assert.equal((node as NormLoopNode).binding, 'i');
    });

    it('normalizes cond → NormCond', () => {
        const node = normalizer.normalize('cond x > 5 { print("big") }', 'grey');
        assert.equal(node.nkind, 'NormCond');
    });
});

// ═══════════════════════════════════════════════════════════════════════════
//  3. Cross-language equivalence — same construct → same AST type
// ═══════════════════════════════════════════════════════════════════════════

describe('Cross-language equivalence', () => {
    const normalizer = new Normalizer();

    it('Python and JS loops both normalize to NormLoop', () => {
        const pyLoop = normalizer.normalize('for i in range(10): print(i)', 'python');
        const jsLoop = normalizer.normalize('for(let i=0;i<10;i++){console.log(i);}', 'javascript');

        assert.equal(pyLoop.nkind, 'NormLoop');
        assert.equal(jsLoop.nkind, 'NormLoop');
        assert.equal((pyLoop as NormLoopNode).variant, 'for');
        assert.equal((jsLoop as NormLoopNode).variant, 'for');
    });

    it('Python and JS loops normalize to same iterable range', () => {
        const pyLoop = normalizer.normalize('for i in range(10): print(i)', 'python') as NormLoopNode;
        const jsLoop = normalizer.normalize('for(let i=0;i<10;i++){console.log(i);}', 'javascript') as NormLoopNode;

        assert.equal(pyLoop.iterable, 'range(0,10)');
        assert.equal(jsLoop.iterable, 'range(0,10)');
        assert.equal(pyLoop.binding, 'i');
        assert.equal(jsLoop.binding, 'i');
    });

    it('Python and JS loops normalize to same body (print)', () => {
        const pyLoop = normalizer.normalize('for i in range(10): print(i)', 'python') as NormLoopNode;
        const jsLoop = normalizer.normalize('for(let i=0;i<10;i++){console.log(i);}', 'javascript') as NormLoopNode;

        assert.equal(pyLoop.body, 'print(i)');
        assert.equal(jsLoop.body, 'print(i)');
    });

    it('Python and JS loops produce identical Grey++ output', () => {
        const pyLoop = normalizer.normalize('for i in range(10): print(i)', 'python');
        const jsLoop = normalizer.normalize('for(let i=0;i<10;i++){console.log(i);}', 'javascript');

        const pyUnified = normalizer.toUnified(pyLoop);
        const jsUnified = normalizer.toUnified(jsLoop);

        assert.equal(pyUnified, jsUnified);
        assert.equal(pyUnified, 'loop i in range(0,10) { print(i) }');
    });

    it('Python, JS, C++ functions normalize to NormFn with same body', () => {
        const pyFn = normalizer.normalize('def hello(): print("hi")', 'python') as NormFnNode;
        const jsFn = normalizer.normalize('function hello(){ console.log("hi"); }', 'javascript') as NormFnNode;
        const cppFn = normalizer.normalize('void hello(){ std::cout << "hi"; }', 'cpp') as NormFnNode;

        assert.equal(pyFn.nkind, 'NormFn');
        assert.equal(jsFn.nkind, 'NormFn');
        assert.equal(cppFn.nkind, 'NormFn');

        assert.equal(pyFn.name, 'hello');
        assert.equal(jsFn.name, 'hello');
        assert.equal(cppFn.name, 'hello');

        // All bodies unify to print("hi")
        assert.equal(pyFn.body, 'print("hi")');
        assert.equal(jsFn.body, 'print("hi")');
        assert.equal(cppFn.body, 'print("hi")');
    });

    it('Python, JS, C++ functions produce identical Grey++ output', () => {
        const pyFn = normalizer.normalize('def hello(): print("hi")', 'python');
        const jsFn = normalizer.normalize('function hello(){ console.log("hi"); }', 'javascript');
        const cppFn = normalizer.normalize('void hello(){ std::cout << "hi"; }', 'cpp');

        const pyUnified = normalizer.toUnified(pyFn);
        const jsUnified = normalizer.toUnified(jsFn);
        const cppUnified = normalizer.toUnified(cppFn);

        assert.equal(pyUnified, 'fn hello() { print("hi") }');
        assert.equal(jsUnified, pyUnified);
        assert.equal(cppUnified, pyUnified);
    });
});

// ═══════════════════════════════════════════════════════════════════════════
//  4. grey_translate.ts — Translation back to target languages
// ═══════════════════════════════════════════════════════════════════════════

describe('Translation — bind', () => {
    const translator = new Translation();
    const bindNode: NormBindNode = {
        nkind: 'NormBind', sourceLang: 'python', sourceText: 'x = 42',
        name: 'x', mutable: true, value: '42',
    };

    it('translates bind → JS let', () => {
        const out = translator.translate(bindNode, 'javascript');
        assert.equal(out.code, 'let x = 42;');
    });

    it('translates bind → Python assignment', () => {
        const out = translator.translate(bindNode, 'python');
        assert.equal(out.code, 'x = 42');
    });

    it('translates bind → C++ auto', () => {
        const out = translator.translate(bindNode, 'cpp');
        assert.equal(out.code, 'auto x = 42;');
    });

    it('translates bind → Rust let mut', () => {
        const out = translator.translate(bindNode, 'rust');
        assert.equal(out.code, 'let mut x = 42;');
    });

    it('translates bind → Grey++ bind', () => {
        const out = translator.translate(bindNode, 'grey');
        assert.equal(out.code, 'bind x = 42');
    });

    it('translates const bind → JS const', () => {
        const constNode: NormBindNode = {
            nkind: 'NormBind', sourceLang: 'javascript', sourceText: 'const y = 10',
            name: 'y', mutable: false, value: '10',
        };
        const out = translator.translate(constNode, 'javascript');
        assert.equal(out.code, 'const y = 10;');
    });
});

describe('Translation — fn with print mapping', () => {
    const translator = new Translation();
    const fnNode: NormFnNode = {
        nkind: 'NormFn', sourceLang: 'python', sourceText: 'def hello(): print("hi")',
        name: 'hello', params: [], body: 'print("hi")',
    };

    it('translates fn → JS function with console.log', () => {
        const out = translator.translate(fnNode, 'javascript');
        assert.ok(out.code.includes('function hello()'));
        assert.ok(out.code.includes('console.log("hi")'));
    });

    it('translates fn → Python def with print', () => {
        const out = translator.translate(fnNode, 'python');
        assert.ok(out.code.includes('def hello()'));
        assert.ok(out.code.includes('print("hi")'));
    });

    it('translates fn → C++ void with std::cout', () => {
        const out = translator.translate(fnNode, 'cpp');
        assert.ok(out.code.includes('void hello()'));
        assert.ok(out.code.includes('std::cout << "hi"'));
    });

    it('translates fn → Rust fn with println!', () => {
        const out = translator.translate(fnNode, 'rust');
        assert.ok(out.code.includes('fn hello()'));
        assert.ok(out.code.includes('println!("hi");'));
    });

    it('translates fn → Grey++ fn with print', () => {
        const out = translator.translate(fnNode, 'grey');
        assert.ok(out.code.includes('fn hello()'));
        assert.ok(out.code.includes('print("hi")'));
    });
});

describe('Translation — loop with print mapping', () => {
    const translator = new Translation();
    const loopNode: NormLoopNode = {
        nkind: 'NormLoop', sourceLang: 'python', sourceText: 'for i in range(10): print(i)',
        variant: 'for', binding: 'i', iterable: 'range(0,10)', body: 'print(i)',
    };

    it('translates loop → JS C-style for with console.log', () => {
        const out = translator.translate(loopNode, 'javascript');
        assert.ok(out.code.includes('for(let i=0;i<10;i++)'));
        assert.ok(out.code.includes('console.log(i)'));
    });

    it('translates loop → Python for-in with print', () => {
        const out = translator.translate(loopNode, 'python');
        assert.ok(out.code.includes('for i in range(0,10)'));
        assert.ok(out.code.includes('print(i)'));
    });

    it('translates loop → C++ for with std::cout', () => {
        const out = translator.translate(loopNode, 'cpp');
        assert.ok(out.code.includes('for(int i=0;i<10;i++)'));
        assert.ok(out.code.includes('std::cout << i'));
    });

    it('translates loop → Rust for-in with println!', () => {
        const out = translator.translate(loopNode, 'rust');
        assert.ok(out.code.includes('for i in range(0,10)'));
        assert.ok(out.code.includes('println!(i);'));
    });

    it('translates loop → Grey++ loop', () => {
        const out = translator.translate(loopNode, 'grey');
        assert.equal(out.code, 'loop i in range(0,10) { print(i) }');
    });
});

describe('Translation — cond with print mapping', () => {
    const translator = new Translation();
    const condNode: NormCondNode = {
        nkind: 'NormCond', sourceLang: 'python', sourceText: 'if x > 5: print("big")',
        condition: 'x > 5', thenBranch: 'print("big")',
    };

    it('translates cond → JS if with console.log', () => {
        const out = translator.translate(condNode, 'javascript');
        assert.ok(out.code.includes('if (x > 5)'));
        assert.ok(out.code.includes('console.log("big")'));
    });

    it('translates cond → Python if with print', () => {
        const out = translator.translate(condNode, 'python');
        assert.ok(out.code.includes('if x > 5'));
        assert.ok(out.code.includes('print("big")'));
    });

    it('translates cond → C++ if with std::cout', () => {
        const out = translator.translate(condNode, 'cpp');
        assert.ok(out.code.includes('if (x > 5)'));
        assert.ok(out.code.includes('std::cout << "big"'));
    });

    it('translates cond → Rust if with println!', () => {
        const out = translator.translate(condNode, 'rust');
        assert.ok(out.code.includes('if x > 5'));
        assert.ok(out.code.includes('println!("big");'));
    });

    it('translates cond → Grey++ cond', () => {
        const out = translator.translate(condNode, 'grey');
        assert.ok(out.code.includes('cond x > 5'));
        assert.ok(out.code.includes('print("big")'));
    });

    it('translates cond with else branch', () => {
        const condElse: NormCondNode = {
            nkind: 'NormCond', sourceLang: 'python', sourceText: 'if x: a else: b',
            condition: 'x', thenBranch: 'print("yes")', elseBranch: 'print("no")',
        };
        const jsOut = translator.translate(condElse, 'javascript');
        assert.ok(jsOut.code.includes('console.log("yes")'));
        assert.ok(jsOut.code.includes('console.log("no")'));
        assert.ok(jsOut.code.includes('else'));
    });
});

describe('Translation — translateAll', () => {
    const translator = new Translation();

    it('produces 5 outputs (js, python, cpp, rust, grey)', () => {
        const bindNode: NormBindNode = {
            nkind: 'NormBind', sourceLang: 'python', sourceText: 'x = 42',
            name: 'x', mutable: true, value: '42',
        };
        const outputs = translator.translateAll(bindNode);
        assert.equal(outputs.length, 5);

        const targets = outputs.map(o => o.target);
        assert.ok(targets.includes('javascript'));
        assert.ok(targets.includes('python'));
        assert.ok(targets.includes('cpp'));
        assert.ok(targets.includes('rust'));
        assert.ok(targets.includes('grey'));
    });

    it('all outputs have TranslationOutput _type', () => {
        const node: NormFnNode = {
            nkind: 'NormFn', sourceLang: 'grey', sourceText: 'fn f() { ... }',
            name: 'f', params: [], body: '...',
        };
        const outputs = translator.translateAll(node);
        for (const out of outputs) {
            assert.equal(out._type, 'TranslationOutput');
            assert.equal(out.meta.stub, true);
        }
    });
});

// ═══════════════════════════════════════════════════════════════════════════
//  5. End-to-end pipeline: detect → normalize → unify → translate
// ═══════════════════════════════════════════════════════════════════════════

describe('End-to-end pipeline', () => {
    const normalizer = new Normalizer();
    const translator = new Translation();

    function pipeline(source: string) {
        const lang = detectLanguage(source);
        const node = normalizer.normalize(source, lang);
        const unified = normalizer.toUnified(node);
        const translations = translator.translateAll(node);
        return { lang, node, unified, translations };
    }

    it('Python def → full round-trip', () => {
        const result = pipeline('def hello(): print("hi")');
        assert.equal(result.lang, 'python');
        assert.equal(result.node.nkind, 'NormFn');
        assert.equal(result.unified, 'fn hello() { print("hi") }');

        const js = result.translations.find(t => t.target === 'javascript')!;
        assert.ok(js.code.includes('function hello()'));
        assert.ok(js.code.includes('console.log("hi")'));

        const py = result.translations.find(t => t.target === 'python')!;
        assert.ok(py.code.includes('def hello()'));
        assert.ok(py.code.includes('print("hi")'));

        const cpp = result.translations.find(t => t.target === 'cpp')!;
        assert.ok(cpp.code.includes('void hello()'));
        assert.ok(cpp.code.includes('std::cout << "hi"'));
    });

    it('JS function → full round-trip', () => {
        const result = pipeline('function hello(){ console.log("hi"); }');
        assert.equal(result.lang, 'javascript');
        assert.equal(result.node.nkind, 'NormFn');
        assert.equal(result.unified, 'fn hello() { print("hi") }');
    });

    it('C++ void fn → full round-trip', () => {
        const result = pipeline('void hello(){ std::cout << "hi"; }');
        assert.equal(result.lang, 'cpp');
        assert.equal(result.node.nkind, 'NormFn');
        assert.equal(result.unified, 'fn hello() { print("hi") }');
    });

    it('Python for-loop → full round-trip', () => {
        const result = pipeline('for i in range(10): print(i)');
        assert.equal(result.lang, 'python');
        assert.equal(result.node.nkind, 'NormLoop');
        assert.equal(result.unified, 'loop i in range(0,10) { print(i) }');
    });

    it('JS classic for-loop → full round-trip', () => {
        const result = pipeline('for(let i=0;i<10;i++){console.log(i);}');
        assert.equal(result.lang, 'javascript');
        assert.equal(result.node.nkind, 'NormLoop');
        assert.equal(result.unified, 'loop i in range(0,10) { print(i) }');
    });

    it('JS let → full round-trip', () => {
        const result = pipeline('let x = 42');
        assert.equal(result.lang, 'javascript');
        assert.equal(result.node.nkind, 'NormBind');
        assert.equal(result.unified, 'bind x = 42');

        const js = result.translations.find(t => t.target === 'javascript')!;
        assert.equal(js.code, 'let x = 42;');
        const py = result.translations.find(t => t.target === 'python')!;
        assert.equal(py.code, 'x = 42');
        const cpp = result.translations.find(t => t.target === 'cpp')!;
        assert.equal(cpp.code, 'auto x = 42;');
    });

    it('Python if → full round-trip', () => {
        const result = pipeline('if x > 5: print("big")');
        assert.equal(result.lang, 'python');
        assert.equal(result.node.nkind, 'NormCond');
        assert.equal(result.unified, 'cond x > 5 { print("big") }');
    });

    it('Grey++ bind → full round-trip', () => {
        const result = pipeline('bind result = compute()');
        assert.equal(result.lang, 'grey');
        assert.equal(result.node.nkind, 'NormBind');
        assert.equal(result.unified, 'bind result = compute()');
    });

    it('Grey++ fn → full round-trip', () => {
        const result = pipeline('fn add(a, b) { return a + b }');
        assert.equal(result.lang, 'grey');
        assert.equal(result.node.nkind, 'NormFn');
    });
});

// ═══════════════════════════════════════════════════════════════════════════
//  6. Unified output format (toUnified)
// ═══════════════════════════════════════════════════════════════════════════

describe('Normalizer — toUnified output strings', () => {
    const normalizer = new Normalizer();

    it('NormBind → "bind name = value"', () => {
        const node = normalizer.normalize('let x = 42', 'javascript');
        assert.equal(normalizer.toUnified(node), 'bind x = 42');
    });

    it('NormFn → "fn name(params) { body }"', () => {
        const node = normalizer.normalize('def greet(name): print(name)', 'python');
        assert.equal(normalizer.toUnified(node), 'fn greet(name) { print(name) }');
    });

    it('NormLoop (for) → "loop binding in iterable { body }"', () => {
        const node = normalizer.normalize('for i in range(10): print(i)', 'python');
        assert.equal(normalizer.toUnified(node), 'loop i in range(0,10) { print(i) }');
    });

    it('NormLoop (while) → "loop while condition { body }"', () => {
        const node = normalizer.normalize('while x > 0: x -= 1', 'python');
        assert.equal(normalizer.toUnified(node), 'loop while x > 0 { x -= 1 }');
    });

    it('NormCond → "cond test { then }"', () => {
        const node = normalizer.normalize('if x: print(x)', 'python');
        assert.equal(normalizer.toUnified(node), 'cond x { print(x) }');
    });
});

// ═══════════════════════════════════════════════════════════════════════════
//  7. Print mapping consistency
// ═══════════════════════════════════════════════════════════════════════════

describe('Print mapping round-trip', () => {
    const normalizer = new Normalizer();
    const translator = new Translation();

    it('console.log normalizes to print, translates back to console.log', () => {
        const node = normalizer.normalize('function test(){ console.log("msg"); }', 'javascript');
        const fn = node as NormFnNode;
        assert.equal(fn.body, 'print("msg")');

        const js = translator.translate(node, 'javascript');
        assert.ok(js.code.includes('console.log("msg")'));
    });

    it('std::cout normalizes to print, translates back to std::cout', () => {
        const node = normalizer.normalize('void test(){ std::cout << "msg"; }', 'cpp');
        const fn = node as NormFnNode;
        assert.equal(fn.body, 'print("msg")');

        const cpp = translator.translate(node, 'cpp');
        assert.ok(cpp.code.includes('std::cout << "msg"'));
    });

    it('print stays as print for Python', () => {
        const node = normalizer.normalize('def test(): print("msg")', 'python');
        const fn = node as NormFnNode;
        assert.equal(fn.body, 'print("msg")');

        const py = translator.translate(node, 'python');
        assert.ok(py.code.includes('print("msg")'));
    });

    it('print translates to println! for Rust', () => {
        const node = normalizer.normalize('def test(): print("msg")', 'python');
        const rs = translator.translate(node, 'rust');
        assert.ok(rs.code.includes('println!("msg");'));
    });
});

// ═══════════════════════════════════════════════════════════════════════════
//  8. Struct normalization & translation
// ═══════════════════════════════════════════════════════════════════════════

describe('Normalizer — struct normalization', () => {
    const normalizer = new Normalizer();

    it('normalizes Python class → NormStruct', () => {
        const node = normalizer.normalize('class Point: x, y', 'python');
        assert.equal(node.nkind, 'NormStruct');
        const s = node as NormStructNode;
        assert.equal(s.name, 'Point');
        assert.equal(s.fields.length, 2);
        assert.equal(s.fields[0].name, 'x');
    });

    it('normalizes JS class → NormStruct', () => {
        const node = normalizer.normalize('class Point { x; y; }', 'javascript');
        assert.equal(node.nkind, 'NormStruct');
        const s = node as NormStructNode;
        assert.equal(s.name, 'Point');
    });

    it('normalizes C++ struct → NormStruct', () => {
        const node = normalizer.normalize('struct Point { int x; int y; };', 'cpp');
        assert.equal(node.nkind, 'NormStruct');
        const s = node as NormStructNode;
        assert.equal(s.name, 'Point');
        assert.equal(s.fields.length, 2);
        assert.equal(s.fields[0].name, 'x');
        assert.equal(s.fields[0].type, 'int');
    });

    it('normalizes Rust struct → NormStruct', () => {
        const node = normalizer.normalize('struct Point { x: f64, y: f64 }', 'rust');
        assert.equal(node.nkind, 'NormStruct');
        const s = node as NormStructNode;
        assert.equal(s.name, 'Point');
        assert.equal(s.fields.length, 2);
        assert.equal(s.fields[0].type, 'f64');
    });

    it('normalizes Grey++ struct → NormStruct', () => {
        const node = normalizer.normalize('struct Point { x, y }', 'grey');
        assert.equal(node.nkind, 'NormStruct');
        const s = node as NormStructNode;
        assert.equal(s.name, 'Point');
    });

    it('all struct sources produce same Grey++ unified output', () => {
        const cppNode = normalizer.normalize('struct Vec2 { float x; float y; };', 'cpp');
        const rustNode = normalizer.normalize('struct Vec2 { x: float, y: float }', 'rust');
        assert.equal(normalizer.toUnified(cppNode), normalizer.toUnified(rustNode));
    });
});

describe('Translation — struct', () => {
    const translator = new Translation();
    const structNode: NormStructNode = {
        nkind: 'NormStruct', sourceLang: 'grey', sourceText: 'struct Point { x, y }',
        name: 'Point', fields: [{ name: 'x' }, { name: 'y' }],
    };

    it('translates struct → JS class', () => {
        const out = translator.translate(structNode, 'javascript');
        assert.ok(out.code.includes('class Point'));
        assert.ok(out.code.includes('constructor(x, y)'));
    });

    it('translates struct → Python class', () => {
        const out = translator.translate(structNode, 'python');
        assert.ok(out.code.includes('class Point'));
        assert.ok(out.code.includes('self.x = x'));
    });

    it('translates struct → C++ struct', () => {
        const out = translator.translate(structNode, 'cpp');
        assert.ok(out.code.includes('struct Point'));
        assert.ok(out.code.includes('auto x;'));
    });

    it('translates struct → Rust struct', () => {
        const out = translator.translate(structNode, 'rust');
        assert.ok(out.code.includes('struct Point'));
        assert.ok(out.code.includes('x: i32'));
    });

    it('translates struct → Grey++ struct', () => {
        const out = translator.translate(structNode, 'grey');
        assert.equal(out.code, 'struct Point { x, y }');
    });
});

// ═══════════════════════════════════════════════════════════════════════════
//  9. Module normalization & translation
// ═══════════════════════════════════════════════════════════════════════════

describe('Normalizer — module normalization', () => {
    const normalizer = new Normalizer();

    it('normalizes Python import → NormModule', () => {
        const node = normalizer.normalize('import os', 'python');
        assert.equal(node.nkind, 'NormModule');
        const m = node as NormModuleNode;
        assert.equal(m.name, 'os');
        assert.deepEqual(m.imports, ['os']);
    });

    it('normalizes Python from-import → NormModule', () => {
        const node = normalizer.normalize('from math import sqrt, pi', 'python');
        assert.equal(node.nkind, 'NormModule');
        const m = node as NormModuleNode;
        assert.equal(m.name, 'math');
        assert.deepEqual(m.imports, ['sqrt', 'pi']);
    });

    it('normalizes JS import → NormModule', () => {
        const node = normalizer.normalize("import { useState } from 'react'", 'javascript');
        assert.equal(node.nkind, 'NormModule');
        const m = node as NormModuleNode;
        assert.equal(m.name, 'react');
        assert.deepEqual(m.imports, ['useState']);
    });

    it('normalizes C++ #include → NormModule', () => {
        const node = normalizer.normalize('#include <iostream>', 'cpp');
        assert.equal(node.nkind, 'NormModule');
        const m = node as NormModuleNode;
        assert.equal(m.name, 'iostream');
    });

    it('normalizes C++ namespace → NormModule', () => {
        const node = normalizer.normalize('namespace utils {', 'cpp');
        assert.equal(node.nkind, 'NormModule');
        const m = node as NormModuleNode;
        assert.equal(m.name, 'utils');
    });

    it('normalizes Rust mod → NormModule', () => {
        const node = normalizer.normalize('mod network;', 'rust');
        assert.equal(node.nkind, 'NormModule');
        const m = node as NormModuleNode;
        assert.equal(m.name, 'network');
    });

    it('normalizes Rust use → NormModule', () => {
        const node = normalizer.normalize('use std::collections::HashMap;', 'rust');
        assert.equal(node.nkind, 'NormModule');
        const m = node as NormModuleNode;
        assert.equal(m.name, 'HashMap');
        assert.deepEqual(m.imports, ['std::collections::HashMap']);
    });

    it('normalizes Grey++ module → NormModule', () => {
        const node = normalizer.normalize('module utils { helpers, math }', 'grey');
        assert.equal(node.nkind, 'NormModule');
        const m = node as NormModuleNode;
        assert.equal(m.name, 'utils');
        assert.deepEqual(m.imports, ['helpers', 'math']);
    });
});

describe('Translation — module', () => {
    const translator = new Translation();
    const modNode: NormModuleNode = {
        nkind: 'NormModule', sourceLang: 'grey', sourceText: 'module utils { helpers }',
        name: 'utils', imports: ['helpers'],
    };

    it('translates module → JS import', () => {
        const out = translator.translate(modNode, 'javascript');
        assert.ok(out.code.includes("import"));
        assert.ok(out.code.includes("'utils'"));
    });

    it('translates module → Python from-import', () => {
        const out = translator.translate(modNode, 'python');
        assert.ok(out.code.includes('from utils import helpers'));
    });

    it('translates module → C++ #include', () => {
        const out = translator.translate(modNode, 'cpp');
        assert.ok(out.code.includes('#include <utils>'));
    });

    it('translates module → Rust use', () => {
        const out = translator.translate(modNode, 'rust');
        assert.ok(out.code.includes('use helpers;'));
    });

    it('translates module → Grey++ module', () => {
        const out = translator.translate(modNode, 'grey');
        assert.equal(out.code, 'module utils { helpers }');
    });
});

// ═══════════════════════════════════════════════════════════════════════════
// 10. Unified output format for struct/module (toUnified)
// ═══════════════════════════════════════════════════════════════════════════

describe('Normalizer — toUnified for struct/module', () => {
    const normalizer = new Normalizer();

    it('NormStruct → "struct Name { fields }"', () => {
        const node = normalizer.normalize('struct Point { x: f64, y: f64 }', 'rust');
        assert.equal(normalizer.toUnified(node), 'struct Point { x: f64, y: f64 }');
    });

    it('NormModule → "module Name { imports }"', () => {
        const node = normalizer.normalize('module utils { helpers, math }', 'grey');
        assert.equal(normalizer.toUnified(node), 'module utils { helpers, math }');
    });
});

// ═══════════════════════════════════════════════════════════════════════════
// 11. Interop tests — cross-language chains
// ═══════════════════════════════════════════════════════════════════════════

describe('Interop — cross-language chains', () => {
    const normalizer = new Normalizer();
    const translator = new Translation();

    it('JS loop → normalize → translate to Python fn body', () => {
        // Normalize a JS for-loop
        const jsNode = normalizer.normalize('for(let i=0;i<5;i++){console.log(i);}', 'javascript');
        assert.equal(jsNode.nkind, 'NormLoop');
        // Translate to Python
        const pyOut = translator.translate(jsNode, 'python');
        assert.ok(pyOut.code.includes('for i in range(0,5)'));
        assert.ok(pyOut.code.includes('print(i)'));
    });

    it('Python fn → normalize → translate to C++', () => {
        const pyNode = normalizer.normalize('def greet(name): print(name)', 'python');
        assert.equal(pyNode.nkind, 'NormFn');
        const cppOut = translator.translate(pyNode, 'cpp');
        assert.ok(cppOut.code.includes('void greet(name)'));
        assert.ok(cppOut.code.includes('std::cout << name'));
    });

    it('C++ fn → normalize → translate to Rust with semicolons', () => {
        const cppNode = normalizer.normalize('void hello(){ std::cout << "hi"; }', 'cpp');
        const rsOut = translator.translate(cppNode, 'rust');
        assert.ok(rsOut.code.includes('fn hello()'));
        assert.ok(rsOut.code.includes('println!("hi");'));
    });

    it('Rust struct → normalize → translate to JS class', () => {
        const rustNode = normalizer.normalize('struct Color { r: u8, g: u8, b: u8 }', 'rust');
        assert.equal(rustNode.nkind, 'NormStruct');
        const jsOut = translator.translate(rustNode, 'javascript');
        assert.ok(jsOut.code.includes('class Color'));
        assert.ok(jsOut.code.includes('constructor(r, g, b)'));
    });

    it('Python import → normalize → translate to Rust use', () => {
        const pyNode = normalizer.normalize('from collections import OrderedDict', 'python');
        assert.equal(pyNode.nkind, 'NormModule');
        const rsOut = translator.translate(pyNode, 'rust');
        assert.ok(rsOut.code.includes('use OrderedDict;'));
    });

    it('JS class → normalize → translate to C++ struct', () => {
        const jsNode = normalizer.normalize('class Rect { width; height; }', 'javascript');
        assert.equal(jsNode.nkind, 'NormStruct');
        const cppOut = translator.translate(jsNode, 'cpp');
        assert.ok(cppOut.code.includes('struct Rect'));
    });

    it('full round-trip: Grey++ struct → all targets → back to Grey++', () => {
        const greyNode = normalizer.normalize('struct Vec3 { x, y, z }', 'grey');
        const outputs = translator.translateAll(greyNode);
        assert.equal(outputs.length, 5);

        const greyOut = outputs.find(o => o.target === 'grey')!;
        assert.equal(greyOut.code, 'struct Vec3 { x, y, z }');
    });

    it('JS loop and Python loop produce same Rust output', () => {
        const jsNode = normalizer.normalize('for(let i=0;i<10;i++){console.log(i);}', 'javascript');
        const pyNode = normalizer.normalize('for i in range(10): print(i)', 'python');
        const jsRust = translator.translate(jsNode, 'rust');
        const pyRust = translator.translate(pyNode, 'rust');
        assert.equal(jsRust.code, pyRust.code);
    });
});

// ═══════════════════════════════════════════════════════════════════════════
// 12. Language detection for new constructs
// ═══════════════════════════════════════════════════════════════════════════

describe('detectLanguage — struct/module patterns', () => {
    it('detects Rust struct', () => {
        assert.equal(detectLanguage('struct Point { x: f64, y: f64 }'), 'rust');
    });

    it('detects Rust mod', () => {
        assert.equal(detectLanguage('mod network;'), 'rust');
    });

    it('detects Rust use', () => {
        assert.equal(detectLanguage('use std::io;'), 'rust');
    });

    it('detects JS class', () => {
        assert.equal(detectLanguage('class Foo { }'), 'javascript');
    });

    it('detects JS import', () => {
        assert.equal(detectLanguage("import { x } from 'y'"), 'javascript');
    });

    it('detects C++ #include', () => {
        assert.equal(detectLanguage('#include <vector>'), 'cpp');
    });

    it('detects C++ namespace', () => {
        assert.equal(detectLanguage('namespace utils {'), 'cpp');
    });

    it('detects C++ struct', () => {
        assert.equal(detectLanguage('struct Vec { int x; };'), 'cpp');
    });

    it('detects Python class', () => {
        assert.equal(detectLanguage('class Point: pass'), 'python');
    });

    it('detects Python import', () => {
        assert.equal(detectLanguage('import os'), 'python');
    });

    it('detects Grey++ struct', () => {
        assert.equal(detectLanguage('struct MyType { a, b }'), 'grey');
    });

    it('detects Grey++ module', () => {
        assert.equal(detectLanguage('module utils { helpers }'), 'grey');
    });
});

describe('detectConstruct — struct/module', () => {
    it('classifies Python class as struct', () => {
        assert.equal(detectConstruct('class Point:', 'python'), 'struct');
    });

    it('classifies JS class as struct', () => {
        assert.equal(detectConstruct('class Foo {}', 'javascript'), 'struct');
    });

    it('classifies C++ struct as struct', () => {
        assert.equal(detectConstruct('struct Vec {};', 'cpp'), 'struct');
    });

    it('classifies Rust struct as struct', () => {
        assert.equal(detectConstruct('struct Point {}', 'rust'), 'struct');
    });

    it('classifies Python import as module', () => {
        assert.equal(detectConstruct('import os', 'python'), 'module');
    });

    it('classifies JS import as module', () => {
        assert.equal(detectConstruct("import x from 'y'", 'javascript'), 'module');
    });

    it('classifies C++ #include as module', () => {
        assert.equal(detectConstruct('#include <iostream>', 'cpp'), 'module');
    });

    it('classifies Rust mod as module', () => {
        assert.equal(detectConstruct('mod network;', 'rust'), 'module');
    });

    it('classifies Rust use as module', () => {
        assert.equal(detectConstruct('use std::io;', 'rust'), 'module');
    });

    it('classifies Grey++ struct as struct', () => {
        assert.equal(detectConstruct('struct Point {}', 'grey'), 'struct');
    });

    it('classifies Grey++ module as module', () => {
        assert.equal(detectConstruct('module utils {}', 'grey'), 'module');
    });
});

// ─── Run ────────────────────────────────────────────────────────────────────
// Self-test entry point for: npx tsx src/ast/grey_test.ts
