// ─── Grey++ REPL ─────────────────────────────────────────────────────────────
// Interactive read-eval-print loop.  Multiline input is accumulated until
// braces are balanced so that `fn` and `query` blocks can be entered naturally.

import * as readline from 'node:readline';
import { parse } from '../parser/parser.js';
import { Kernel } from '../runtime/kernel.js';
import { loadPlugins } from '../runtime/registry.js';

export function startREPL() {
    const kernel = new Kernel();

    // ── Load all Stage 1 plugins via the registry ─────────────────────────
    loadPlugins(kernel);

    // ── Seed built-in functions into the root scope ───────────────────────
    kernel.defineGlobal('print', (...args) => {
        console.log(...args);
        return args[args.length - 1];
    });

    // Expose per-kernel table helpers so users can populate data from the REPL
    const store = kernel._tableStore;
    kernel.defineGlobal('createTable', store.createTable);
    kernel.defineGlobal('insertRow', store.insertRow);

    // ── REPL interface ────────────────────────────────────────────────────
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: 'grey++ > ',
    });

    console.log('Grey++ v0.1.0  —  Stage 1 REPL');
    console.log('Supports: fn, query, expressions.  Type .exit to quit.\n');
    rl.prompt();

    let buffer = '';

    rl.on('line', (line) => {
        const trimmed = line.trim();

        // ── Meta commands ──
        if (trimmed === '.exit') { rl.close(); return; }
        if (trimmed === '.ast') {
            // Peek mode: parse buffer and dump the AST tree
            if (buffer) {
                try { console.dir(parse(buffer), { depth: null }); } catch (e) { console.error(e.message); }
                buffer = '';
            }
            rl.prompt();
            return;
        }

        buffer += (buffer ? '\n' : '') + line;

        // Keep accumulating until braces are balanced
        if (!bracesBalanced(buffer)) {
            process.stdout.write('  ... > ');
            return;
        }

        // ── Evaluate ────────────────────────────────────────────────────────
        try {
            const ast = parse(buffer);
            const result = kernel.exec(ast);
            if (result !== undefined) {
                console.log(formatResult(result));
            }
        } catch (err) {
            console.error(`Error: ${err.message}`);
        }

        buffer = '';
        rl.prompt();
    });

    rl.on('close', () => {
        console.log('\nGoodbye.');
        process.exit(0);
    });
}

// ── Utilities ──────────────────────────────────────────────────────────────
function bracesBalanced(src) {
    let depth = 0;
    for (const ch of src) {
        if (ch === '{' || ch === '[') depth++;
        if (ch === '}' || ch === ']') depth--;
        if (depth < 0) return true; // malformed, let parser report
    }
    return depth === 0;
}

function formatResult(value) {
    if (Array.isArray(value)) {
        return JSON.stringify(value, null, 2);
    }
    if (typeof value === 'object' && value !== null) {
        return JSON.stringify(value, null, 2);
    }
    return String(value);
}
