// ─── Grey++ File Runner ──────────────────────────────────────────────────────
// Loads, parses, and executes .greypp source files.
// Usage:  node src/runner.js <file.greypp>
//         grey run <file.greypp>

import * as fs from 'node:fs';
import * as path from 'node:path';
import { parse } from './parser/parser.js';
import { Kernel } from './runtime/kernel.js';
import { loadPlugins } from './runtime/registry.js';

/** Supported Grey++ file extensions */
export const GREYPP_EXTENSIONS = ['.greypp', '.gpp', '.grey'];

/**
 * Validate that a file path has a supported Grey++ extension.
 * @param {string} filePath 
 * @returns {boolean}
 */
export function isGreyppFile(filePath) {
    const ext = path.extname(filePath).toLowerCase();
    return GREYPP_EXTENSIONS.includes(ext);
}

/**
 * Load and execute a .greypp source file.
 * @param {string} filePath  Absolute or relative path to a .greypp file
 * @param {{ verbose?: boolean, dumpAst?: boolean }} [options]
 * @returns {*} The result of the last expression
 */
export function runFile(filePath, options = {}) {
    const resolved = path.resolve(filePath);

    // ── Validate extension ──
    if (!isGreyppFile(resolved)) {
        const ext = path.extname(resolved);
        throw new Error(
            `Unsupported file extension "${ext}". Grey++ files must use: ${GREYPP_EXTENSIONS.join(', ')}`
        );
    }

    // ── Read source ──
    if (!fs.existsSync(resolved)) {
        throw new Error(`File not found: ${resolved}`);
    }
    const source = fs.readFileSync(resolved, 'utf-8');

    if (options.verbose) {
        console.log(`── Loading: ${resolved}`);
        console.log(`── Size: ${source.length} bytes, ${source.split('\n').length} lines`);
    }

    // ── Parse ──
    const ast = parse(source);

    if (options.dumpAst) {
        console.log('── AST ──');
        console.dir(ast, { depth: null, colors: true });
    }

    // ── Execute ──
    const kernel = new Kernel();
    loadPlugins(kernel);

    // Seed built-in globals
    seedStdlib(kernel);

    // Set __file__ and __dir__ for the running script
    kernel.defineGlobal('__file__', resolved);
    kernel.defineGlobal('__dir__', path.dirname(resolved));

    const result = kernel.exec(ast);
    return result;
}

/**
 * Run a Grey++ source string directly (no file).
 * @param {string} source Grey++ source code
 * @returns {*}
 */
export function runSource(source) {
    const kernel = new Kernel();
    loadPlugins(kernel);
    seedStdlib(kernel);

    const ast = parse(source);
    return kernel.exec(ast);
}

// ─── Grey++ Standard Library ────────────────────────────────────────────────
// Built-in functions seeded into every kernel instance.

/** Sentinel: return value should NOT be echoed by the CLI runner */
export const PRINTED = Symbol('PRINTED');

function seedStdlib(kernel) {
    // ── Constants ──
    kernel.defineGlobal('nil', null);

    // ── I/O ──
    kernel.defineGlobal('print', (...args) => { console.log(...args); return PRINTED; });
    kernel.defineGlobal('log', (...args) => { console.log('[LOG]', ...args); return PRINTED; });
    kernel.defineGlobal('debug', (...args) => { console.log('[DEBUG]', ...args); return PRINTED; });
    kernel.defineGlobal('warn', (...args) => { console.log('[WARN]', ...args); return PRINTED; });
    kernel.defineGlobal('error', (...args) => { console.error('[ERROR]', ...args); return PRINTED; });

    // ── Control flow (compensates for no if/else in parser) ──
    kernel.defineGlobal('if_then', (cond, then_val, else_val) => cond ? (typeof then_val === 'function' ? then_val() : then_val) : (typeof else_val === 'function' ? else_val() : else_val));
    kernel.defineGlobal('when', (cond, fn) => cond ? fn() : undefined);
    kernel.defineGlobal('unless', (cond, fn) => !cond ? fn() : undefined);
    kernel.defineGlobal('cond', (...pairs) => { for (let i = 0; i < pairs.length - 1; i += 2) { if (pairs[i]) return typeof pairs[i + 1] === 'function' ? pairs[i + 1]() : pairs[i + 1]; } return pairs.length % 2 !== 0 ? (typeof pairs[pairs.length - 1] === 'function' ? pairs[pairs.length - 1]() : pairs[pairs.length - 1]) : undefined; });
    kernel.defineGlobal('match', (val, ...pairs) => { for (let i = 0; i < pairs.length - 1; i += 2) { if (val === pairs[i]) return typeof pairs[i + 1] === 'function' ? pairs[i + 1]() : pairs[i + 1]; } return pairs.length % 2 !== 0 ? (typeof pairs[pairs.length - 1] === 'function' ? pairs[pairs.length - 1]() : pairs[pairs.length - 1]) : undefined; });

    // ── Logic ──
    kernel.defineGlobal('not', (v) => !v);
    kernel.defineGlobal('and', (a, b) => a && b);
    kernel.defineGlobal('or', (a, b) => a || b);
    kernel.defineGlobal('all', (...args) => args.every(Boolean));
    kernel.defineGlobal('any', (...args) => args.some(Boolean));

    // ── Math ──
    kernel.defineGlobal('abs', Math.abs);
    kernel.defineGlobal('sqrt', Math.sqrt);
    kernel.defineGlobal('pow', Math.pow);
    kernel.defineGlobal('floor', Math.floor);
    kernel.defineGlobal('ceil', Math.ceil);
    kernel.defineGlobal('round', Math.round);
    kernel.defineGlobal('min', Math.min);
    kernel.defineGlobal('max', Math.max);
    kernel.defineGlobal('mod', (a, b) => a % b);
    kernel.defineGlobal('random', () => Math.random());
    kernel.defineGlobal('random_int', (lo, hi) => Math.floor(Math.random() * (hi - lo + 1)) + lo);
    kernel.defineGlobal('PI', Math.PI);
    kernel.defineGlobal('E', Math.E);
    kernel.defineGlobal('sin', Math.sin);
    kernel.defineGlobal('cos', Math.cos);
    kernel.defineGlobal('log_n', Math.log);
    kernel.defineGlobal('exp', Math.exp);
    kernel.defineGlobal('clamp', (v, lo, hi) => Math.min(Math.max(v, lo), hi));
    kernel.defineGlobal('lerp', (a, b, t) => a + (b - a) * t);
    kernel.defineGlobal('sum', (...args) => { const arr = args.length === 1 && Array.isArray(args[0]) ? args[0] : args; return arr.reduce((a, b) => a + b, 0); });
    kernel.defineGlobal('avg', (...args) => { const arr = args.length === 1 && Array.isArray(args[0]) ? args[0] : args; return arr.reduce((a, b) => a + b, 0) / arr.length; });

    // ── Collections ──
    kernel.defineGlobal('len', (v) => v == null ? 0 : (v.length !== undefined ? v.length : Object.keys(v).length));
    kernel.defineGlobal('map', (arr, fn) => arr.map(fn));
    kernel.defineGlobal('filter', (arr, fn) => arr.filter(fn));
    kernel.defineGlobal('reduce', (arr, fn, init) => arr.reduce(fn, init));
    kernel.defineGlobal('forEach', (arr, fn) => { arr.forEach(fn); return arr; });
    kernel.defineGlobal('find', (arr, fn) => arr.find(fn));
    kernel.defineGlobal('some', (arr, fn) => arr.some(fn));
    kernel.defineGlobal('every', (arr, fn) => arr.every(fn));
    kernel.defineGlobal('sort', (arr, fn) => [...arr].sort(fn));
    kernel.defineGlobal('reverse', (arr) => [...arr].reverse());
    kernel.defineGlobal('slice', (arr, start, end) => arr.slice(start, end));
    kernel.defineGlobal('push', (arr, ...vals) => [...arr, ...vals]);
    kernel.defineGlobal('concat', (a, b) => Array.isArray(a) ? [...a, ...b] : a + b);
    kernel.defineGlobal('flat', (arr) => arr.flat());
    kernel.defineGlobal('flat_map', (arr, fn) => arr.flatMap(fn));
    kernel.defineGlobal('zip', (a, b) => a.map((v, i) => [v, b[i]]));
    kernel.defineGlobal('uniq', (arr) => [...new Set(arr)]);
    kernel.defineGlobal('group_by', (arr, fn) => { const m = {}; arr.forEach(v => { const k = fn(v); (m[k] = m[k] || []).push(v); }); return m; });
    kernel.defineGlobal('count', (arr, fn) => fn ? arr.filter(fn).length : arr.length);
    kernel.defineGlobal('range', (start, end, step) => { const r = []; step = step || 1; for (let i = start; i < end; i += step) r.push(i); return r; });
    kernel.defineGlobal('repeat', (n, fn) => { const r = []; for (let i = 0; i < n; i++) r.push(fn(i)); return r; });
    kernel.defineGlobal('take', (arr, n) => arr.slice(0, n));
    kernel.defineGlobal('drop', (arr, n) => arr.slice(n));
    kernel.defineGlobal('head', (arr) => arr[0]);
    kernel.defineGlobal('tail', (arr) => arr.slice(1));
    kernel.defineGlobal('last', (arr) => arr[arr.length - 1]);
    kernel.defineGlobal('index_of', (arr, val) => arr.indexOf(val));
    kernel.defineGlobal('includes', (arr, val) => Array.isArray(arr) ? arr.includes(val) : String(arr).includes(val));
    kernel.defineGlobal('flatten', (arr) => arr.flat(Infinity));

    // ── Object operations ──
    kernel.defineGlobal('get', (obj, key) => obj == null ? undefined : obj[key]);
    kernel.defineGlobal('set', (obj, key, val) => ({ ...obj, [key]: val }));
    kernel.defineGlobal('keys', (obj) => Object.keys(obj));
    kernel.defineGlobal('values', (obj) => Object.values(obj));
    kernel.defineGlobal('entries', (obj) => Object.entries(obj));
    kernel.defineGlobal('merge', (...objs) => Object.assign({}, ...objs));
    kernel.defineGlobal('pick', (obj, ...ks) => { const o = {}; ks.forEach(k => { if (k in obj) o[k] = obj[k]; }); return o; });
    kernel.defineGlobal('omit', (obj, ...ks) => { const o = { ...obj }; ks.forEach(k => delete o[k]); return o; });
    kernel.defineGlobal('has_key', (obj, key) => obj != null && key in obj);
    kernel.defineGlobal('from_entries', (arr) => Object.fromEntries(arr));
    kernel.defineGlobal('deep_clone', (v) => JSON.parse(JSON.stringify(v)));

    // ── String operations ──
    kernel.defineGlobal('str', (...args) => args.map(String).join(''));
    kernel.defineGlobal('upper', (s) => String(s).toUpperCase());
    kernel.defineGlobal('lower', (s) => String(s).toLowerCase());
    kernel.defineGlobal('trim', (s) => String(s).trim());
    kernel.defineGlobal('split', (s, sep) => String(s).split(sep));
    kernel.defineGlobal('join', (arr, sep) => arr.join(sep ?? ','));
    kernel.defineGlobal('replace', (s, from, to) => String(s).replace(from, to));
    kernel.defineGlobal('starts_with', (s, prefix) => String(s).startsWith(prefix));
    kernel.defineGlobal('ends_with', (s, suffix) => String(s).endsWith(suffix));
    kernel.defineGlobal('char_at', (s, i) => String(s).charAt(i));
    kernel.defineGlobal('substr', (s, start, len) => String(s).substring(start, start + (len ?? String(s).length)));
    kernel.defineGlobal('pad_start', (s, n, ch) => String(s).padStart(n, ch));
    kernel.defineGlobal('pad_end', (s, n, ch) => String(s).padEnd(n, ch));
    kernel.defineGlobal('repeat_str', (s, n) => String(s).repeat(n));
    kernel.defineGlobal('template', (tmpl, ...vals) => { let r = tmpl; vals.forEach((v, i) => { r = r.replace(new RegExp('\\{' + i + '\\}', 'g'), String(v)); }); return r; });

    // ── Type operations ──
    kernel.defineGlobal('type_of', (v) => v === null ? 'null' : Array.isArray(v) ? 'array' : typeof v);
    kernel.defineGlobal('is_array', Array.isArray);
    kernel.defineGlobal('is_number', (v) => typeof v === 'number' && !isNaN(v));
    kernel.defineGlobal('is_string', (v) => typeof v === 'string');
    kernel.defineGlobal('is_bool', (v) => typeof v === 'boolean');
    kernel.defineGlobal('is_fn', (v) => typeof v === 'function');
    kernel.defineGlobal('is_nil', (v) => v == null);
    kernel.defineGlobal('to_number', Number);
    kernel.defineGlobal('to_string', String);
    kernel.defineGlobal('to_bool', Boolean);
    kernel.defineGlobal('parse_int', (s) => parseInt(s, 10));
    kernel.defineGlobal('parse_float', parseFloat);

    // ── JSON ──
    kernel.defineGlobal('json_stringify', (v) => JSON.stringify(v, null, 2));
    kernel.defineGlobal('json_parse', JSON.parse);

    // ── Time ──
    kernel.defineGlobal('timestamp', () => Date.now());
    kernel.defineGlobal('now', () => new Date().toISOString());
    kernel.defineGlobal('elapsed', (start) => Date.now() - start);

    // ── Hashing / Crypto simulation ──
    kernel.defineGlobal('hash', (s) => { let h = 0; const str = String(s); for (let i = 0; i < str.length; i++) { h = ((h << 5) - h + str.charCodeAt(i)) | 0; } return Math.abs(h).toString(16).padStart(8, '0'); });
    kernel.defineGlobal('uuid', () => 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, c => { const r = Math.random() * 16 | 0; return (c === 'x' ? r : (r & 0x3 | 0x8)).toString(16); }));

    // ── Functional ──
    kernel.defineGlobal('compose', (f, g) => (x) => f(g(x)));
    kernel.defineGlobal('pipe', (...fns) => (x) => fns.reduce((v, f) => f(v), x));
    kernel.defineGlobal('identity', (x) => x);
    kernel.defineGlobal('constant', (x) => () => x);
    kernel.defineGlobal('apply', (fn, args) => fn(...args));
    kernel.defineGlobal('partial', (fn, ...bound) => (...rest) => fn(...bound, ...rest));
    kernel.defineGlobal('memoize', (fn) => { const cache = new Map(); return (...args) => { const k = JSON.stringify(args); if (cache.has(k)) return cache.get(k); const v = fn(...args); cache.set(k, v); return v; }; });
    kernel.defineGlobal('tap', (v, fn) => { fn(v); return v; });
    kernel.defineGlobal('chain', (init, ...fns) => fns.reduce((v, f) => f(v), init));

    // ── Assertions / Testing ──
    kernel.defineGlobal('assert', (cond, msg) => { if (!cond) throw new Error('Assertion failed: ' + (msg || '')); return true; });
    kernel.defineGlobal('assert_eq', (a, b, msg) => { const as = JSON.stringify(a); const bs = JSON.stringify(b); if (as !== bs) throw new Error('assert_eq failed: ' + as + ' !== ' + bs + (msg ? ' — ' + msg : '')); return true; });
    kernel.defineGlobal('bench', (label, fn) => { const s = Date.now(); const r = fn(); const e = Date.now() - s; console.log('[BENCH] ' + label + ': ' + e + 'ms'); return r; });

    // ── Data Tables (per-kernel isolated store) ──
    const store = kernel._tableStore;
    kernel.defineGlobal('createTable', store.createTable);
    kernel.defineGlobal('insertRow', store.insertRow);

    // ── Pretty print ──
    kernel.defineGlobal('inspect', (v) => { console.dir(v, { depth: null, colors: true }); return v; });
    kernel.defineGlobal('table', (arr) => { console.table(arr); return arr; });
    kernel.defineGlobal('banner', (title) => { const line = repeat_str('═', title.length + 4); console.log('╔' + line + '╗'); console.log('║  ' + title + '  ║'); console.log('╚' + line + '╝'); return PRINTED; });
    kernel.defineGlobal('section', (title) => { console.log('\n── ' + title + ' ' + '─'.repeat(Math.max(0, 60 - title.length)) + '──'); return PRINTED; });
    kernel.defineGlobal('divider', () => { console.log('─'.repeat(66)); return PRINTED; });
}

function repeat_str(s, n) { return String(s).repeat(n); }

// ─── CLI Entry Point (only when runner.js is invoked directly) ───────────────

const __runnerFile = new URL(import.meta.url).pathname.replace(/^\/([A-Z]:)/, '$1');
const __invokedFile = path.resolve(process.argv[1] ?? '');

if (__runnerFile === __invokedFile) {
    const args = process.argv.slice(2);
    const flags = args.filter(a => a.startsWith('--'));
    const files = args.filter(a => !a.startsWith('--'));

    if (files.length === 0) {
        console.log('Grey++ File Runner');
        console.log('');
        console.log('Usage:');
        console.log('  node src/runner.js <file.greypp>          Run a Grey++ source file');
        console.log('  node src/runner.js <file.greypp> --ast    Run and dump the AST');
        console.log('  node src/runner.js <file.greypp> --verbose Verbose output');
        console.log('');
        console.log(`Supported extensions: ${GREYPP_EXTENSIONS.join(', ')}`);
        process.exit(0);
    }

    const filePath = files[0];
    try {
        const result = runFile(filePath, {
            verbose: flags.includes('--verbose') || flags.includes('-v'),
            dumpAst: flags.includes('--ast'),
        });
        if (result !== undefined && result !== PRINTED) {
            console.log(result);
        }
    } catch (err) {
        console.error(`Error: ${err.message}`);
        process.exit(1);
    }
}
