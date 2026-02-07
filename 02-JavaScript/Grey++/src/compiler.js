// ─── Grey++ Compiler ─────────────────────────────────────────────────────────
// Transpiles Grey++ source (.greypp) into standalone JavaScript that runs
// with Node.js — no Grey++ runtime dependency required.
//
// Usage:
//   import { compileFile, compileSource } from './compiler.js';
//   compileFile('app.greypp');                   // writes app.js
//   compileSource(src, { outFile: 'out.js' });   // writes out.js

import * as fs from 'node:fs';
import * as path from 'node:path';
import { parse } from './parser/parser.js';
import { GREYPP_EXTENSIONS, isGreyppFile } from './runner.js';
import { NodeKind } from './ast/types.js';

// ─── Public API ─────────────────────────────────────────────────────────────

/**
 * Compile a .greypp source file to a standalone .js file.
 * @param {string} filePath   Path to .greypp source
 * @param {{ outFile?: string, verbose?: boolean, dumpAst?: boolean }} [opts]
 * @returns {string} path of the emitted .js file
 */
export function compileFile(filePath, opts = {}) {
    const resolved = path.resolve(filePath);

    if (!isGreyppFile(resolved)) {
        const ext = path.extname(resolved);
        throw new Error(
            `Unsupported file extension "${ext}". Grey++ files must use: ${GREYPP_EXTENSIONS.join(', ')}`
        );
    }
    if (!fs.existsSync(resolved)) {
        throw new Error(`File not found: ${resolved}`);
    }

    const source = fs.readFileSync(resolved, 'utf-8');

    if (opts.verbose) {
        console.log(`── Compiling: ${resolved}`);
        console.log(`── Size: ${source.length} bytes, ${source.split('\n').length} lines`);
    }

    const ast = parse(source);

    if (opts.dumpAst) {
        console.log('── AST ──');
        console.dir(ast, { depth: null, colors: true });
    }

    const outFile = opts.outFile || resolved.replace(/\.\w+$/, '.js');
    const js = emit(ast, { sourceFile: resolved });

    fs.writeFileSync(outFile, js, 'utf-8');

    if (opts.verbose) {
        console.log(`── Emitted: ${outFile} (${js.length} bytes)`);
    }
    return outFile;
}

/**
 * Compile Grey++ source code string to JavaScript string.
 * Optionally writes to outFile.
 * @param {string} source
 * @param {{ outFile?: string, sourceFile?: string }} [opts]
 * @returns {string} generated JavaScript code
 */
export function compileSource(source, opts = {}) {
    const ast = parse(source);
    const js = emit(ast, { sourceFile: opts.sourceFile || '<stdin>' });
    if (opts.outFile) {
        fs.writeFileSync(opts.outFile, js, 'utf-8');
    }
    return js;
}

// ─── Code Emitter ───────────────────────────────────────────────────────────

function emit(ast, opts = {}) {
    const lines = [];
    const indent = (n) => '    '.repeat(n);

    // ── Preamble ────────────────────────────────────────────────────────
    lines.push('#!/usr/bin/env node');
    lines.push('// Grey++ compiled output — standalone, no runtime dependency');
    lines.push(`// Source: ${opts.sourceFile || '<unknown>'}`);
    lines.push(`// Compiled: ${new Date().toISOString()}`);
    lines.push('');
    lines.push(RUNTIME_PREAMBLE);
    lines.push('');

    // ── Programme body ──────────────────────────────────────────────────
    lines.push('// ── Programme ───────────────────────────────────────────');
    lines.push('(function __greypp_main() {');

    if (ast.kind !== NodeKind.Program) {
        throw new Error('Top-level node must be Program');
    }

    const body = ast.body;
    for (let i = 0; i < body.length; i++) {
        const stmt = body[i];
        const isLast = i === body.length - 1;
        if (isLast && stmt.kind !== NodeKind.FnDecl && stmt.kind !== NodeKind.ReturnStmt) {
            // Print last top-level expression result (mirrors interpreter behaviour)
            const pad = '    ';
            lines.push(`${pad}var __result = ${emitExpr(stmt, 1)};`);
            lines.push(`${pad}if (__result !== undefined) console.log(__result);`);
        } else {
            lines.push(emitNode(stmt, 1));
        }
    }

    lines.push('})();');

    return lines.join('\n') + '\n';
}

// ─── Node Emitters ──────────────────────────────────────────────────────────

function emitNode(node, depth) {
    const pad = '    '.repeat(depth);
    switch (node.kind) {
        case NodeKind.FnDecl:
            return emitFnDecl(node, depth);
        case NodeKind.CallExpr:
            return pad + emitCallExpr(node, depth) + ';';
        case NodeKind.ReturnStmt:
            return pad + 'return ' + emitExpr(node.value, depth) + ';';
        case NodeKind.BinaryExpr:
            return pad + emitBinaryExpr(node, depth) + ';';
        case NodeKind.Identifier:
            return pad + emitIdentifier(node) + ';';
        case NodeKind.NumberLit:
            return pad + String(node.value) + ';';
        case NodeKind.StringLit:
            return pad + JSON.stringify(node.value) + ';';
        case NodeKind.BoolLit:
            return pad + String(node.value) + ';';
        case NodeKind.ArrayLit:
            return pad + emitArrayLit(node, depth) + ';';
        case NodeKind.ObjectLit:
            return pad + emitObjectLit(node, depth) + ';';
        case NodeKind.Query:
            return pad + emitQuery(node, depth) + ';';
        default:
            throw new Error(`Compiler: unsupported node kind "${node.kind}"`);
    }
}

function emitExpr(node, depth) {
    switch (node.kind) {
        case NodeKind.NumberLit:
            return String(node.value);
        case NodeKind.StringLit:
            return JSON.stringify(node.value);
        case NodeKind.BoolLit:
            return String(node.value);
        case NodeKind.Identifier:
            return emitIdentifier(node);
        case NodeKind.BinaryExpr:
            return emitBinaryExpr(node, depth);
        case NodeKind.CallExpr:
            return emitCallExpr(node, depth);
        case NodeKind.ArrayLit:
            return emitArrayLit(node, depth);
        case NodeKind.ObjectLit:
            return emitObjectLit(node, depth);
        case NodeKind.FnDecl:
            return emitFnDeclExpr(node, depth);
        case NodeKind.Query:
            return emitQuery(node, depth);
        default:
            throw new Error(`Compiler: unsupported expression kind "${node.kind}"`);
    }
}

// ── Function Declaration ────────────────────────────────────────────────────

function emitFnDecl(node, depth) {
    const pad = '    '.repeat(depth);
    const params = node.params.map(sanitizeIdent).join(', ');

    if (!node.name) {
        // Anonymous function as expression statement
        return pad + emitFnDeclExpr(node, depth) + ';';
    }

    const lines = [];
    lines.push(`${pad}function ${sanitizeIdent(node.name)}(${params}) {`);
    const bodyLines = emitBody(node.body, depth + 1);
    lines.push(bodyLines);
    lines.push(`${pad}}`);
    return lines.join('\n');
}

function emitFnDeclExpr(node, depth) {
    const params = node.params.map(sanitizeIdent).join(', ');
    const pad = '    '.repeat(depth);

    if (node.body.length === 1) {
        const inner = emitExpr(node.body[0], depth + 1);
        return `(function(${params}) { return ${inner}; })`;
    }

    const lines = [];
    lines.push(`(function(${params}) {`);
    lines.push(emitBody(node.body, depth + 1));
    lines.push(`${pad}})`);
    return lines.join('\n');
}

function emitBody(body, depth) {
    const pad = '    '.repeat(depth);
    const lines = [];
    for (let i = 0; i < body.length; i++) {
        const stmt = body[i];
        const isLast = i === body.length - 1;
        // Named fn declarations are statements, not returned implicitly.
        // Anonymous fn declarations ARE expressions — they need implicit return.
        const isNamedFnDecl = stmt.kind === NodeKind.FnDecl && stmt.name;
        if (isLast && !isNamedFnDecl && stmt.kind !== NodeKind.ReturnStmt) {
            // Implicit return of last expression
            lines.push(pad + 'return ' + emitExpr(stmt, depth) + ';');
        } else {
            lines.push(emitNode(stmt, depth));
        }
    }
    return lines.join('\n');
}

// ── Call Expression ─────────────────────────────────────────────────────────

function emitCallExpr(node, depth) {
    const args = node.args.map(a => emitExpr(a, depth)).join(', ');
    if (typeof node.callee === 'string') {
        return `${sanitizeIdent(node.callee)}(${args})`;
    }
    // Expression callee (chained calls)
    return `(${emitExpr(node.callee, depth)})(${args})`;
}

// ── Binary Expression ───────────────────────────────────────────────────────

function emitBinaryExpr(node, depth) {
    const left = emitExpr(node.left, depth);
    const right = emitExpr(node.right, depth);
    const op = node.op;

    // Map == / != to strict equality
    if (op === '==') return `(${left} === ${right})`;
    if (op === '!=') return `(${left} !== ${right})`;

    // Division with zero-check
    if (op === '/') return `__greypp_div(${left}, ${right})`;

    return `(${left} ${op} ${right})`;
}

// ── Literals ────────────────────────────────────────────────────────────────

function emitArrayLit(node, depth) {
    const els = node.elements.map(e => emitExpr(e, depth)).join(', ');
    return `[${els}]`;
}

function emitObjectLit(node, depth) {
    if (node.entries.length === 0) return '({})';
    const props = node.entries.map(e => {
        // Object property keys are always safe in JS (even reserved words are valid as property names)
        const key = /^[a-zA-Z_$][a-zA-Z0-9_$]*$/.test(e.key) ? e.key : JSON.stringify(e.key);
        return `${key}: ${emitExpr(e.value, depth)}`;
    }).join(', ');
    return `({ ${props} })`;
}

function emitIdentifier(node) {
    return sanitizeIdent(node.name);
}

// ── Query ───────────────────────────────────────────────────────────────────

function emitQuery(node, depth) {
    const { select, from, where } = node;

    const parts = [];
    parts.push('(function() {');

    const pad = '    '.repeat(depth + 1);
    const tableName = from ? JSON.stringify(from.table) : null;

    if (from) {
        parts.push(`${pad}var __rows = __greypp_tables.get(${tableName}) || [];`);
        parts.push(`${pad}__rows = __rows.map(function(r) { return Object.assign({}, r); });`);
    } else {
        parts.push(`${pad}var __rows = [{}];`);
    }

    if (where) {
        const cond = emitWhereCondition(where.condition, depth + 1);
        parts.push(`${pad}__rows = __rows.filter(function(__row) { return ${cond}; });`);
    }

    const cols = select.columns;
    if (cols.length === 1 && cols[0] === '*') {
        parts.push(`${pad}return __rows;`);
    } else {
        const proj = cols.map(c => `${JSON.stringify(c)}: __row[${JSON.stringify(c)}]`).join(', ');
        parts.push(`${pad}return __rows.map(function(__row) { return { ${proj} }; });`);
    }

    parts.push('    '.repeat(depth) + '})()');
    return parts.join('\n');
}

function emitWhereCondition(node, depth) {
    if (node.kind === NodeKind.BinaryExpr) {
        const left = emitWhereValue(node.left);
        const right = emitWhereValue(node.right);
        let op = node.op;
        if (op === '==') op = '===';
        if (op === '!=') op = '!==';
        return `(${left} ${op} ${right})`;
    }
    if (node.kind === NodeKind.BoolLit) return String(node.value);
    throw new Error(`Compiler: unsupported WHERE node kind "${node.kind}"`);
}

function emitWhereValue(node) {
    if (node.kind === NodeKind.Identifier) return `__row[${JSON.stringify(node.name)}]`;
    if (node.kind === NodeKind.NumberLit) return String(node.value);
    if (node.kind === NodeKind.StringLit) return JSON.stringify(node.value);
    if (node.kind === NodeKind.BoolLit) return String(node.value);
    throw new Error(`Compiler: unsupported WHERE value kind "${node.kind}"`);
}

// ── Identifier sanitizer ───────────────────────────────────────────────────

const JS_RESERVED = new Set([
    'break', 'case', 'catch', 'continue', 'debugger', 'default', 'delete',
    'do', 'else', 'finally', 'for', 'function', 'if', 'in', 'instanceof',
    'new', 'return', 'switch', 'this', 'throw', 'try', 'typeof', 'var',
    'void', 'while', 'with', 'class', 'const', 'enum', 'export', 'extends',
    'import', 'super', 'implements', 'interface', 'let', 'package', 'private',
    'protected', 'public', 'static', 'yield', 'await', 'async',
    // Grey++ keywords that map to JS reserved words
    'return',
]);

function sanitizeIdent(name) {
    if (JS_RESERVED.has(name)) return `_gpp_${name}`;
    return name;
}

// ─── Embedded Runtime Preamble ──────────────────────────────────────────────
// Minimal runtime inlined into every compiled file so it is fully standalone.

const RUNTIME_PREAMBLE = `
// ── Grey++ Runtime (embedded) ───────────────────────────────────────────
"use strict";

// ── Division helper ──
function __greypp_div(a, b) { if (b === 0) throw new Error('Division by zero'); return a / b; }

// ── Table store ──
var __greypp_tables = new Map();

// ── Constants ──
var nil = null;
var PI = Math.PI;
var E = Math.E;

// ── I/O ──
function print() { console.log.apply(console, arguments); }
function log() { var a = Array.prototype.slice.call(arguments); a.unshift('[LOG]'); console.log.apply(console, a); }
function debug() { var a = Array.prototype.slice.call(arguments); a.unshift('[DEBUG]'); console.log.apply(console, a); }
function warn() { var a = Array.prototype.slice.call(arguments); a.unshift('[WARN]'); console.log.apply(console, a); }
function error() { var a = Array.prototype.slice.call(arguments); a.unshift('[ERROR]'); console.error.apply(console, a); }

// ── Control flow ──
function if_then(cond, then_val, else_val) { return cond ? (typeof then_val === 'function' ? then_val() : then_val) : (typeof else_val === 'function' ? else_val() : else_val); }
function when(cond, fn) { return cond ? fn() : undefined; }
function unless(cond, fn) { return !cond ? fn() : undefined; }
function cond() { var pairs = Array.prototype.slice.call(arguments); for (var i = 0; i < pairs.length - 1; i += 2) { if (pairs[i]) return typeof pairs[i + 1] === 'function' ? pairs[i + 1]() : pairs[i + 1]; } return pairs.length % 2 !== 0 ? (typeof pairs[pairs.length - 1] === 'function' ? pairs[pairs.length - 1]() : pairs[pairs.length - 1]) : undefined; }
function match(val) { var pairs = Array.prototype.slice.call(arguments, 1); for (var i = 0; i < pairs.length - 1; i += 2) { if (val === pairs[i]) return typeof pairs[i + 1] === 'function' ? pairs[i + 1]() : pairs[i + 1]; } return pairs.length % 2 !== 0 ? (typeof pairs[pairs.length - 1] === 'function' ? pairs[pairs.length - 1]() : pairs[pairs.length - 1]) : undefined; }

// ── Logic ──
function not(v) { return !v; }
function and(a, b) { return a && b; }
function or(a, b) { return a || b; }
function all() { return Array.prototype.slice.call(arguments).every(Boolean); }
function any() { return Array.prototype.slice.call(arguments).some(Boolean); }

// ── Math ──
var abs = Math.abs;
var sqrt = Math.sqrt;
var pow = Math.pow;
var floor = Math.floor;
var ceil = Math.ceil;
var round = Math.round;
var min = Math.min;
var max = Math.max;
function mod(a, b) { return a % b; }
function random() { return Math.random(); }
function random_int(lo, hi) { return Math.floor(Math.random() * (hi - lo + 1)) + lo; }
var sin = Math.sin;
var cos = Math.cos;
var log_n = Math.log;
var exp = Math.exp;
function clamp(v, lo, hi) { return Math.min(Math.max(v, lo), hi); }
function lerp(a, b, t) { return a + (b - a) * t; }
function sum() { var arr = arguments.length === 1 && Array.isArray(arguments[0]) ? arguments[0] : Array.prototype.slice.call(arguments); return arr.reduce(function(a, b) { return a + b; }, 0); }
function avg() { var arr = arguments.length === 1 && Array.isArray(arguments[0]) ? arguments[0] : Array.prototype.slice.call(arguments); return arr.reduce(function(a, b) { return a + b; }, 0) / arr.length; }

// ── Collections ──
function len(v) { return v == null ? 0 : (v.length !== undefined ? v.length : Object.keys(v).length); }
function map(arr, fn) { return arr.map(fn); }
function filter(arr, fn) { return arr.filter(fn); }
function reduce(arr, fn, init) { return arr.reduce(fn, init); }
function forEach(arr, fn) { arr.forEach(fn); return arr; }
function find(arr, fn) { return arr.find(fn); }
function some(arr, fn) { return arr.some(fn); }
function every(arr, fn) { return arr.every(fn); }
function sort(arr, fn) { return arr.slice().sort(fn); }
function reverse(arr) { return arr.slice().reverse(); }
function slice(arr, start, end) { return arr.slice(start, end); }
function push(arr) { var vals = Array.prototype.slice.call(arguments, 1); return arr.concat(vals); }
function concat(a, b) { return Array.isArray(a) ? a.concat(b) : a + b; }
function flat(arr) { return arr.flat(); }
function flat_map(arr, fn) { return arr.flatMap(fn); }
function zip(a, b) { return a.map(function(v, i) { return [v, b[i]]; }); }
function uniq(arr) { return Array.from(new Set(arr)); }
function group_by(arr, fn) { var m = {}; arr.forEach(function(v) { var k = fn(v); (m[k] = m[k] || []).push(v); }); return m; }
function count(arr, fn) { return fn ? arr.filter(fn).length : arr.length; }
function range(start, end, step) { var r = []; step = step || 1; for (var i = start; i < end; i += step) r.push(i); return r; }
function repeat(n, fn) { var r = []; for (var i = 0; i < n; i++) r.push(fn(i)); return r; }
function take(arr, n) { return arr.slice(0, n); }
function drop(arr, n) { return arr.slice(n); }
function head(arr) { return arr[0]; }
function tail(arr) { return arr.slice(1); }
function last(arr) { return arr[arr.length - 1]; }
function index_of(arr, val) { return arr.indexOf(val); }
function includes(arr, val) { return Array.isArray(arr) ? arr.includes(val) : String(arr).includes(val); }
function flatten(arr) { return arr.flat(Infinity); }

// ── Object operations ──
function get(obj, key) { return obj == null ? undefined : obj[key]; }
function set(obj, key, val) { var r = Object.assign({}, obj); r[key] = val; return r; }
function keys(obj) { return Object.keys(obj); }
function values(obj) { return Object.values(obj); }
function entries(obj) { return Object.entries(obj); }
function merge() { return Object.assign.apply(null, [{}].concat(Array.prototype.slice.call(arguments))); }
function pick(obj) { var ks = Array.prototype.slice.call(arguments, 1); var o = {}; ks.forEach(function(k) { if (k in obj) o[k] = obj[k]; }); return o; }
function omit(obj) { var ks = Array.prototype.slice.call(arguments, 1); var o = Object.assign({}, obj); ks.forEach(function(k) { delete o[k]; }); return o; }
function has_key(obj, key) { return obj != null && key in obj; }
function from_entries(arr) { return Object.fromEntries(arr); }
function deep_clone(v) { return JSON.parse(JSON.stringify(v)); }

// ── String operations ──
function str() { return Array.prototype.slice.call(arguments).map(String).join(''); }
function upper(s) { return String(s).toUpperCase(); }
function lower(s) { return String(s).toLowerCase(); }
function trim(s) { return String(s).trim(); }
function split(s, sep) { return String(s).split(sep); }
function join(arr, sep) { return arr.join(sep != null ? sep : ','); }
function replace(s, from, to) { return String(s).replace(from, to); }
function starts_with(s, prefix) { return String(s).startsWith(prefix); }
function ends_with(s, suffix) { return String(s).endsWith(suffix); }
function char_at(s, i) { return String(s).charAt(i); }
function substr(s, start, len) { return String(s).substring(start, start + (len != null ? len : String(s).length)); }
function pad_start(s, n, ch) { return String(s).padStart(n, ch); }
function pad_end(s, n, ch) { return String(s).padEnd(n, ch); }
function repeat_str(s, n) { return String(s).repeat(n); }
function template(tmpl) { var vals = Array.prototype.slice.call(arguments, 1); var r = tmpl; vals.forEach(function(v, i) { r = r.replace(new RegExp('\\\\{' + i + '\\\\}', 'g'), String(v)); }); return r; }

// ── Type operations ──
function type_of(v) { return v === null ? 'null' : Array.isArray(v) ? 'array' : typeof v; }
function is_array(v) { return Array.isArray(v); }
function is_number(v) { return typeof v === 'number' && !isNaN(v); }
function is_string(v) { return typeof v === 'string'; }
function is_bool(v) { return typeof v === 'boolean'; }
function is_fn(v) { return typeof v === 'function'; }
function is_nil(v) { return v == null; }
function to_number(v) { return Number(v); }
function to_string(v) { return String(v); }
function to_bool(v) { return Boolean(v); }
function parse_int(s) { return parseInt(s, 10); }
function parse_float(s) { return parseFloat(s); }

// ── JSON ──
function json_stringify(v) { return JSON.stringify(v, null, 2); }
function json_parse(s) { return JSON.parse(s); }

// ── Time ──
function timestamp() { return Date.now(); }
function now() { return new Date().toISOString(); }
function elapsed(start) { return Date.now() - start; }

// ── Hashing / Crypto ──
function hash(s) { var h = 0; var str = String(s); for (var i = 0; i < str.length; i++) { h = ((h << 5) - h + str.charCodeAt(i)) | 0; } return Math.abs(h).toString(16).padStart(8, '0'); }
function uuid() { return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) { var r = Math.random() * 16 | 0; return (c === 'x' ? r : (r & 0x3 | 0x8)).toString(16); }); }

// ── Functional ──
function compose(f, g) { return function(x) { return f(g(x)); }; }
function pipe() { var fns = Array.prototype.slice.call(arguments); return function(x) { return fns.reduce(function(v, f) { return f(v); }, x); }; }
function identity(x) { return x; }
function constant(x) { return function() { return x; }; }
function apply(fn, args) { return fn.apply(null, args); }
function partial(fn) { var bound = Array.prototype.slice.call(arguments, 1); return function() { return fn.apply(null, bound.concat(Array.prototype.slice.call(arguments))); }; }
function memoize(fn) { var cache = new Map(); return function() { var k = JSON.stringify(Array.prototype.slice.call(arguments)); if (cache.has(k)) return cache.get(k); var v = fn.apply(null, arguments); cache.set(k, v); return v; }; }
function tap(v, fn) { fn(v); return v; }
function chain(init) { var fns = Array.prototype.slice.call(arguments, 1); return fns.reduce(function(v, f) { return f(v); }, init); }

// ── Assertions / Testing ──
function assert(cond, msg) { if (!cond) throw new Error('Assertion failed: ' + (msg || '')); return true; }
function assert_eq(a, b, msg) { var as = JSON.stringify(a); var bs = JSON.stringify(b); if (as !== bs) throw new Error('assert_eq failed: ' + as + ' !== ' + bs + (msg ? ' — ' + msg : '')); return true; }
function bench(label, fn) { var s = Date.now(); var r = fn(); var e = Date.now() - s; console.log('[BENCH] ' + label + ': ' + e + 'ms'); return r; }

// ── Data Tables ──
function createTable(name, rows) { __greypp_tables.set(name, rows || []); }
function insertRow(tableName, row) { if (!__greypp_tables.has(tableName)) __greypp_tables.set(tableName, []); __greypp_tables.get(tableName).push(row); }

// ── Pretty print ──
function inspect(v) { console.dir(v, { depth: null, colors: true }); return v; }
function table(arr) { console.table(arr); return arr; }
function banner(title) { var line = '═'.repeat(title.length + 4); console.log('╔' + line + '╗'); console.log('║  ' + title + '  ║'); console.log('╚' + line + '╝'); }
function section(title) { console.log('\\n── ' + title + ' ' + '─'.repeat(Math.max(0, 60 - title.length)) + '──'); }
function divider() { console.log('─'.repeat(66)); }
`.trim();

