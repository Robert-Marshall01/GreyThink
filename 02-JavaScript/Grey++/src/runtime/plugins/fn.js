// ─── Function Executor Plugin ────────────────────────────────────────────────
// Handles FnDecl, CallExpr, ReturnStmt, BinaryExpr, literals, identifiers,
// arrays, and objects.
//
// Isolation model:
//   • Each fn invocation creates a *child* scope — parameters and locals
//     cannot leak into the caller or sibling calls.
//   • The plugin never touches kernel internals; it only uses the
//     ExecutionContext API (ctx.exec, ctx.childScope, ctx.execWith).

import { NodeKind } from '../../ast/types.js';

// ── Sentinel for return-value propagation ───────────────────────────────
const RETURN = Symbol('RETURN');
function makeReturn(value) { return { [RETURN]: true, value }; }
function isReturn(val) { return val != null && typeof val === 'object' && val[RETURN] === true; }

/**
 * @param {import('../kernel.js').Kernel} kernel
 */
export function registerFnExecutor(kernel) {
    kernel.register(NodeKind.Program, execProgram);
    kernel.register(NodeKind.FnDecl, execFnDecl);
    kernel.register(NodeKind.CallExpr, execCallExpr);
    kernel.register(NodeKind.ReturnStmt, execReturn);
    kernel.register(NodeKind.BinaryExpr, execBinaryExpr);
    kernel.register(NodeKind.Identifier, execIdentifier);
    kernel.register(NodeKind.NumberLit, (n) => n.value);
    kernel.register(NodeKind.StringLit, (n) => n.value);
    kernel.register(NodeKind.BoolLit, (n) => n.value);
    kernel.register(NodeKind.ArrayLit, execArrayLit);
    kernel.register(NodeKind.ObjectLit, execObjectLit);
}

// ─── Executors ──────────────────────────────────────────────────────────────

function execProgram(node, ctx) {
    let last;
    for (const stmt of node.body) {
        last = ctx.exec(stmt);
        if (isReturn(last)) return last.value;
    }
    return last;
}

function execFnDecl(node, ctx) {
    // Capture the *defining* scope so closures work correctly.
    const definingScope = ctx.scope;

    const closure = (...args) => {
        // Each call gets an isolated child scope — no cross-call leakage.
        const callScope = definingScope.child();
        node.params.forEach((p, i) => callScope.define(p, args[i]));

        let result;
        for (const stmt of node.body) {
            result = ctx.execWith(stmt, callScope);
            if (isReturn(result)) { result = result.value; break; }
        }
        return result;
    };

    // Anonymous function (name === null) → return closure as a value
    if (!node.name) return closure;

    // Named function → define in scope
    ctx.scope.define(node.name, closure);
    return undefined;
}

function execCallExpr(node, ctx) {
    let fn;
    if (typeof node.callee === 'string') {
        // Simple named call: name(args)
        fn = ctx.scope.get(node.callee);
    } else {
        // Expression callee: expr(args) — supports chained calls
        fn = ctx.exec(node.callee);
    }
    if (typeof fn !== 'function') {
        const name = typeof node.callee === 'string' ? node.callee : '<expr>';
        throw new Error(`Cannot call non-function "${name}" (got ${typeof fn})`);
    }
    const args = node.args.map(a => ctx.exec(a));
    return fn(...args);
}

function execReturn(node, ctx) {
    return makeReturn(ctx.exec(node.value));
}

function execBinaryExpr(node, ctx) {
    const left = ctx.exec(node.left);
    const right = ctx.exec(node.right);
    switch (node.op) {
        case '+': return left + right;
        case '-': return left - right;
        case '*': return left * right;
        case '/':
            if (right === 0) throw new Error('Division by zero');
            return left / right;
        case '==': return left === right;
        case '!=': return left !== right;
        case '<': return left < right;
        case '>': return left > right;
        case '<=': return left <= right;
        case '>=': return left >= right;
        default: throw new Error(`Unknown operator "${node.op}"`);
    }
}

function execIdentifier(node, ctx) {
    if (!ctx.scope.has(node.name)) {
        throw new Error(`Undefined variable "${node.name}"`);
    }
    return ctx.scope.get(node.name);
}

function execArrayLit(node, ctx) {
    return node.elements.map(el => ctx.exec(el));
}

function execObjectLit(node, ctx) {
    const obj = {};
    for (const { key, value } of node.entries) {
        obj[key] = ctx.exec(value);
    }
    return obj;
}
