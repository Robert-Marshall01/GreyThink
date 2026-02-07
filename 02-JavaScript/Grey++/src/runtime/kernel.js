// ─── Grey++ Runtime Kernel ───────────────────────────────────────────────────
// The kernel is a thin, modular plugin host.  Each AST node kind is handled
// by a registered executor plugin.  Plugins are fully isolated — they receive
// an `ExecutionContext` that exposes only the capabilities they need, so one
// plugin can never accidentally corrupt another's state.
//
// Key isolation guarantees:
//   • Plugins never share mutable state — the Scope chain provides lexical
//     isolation for variables.
//   • Each plugin registers via a well-defined interface; the kernel mediates
//     all node dispatch.
//   • The kernel itself holds no business logic — it is purely a dispatcher.

import { Scope } from './scope.js';

// ─── Execution Context ──────────────────────────────────────────────────────
// Lightweight value object passed into every executor call.  It carries the
// current scope and a reference back to the kernel for recursive dispatch,
// but *not* a reference to the executor map — plugins can't tamper with
// registration.

export class ExecutionContext {
    /**
     * @param {Kernel} kernel
     * @param {Scope}  scope
     */
    constructor(kernel, scope) {
        /** @private */
        this._kernel = kernel;
        /** Current lexical scope */
        this.scope = scope;
    }

    /** Recursively execute a child AST node in the *same* scope. */
    exec(node) {
        return this._kernel.execIn(node, this.scope);
    }

    /** Execute a child node in a *new* child scope (isolation boundary). */
    execScoped(node) {
        return this._kernel.execIn(node, this.scope.child());
    }

    /** Execute a node in an *explicit* scope (used by fn invocation). */
    execWith(node, scope) {
        return this._kernel.execIn(node, scope);
    }

    /** Create a child scope from the current one. */
    childScope() {
        return this.scope.child();
    }
}

// ─── Kernel ─────────────────────────────────────────────────────────────────

export class Kernel {
    constructor() {
        /** @type {Map<string, (node: any, ctx: ExecutionContext) => any>} */
        this._executors = new Map();

        /** Root scope — built-in globals live here. */
        this.rootScope = new Scope();
    }

    // ── Plugin registration ─────────────────────────────────────────────────

    /**
     * Register an executor for one or more AST node kinds.
     * @param {string | string[]} kinds
     * @param {(node: any, ctx: ExecutionContext) => any} handler
     */
    register(kinds, handler) {
        const list = Array.isArray(kinds) ? kinds : [kinds];
        for (const k of list) {
            if (this._executors.has(k)) {
                throw new Error(`Executor already registered for "${k}"`);
            }
            this._executors.set(k, handler);
        }
    }

    // ── Execution ───────────────────────────────────────────────────────────

    /**
     * Execute a node starting from the root scope.
     * This is the public entry-point used by the REPL.
     */
    exec(node) {
        return this.execIn(node, this.rootScope);
    }

    /**
     * Execute a node in an explicit scope.  Called internally by
     * ExecutionContext — plugins should use `ctx.exec()` / `ctx.execScoped()`
     * rather than calling this directly.
     * @param {any}   node
     * @param {Scope} scope
     */
    execIn(node, scope) {
        if (!node || !node.kind) {
            throw new Error('Kernel.exec received a non-AST value');
        }
        const handler = this._executors.get(node.kind);
        if (!handler) {
            throw new Error(`No executor registered for node kind "${node.kind}"`);
        }
        return handler(node, new ExecutionContext(this, scope));
    }

    // ── Convenience: define a global binding ────────────────────────────────

    defineGlobal(name, value) {
        this.rootScope.define(name, value);
    }
}
