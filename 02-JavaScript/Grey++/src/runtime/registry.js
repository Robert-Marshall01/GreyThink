// ─── Grey++ Plugin Registry ──────────────────────────────────────────────────
// Central loader that wires all available plugins into a Kernel instance.
// Adding a new paradigm (infer, sys, module) requires only:
//   1. Write a plugin in  runtime/plugins/<name>.js
//   2. Import & register it here.
//
// The registry guarantees:
//   • Each plugin is loaded exactly once.
//   • Plugin order doesn't matter — the kernel rejects duplicate kind
//     registrations, so conflicts surface immediately.
//   • The REPL (or any other host) calls `loadPlugins(kernel)` once and
//     doesn't need to know about individual plugins.

import { registerFnExecutor } from './plugins/fn.js';
import { registerQueryExecutor } from './plugins/query.js';

/**
 * Wire every Stage-1 plugin into the given kernel.
 * @param {import('./kernel.js').Kernel} kernel
 */
export function loadPlugins(kernel) {
    registerFnExecutor(kernel);
    registerQueryExecutor(kernel);
}
