// ─── Query Executor Plugin ───────────────────────────────────────────────────
// Evaluates `query { select ... from ... where ... }` nodes against an
// in-memory table store.
//
// Isolation model:
//   • Each kernel gets its own table store via createTableStore() — no
//     cross-kernel leakage.
//   • Query execution receives only an ExecutionContext; it never touches
//     the kernel's internal executor map.
//   • Condition evaluation is self-contained and delegates value resolution
//     back through ctx.exec only when needed.

import { NodeKind } from '../../ast/types.js';

// ─── Table Store Factory ────────────────────────────────────────────────────
// Each kernel gets its own isolated store.  The returned helpers are bound
// to that store instance and injected as globals by the runner.

/**
 * Create a fresh, isolated table store.
 * @returns {{ createTable, insertRow, getTableNames, _tables }}
 */
export function createTableStore() {
    /** @type {Map<string, Record<string, any>[]>} */
    const tables = new Map();

    function createTable(name, rows = []) {
        tables.set(name, rows);
    }

    function insertRow(tableName, row) {
        if (!tables.has(tableName)) tables.set(tableName, []);
        tables.get(tableName).push(row);
    }

    function getTableNames() {
        return [...tables.keys()];
    }

    return { createTable, insertRow, getTableNames, _tables: tables };
}

// ─── Plugin Registration ────────────────────────────────────────────────────

/**
 * @param {import('../kernel.js').Kernel} kernel
 */
export function registerQueryExecutor(kernel) {
    // Create a per-kernel table store and attach it to the kernel
    const store = createTableStore();
    kernel._tableStore = store;
    kernel.register(NodeKind.Query, (node, ctx) => execQuery(node, ctx, store._tables));
}

// ─── Query Execution ────────────────────────────────────────────────────────

function execQuery(node, ctx, tables) {
    const { select, from, where } = node;

    // Resolve source rows
    let rows;
    if (from) {
        const tbl = tables.get(from.table);
        if (!tbl) throw new Error(`Unknown table "${from.table}"`);
        rows = [...tbl]; // shallow copy — never mutate the store
    } else {
        rows = [{}]; // No FROM → synthetic single row
    }

    // Apply WHERE filter
    if (where) {
        rows = rows.filter(row => evalCondition(where.condition, row, ctx));
    }

    // Project columns
    const cols = select.columns;
    if (cols.length === 1 && cols[0] === '*') return rows;

    return rows.map(row => {
        const projected = {};
        for (const c of cols) {
            projected[c] = row[c];
        }
        return projected;
    });
}

// ─── Condition Evaluator ────────────────────────────────────────────────────

function evalCondition(node, row, ctx) {
    if (node.kind === NodeKind.BinaryExpr) {
        const left = resolveValue(node.left, row, ctx);
        const right = resolveValue(node.right, row, ctx);
        switch (node.op) {
            case '==': return left === right;
            case '!=': return left !== right;
            case '<': return left < right;
            case '>': return left > right;
            case '<=': return left <= right;
            case '>=': return left >= right;
            default: throw new Error(`Unsupported operator in WHERE: ${node.op}`);
        }
    }
    if (node.kind === NodeKind.BoolLit) return node.value;
    throw new Error(`Unsupported WHERE condition kind: ${node.kind}`);
}

function resolveValue(node, row, _ctx) {
    if (node.kind === NodeKind.Identifier) return row[node.name];
    if (node.kind === NodeKind.NumberLit) return node.value;
    if (node.kind === NodeKind.StringLit) return node.value;
    if (node.kind === NodeKind.BoolLit) return node.value;
    throw new Error(`Cannot resolve value for node kind: ${node.kind}`);
}
