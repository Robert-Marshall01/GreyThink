// ─── Grey++ Universal AST Node Types ─────────────────────────────────────────
// Every construct in Grey++ compiles down to one of these node kinds.
// The AST is paradigm-neutral: imperative, functional, declarative, and query
// constructs all share the same tree structure.

/** @enum {string} */
export const NodeKind = Object.freeze({
    // ── Primitives from the foundation spec ──
    FnDecl: 'FnDecl',       // fn name(params) { body }
    Query: 'Query',        // query { ... }
    Infer: 'Infer',        // (future) AI inference
    Sys: 'Sys',          // (future) system calls
    Module: 'Module',       // (future) module declarations

    // ── Expressions ──
    Identifier: 'Identifier',
    NumberLit: 'NumberLit',
    StringLit: 'StringLit',
    BoolLit: 'BoolLit',
    BinaryExpr: 'BinaryExpr',
    CallExpr: 'CallExpr',
    ReturnStmt: 'ReturnStmt',
    ArrayLit: 'ArrayLit',
    ObjectLit: 'ObjectLit',

    // ── Query-specific ──
    SelectClause: 'SelectClause',
    FromClause: 'FromClause',
    WhereClause: 'WhereClause',

    // ── Top-level ──
    Program: 'Program',
});

// ─── Factory helpers ─────────────────────────────────────────────────────────
// Lightweight constructors – no classes, just plain objects for easy
// serialisation / inspection.

/** @param {import('./types.js').ASTNode[]} body */
export function Program(body) {
    return { kind: NodeKind.Program, body };
}

/**
 * @param {string} name
 * @param {string[]} params
 * @param {import('./types.js').ASTNode[]} body
 */
export function FnDecl(name, params, body) {
    return { kind: NodeKind.FnDecl, name, params, body };
}

/**
 * @param {{ select: import('./types.js').ASTNode, from: import('./types.js').ASTNode|null, where: import('./types.js').ASTNode|null }} clauses
 */
export function Query(clauses) {
    return { kind: NodeKind.Query, ...clauses };
}

/** @param {string[]} columns */
export function SelectClause(columns) {
    return { kind: NodeKind.SelectClause, columns };
}

/** @param {string} table */
export function FromClause(table) {
    return { kind: NodeKind.FromClause, table };
}

/** @param {import('./types.js').ASTNode} condition */
export function WhereClause(condition) {
    return { kind: NodeKind.WhereClause, condition };
}

/** @param {string} name */
export function Identifier(name) {
    return { kind: NodeKind.Identifier, name };
}

/** @param {number} value */
export function NumberLit(value) {
    return { kind: NodeKind.NumberLit, value };
}

/** @param {string} value */
export function StringLit(value) {
    return { kind: NodeKind.StringLit, value };
}

/** @param {boolean} value */
export function BoolLit(value) {
    return { kind: NodeKind.BoolLit, value };
}

/**
 * @param {string} op
 * @param {import('./types.js').ASTNode} left
 * @param {import('./types.js').ASTNode} right
 */
export function BinaryExpr(op, left, right) {
    return { kind: NodeKind.BinaryExpr, op, left, right };
}

/**
 * @param {string} callee
 * @param {import('./types.js').ASTNode[]} args
 */
export function CallExpr(callee, args) {
    return { kind: NodeKind.CallExpr, callee, args };
}

/** @param {import('./types.js').ASTNode} value */
export function ReturnStmt(value) {
    return { kind: NodeKind.ReturnStmt, value };
}

/** @param {import('./types.js').ASTNode[]} elements */
export function ArrayLit(elements) {
    return { kind: NodeKind.ArrayLit, elements };
}

/** @param {{ key: string, value: import('./types.js').ASTNode }[]} entries */
export function ObjectLit(entries) {
    return { kind: NodeKind.ObjectLit, entries };
}
