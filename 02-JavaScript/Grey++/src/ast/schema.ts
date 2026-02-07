// ─── Grey++ TypeScript AST Schema ────────────────────────────────────────────
// Canonical type definitions for every AST node in the language.
// The JS runtime uses plain objects that conform to these shapes.
//
// Design principles:
//   • Discriminated union on `kind` — enables exhaustive switch/match.
//   • Each foundation primitive (fn, query, infer, sys, module) has its own
//     node type so plugins can pattern-match cheaply.
//   • `meta` bag on BaseNode for future source-maps, spans, diagnostics.
//   • Fully extensible: add a new interface + union member, done.

// ─── Node Kind Enum ─────────────────────────────────────────────────────────

export const enum NodeKind {
    // Foundation primitives
    FnDecl = 'FnDecl',
    Query = 'Query',
    Infer = 'Infer',
    Sys = 'Sys',
    Module = 'Module',

    // Expressions
    Identifier = 'Identifier',
    NumberLit = 'NumberLit',
    StringLit = 'StringLit',
    BoolLit = 'BoolLit',
    BinaryExpr = 'BinaryExpr',
    CallExpr = 'CallExpr',
    ArrayLit = 'ArrayLit',
    ObjectLit = 'ObjectLit',

    // Statements
    ReturnStmt = 'ReturnStmt',

    // Query-specific clauses
    SelectClause = 'SelectClause',
    FromClause = 'FromClause',
    WhereClause = 'WhereClause',

    // Top-level
    Program = 'Program',
}

// ─── Base ───────────────────────────────────────────────────────────────────

/** Shared fields present on every node. */
export interface BaseNode {
    kind: NodeKind;
    /** Extensible metadata bag (source spans, diagnostics, etc.) */
    meta?: Record<string, unknown>;
}

// ─── Foundation Primitives ──────────────────────────────────────────────────

/** `fn name(params) { body }` */
export interface FunctionNode extends BaseNode {
    kind: NodeKind.FnDecl;
    name: string;
    params: string[];
    body: ASTNode[];
}

/** `query { select … from … where … }` */
export interface QueryNode extends BaseNode {
    kind: NodeKind.Query;
    select: SelectClauseNode;
    from: FromClauseNode | null;
    where: WhereClauseNode | null;
}

/** `infer { … }` — AI / ML inference block (Stage 2+). */
export interface InferNode extends BaseNode {
    kind: NodeKind.Infer;
    /** Model identifier, e.g. "gpt-4", "local/llama" */
    model: string;
    /** Prompt or input expression */
    input: ASTNode;
    /** Optional configuration overrides */
    options?: Record<string, ASTNode>;
}

/** `sys { … }` — System-call block (Stage 2+). */
export interface SysNode extends BaseNode {
    kind: NodeKind.Sys;
    /** System operation name, e.g. "exec", "read", "env" */
    op: string;
    args: ASTNode[];
}

/** `module name { … }` — Module declaration (Stage 2+). */
export interface ModuleNode extends BaseNode {
    kind: NodeKind.Module;
    name: string;
    exports: ASTNode[];
}

// ─── Expressions ────────────────────────────────────────────────────────────

export interface IdentifierNode extends BaseNode {
    kind: NodeKind.Identifier;
    name: string;
}

export interface NumberLitNode extends BaseNode {
    kind: NodeKind.NumberLit;
    value: number;
}

export interface StringLitNode extends BaseNode {
    kind: NodeKind.StringLit;
    value: string;
}

export interface BoolLitNode extends BaseNode {
    kind: NodeKind.BoolLit;
    value: boolean;
}

export interface BinaryExprNode extends BaseNode {
    kind: NodeKind.BinaryExpr;
    op: string;
    left: ASTNode;
    right: ASTNode;
}

export interface CallExprNode extends BaseNode {
    kind: NodeKind.CallExpr;
    callee: string;
    args: ASTNode[];
}

export interface ArrayLitNode extends BaseNode {
    kind: NodeKind.ArrayLit;
    elements: ASTNode[];
}

export interface ObjectLitNode extends BaseNode {
    kind: NodeKind.ObjectLit;
    entries: ObjectEntry[];
}

export interface ObjectEntry {
    key: string;
    value: ASTNode;
}

// ─── Statements ─────────────────────────────────────────────────────────────

export interface ReturnStmtNode extends BaseNode {
    kind: NodeKind.ReturnStmt;
    value: ASTNode;
}

// ─── Query Clauses ──────────────────────────────────────────────────────────

export interface SelectClauseNode extends BaseNode {
    kind: NodeKind.SelectClause;
    columns: string[];
}

export interface FromClauseNode extends BaseNode {
    kind: NodeKind.FromClause;
    table: string;
}

export interface WhereClauseNode extends BaseNode {
    kind: NodeKind.WhereClause;
    condition: ASTNode;
}

// ─── Top-level ──────────────────────────────────────────────────────────────

export interface ProgramNode extends BaseNode {
    kind: NodeKind.Program;
    body: ASTNode[];
}

// ─── Discriminated Union ────────────────────────────────────────────────────
// Single union type that every consumer can switch on via `node.kind`.

export type ASTNode =
    // Foundation primitives
    | FunctionNode
    | QueryNode
    | InferNode
    | SysNode
    | ModuleNode
    // Expressions
    | IdentifierNode
    | NumberLitNode
    | StringLitNode
    | BoolLitNode
    | BinaryExprNode
    | CallExprNode
    | ArrayLitNode
    | ObjectLitNode
    // Statements
    | ReturnStmtNode
    // Query clauses
    | SelectClauseNode
    | FromClauseNode
    | WhereClauseNode
    // Top-level
    | ProgramNode;
