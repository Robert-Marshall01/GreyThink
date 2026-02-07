// ─── Grey++ Universal AST ────────────────────────────────────────────────────
// Single-file, minimal-but-extensible type definitions for the Grey++
// meta-language.  Every paradigm (imperative, functional, declarative, query,
// AI-inference, system, framework-adapter) compiles to the same tree.
// Extend by:
//   1. Add a member to `NodeKind`.
//   2. Define an interface extending `BaseNode`.
//   3. Add it to the `ASTNode` union.

// ─── Node Kinds ─────────────────────────────────────────────────────────────

export const enum NodeKind {
    // Foundation primitives
    Function = 'Function',
    Query = 'Query',
    Infer = 'Infer',
    Sys = 'Sys',
    Module = 'Module',
    Storage = 'Storage',
    Net = 'Net',
    Pipeline = 'Pipeline',
    PipelineStep = 'PipelineStep',
    Sec = 'Sec',
    Dist = 'Dist',
    Doc = 'Doc',
    Uni = 'Uni',
    Reflect = 'Reflect',
    Artifact = 'Artifact',
    Auto = 'Auto',
    Orchestration = 'Orchestration',
    SelfOpt = 'SelfOpt',
    Memory = 'Memory',
    Narrative = 'Narrative',
    Unify = 'Unify',
    Compiler = 'Compiler',
    ExecMulti = 'ExecMulti',
    Interop = 'Interop',

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

    // Query clauses
    SelectClause = 'SelectClause',
    FromClause = 'FromClause',
    WhereClause = 'WhereClause',

    // Top-level
    Program = 'Program',
}

// ─── Source Location (opt-in) ───────────────────────────────────────────────

export interface SourceSpan {
    file?: string;
    line: number;
    col: number;
    end?: { line: number; col: number };
}

// ─── Base ───────────────────────────────────────────────────────────────────

export interface BaseNode {
    kind: NodeKind;
    /** Source location — attached by the parser, consumed by diagnostics. */
    span?: SourceSpan;
    /** Open-ended metadata bag for tooling (linters, formatters, etc.). */
    meta?: Record<string, unknown>;
}

// ═══════════════════════════════════════════════════════════════════════════
//  FOUNDATION PRIMITIVES
// ═══════════════════════════════════════════════════════════════════════════

// ── FunctionNode ────────────────────────────────────────────────────────────
// Covers both imperative procedures and functional-style lambdas.
//
//   fn add(a, b) { return a + b }          — named, imperative
//   fn (x) { return x * 2 }               — anonymous / lambda
//   fn greet(name) async { ... }           — async variant

export type FnStyle = 'imperative' | 'functional' | 'async';

export interface FunctionNode extends BaseNode {
    kind: NodeKind.Function;
    /** `null` for anonymous / lambda functions. */
    name: string | null;
    params: ParamDecl[];
    body: ASTNode[];
    /** Defaults to 'imperative'. */
    style?: FnStyle;
    /** Return-type annotation (optional, future type system). */
    returnType?: string;
}

export interface ParamDecl {
    name: string;
    /** Optional type annotation. */
    type?: string;
    /** Optional default-value expression. */
    defaultValue?: ASTNode;
}

// ── QueryNode ───────────────────────────────────────────────────────────────
// SQL-style declarative queries.
//
//   query { select name, age from users where age > 21 }

export interface QueryNode extends BaseNode {
    kind: NodeKind.Query;
    select: SelectClauseNode;
    from: FromClauseNode | null;
    where: WhereClauseNode | null;
}

export interface SelectClauseNode extends BaseNode {
    kind: NodeKind.SelectClause;
    /** Column names, or `['*']` for wildcard. */
    columns: string[];
}

export interface FromClauseNode extends BaseNode {
    kind: NodeKind.FromClause;
    table: string;
    /** Optional alias: `from users as u` */
    alias?: string;
}

export interface WhereClauseNode extends BaseNode {
    kind: NodeKind.WhereClause;
    condition: ASTNode;
}

// ── InferNode ───────────────────────────────────────────────────────────────
// AI / ML inference blocks.
//
//   infer "gpt-4" { prompt: "explain closures" }
//   infer "local/llama" { input: embeddings, temperature: 0.7 }

export interface InferNode extends BaseNode {
    kind: NodeKind.Infer;
    /** Model identifier — cloud or local path. */
    model: string;
    /** Primary input expression (prompt, tensor, embedding, etc.). */
    input: ASTNode;
    /** Optional key → value config passed to the inference runtime. */
    options?: Record<string, ASTNode>;
}

// ── SysNode ─────────────────────────────────────────────────────────────────
// System-level primitives: logging, env access, process control, threading.
//
//   sys log("server started")
//   sys exec("ls", "-la")
//   sys spawn { ... }                — lightweight concurrency
//   sys env("DATABASE_URL")

export type SysOp =
    | 'log'       // console / structured logging
    | 'exec'      // run an external process
    | 'env'       // read an environment variable
    | 'read'      // read a file / resource
    | 'write'     // write a file / resource
    | 'spawn'     // spawn a concurrent task / thread
    | 'sleep'     // pause execution
    | (string & {});  // extensible — any string is valid

export interface SysNode extends BaseNode {
    kind: NodeKind.Sys;
    /** The system operation to perform. */
    op: SysOp;
    /** Positional arguments to the operation. */
    args: ASTNode[];
    /** Optional flags / config. */
    flags?: Record<string, ASTNode>;
}

// ── ModuleNode ──────────────────────────────────────────────────────────────
// Framework adapter declarations.  A module wraps Grey++ logic so it can be
// consumed by (or emit code for) external frameworks.
//
//   module "react" Dashboard { ... }
//   module "django" UserAPI  { ... }
//   module "spring" OrderService { ... }

export type FrameworkTarget =
    | 'react'
    | 'django'
    | 'spring'
    | 'express'
    | 'fastapi'
    | (string & {});  // extensible

export interface ModuleNode extends BaseNode {
    kind: NodeKind.Module;
    /** Human-readable module name. */
    name: string;
    /** Target framework this module adapts to. */
    framework?: FrameworkTarget;
    /** Declarations / exports inside the module. */
    exports: ASTNode[];
    /** Declared dependencies on other modules. */
    imports?: string[];
}

// ── StorageNode ─────────────────────────────────────────────────────────────
// Storage operations: PDF processing, DB queries, file I/O.
//
//   storage pdf load "demo.pdf"
//   storage pdf query "find headings"
//   storage db query "SELECT * FROM users"
//   storage file load "config.json"

export type StorageBackend = 'pdf' | 'db' | 'file' | (string & {});
export type StorageOp = 'load' | 'query' | 'edit' | 'list' | (string & {});

export interface StorageNode extends BaseNode {
    kind: NodeKind.Storage;
    /** Target storage backend. */
    backend: StorageBackend;
    /** Operation to perform. */
    op: StorageOp;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config. */
    flags?: Record<string, ASTNode>;
}

// ── NetNode ─────────────────────────────────────────────────────────────────
// Networking primitives: HTTP requests, socket connections, RPC calls.
//
//   net.http GET "https://example.com"
//   net.socket connect host=localhost port=8080
//   net.rpc call service=math method=add args=[1,2]

export type NetPrimitive = 'http' | 'socket' | 'rpc' | (string & {});

export interface NetNode extends BaseNode {
    kind: NodeKind.Net;
    /** The networking primitive type. */
    primitive: NetPrimitive;
    /** The operation / verb (GET, POST, connect, call, etc.). */
    op: string;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (host, port, service, method, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── PipelineNode ────────────────────────────────────────────────────────────
// Multi-step pipelines that chain inference, storage, and networking.
//
//   pipeline {
//     infer model=basic dataset=demo
//     store backend=pdf file="demo.pdf"
//     net.http POST "https://api.example.com"
//   }

export type PipelineStepKind = 'infer' | 'store' | 'net' | (string & {});

export interface PipelineStepNode extends BaseNode {
    kind: NodeKind.PipelineStep;
    /** The step type: infer, store, or net. */
    stepKind: PipelineStepKind;
    /** Key-value flags / config for this step. */
    flags?: Record<string, ASTNode>;
    /** Positional arguments. */
    args: ASTNode[];
    /** For net steps: the primitive (http, socket, rpc). */
    primitive?: string;
    /** For net steps: the operation / verb (GET, POST, connect, call). */
    op?: string;
}

export interface PipelineNode extends BaseNode {
    kind: NodeKind.Pipeline;
    /** Optional pipeline name for registration / reuse. */
    name?: string;
    /** Ordered list of steps to execute sequentially. */
    steps: PipelineStepNode[];
}

// ── SecNode ─────────────────────────────────────────────────────────────────
// Security primitives: permission checks, sandboxed execution, audit logging.
//
//   sec.perm user=alice action=net.http
//   sec.sandbox run "rm -rf testdir"
//   sec.audit message="Pipeline executed"

export type SecPrimitive = 'perm' | 'sandbox' | 'audit' | (string & {});

export interface SecNode extends BaseNode {
    kind: NodeKind.Sec;
    /** The security primitive type. */
    primitive: SecPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (user, action, message, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── DistNode ────────────────────────────────────────────────────────────────
// Distributed primitives: node registration, consensus voting, sync barriers.
//
//   dist.register node=alpha
//   dist.consensus proposal="upgrade pipeline"
//   dist.sync barrier="stage8"

export type DistPrimitive = 'register' | 'consensus' | 'sync' | (string & {});

export interface DistNode extends BaseNode {
    kind: NodeKind.Dist;
    /** The distributed primitive type. */
    primitive: DistPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (node, proposal, barrier, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── DocNode ─────────────────────────────────────────────────────────────────
// Document primitives: parsing, rendering, and semantic editing.
//
//   doc.parse file="report.pdf"
//   doc.render doc="report" format=html
//   doc.edit doc="report" target=heading.1 value="New Title"

export type DocPrimitive = 'parse' | 'render' | 'edit' | (string & {});

export interface DocNode extends BaseNode {
    kind: NodeKind.Doc;
    /** The document primitive type. */
    primitive: DocPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (file, doc, format, target, value, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── UniNode ─────────────────────────────────────────────────────────────────
// Unified syntax blocks that compose multiple primitive operations into a
// single `do { … }` block.  Each step is a child ASTNode (Net, Sec, Dist,
// Doc, Pipeline, etc.) executed sequentially, with consolidated output.
//
//   do {
//     net.http GET "https://api.example.com"
//     sec.perm user=alice action=doc.edit
//     doc.edit doc="report" target=heading.1 value="New Title"
//   }

export interface UniNode extends BaseNode {
    kind: NodeKind.Uni;
    /** Optional label for the unified block. */
    label?: string;
    /** Ordered list of primitive sub-nodes to execute sequentially. */
    steps: ASTNode[];
}

// ── ReflectNode ─────────────────────────────────────────────────────────────
// Reflection primitives: introspection of pipelines, grammar, trust, and
// distributed state.
//
//   reflect.pipeline id="p1"
//   reflect.grammar rule="doc.edit"
//   reflect.trust policy="sec.perm"
//   reflect.dist cluster="alpha"

export type ReflectPrimitive = 'pipeline' | 'grammar' | 'trust' | 'dist' | (string & {});

export interface ReflectNode extends BaseNode {
    kind: NodeKind.Reflect;
    /** The reflection primitive type. */
    primitive: ReflectPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (id, rule, policy, cluster, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── ArtifactNode ────────────────────────────────────────────────────────────
// Artifact primitives: density calculation, rarity mapping, and proof export.
//
//   artifact.density target="pipeline1"
//   artifact.rarity target="doc.edit"
//   artifact.proof target="dist.consensus"

export type ArtifactPrimitive = 'density' | 'rarity' | 'proof' | (string & {});

export interface ArtifactNode extends BaseNode {
    kind: NodeKind.Artifact;
    /** The artifact primitive type. */
    primitive: ArtifactPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (target, format, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── AutoNode ────────────────────────────────────────────────────────────────
// Auto-approval primitives: harmless command auto-approval, risky command
// sandboxing, and explicit approval prompting.
//
//   auto.safe command="ls"
//   auto.sandbox command="rm -rf testdir"
//   auto.prompt command="git reset --hard"

export type AutoPrimitive = 'safe' | 'sandbox' | 'prompt' | (string & {});

export interface AutoNode extends BaseNode {
    kind: NodeKind.Auto;
    /** The auto-approval primitive type. */
    primitive: AutoPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (command, reason, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── OrchestrationNode ───────────────────────────────────────────────────────
// Orchestration primitives: multi-domain pipeline composition, cross-module
// coordination, and global execution fabric.
//
//   orch.pipeline steps="net.http, doc.edit, dist.consensus"
//   orch.cross modules="pipeline, security, artifact"
//   orch.global scenario="civilization-tier coordination"

export type OrchestrationPrimitive = 'pipeline' | 'cross' | 'global' | (string & {});

export interface OrchestrationNode extends BaseNode {
    kind: NodeKind.Orchestration;
    /** The orchestration primitive type. */
    primitive: OrchestrationPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (steps, modules, scenario, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── SelfOptNode ─────────────────────────────────────────────────────────────
// Self-optimization primitives: pipeline optimization, trust optimization,
// and artifact optimization with adaptive feedback loops.
//
//   selfopt.pipeline target="pipeline1" strategy="latency"
//   selfopt.trust target="sec.perm" threshold=0.9
//   selfopt.artifact target="density" mode="aggressive"

export type SelfOptPrimitive = 'pipeline' | 'trust' | 'artifact' | (string & {});

export interface SelfOptNode extends BaseNode {
    kind: NodeKind.SelfOpt;
    /** The self-optimization primitive type. */
    primitive: SelfOptPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (target, strategy, threshold, mode, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── MemoryNode ──────────────────────────────────────────────────────────────
// Memory primitives: semantic storage, recall, and evolution of persistent
// knowledge within the Grey++ runtime.
//
//   memory.store key="pipeline1" value="optimized"
//   memory.recall key="doc.edit"
//   memory.evolve key="artifact.rarity"

export type MemoryPrimitive = 'store' | 'recall' | 'evolve' | (string & {});

export interface MemoryNode extends BaseNode {
    kind: NodeKind.Memory;
    /** The memory primitive type. */
    primitive: MemoryPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (key, value, strategy, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── NarrativeNode ───────────────────────────────────────────────────────────
// Narrative primitives: recruiter-ready proof generation, civilization-scale
// storytelling, and evolution logging.
//
//   narrative.proof source="artifact.rarity"
//   narrative.story theme="civilization-tier orchestration"
//   narrative.log stage="Stage 16"

export type NarrativePrimitive = 'proof' | 'story' | 'log' | (string & {});

export interface NarrativeNode extends BaseNode {
    kind: NodeKind.Narrative;
    /** The narrative primitive type. */
    primitive: NarrativePrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (source, theme, stage, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── UnifyNode ───────────────────────────────────────────────────────────────
// Meta-language unification primitives: syntax unification, semantic
// unification, and execution unification.
//
//   unify.syntax modules="pipeline, orchestration, memory"
//   unify.semantic source="artifact.rarity"
//   unify.exec scenario="civilization-tier orchestration"

export type UnifyPrimitive = 'syntax' | 'semantic' | 'exec' | (string & {});

export interface UnifyNode extends BaseNode {
    kind: NodeKind.Unify;
    /** The unification primitive type. */
    primitive: UnifyPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (modules, source, scenario, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── CompilerNode ────────────────────────────────────────────────────────────
// Compiler primitives: syntax compilation, backend code generation, and
// execution compilation.
//
//   compiler.syntax source="pipeline, orchestration"
//   compiler.backend target="javascript" optimize=true
//   compiler.exec scenario="full-stack deployment"

export type CompilerPrimitive = 'syntax' | 'backend' | 'exec' | (string & {});

export interface CompilerNode extends BaseNode {
    kind: NodeKind.Compiler;
    /** The compiler primitive type. */
    primitive: CompilerPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (source, target, optimize, scenario, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── ExecMultiNode ───────────────────────────────────────────────────────────
// Multi-backend execution primitives: JavaScript, C++, and RISC-V execution
// dispatch.
//
//   execmulti.js program="console.log('Hello Grey++')"
//   execmulti.cpp program="int main(){return 0;}"
//   execmulti.riscv program="addi x1, x0, 5"

export type ExecMultiPrimitive = 'js' | 'cpp' | 'riscv' | (string & {});

export interface ExecMultiNode extends BaseNode {
    kind: NodeKind.ExecMulti;
    /** The execution backend primitive type. */
    primitive: ExecMultiPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (program, optimize, arch, etc.). */
    flags?: Record<string, ASTNode>;
}

// ── InteropNode ─────────────────────────────────────────────────────────────
// Cross-backend interoperability primitives: data exchange, function call
// bridging, and orchestration unification.
//
//   interop.data source="execmulti.js" target="execmulti.cpp"
//   interop.call source="execmulti.cpp" target="execmulti.riscv"
//   interop.orch modules="js, cpp, riscv"

export type InteropPrimitive = 'data' | 'call' | 'orch' | (string & {});

export interface InteropNode extends BaseNode {
    kind: NodeKind.Interop;
    /** The interoperability primitive type. */
    primitive: InteropPrimitive;
    /** Positional arguments. */
    args: ASTNode[];
    /** Optional flags / config (source, target, modules, etc.). */
    flags?: Record<string, ASTNode>;
}

// ═══════════════════════════════════════════════════════════════════════════
//  EXPRESSIONS
// ═══════════════════════════════════════════════════════════════════════════

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

// ═══════════════════════════════════════════════════════════════════════════
//  STATEMENTS
// ═══════════════════════════════════════════════════════════════════════════

export interface ReturnStmtNode extends BaseNode {
    kind: NodeKind.ReturnStmt;
    value: ASTNode;
}

// ═══════════════════════════════════════════════════════════════════════════
//  TOP-LEVEL
// ═══════════════════════════════════════════════════════════════════════════

export interface ProgramNode extends BaseNode {
    kind: NodeKind.Program;
    body: ASTNode[];
}

// ═══════════════════════════════════════════════════════════════════════════
//  DISCRIMINATED UNION
// ═══════════════════════════════════════════════════════════════════════════
// Switch on `node.kind` for exhaustive handling.

export type ASTNode =
    // Foundation primitives
    | FunctionNode
    | QueryNode
    | InferNode
    | SysNode
    | ModuleNode
    | StorageNode
    | NetNode
    | PipelineNode
    | PipelineStepNode
    | SecNode
    | DistNode
    | DocNode
    | UniNode
    | ReflectNode
    | ArtifactNode
    | AutoNode
    | OrchestrationNode
    | SelfOptNode
    | MemoryNode
    | NarrativeNode
    | UnifyNode
    | CompilerNode
    | ExecMultiNode
    | InteropNode
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

// ═══════════════════════════════════════════════════════════════════════════
//  NORMALIZED AST  (Stage 18.5 — Universal Grammar Normalization)
// ═══════════════════════════════════════════════════════════════════════════
// These types form a parallel type system used by the Normalizer.
// They are NOT part of the main ASTNode union — they represent the
// cross-language universal constructs (bind, fn, loop, cond) that the
// normalizer produces from multi-language input.

/** Normalized node kinds — universal constructs. */
export type NormalizedKind = 'NormBind' | 'NormFn' | 'NormLoop' | 'NormCond' | 'NormStruct' | 'NormModule' | 'NormExpr' | 'NormBlock';

/** Base for all normalized nodes. */
export interface NormalizedBase {
    nkind: NormalizedKind;
    /** The source language this was normalized from. */
    sourceLang: string;
    /** The original source text. */
    sourceText: string;
}

/** Normalized variable binding: `bind x = <expr>` */
export interface NormBindNode extends NormalizedBase {
    nkind: 'NormBind';
    name: string;
    mutable: boolean;
    typeAnnotation?: string;
    value: string;
}

/** Normalized function definition: `fn name(params) { body }` */
export interface NormFnNode extends NormalizedBase {
    nkind: 'NormFn';
    name: string | null;
    params: string[];
    returnType?: string;
    body: string;
}

/** Normalized loop: `loop binding in iterable { body }` */
export interface NormLoopNode extends NormalizedBase {
    nkind: 'NormLoop';
    variant: 'for' | 'while' | 'infinite';
    binding?: string;
    iterable?: string;
    condition?: string;
    body: string;
}

/** Normalized conditional: `cond test { then } else { otherwise }` */
export interface NormCondNode extends NormalizedBase {
    nkind: 'NormCond';
    condition: string;
    thenBranch: string;
    elseBranch?: string;
    elifBranches?: Array<{ condition: string; body: string }>;
}

/** Normalized struct / class definition: `struct Name { fields }` */
export interface NormStructNode extends NormalizedBase {
    nkind: 'NormStruct';
    name: string;
    fields: Array<{ name: string; type?: string }>;
    methods?: string[];
}

/** Normalized module / import declaration: `module name { body }` */
export interface NormModuleNode extends NormalizedBase {
    nkind: 'NormModule';
    name: string;
    imports: string[];
    exports?: string[];
}

/** Normalized expression (pass-through). */
export interface NormExprNode extends NormalizedBase {
    nkind: 'NormExpr';
    expression: string;
}

/** Normalized block (sequence of normalized nodes). */
export interface NormBlockNode extends NormalizedBase {
    nkind: 'NormBlock';
    children: NormalizedNode[];
}

/** Discriminated union for all normalized AST nodes. */
export type NormalizedNode =
    | NormBindNode
    | NormFnNode
    | NormLoopNode
    | NormCondNode
    | NormStructNode
    | NormModuleNode
    | NormExprNode
    | NormBlockNode;

// ═══════════════════════════════════════════════════════════════════════════
//  NORMALIZER  (Stage 18.5 → 19 — Universal 50+ Language Normalization)
// ═══════════════════════════════════════════════════════════════════════════
// Accepts multi-language source constructs and normalizes them into the
// universal Grey++ normalized AST.  Supports ALL 50+ unified languages.

export class Normalizer {

    /** Normalize a single source line from a detected or specified language.
     *  Supports 50+ languages across all five groups. */
    normalize(source: string, language: string): NormalizedNode {
        const trimmed = source.trim();

        switch (language) {
            // ── Group 1: Core Application & Systems ──
            case 'python': return this.normalizePython(trimmed, source);
            case 'javascript': return this.normalizeJavaScript(trimmed, source);
            case 'typescript': return this.normalizeTypeScript(trimmed, source);
            case 'java': return this.normalizeJava(trimmed, source);
            case 'go': return this.normalizeGo(trimmed, source);
            case 'rust': return this.normalizeRust(trimmed, source);
            case 'csharp': return this.normalizeCSharp(trimmed, source);
            case 'cpp': return this.normalizeCpp(trimmed, source);
            case 'swift': return this.normalizeSwift(trimmed, source);
            case 'kotlin': return this.normalizeKotlin(trimmed, source);
            // ── Group 2: Data & Infrastructure ──
            case 'sql': return this.normalizeSQL(trimmed, source);
            case 'ruby': return this.normalizeRuby(trimmed, source);
            case 'php': return this.normalizePHP(trimmed, source);
            case 'bash': return this.normalizeBash(trimmed, source);
            case 'powershell': return this.normalizePowerShell(trimmed, source);
            case 'r': return this.normalizeR(trimmed, source);
            case 'scala': return this.normalizeScala(trimmed, source);
            case 'julia': return this.normalizeJulia(trimmed, source);
            case 'dart': return this.normalizeDart(trimmed, source);
            case 'objc': return this.normalizeObjC(trimmed, source);
            // ── Group 3: Functional & Niche ──
            case 'elixir': return this.normalizeElixir(trimmed, source);
            case 'clojure': return this.normalizeClojure(trimmed, source);
            case 'haskell': return this.normalizeHaskell(trimmed, source);
            case 'erlang': return this.normalizeErlang(trimmed, source);
            case 'fsharp': return this.normalizeFSharp(trimmed, source);
            case 'ocaml': return this.normalizeOCaml(trimmed, source);
            case 'lisp': return this.normalizeLisp(trimmed, source);
            case 'prolog': return this.normalizeProlog(trimmed, source);
            case 'solidity': return this.normalizeSolidity(trimmed, source);
            case 'zig': return this.normalizeZig(trimmed, source);
            // ── Group 4: Low-Level, Embedded & Legacy ──
            case 'c': return this.normalizeC(trimmed, source);
            case 'fortran': return this.normalizeFortran(trimmed, source);
            case 'vhdl': return this.normalizeVHDL(trimmed, source);
            case 'matlab': return this.normalizeMatlab(trimmed, source);
            case 'pascal': return this.normalizePascal(trimmed, source);
            case 'perl': return this.normalizePerl(trimmed, source);
            case 'apex': return this.normalizeApex(trimmed, source);
            // ── Group 5: Modern & Emerging ──
            case 'mojo': return this.normalizeMojo(trimmed, source);
            case 'lua': return this.normalizeLua(trimmed, source);
            case 'groovy': return this.normalizeGroovy(trimmed, source);
            case 'elm': return this.normalizeElm(trimmed, source);
            case 'nim': return this.normalizeNim(trimmed, source);
            case 'crystal': return this.normalizeCrystal(trimmed, source);
            case 'd': return this.normalizeD(trimmed, source);
            case 'vbnet': return this.normalizeVBNet(trimmed, source);
            case 'smalltalk': return this.normalizeSmalltalk(trimmed, source);
            // ── Grey++ ──
            case 'grey': return this.normalizeGrey(trimmed, source);
            default: return { nkind: 'NormExpr', sourceLang: language, sourceText: source, expression: trimmed };
        }
    }

    // ── Python ──────────────────────────────────────────────────────────

    private normalizePython(src: string, raw: string): NormalizedNode {
        // def name(params): body
        const defMatch = src.match(/^def\s+(\w+)\s*\(([^)]*)\)\s*(?:->.*?)?\s*:\s*(.*)$/);
        if (defMatch) {
            const body = defMatch[3].trim() || '...';
            return {
                nkind: 'NormFn', sourceLang: 'python', sourceText: raw,
                name: defMatch[1], params: defMatch[2].split(',').map(p => p.trim()).filter(Boolean),
                body: this.unifyBody(body),
            };
        }
        // class Name: or class Name(Base):
        const classMatch = src.match(/^class\s+(\w+)\s*(?:\([^)]*\))?\s*:\s*(.*)$/);
        if (classMatch) {
            const body = classMatch[2].trim();
            const fields = this.extractFields(body);
            return {
                nkind: 'NormStruct', sourceLang: 'python', sourceText: raw,
                name: classMatch[1], fields,
            };
        }
        // import x / from x import y
        const importMatch = src.match(/^(?:from\s+(\w+)\s+)?import\s+(.+)$/);
        if (importMatch) {
            const moduleName = importMatch[1] ?? importMatch[2].split(',')[0].trim();
            const imports = importMatch[2].split(',').map(s => s.trim());
            return {
                nkind: 'NormModule', sourceLang: 'python', sourceText: raw,
                name: moduleName, imports,
            };
        }
        // for x in iterable: body
        const forMatch = src.match(/^for\s+(\w+)\s+in\s+(.+?)\s*:\s*(.+)$/);
        if (forMatch) {
            const body = forMatch[3].trim() || '...';
            // Normalize range(N) → range(0,N)
            let iterable = forMatch[2];
            const rangeOne = iterable.match(/^range\s*\((\d+)\)$/);
            if (rangeOne) iterable = `range(0,${rangeOne[1]})`;
            return {
                nkind: 'NormLoop', sourceLang: 'python', sourceText: raw,
                variant: 'for', binding: forMatch[1], iterable,
                body: this.unifyBody(body),
            };
        }
        // while cond: body
        const whileMatch = src.match(/^while\s+(.+?)\s*:\s*(.*)$/);
        if (whileMatch) {
            const body = whileMatch[2].trim() || '...';
            return {
                nkind: 'NormLoop', sourceLang: 'python', sourceText: raw,
                variant: 'while', condition: whileMatch[1],
                body: this.unifyBody(body),
            };
        }
        // if cond: body
        const ifMatch = src.match(/^if\s+(.+?)\s*:\s*(.*)$/);
        if (ifMatch) {
            const body = ifMatch[2].trim() || '...';
            return {
                nkind: 'NormCond', sourceLang: 'python', sourceText: raw,
                condition: ifMatch[1], thenBranch: this.unifyBody(body),
            };
        }
        // assignment: name = expr
        const assignMatch = src.match(/^([a-zA-Z_]\w*)\s*=\s*(.+)$/);
        if (assignMatch) {
            return {
                nkind: 'NormBind', sourceLang: 'python', sourceText: raw,
                name: assignMatch[1], mutable: true, value: assignMatch[2],
            };
        }
        return { nkind: 'NormExpr', sourceLang: 'python', sourceText: raw, expression: src };
    }

    // ── JavaScript ──────────────────────────────────────────────────────

    private normalizeJavaScript(src: string, raw: string): NormalizedNode {
        // class Name { ... }
        const classMatch = src.match(/^class\s+(\w+)\s*(?:extends\s+\w+\s*)?\{\s*(.*)\}?\s*$/);
        if (classMatch) {
            const body = classMatch[2].replace(/\}\s*$/, '').trim();
            const fields = this.extractFields(body);
            return {
                nkind: 'NormStruct', sourceLang: 'javascript', sourceText: raw,
                name: classMatch[1], fields,
            };
        }
        // import ... from '...'  or  import '...'
        const importMatch = src.match(/^import\s+(?:\{?\s*(.+?)\s*\}?\s+from\s+)?['"](.+?)['"]\s*;?\s*$/);
        if (importMatch) {
            const moduleName = importMatch[2];
            const imports = importMatch[1] ? importMatch[1].split(',').map(s => s.trim()) : [moduleName];
            return {
                nkind: 'NormModule', sourceLang: 'javascript', sourceText: raw,
                name: moduleName, imports,
            };
        }
        // export { ... } or export default ...
        const exportMatch = src.match(/^export\s+(?:default\s+)?(?:\{?\s*(.+?)\s*\}?)\s*;?\s*$/);
        if (exportMatch) {
            const exports = exportMatch[1].split(',').map(s => s.trim());
            return {
                nkind: 'NormModule', sourceLang: 'javascript', sourceText: raw,
                name: 'exports', imports: [], exports,
            };
        }
        // function name(params) { body }
        const fnMatch = src.match(/^function\s+(\w+)\s*\(([^)]*)\)\s*\{\s*(.*)\}?\s*$/);
        if (fnMatch) {
            const body = fnMatch[3].replace(/\}\s*$/, '').trim() || '...';
            return {
                nkind: 'NormFn', sourceLang: 'javascript', sourceText: raw,
                name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim()).filter(Boolean),
                body: this.unifyBody(body),
            };
        }
        // let/const/var name = expr
        const bindMatch = src.match(/^(let|const|var)\s+(\w+)\s*=\s*(.+?)[\s;]*$/);
        if (bindMatch) {
            return {
                nkind: 'NormBind', sourceLang: 'javascript', sourceText: raw,
                name: bindMatch[2], mutable: bindMatch[1] !== 'const', value: bindMatch[3],
            };
        }
        // for(let i=0;i<N;i++){ body }
        const forClassicMatch = src.match(/^for\s*\(\s*(?:let|var|const)?\s*(\w+)\s*=\s*(\d+)\s*;\s*\w+\s*<\s*(\w+|\d+)\s*;.*?\)\s*\{\s*(.*)\}?\s*$/);
        if (forClassicMatch) {
            const body = forClassicMatch[4].replace(/\}\s*$/, '').trim() || '...';
            return {
                nkind: 'NormLoop', sourceLang: 'javascript', sourceText: raw,
                variant: 'for', binding: forClassicMatch[1],
                iterable: `range(${forClassicMatch[2]},${forClassicMatch[3]})`,
                body: this.unifyBody(body),
            };
        }
        // for (... of/in ...)
        const forOfMatch = src.match(/^for\s*\(\s*(?:let|const|var)?\s*(\w+)\s+(of|in)\s+(.+?)\s*\)\s*\{?\s*(.*)\}?\s*$/);
        if (forOfMatch) {
            const body = forOfMatch[4]?.replace(/\}\s*$/, '').trim() || '...';
            return {
                nkind: 'NormLoop', sourceLang: 'javascript', sourceText: raw,
                variant: 'for', binding: forOfMatch[1], iterable: forOfMatch[3],
                body: this.unifyBody(body),
            };
        }
        // while (cond) { body }
        const whileMatch = src.match(/^while\s*\((.+?)\)\s*\{?\s*(.*)\}?\s*$/);
        if (whileMatch) {
            const body = whileMatch[2]?.replace(/\}\s*$/, '').trim() || '...';
            return {
                nkind: 'NormLoop', sourceLang: 'javascript', sourceText: raw,
                variant: 'while', condition: whileMatch[1],
                body: this.unifyBody(body),
            };
        }
        // if (cond) { body }
        const ifMatch = src.match(/^if\s*\((.+?)\)\s*\{?\s*(.*)\}?\s*$/);
        if (ifMatch) {
            const body = ifMatch[2]?.replace(/\}\s*$/, '').trim() || '...';
            return {
                nkind: 'NormCond', sourceLang: 'javascript', sourceText: raw,
                condition: ifMatch[1],
                thenBranch: this.unifyBody(body),
            };
        }
        return { nkind: 'NormExpr', sourceLang: 'javascript', sourceText: raw, expression: src };
    }

    // ── C++ ─────────────────────────────────────────────────────────────

    private normalizeCpp(src: string, raw: string): NormalizedNode {
        // struct/class Name { fields };
        const structMatch = src.match(/^(?:struct|class)\s+(\w+)\s*\{\s*(.*?)\s*\}?\s*;?\s*$/);
        if (structMatch) {
            const body = structMatch[2].replace(/\}\s*;?\s*$/, '').trim();
            const fields = this.extractCppFields(body);
            return {
                nkind: 'NormStruct', sourceLang: 'cpp', sourceText: raw,
                name: structMatch[1], fields,
            };
        }
        // #include <header> or #include "header"
        const includeMatch = src.match(/^#include\s+[<"](.+?)[>"]\s*$/);
        if (includeMatch) {
            return {
                nkind: 'NormModule', sourceLang: 'cpp', sourceText: raw,
                name: includeMatch[1], imports: [includeMatch[1]],
            };
        }
        // namespace name { ... }
        const nsMatch = src.match(/^namespace\s+(\w+)\s*\{?\s*(.*)$/);
        if (nsMatch) {
            return {
                nkind: 'NormModule', sourceLang: 'cpp', sourceText: raw,
                name: nsMatch[1], imports: [],
            };
        }
        // Type name(params) { body }
        const fnMatch = src.match(/^(void|int|float|double|char|auto|std::\w+)\s+(\w+)\s*\(([^)]*)\)\s*\{\s*(.*)\}?\s*$/);
        if (fnMatch) {
            const body = fnMatch[4].replace(/\}\s*$/, '').trim() || '...';
            return {
                nkind: 'NormFn', sourceLang: 'cpp', sourceText: raw,
                name: fnMatch[2], params: fnMatch[3].split(',').map(p => p.trim()).filter(Boolean),
                returnType: fnMatch[1], body: this.unifyBody(body),
            };
        }
        // Type name(params) — function decl without brace
        const fnDeclMatch = src.match(/^(void|int|float|double|char|auto|std::\w+)\s+(\w+)\s*\(([^)]*)\)\s*$/);
        if (fnDeclMatch) {
            return {
                nkind: 'NormFn', sourceLang: 'cpp', sourceText: raw,
                name: fnDeclMatch[2], params: fnDeclMatch[3].split(',').map(p => p.trim()).filter(Boolean),
                returnType: fnDeclMatch[1], body: '...',
            };
        }
        // Type name = expr — binding
        const bindMatch = src.match(/^(int|float|double|char|auto|std::\w+)\s+(\w+)\s*=\s*(.+?)[\s;]*$/);
        if (bindMatch) {
            return {
                nkind: 'NormBind', sourceLang: 'cpp', sourceText: raw,
                name: bindMatch[2], mutable: true, typeAnnotation: bindMatch[1], value: bindMatch[3],
            };
        }
        // for (init; cond; step) { body }
        const forMatch = src.match(/^for\s*\((.+?);(.+?);(.+?)\)\s*\{?\s*(.*)\}?\s*$/);
        if (forMatch) {
            const body = forMatch[4]?.replace(/\}\s*$/, '').trim() || '...';
            // Extract binding and range from init/cond
            const initMatch = forMatch[1].trim().match(/(?:int|auto)?\s*(\w+)\s*=\s*(\d+)/);
            const condMatch = forMatch[2].trim().match(/\w+\s*<\s*(\w+|\d+)/);
            return {
                nkind: 'NormLoop', sourceLang: 'cpp', sourceText: raw,
                variant: 'for',
                binding: initMatch?.[1],
                iterable: initMatch && condMatch ? `range(${initMatch[2]},${condMatch[1]})` : undefined,
                condition: forMatch[2].trim(),
                body: this.unifyBody(body),
            };
        }
        // if (cond) { body }
        const ifMatch = src.match(/^if\s*\((.+?)\)\s*\{?\s*(.*)\}?\s*$/);
        if (ifMatch) {
            const body = ifMatch[2]?.replace(/\}\s*$/, '').trim() || '...';
            return {
                nkind: 'NormCond', sourceLang: 'cpp', sourceText: raw,
                condition: ifMatch[1],
                thenBranch: this.unifyBody(body),
            };
        }
        return { nkind: 'NormExpr', sourceLang: 'cpp', sourceText: raw, expression: src };
    }

    // ── Rust ────────────────────────────────────────────────────────────

    private normalizeRust(src: string, raw: string): NormalizedNode {
        // struct Name { fields }
        const structMatch = src.match(/^struct\s+(\w+)\s*\{\s*(.*?)\s*\}?\s*$/);
        if (structMatch) {
            const body = structMatch[2].replace(/\}\s*$/, '').trim();
            const fields = this.extractRustFields(body);
            return {
                nkind: 'NormStruct', sourceLang: 'rust', sourceText: raw,
                name: structMatch[1], fields,
            };
        }
        // mod name;  or  mod name { ... }
        const modMatch = src.match(/^mod\s+(\w+)\s*[;{]/);
        if (modMatch) {
            return {
                nkind: 'NormModule', sourceLang: 'rust', sourceText: raw,
                name: modMatch[1], imports: [],
            };
        }
        // use path::item;
        const useMatch = src.match(/^use\s+(.+?)\s*;?\s*$/);
        if (useMatch) {
            const path = useMatch[1];
            const name = path.split('::').pop() ?? path;
            return {
                nkind: 'NormModule', sourceLang: 'rust', sourceText: raw,
                name, imports: [path],
            };
        }
        // fn name(params) -> rettype {
        const fnMatch = src.match(/^fn\s+(\w+)\s*\(([^)]*)\)\s*(?:->\s*(\w+))?\s*\{?/);
        if (fnMatch) {
            return {
                nkind: 'NormFn', sourceLang: 'rust', sourceText: raw,
                name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim()).filter(Boolean),
                returnType: fnMatch[3], body: '...',
            };
        }
        // let / let mut
        const bindMatch = src.match(/^let\s+(mut\s+)?(\w+)\s*(?::\s*(\w+))?\s*=\s*(.+?)[\s;]*$/);
        if (bindMatch) {
            return {
                nkind: 'NormBind', sourceLang: 'rust', sourceText: raw,
                name: bindMatch[2], mutable: !!bindMatch[1],
                typeAnnotation: bindMatch[3], value: bindMatch[4],
            };
        }
        // for x in iterable
        const forMatch = src.match(/^for\s+(\w+)\s+in\s+(.+?)\s*\{?/);
        if (forMatch) {
            return {
                nkind: 'NormLoop', sourceLang: 'rust', sourceText: raw,
                variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: '...',
            };
        }
        // loop {
        if (src.startsWith('loop')) {
            return {
                nkind: 'NormLoop', sourceLang: 'rust', sourceText: raw,
                variant: 'infinite', body: '...',
            };
        }
        // if cond {
        const ifMatch = src.match(/^if\s+(.+?)\s*\{/);
        if (ifMatch) {
            return {
                nkind: 'NormCond', sourceLang: 'rust', sourceText: raw,
                condition: ifMatch[1], thenBranch: '...',
            };
        }
        return { nkind: 'NormExpr', sourceLang: 'rust', sourceText: raw, expression: src };
    }

    // ── Grey++ ──────────────────────────────────────────────────────────

    private normalizeGrey(src: string, raw: string): NormalizedNode {
        // bind name = expr
        const bindMatch = src.match(/^bind\s+(\w+)\s*=\s*(.+)$/);
        if (bindMatch) {
            return {
                nkind: 'NormBind', sourceLang: 'grey', sourceText: raw,
                name: bindMatch[1], mutable: false, value: bindMatch[2],
            };
        }
        // struct Name { fields }
        const structMatch = src.match(/^struct\s+(\w+)\s*\{\s*(.*?)\s*\}?\s*$/);
        if (structMatch) {
            const body = structMatch[2].replace(/\}\s*$/, '').trim();
            const fields = this.extractFields(body);
            return {
                nkind: 'NormStruct', sourceLang: 'grey', sourceText: raw,
                name: structMatch[1], fields,
            };
        }
        // module name { body }
        const moduleMatch = src.match(/^module\s+(\w+)\s*\{?\s*(.*)$/);
        if (moduleMatch) {
            const imports = moduleMatch[2]?.replace(/\}\s*$/, '').trim();
            return {
                nkind: 'NormModule', sourceLang: 'grey', sourceText: raw,
                name: moduleMatch[1], imports: imports ? imports.split(',').map(s => s.trim()).filter(Boolean) : [],
            };
        }
        // fn name(params) { body }
        const fnMatch = src.match(/^fn\s+(\w+)\s*\(([^)]*)\)\s*\{?/);
        if (fnMatch) {
            return {
                nkind: 'NormFn', sourceLang: 'grey', sourceText: raw,
                name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim()).filter(Boolean),
                body: '...',
            };
        }
        // loop binding in iterable { body }
        const loopMatch = src.match(/^loop\s+(\w+)\s+in\s+(.+?)\s*\{?/);
        if (loopMatch) {
            return {
                nkind: 'NormLoop', sourceLang: 'grey', sourceText: raw,
                variant: 'for', binding: loopMatch[1], iterable: loopMatch[2], body: '...',
            };
        }
        // cond test { then } else { otherwise }
        const condMatch = src.match(/^cond\s+(.+?)\s*\{/);
        if (condMatch) {
            return {
                nkind: 'NormCond', sourceLang: 'grey', sourceText: raw,
                condition: condMatch[1], thenBranch: '...',
            };
        }
        return { nkind: 'NormExpr', sourceLang: 'grey', sourceText: raw, expression: src };
    }

    // ── TypeScript ──────────────────────────────────────────────────────
    private normalizeTypeScript(src: string, raw: string): NormalizedNode {
        // function name(params): RetType { body }
        const fnMatch = src.match(/^(?:export\s+)?(?:async\s+)?function\s+(\w+)\s*(?:<[^>]*>)?\s*\(([^)]*)\)(?:\s*:\s*\S+)?\s*\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'typescript', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // const name = (params) => body
        const arrowMatch = src.match(/^(?:export\s+)?(?:const|let)\s+(\w+)\s*(?:<[^>]*>)?\s*=\s*(?:async\s+)?\(([^)]*)\)\s*(?::\s*\S+)?\s*=>\s*(.*)/);
        if (arrowMatch) return { nkind: 'NormFn', sourceLang: 'typescript', sourceText: raw, name: arrowMatch[1], params: arrowMatch[2].split(',').map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: this.unifyBody(arrowMatch[3] || '...') };
        // class/interface Name { ... }
        const classMatch = src.match(/^(?:export\s+)?(?:abstract\s+)?(?:class|interface)\s+(\w+)(?:<[^>]*>)?(?:\s+(?:extends|implements)\s+\S+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'typescript', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // type Name = ...
        const typeMatch = src.match(/^(?:export\s+)?type\s+(\w+)(?:<[^>]*>)?\s*=\s*(.*)/);
        if (typeMatch) return { nkind: 'NormStruct', sourceLang: 'typescript', sourceText: raw, name: typeMatch[1], fields: [] };
        // import
        const importMatch = src.match(/^import\s+(?:\{([^}]*)\}|(\w+))\s+from\s+['"](.*)['"]/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'typescript', sourceText: raw, name: importMatch[3], imports: (importMatch[1] || importMatch[2] || '').split(',').map(s => s.trim()).filter(Boolean) };
        // let/const binding
        const bindMatch = src.match(/^(?:export\s+)?(?:const|let|var)\s+(\w+)\s*(?::\s*\S+)?\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'typescript', sourceText: raw, name: bindMatch[1], mutable: src.startsWith('let'), value: bindMatch[2].replace(/;\s*$/, '') };
        return { nkind: 'NormExpr', sourceLang: 'typescript', sourceText: raw, expression: src };
    }

    // ── Java ────────────────────────────────────────────────────────────
    private normalizeJava(src: string, raw: string): NormalizedNode {
        // public/static RetType name(params) { body }
        const fnMatch = src.match(/^(?:(?:public|private|protected|static|final|abstract|synchronized)\s+)*(\w+(?:<[^>]*>)?)\s+(\w+)\s*\(([^)]*)\)\s*(?:throws\s+\w+(?:,\s*\w+)*)?\s*\{?\s*(.*)/);
        if (fnMatch && !['class', 'interface', 'enum', 'record'].includes(fnMatch[1])) return { nkind: 'NormFn', sourceLang: 'java', sourceText: raw, name: fnMatch[2], params: fnMatch[3].split(',').map(p => p.trim().split(/\s+/).pop() || '').filter(Boolean), body: this.unifyBody(fnMatch[4] || '...') };
        // class Name { ... }
        const classMatch = src.match(/^(?:(?:public|private|protected|static|final|abstract)\s+)*(?:class|interface|enum|record)\s+(\w+)(?:<[^>]*>)?(?:\s+(?:extends|implements)\s+[^{]+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'java', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // import
        const importMatch = src.match(/^import\s+(?:static\s+)?([\w.]+(?:\.\*)?)\s*;/);
        if (importMatch) { const parts = importMatch[1].split('.'); return { nkind: 'NormModule', sourceLang: 'java', sourceText: raw, name: parts.slice(0, -1).join('.'), imports: [parts[parts.length - 1]] }; }
        // Type name = value;
        const bindMatch = src.match(/^(?:(?:public|private|protected|static|final)\s+)*(\w+(?:<[^>]*>)?)\s+(\w+)\s*=\s*(.*?)\s*;?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'java', sourceText: raw, name: bindMatch[2], mutable: !src.includes('final'), value: bindMatch[3] };
        // for (Type x : collection) { ... }
        const forMatch = src.match(/^for\s*\(\s*\w+\s+(\w+)\s*:\s*(.+?)\s*\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'java', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'java', sourceText: raw, expression: src };
    }

    // ── Go ──────────────────────────────────────────────────────────────
    private normalizeGo(src: string, raw: string): NormalizedNode {
        // func name(params) retType { body }
        const fnMatch = src.match(/^func\s+(?:\(\s*\w+\s+\*?\w+\s*\)\s+)?(\w+)\s*\(([^)]*)\)\s*(?:\(?\s*[\w*,\s]*\)?\s*)?\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'go', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/\s+/)[0]).filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // type Name struct { ... }
        const structMatch = src.match(/^type\s+(\w+)\s+struct\s*\{?\s*(.*)/);
        if (structMatch) return { nkind: 'NormStruct', sourceLang: 'go', sourceText: raw, name: structMatch[1], fields: this.extractGoFields(structMatch[2]) };
        // type Name interface { ... }
        const ifaceMatch = src.match(/^type\s+(\w+)\s+interface\s*\{?\s*(.*)/);
        if (ifaceMatch) return { nkind: 'NormStruct', sourceLang: 'go', sourceText: raw, name: ifaceMatch[1], fields: [] };
        // import "pkg" / import ( ... )
        const importMatch = src.match(/^import\s+(?:\(\s*)?"([^"]+)"\s*\)?/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'go', sourceText: raw, name: importMatch[1], imports: [importMatch[1].split('/').pop() || importMatch[1]] };
        // name := value
        const bindMatch = src.match(/^(\w+)\s*:=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'go', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        // var name Type = value
        const varMatch = src.match(/^var\s+(\w+)\s+\S+\s*=\s*(.*)/);
        if (varMatch) return { nkind: 'NormBind', sourceLang: 'go', sourceText: raw, name: varMatch[1], mutable: true, value: varMatch[2] };
        // for range
        const forMatch = src.match(/^for\s+(?:(\w+)(?:\s*,\s*(\w+))?\s*:=\s*range\s+(.+?)|(.+?))\s*\{?\s*(.*)/);
        if (forMatch && forMatch[1]) return { nkind: 'NormLoop', sourceLang: 'go', sourceText: raw, variant: 'for', binding: forMatch[2] || forMatch[1], iterable: forMatch[3], body: forMatch[5] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'go', sourceText: raw, expression: src };
    }

    // ── C# ──────────────────────────────────────────────────────────────
    private normalizeCSharp(src: string, raw: string): NormalizedNode {
        // access RetType Name(params) { body }
        const fnMatch = src.match(/^(?:(?:public|private|protected|internal|static|virtual|override|async|abstract|sealed)\s+)*(\w+(?:<[^>]*>)?)\s+(\w+)\s*\(([^)]*)\)\s*\{?\s*(.*)/);
        if (fnMatch && !['class', 'struct', 'interface', 'enum', 'record', 'namespace'].includes(fnMatch[1])) return { nkind: 'NormFn', sourceLang: 'csharp', sourceText: raw, name: fnMatch[2], params: fnMatch[3].split(',').map(p => p.trim().split(/\s+/).pop() || '').filter(Boolean), body: this.unifyBody(fnMatch[4] || '...') };
        // class/struct/record Name { ... }
        const classMatch = src.match(/^(?:(?:public|private|protected|internal|static|abstract|sealed|partial)\s+)*(?:class|struct|interface|enum|record)\s+(\w+)(?:<[^>]*>)?(?:\s*:\s*[^{]+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'csharp', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // using Namespace;
        const usingMatch = src.match(/^using\s+([\w.]+)\s*;/);
        if (usingMatch) return { nkind: 'NormModule', sourceLang: 'csharp', sourceText: raw, name: usingMatch[1], imports: [usingMatch[1].split('.').pop()!] };
        // var/Type name = value;
        const bindMatch = src.match(/^(?:(?:public|private|protected|internal|static|readonly|const)\s+)*(?:var|(\w+))\s+(\w+)\s*=\s*(.*?)\s*;?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'csharp', sourceText: raw, name: bindMatch[2], mutable: !src.includes('readonly') && !src.includes('const'), value: bindMatch[3] };
        // foreach (Type x in collection) { ... }
        const forMatch = src.match(/^foreach\s*\(\s*(?:var|\w+)\s+(\w+)\s+in\s+(.+?)\s*\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'csharp', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'csharp', sourceText: raw, expression: src };
    }

    // ── Swift ───────────────────────────────────────────────────────────
    private normalizeSwift(src: string, raw: string): NormalizedNode {
        // func name(params) -> RetType { body }
        const fnMatch = src.match(/^(?:(?:public|private|internal|open|static|class|override|mutating)\s+)*func\s+(\w+)\s*\(([^)]*)\)\s*(?:->\s*\S+)?\s*\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'swift', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // class/struct/enum Name { ... }
        const classMatch = src.match(/^(?:(?:public|private|internal|open|final)\s+)*(?:class|struct|enum|protocol)\s+(\w+)(?:<[^>]*>)?(?:\s*:\s*[^{]+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'swift', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // import Module
        const importMatch = src.match(/^import\s+(\w+)/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'swift', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        // let/var name: Type = value
        const bindMatch = src.match(/^(?:let|var)\s+(\w+)\s*(?::\s*\S+)?\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'swift', sourceText: raw, name: bindMatch[1], mutable: src.startsWith('var'), value: bindMatch[2] };
        // for x in collection { ... }
        const forMatch = src.match(/^for\s+(\w+)\s+in\s+(.+?)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'swift', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'swift', sourceText: raw, expression: src };
    }

    // ── Kotlin ──────────────────────────────────────────────────────────
    private normalizeKotlin(src: string, raw: string): NormalizedNode {
        // fun name(params): RetType { body }
        const fnMatch = src.match(/^(?:(?:public|private|protected|internal|open|override|suspend|inline)\s+)*fun\s+(?:<[^>]*>\s*)?(\w+)\s*\(([^)]*)\)\s*(?::\s*\S+)?\s*[={]?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'kotlin', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // class/data class/object Name { ... }
        const classMatch = src.match(/^(?:(?:public|private|protected|internal|open|abstract|sealed|data|enum|annotation|inner)\s+)*(?:class|object|interface)\s+(\w+)(?:<[^>]*>)?(?:\s*(?:\(.*?\))?\s*(?::\s*[^{]+)?)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'kotlin', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // import pkg.Name
        const importMatch = src.match(/^import\s+([\w.]+)/);
        if (importMatch) { const parts = importMatch[1].split('.'); return { nkind: 'NormModule', sourceLang: 'kotlin', sourceText: raw, name: parts.slice(0, -1).join('.'), imports: [parts[parts.length - 1]] }; }
        // val/var name: Type = value
        const bindMatch = src.match(/^(?:val|var)\s+(\w+)\s*(?::\s*\S+)?\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'kotlin', sourceText: raw, name: bindMatch[1], mutable: src.startsWith('var'), value: bindMatch[2] };
        // for (x in collection) { ... }
        const forMatch = src.match(/^for\s*\(\s*(\w+)\s+in\s+(.+?)\s*\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'kotlin', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'kotlin', sourceText: raw, expression: src };
    }

    // ── SQL ─────────────────────────────────────────────────────────────
    private normalizeSQL(src: string, raw: string): NormalizedNode {
        const upper = src.toUpperCase();
        // SELECT ... FROM ...
        const selMatch = src.match(/^SELECT\s+(.*?)\s+FROM\s+(\w+)/i);
        if (selMatch) return { nkind: 'NormExpr', sourceLang: 'sql', sourceText: raw, expression: `query ${selMatch[2]} select ${selMatch[1]}` };
        // CREATE TABLE Name ( ... )
        const createMatch = src.match(/^CREATE\s+TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?(\w+)\s*\(\s*(.*)/i);
        if (createMatch) return { nkind: 'NormStruct', sourceLang: 'sql', sourceText: raw, name: createMatch[1], fields: this.extractSQLFields(createMatch[2]) };
        // INSERT INTO Name ...
        const insMatch = src.match(/^INSERT\s+INTO\s+(\w+)/i);
        if (insMatch) return { nkind: 'NormExpr', sourceLang: 'sql', sourceText: raw, expression: `insert ${insMatch[1]}` };
        // CREATE FUNCTION / CREATE PROCEDURE
        const procMatch = src.match(/^CREATE\s+(?:OR\s+REPLACE\s+)?(?:FUNCTION|PROCEDURE)\s+(\w+)\s*\(([^)]*)\)/i);
        if (procMatch) return { nkind: 'NormFn', sourceLang: 'sql', sourceText: raw, name: procMatch[1], params: procMatch[2].split(',').map(p => p.trim().split(/\s+/)[0]).filter(Boolean), body: '...' };
        return { nkind: 'NormExpr', sourceLang: 'sql', sourceText: raw, expression: src };
    }

    // ── Ruby ────────────────────────────────────────────────────────────
    private normalizeRuby(src: string, raw: string): NormalizedNode {
        // def name(params) body end
        const fnMatch = src.match(/^def\s+(?:self\.)?(\w+[?!]?)\s*(?:\(([^)]*)\))?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'ruby', sourceText: raw, name: fnMatch[1], params: (fnMatch[2] || '').split(',').map(p => p.trim()).filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // class Name < Base ... end
        const classMatch = src.match(/^class\s+(\w+)(?:\s*<\s*\w+)?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'ruby', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // module Name ... end
        const modMatch = src.match(/^module\s+(\w+)/);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'ruby', sourceText: raw, name: modMatch[1], imports: [] };
        // require 'name'
        const reqMatch = src.match(/^require(?:_relative)?\s+['"]([^'"]+)['"]/);
        if (reqMatch) return { nkind: 'NormModule', sourceLang: 'ruby', sourceText: raw, name: reqMatch[1], imports: [reqMatch[1]] };
        // name = value
        const bindMatch = src.match(/^(\w+)\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'ruby', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        return { nkind: 'NormExpr', sourceLang: 'ruby', sourceText: raw, expression: src };
    }

    // ── PHP ─────────────────────────────────────────────────────────────
    private normalizePHP(src: string, raw: string): NormalizedNode {
        // function name(params) { body }
        const fnMatch = src.match(/^(?:(?:public|private|protected|static)\s+)*function\s+(\w+)\s*\(([^)]*)\)\s*(?::\s*\??\w+)?\s*\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'php', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().replace(/^\$/, '')).filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // class Name { ... }
        const classMatch = src.match(/^(?:(?:abstract|final)\s+)?class\s+(\w+)(?:\s+extends\s+\w+)?(?:\s+implements\s+[^{]+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'php', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // use Namespace;
        const useMatch = src.match(/^use\s+([\w\\]+)\s*;/);
        if (useMatch) return { nkind: 'NormModule', sourceLang: 'php', sourceText: raw, name: useMatch[1], imports: [useMatch[1].split('\\').pop()!] };
        // $name = value;
        const bindMatch = src.match(/^\$(\w+)\s*=\s*(.*?)\s*;?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'php', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        // foreach ($arr as $key => $val) { ... }
        const forMatch = src.match(/^foreach\s*\(\s*\$(\w+)\s+as\s+(?:\$\w+\s*=>\s*)?\$(\w+)\s*\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'php', sourceText: raw, variant: 'for', binding: forMatch[2], iterable: `$${forMatch[1]}`, body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'php', sourceText: raw, expression: src };
    }

    // ── Bash/Shell ──────────────────────────────────────────────────────
    private normalizeBash(src: string, raw: string): NormalizedNode {
        // name() { body } or function name { body }
        const fnMatch = src.match(/^(?:function\s+)?(\w+)\s*\(\)\s*\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'bash', sourceText: raw, name: fnMatch[1], params: [], body: fnMatch[2] || '...' };
        // name=value
        const bindMatch = src.match(/^(?:export\s+)?(\w+)=(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'bash', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2].replace(/^["']|["']$/g, '') };
        // for x in ...; do ... done
        const forMatch = src.match(/^for\s+(\w+)\s+in\s+(.*?)\s*;?\s*do\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'bash', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        // if [ condition ]; then
        const ifMatch = src.match(/^if\s+\[?\[?\s*(.*?)\s*\]?\]?\s*;\s*then\s*(.*)/);
        if (ifMatch) return { nkind: 'NormCond', sourceLang: 'bash', sourceText: raw, condition: ifMatch[1], thenBranch: ifMatch[2] || '...' };
        // source / . script
        const srcMatch = src.match(/^(?:source|\.) (.+)/);
        if (srcMatch) return { nkind: 'NormModule', sourceLang: 'bash', sourceText: raw, name: srcMatch[1], imports: [srcMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'bash', sourceText: raw, expression: src };
    }

    // ── PowerShell ──────────────────────────────────────────────────────
    private normalizePowerShell(src: string, raw: string): NormalizedNode {
        // function Name { param($x) body }
        const fnMatch = src.match(/^function\s+([\w-]+)\s*(?:\(([^)]*)\))?\s*\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'powershell', sourceText: raw, name: fnMatch[1], params: (fnMatch[2] || '').split(',').map(p => p.trim().replace(/^\$/, '')).filter(Boolean), body: fnMatch[3] || '...' };
        // $Name = value
        const bindMatch = src.match(/^\$(\w+)\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'powershell', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        // foreach ($x in $collection) { ... }
        const forMatch = src.match(/^foreach\s*\(\s*\$(\w+)\s+in\s+(.+?)\s*\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'powershell', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        // if (condition) { ... }
        const ifMatch = src.match(/^if\s*\(\s*(.*?)\s*\)\s*\{?\s*(.*)/);
        if (ifMatch) return { nkind: 'NormCond', sourceLang: 'powershell', sourceText: raw, condition: ifMatch[1], thenBranch: ifMatch[2] || '...' };
        // Import-Module Name
        const modMatch = src.match(/^Import-Module\s+([\w.-]+)/i);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'powershell', sourceText: raw, name: modMatch[1], imports: [modMatch[1]] };
        // class Name { ... }
        const classMatch = src.match(/^class\s+(\w+)(?:\s*:\s*\w+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'powershell', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        return { nkind: 'NormExpr', sourceLang: 'powershell', sourceText: raw, expression: src };
    }

    // ── R ───────────────────────────────────────────────────────────────
    private normalizeR(src: string, raw: string): NormalizedNode {
        // name <- function(params) { body }
        const fnMatch = src.match(/^(\w+)\s*<-\s*function\s*\(([^)]*)\)\s*\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'r', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split('=')[0].trim()).filter(Boolean), body: fnMatch[3] || '...' };
        // name <- value
        const bindMatch = src.match(/^(\w[\w.]*)\s*<-\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'r', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        // for (x in seq) { ... }
        const forMatch = src.match(/^for\s*\(\s*(\w+)\s+in\s+(.+?)\s*\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'r', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        // library(name) / require(name)
        const libMatch = src.match(/^(?:library|require)\s*\(\s*['"]*(\w+)['"]*\s*\)/);
        if (libMatch) return { nkind: 'NormModule', sourceLang: 'r', sourceText: raw, name: libMatch[1], imports: [libMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'r', sourceText: raw, expression: src };
    }

    // ── Scala ───────────────────────────────────────────────────────────
    private normalizeScala(src: string, raw: string): NormalizedNode {
        // def name(params): RetType = body
        const fnMatch = src.match(/^(?:(?:private|protected|override)\s+)*def\s+(\w+)\s*(?:\[.*?\])?\s*\(([^)]*)\)\s*(?::\s*\S+)?\s*=?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'scala', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // class/object/trait Name { ... }
        const classMatch = src.match(/^(?:(?:sealed|abstract|final|case|implicit)\s+)*(?:class|object|trait)\s+(\w+)(?:\[.*?\])?(?:\s*(?:extends|with)\s+[^{]+)?\s*(?:\(.*?\))?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'scala', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // import pkg.Name
        const importMatch = src.match(/^import\s+([\w.{},\s_]+)/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'scala', sourceText: raw, name: importMatch[1].split('.')[0], imports: [importMatch[1].trim()] };
        // val/var name: Type = value
        const bindMatch = src.match(/^(?:val|var|lazy\s+val)\s+(\w+)\s*(?::\s*\S+)?\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'scala', sourceText: raw, name: bindMatch[1], mutable: src.startsWith('var'), value: bindMatch[2] };
        // for (x <- collection) { ... }
        const forMatch = src.match(/^for\s*\(\s*(\w+)\s*<-\s*(.+?)\s*\)\s*(?:yield\s+)?(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'scala', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'scala', sourceText: raw, expression: src };
    }

    // ── Julia ───────────────────────────────────────────────────────────
    private normalizeJulia(src: string, raw: string): NormalizedNode {
        // function name(params) body end
        const fnMatch = src.match(/^function\s+(\w+)\s*\(([^)]*)\)\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'julia', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/::/)[0]).filter(Boolean), body: fnMatch[3] || '...' };
        // short function: name(params) = body
        const shortFn = src.match(/^(\w+)\s*\(([^)]*)\)\s*=\s*(.*)/);
        if (shortFn) return { nkind: 'NormFn', sourceLang: 'julia', sourceText: raw, name: shortFn[1], params: shortFn[2].split(',').map(p => p.trim().split(/::/)[0]).filter(Boolean), body: shortFn[3] };
        // struct Name ... end
        const structMatch = src.match(/^(?:mutable\s+)?struct\s+(\w+)(?:<:\s*\w+)?\s*(.*)/);
        if (structMatch) return { nkind: 'NormStruct', sourceLang: 'julia', sourceText: raw, name: structMatch[1], fields: this.extractFields(structMatch[2]) };
        // using Package / import Package
        const importMatch = src.match(/^(?:using|import)\s+([\w.,:\s]+)/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'julia', sourceText: raw, name: importMatch[1].split(',')[0].trim(), imports: importMatch[1].split(',').map(s => s.trim()) };
        // name = value
        const bindMatch = src.match(/^(?:const\s+)?(\w+)\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'julia', sourceText: raw, name: bindMatch[1], mutable: !src.startsWith('const'), value: bindMatch[2] };
        // for x in collection ... end
        const forMatch = src.match(/^for\s+(\w+)\s+in\s+(.+)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'julia', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: '...' };
        return { nkind: 'NormExpr', sourceLang: 'julia', sourceText: raw, expression: src };
    }

    // ── Dart ────────────────────────────────────────────────────────────
    private normalizeDart(src: string, raw: string): NormalizedNode {
        // RetType name(params) { body }
        const fnMatch = src.match(/^(?:(?:static|async|Future<\w+>|void|int|double|String|bool|dynamic)\s+)+(\w+)\s*\(([^)]*)\)\s*(?:async\s*)?\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'dart', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/\s+/).pop() || '').filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // class Name { ... }
        const classMatch = src.match(/^(?:abstract\s+)?class\s+(\w+)(?:<[^>]*>)?(?:\s+(?:extends|with|implements)\s+[^{]+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'dart', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // import 'package:name/...';
        const importMatch = src.match(/^import\s+['"](.*?)['"]\s*;/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'dart', sourceText: raw, name: importMatch[1], imports: [importMatch[1].split('/').pop()!.replace('.dart', '')] };
        // var/final/const name = value;
        const bindMatch = src.match(/^(?:(?:final|const|late)\s+)?(?:var|(\w+))\s+(\w+)\s*=\s*(.*?)\s*;?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'dart', sourceText: raw, name: bindMatch[2], mutable: !src.includes('final') && !src.includes('const'), value: bindMatch[3] };
        // for (var x in collection) { ... }
        const forMatch = src.match(/^for\s*\(\s*(?:var|final)\s+(\w+)\s+in\s+(.+?)\s*\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'dart', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'dart', sourceText: raw, expression: src };
    }

    // ── Objective-C ─────────────────────────────────────────────────────
    private normalizeObjC(src: string, raw: string): NormalizedNode {
        // - (RetType)name:(Type)param ...
        const fnMatch = src.match(/^[+-]\s*\(\s*(\w+)\s*\)\s*(\w+)(?:\s*:\s*\((\w+)\s*\)\s*(\w+))?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'objc', sourceText: raw, name: fnMatch[2], params: fnMatch[4] ? [fnMatch[4]] : [], body: this.unifyBody(fnMatch[5] || '...') };
        // @interface Name : Base ... @end
        const classMatch = src.match(/^@(?:interface|implementation)\s+(\w+)(?:\s*:\s*\w+)?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'objc', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // #import <...> or #import "..."
        const importMatch = src.match(/^#import\s+[<"]([^>"]+)[>"]/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'objc', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'objc', sourceText: raw, expression: src };
    }

    // ── Elixir ──────────────────────────────────────────────────────────
    private normalizeElixir(src: string, raw: string): NormalizedNode {
        // def name(params) do body end
        const fnMatch = src.match(/^(?:def|defp)\s+(\w+)\s*(?:\(([^)]*)\))?\s*(?:do\s*(.*))?/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'elixir', sourceText: raw, name: fnMatch[1], params: (fnMatch[2] || '').split(',').map(p => p.trim()).filter(Boolean), body: fnMatch[3] || '...' };
        // defmodule Name do ... end
        const modMatch = src.match(/^defmodule\s+([\w.]+)\s+do\s*(.*)/);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'elixir', sourceText: raw, name: modMatch[1], imports: [] };
        // defstruct [ ... ]
        const structMatch = src.match(/^defstruct\s+\[?(.*?)\]?\s*$/);
        if (structMatch) return { nkind: 'NormStruct', sourceLang: 'elixir', sourceText: raw, name: 'struct', fields: structMatch[1].split(',').map(f => ({ name: f.trim().replace(/^:/, '') })) };
        // name = value
        const bindMatch = src.match(/^(\w+)\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'elixir', sourceText: raw, name: bindMatch[1], mutable: false, value: bindMatch[2] };
        // for x <- collection do ... end
        const forMatch = src.match(/^for\s+(\w+)\s*<-\s*(.+?)\s*(?:,\s*do:\s*(.*))?/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'elixir', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        // use/alias/import/require
        const useMatch = src.match(/^(?:use|alias|import|require)\s+([\w.]+)/);
        if (useMatch) return { nkind: 'NormModule', sourceLang: 'elixir', sourceText: raw, name: useMatch[1], imports: [useMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'elixir', sourceText: raw, expression: src };
    }

    // ── Clojure ─────────────────────────────────────────────────────────
    private normalizeClojure(src: string, raw: string): NormalizedNode {
        // (defn name [params] body)
        const fnMatch = src.match(/^\(defn-?\s+(\w[\w-]*)\s*\[([^\]]*)\]\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'clojure', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(/\s+/).filter(Boolean), body: fnMatch[3].replace(/\)\s*$/, '') || '...' };
        // (defrecord Name [fields])
        const recMatch = src.match(/^\(defrecord\s+(\w+)\s*\[([^\]]*)\]/);
        if (recMatch) return { nkind: 'NormStruct', sourceLang: 'clojure', sourceText: raw, name: recMatch[1], fields: recMatch[2].split(/\s+/).filter(Boolean).map(n => ({ name: n })) };
        // (ns name (:require ...))
        const nsMatch = src.match(/^\(ns\s+([\w.-]+)/);
        if (nsMatch) return { nkind: 'NormModule', sourceLang: 'clojure', sourceText: raw, name: nsMatch[1], imports: [] };
        // (def name value)
        const defMatch = src.match(/^\(def\s+(\w[\w-]*)\s+(.*)/);
        if (defMatch) return { nkind: 'NormBind', sourceLang: 'clojure', sourceText: raw, name: defMatch[1], mutable: false, value: defMatch[2].replace(/\)\s*$/, '') };
        // (require ...)
        const reqMatch = src.match(/^\((?:require|use)\s+'\[?([\w.-]+)/);
        if (reqMatch) return { nkind: 'NormModule', sourceLang: 'clojure', sourceText: raw, name: reqMatch[1], imports: [reqMatch[1]] };
        // (doseq [x coll] body)
        const forMatch = src.match(/^\(doseq\s+\[(\w+)\s+(.+?)\]\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'clojure', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3].replace(/\)\s*$/, '') || '...' };
        return { nkind: 'NormExpr', sourceLang: 'clojure', sourceText: raw, expression: src };
    }

    // ── Haskell ─────────────────────────────────────────────────────────
    private normalizeHaskell(src: string, raw: string): NormalizedNode {
        // name :: Type -> Type (type signature)
        const sigMatch = src.match(/^(\w+)\s*::\s*(.+)/);
        if (sigMatch) return { nkind: 'NormExpr', sourceLang: 'haskell', sourceText: raw, expression: `typesig ${sigMatch[1]} :: ${sigMatch[2]}` };
        // name params = body
        const fnMatch = src.match(/^(\w+)\s+([\w\s]+?)\s*=\s*(.*)/);
        if (fnMatch && !/^(let|where|module|import|data|newtype|type|class|instance|if|then|else|do)$/.test(fnMatch[1]))
            return { nkind: 'NormFn', sourceLang: 'haskell', sourceText: raw, name: fnMatch[1], params: fnMatch[2].trim().split(/\s+/), body: fnMatch[3] };
        // data Name = ...
        const dataMatch = src.match(/^(?:data|newtype)\s+(\w+)(?:\s+[\w\s]*)?\s*=\s*(.*)/);
        if (dataMatch) return { nkind: 'NormStruct', sourceLang: 'haskell', sourceText: raw, name: dataMatch[1], fields: [] };
        // module Name where
        const modMatch = src.match(/^module\s+([\w.]+)/);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'haskell', sourceText: raw, name: modMatch[1], imports: [] };
        // import Module
        const importMatch = src.match(/^import\s+(?:qualified\s+)?([\w.]+)/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'haskell', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'haskell', sourceText: raw, expression: src };
    }

    // ── Erlang ──────────────────────────────────────────────────────────
    private normalizeErlang(src: string, raw: string): NormalizedNode {
        // name(Params) -> body.
        const fnMatch = src.match(/^(\w+)\s*\(([^)]*)\)\s*->\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'erlang', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim()).filter(Boolean), body: fnMatch[3].replace(/\.\s*$/, '') };
        // -module(name).
        const modMatch = src.match(/^-module\((\w+)\)\./);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'erlang', sourceText: raw, name: modMatch[1], imports: [] };
        // -record(name, {fields}).
        const recMatch = src.match(/^-record\((\w+),\s*\{([^}]*)\}\)/);
        if (recMatch) return { nkind: 'NormStruct', sourceLang: 'erlang', sourceText: raw, name: recMatch[1], fields: recMatch[2].split(',').map(f => ({ name: f.trim().split(/\s*=/)[0].trim() })) };
        // -import(module, [fns]).
        const importMatch = src.match(/^-import\((\w+),/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'erlang', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        // Name = value
        const bindMatch = src.match(/^([A-Z]\w*)\s*=\s*(.*?)\.?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'erlang', sourceText: raw, name: bindMatch[1], mutable: false, value: bindMatch[2] };
        return { nkind: 'NormExpr', sourceLang: 'erlang', sourceText: raw, expression: src };
    }

    // ── F# ──────────────────────────────────────────────────────────────
    private normalizeFSharp(src: string, raw: string): NormalizedNode {
        // let name params = body
        const fnMatch = src.match(/^let\s+(?:rec\s+)?(\w+)\s+(.+?)\s*=\s*(.*)/);
        if (fnMatch && fnMatch[2].trim().match(/^\w/)) return { nkind: 'NormFn', sourceLang: 'fsharp', sourceText: raw, name: fnMatch[1], params: fnMatch[2].trim().split(/\s+/), body: fnMatch[3] };
        // let name = value (binding, no params)
        const bindMatch = src.match(/^let\s+(?:mutable\s+)?(\w+)\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'fsharp', sourceText: raw, name: bindMatch[1], mutable: src.includes('mutable'), value: bindMatch[2] };
        // type Name = { fields }
        const typeMatch = src.match(/^type\s+(\w+)\s*=\s*\{?\s*(.*)/);
        if (typeMatch) return { nkind: 'NormStruct', sourceLang: 'fsharp', sourceText: raw, name: typeMatch[1], fields: this.extractFields(typeMatch[2].replace(/\}$/, '')) };
        // module Name
        const modMatch = src.match(/^module\s+([\w.]+)/);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'fsharp', sourceText: raw, name: modMatch[1], imports: [] };
        // open Namespace
        const openMatch = src.match(/^open\s+([\w.]+)/);
        if (openMatch) return { nkind: 'NormModule', sourceLang: 'fsharp', sourceText: raw, name: openMatch[1], imports: [openMatch[1]] };
        // for x in collection do ... done
        const forMatch = src.match(/^for\s+(\w+)\s+in\s+(.+?)\s+do\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'fsharp', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'fsharp', sourceText: raw, expression: src };
    }

    // ── OCaml ───────────────────────────────────────────────────────────
    private normalizeOCaml(src: string, raw: string): NormalizedNode {
        // let name params = body
        const fnMatch = src.match(/^let\s+(?:rec\s+)?(\w+)\s+(.+?)\s*=\s*(.*)/);
        if (fnMatch && fnMatch[2].trim().match(/^\w/)) return { nkind: 'NormFn', sourceLang: 'ocaml', sourceText: raw, name: fnMatch[1], params: fnMatch[2].trim().split(/\s+/), body: fnMatch[3] };
        // let name = value
        const bindMatch = src.match(/^let\s+(\w+)\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'ocaml', sourceText: raw, name: bindMatch[1], mutable: false, value: bindMatch[2] };
        // type name = { fields }
        const typeMatch = src.match(/^type\s+(\w+)\s*=\s*\{?\s*(.*)/);
        if (typeMatch) return { nkind: 'NormStruct', sourceLang: 'ocaml', sourceText: raw, name: typeMatch[1], fields: this.extractFields(typeMatch[2].replace(/\}$/, '')) };
        // module Name = struct ... end
        const modMatch = src.match(/^module\s+(\w+)/);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'ocaml', sourceText: raw, name: modMatch[1], imports: [] };
        // open Module
        const openMatch = src.match(/^open\s+(\w+)/);
        if (openMatch) return { nkind: 'NormModule', sourceLang: 'ocaml', sourceText: raw, name: openMatch[1], imports: [openMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'ocaml', sourceText: raw, expression: src };
    }

    // ── Lisp ────────────────────────────────────────────────────────────
    private normalizeLisp(src: string, raw: string): NormalizedNode {
        // (defun name (params) body)
        const fnMatch = src.match(/^\(defun\s+(\w[\w-]*)\s*\(([^)]*)\)\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'lisp', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(/\s+/).filter(Boolean), body: fnMatch[3].replace(/\)\s*$/, '') || '...' };
        // (defvar name value) / (defparameter name value)
        const defMatch = src.match(/^\((?:defvar|defparameter|defconstant|setf|setq)\s+([\w*-]+)\s+(.*)/);
        if (defMatch) return { nkind: 'NormBind', sourceLang: 'lisp', sourceText: raw, name: defMatch[1], mutable: !src.includes('defconstant'), value: defMatch[2].replace(/\)\s*$/, '') };
        // (defstruct name ...)
        const structMatch = src.match(/^\(defstruct\s+(\w+)\s*(.*)/);
        if (structMatch) return { nkind: 'NormStruct', sourceLang: 'lisp', sourceText: raw, name: structMatch[1], fields: structMatch[2].replace(/\)\s*$/, '').split(/\s+/).filter(Boolean).map(n => ({ name: n })) };
        // (require 'name) / (use-package :name)
        const reqMatch = src.match(/^\((?:require|use-package|load)\s+[':]+(\w[\w-]*)/);
        if (reqMatch) return { nkind: 'NormModule', sourceLang: 'lisp', sourceText: raw, name: reqMatch[1], imports: [reqMatch[1]] };
        // (dolist (x list) body) / (loop for x in list do body)
        const loopMatch = src.match(/^\((?:dolist|loop\s+for)\s+\(?(\w+)(?:\s+in)?\s+(\S+)/);
        if (loopMatch) return { nkind: 'NormLoop', sourceLang: 'lisp', sourceText: raw, variant: 'for', binding: loopMatch[1], iterable: loopMatch[2], body: '...' };
        return { nkind: 'NormExpr', sourceLang: 'lisp', sourceText: raw, expression: src };
    }

    // ── Prolog ──────────────────────────────────────────────────────────
    private normalizeProlog(src: string, raw: string): NormalizedNode {
        // name(Args) :- body.
        const ruleMatch = src.match(/^(\w+)\s*\(([^)]*)\)\s*:-\s*(.*?)\.?\s*$/);
        if (ruleMatch) return { nkind: 'NormFn', sourceLang: 'prolog', sourceText: raw, name: ruleMatch[1], params: ruleMatch[2].split(',').map(p => p.trim()).filter(Boolean), body: ruleMatch[3] };
        // name(Args).  (fact)
        const factMatch = src.match(/^(\w+)\s*\(([^)]*)\)\s*\.?\s*$/);
        if (factMatch) return { nkind: 'NormExpr', sourceLang: 'prolog', sourceText: raw, expression: `fact ${factMatch[1]}(${factMatch[2]})` };
        // :- use_module(library(name)).
        const modMatch = src.match(/^:-\s*(?:use_module|module)\s*\(\s*(?:library\()?([\w/]+)/);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'prolog', sourceText: raw, name: modMatch[1], imports: [modMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'prolog', sourceText: raw, expression: src };
    }

    // ── Solidity ────────────────────────────────────────────────────────
    private normalizeSolidity(src: string, raw: string): NormalizedNode {
        // function name(params) access returns (Type) { body }
        const fnMatch = src.match(/^function\s+(\w+)\s*\(([^)]*)\)\s*(?:(?:public|private|internal|external|view|pure|payable|override|virtual)\s*)*(?:returns\s*\([^)]*\))?\s*\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'solidity', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/\s+/).pop() || '').filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // contract Name { ... }
        const contractMatch = src.match(/^(?:abstract\s+)?contract\s+(\w+)(?:\s+is\s+[^{]+)?\s*\{?\s*(.*)/);
        if (contractMatch) return { nkind: 'NormStruct', sourceLang: 'solidity', sourceText: raw, name: contractMatch[1], fields: this.extractFields(contractMatch[2]) };
        // import "file" / import {Name} from "file"
        const importMatch = src.match(/^import\s+(?:\{[^}]+\}\s+from\s+)?['"](.*?)['"]\s*;/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'solidity', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        // mapping / Type name = value;
        const bindMatch = src.match(/^(?:(?:uint|int|address|bool|string|bytes|mapping)\S*\s+)(?:public\s+|private\s+|internal\s+)?(\w+)\s*(?:=\s*(.*?))?;/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'solidity', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] || '' };
        return { nkind: 'NormExpr', sourceLang: 'solidity', sourceText: raw, expression: src };
    }

    // ── Zig ─────────────────────────────────────────────────────────────
    private normalizeZig(src: string, raw: string): NormalizedNode {
        // fn name(params) RetType { body }
        const fnMatch = src.match(/^(?:pub\s+)?fn\s+(\w+)\s*\(([^)]*)\)\s*(?:\w+)?\s*\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'zig', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // const name = struct { ... }
        const structMatch = src.match(/^(?:pub\s+)?const\s+(\w+)\s*=\s*(?:packed\s+)?struct\s*\{?\s*(.*)/);
        if (structMatch) return { nkind: 'NormStruct', sourceLang: 'zig', sourceText: raw, name: structMatch[1], fields: this.extractFields(structMatch[2]) };
        // const name = @import("name");
        const importMatch = src.match(/^const\s+(\w+)\s*=\s*@import\s*\(\s*"([^"]+)"\s*\)/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'zig', sourceText: raw, name: importMatch[2], imports: [importMatch[1]] };
        // const/var name: Type = value;
        const bindMatch = src.match(/^(?:pub\s+)?(?:const|var)\s+(\w+)\s*(?::\s*\S+)?\s*=\s*(.*?)\s*;?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'zig', sourceText: raw, name: bindMatch[1], mutable: src.includes('var'), value: bindMatch[2] };
        // for (items) |item| { ... }
        const forMatch = src.match(/^for\s*\((.+?)\)\s*\|(\w+)\|\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'zig', sourceText: raw, variant: 'for', binding: forMatch[2], iterable: forMatch[1], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'zig', sourceText: raw, expression: src };
    }

    // ── C ───────────────────────────────────────────────────────────────
    private normalizeC(src: string, raw: string): NormalizedNode {
        // RetType name(params) { body }
        const fnMatch = src.match(/^(?:(?:static|inline|extern|const)\s+)*(\w+(?:\s*\*)?)\s+(\w+)\s*\(([^)]*)\)\s*\{?\s*(.*)/);
        if (fnMatch && !['if', 'for', 'while', 'switch', 'return', 'struct', 'union', 'enum', 'typedef'].includes(fnMatch[2])) return { nkind: 'NormFn', sourceLang: 'c', sourceText: raw, name: fnMatch[2], params: fnMatch[3].split(',').map(p => p.trim().split(/\s+/).pop()?.replace(/^\*/, '') || '').filter(Boolean), body: this.unifyBody(fnMatch[4] || '...') };
        // struct Name { ... }
        const structMatch = src.match(/^(?:typedef\s+)?struct\s+(\w+)?\s*\{?\s*(.*)/);
        if (structMatch) return { nkind: 'NormStruct', sourceLang: 'c', sourceText: raw, name: structMatch[1] || 'anon', fields: this.extractCppFields(structMatch[2]) };
        // #include <...> or #include "..."
        const includeMatch = src.match(/^#include\s+[<"]([^>"]+)[>"]/);
        if (includeMatch) return { nkind: 'NormModule', sourceLang: 'c', sourceText: raw, name: includeMatch[1], imports: [includeMatch[1]] };
        // Type name = value;
        const bindMatch = src.match(/^(?:(?:static|const|extern|volatile|register)\s+)*(\w+(?:\s*\*)?)\s+(\w+)\s*=\s*(.*?)\s*;?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'c', sourceText: raw, name: bindMatch[2], mutable: !src.includes('const'), value: bindMatch[3] };
        // for (int i = 0; i < n; i++) { ... }
        const forMatch = src.match(/^for\s*\(\s*(?:\w+\s+)?(\w+)\s*=\s*.*?;\s*\1\s*[<>=!]+\s*(.*?)\s*;\s*.*?\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'c', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: `0..${forMatch[2]}`, body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'c', sourceText: raw, expression: src };
    }

    // ── Fortran ─────────────────────────────────────────────────────────
    private normalizeFortran(src: string, raw: string): NormalizedNode {
        // subroutine name(params)
        const subMatch = src.match(/^(?:pure\s+|elemental\s+|recursive\s+)*subroutine\s+(\w+)\s*\(([^)]*)\)/i);
        if (subMatch) return { nkind: 'NormFn', sourceLang: 'fortran', sourceText: raw, name: subMatch[1], params: subMatch[2].split(',').map(p => p.trim()).filter(Boolean), body: '...' };
        // function name(params) result(res)
        const fnMatch = src.match(/^(?:(?:integer|real|double|character|logical|complex)\s+)?function\s+(\w+)\s*\(([^)]*)\)/i);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'fortran', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim()).filter(Boolean), body: '...' };
        // module name
        const modMatch = src.match(/^module\s+(\w+)/i);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'fortran', sourceText: raw, name: modMatch[1], imports: [] };
        // use module
        const useMatch = src.match(/^use\s+(\w+)/i);
        if (useMatch) return { nkind: 'NormModule', sourceLang: 'fortran', sourceText: raw, name: useMatch[1], imports: [useMatch[1]] };
        // type :: name
        const typeMatch = src.match(/^type(?:\s*,\s*\w+)*\s*::\s*(\w+)/i);
        if (typeMatch) return { nkind: 'NormStruct', sourceLang: 'fortran', sourceText: raw, name: typeMatch[1], fields: [] };
        // Type :: name = value
        const bindMatch = src.match(/^(?:integer|real|double\s*precision|character|logical|complex)(?:\s*\([^)]*\))?\s*(?:,\s*\w+)*\s*::\s*(\w+)\s*=\s*(.*)/i);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'fortran', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        // do i = start, end
        const doMatch = src.match(/^do\s+(\w+)\s*=\s*(\d+)\s*,\s*(\d+)/i);
        if (doMatch) return { nkind: 'NormLoop', sourceLang: 'fortran', sourceText: raw, variant: 'for', binding: doMatch[1], iterable: `${doMatch[2]}..${doMatch[3]}`, body: '...' };
        return { nkind: 'NormExpr', sourceLang: 'fortran', sourceText: raw, expression: src };
    }

    // ── VHDL ────────────────────────────────────────────────────────────
    private normalizeVHDL(src: string, raw: string): NormalizedNode {
        // entity Name is ... end
        const entityMatch = src.match(/^entity\s+(\w+)\s+is/i);
        if (entityMatch) return { nkind: 'NormStruct', sourceLang: 'vhdl', sourceText: raw, name: entityMatch[1], fields: [] };
        // architecture Name of Entity is
        const archMatch = src.match(/^architecture\s+(\w+)\s+of\s+(\w+)/i);
        if (archMatch) return { nkind: 'NormModule', sourceLang: 'vhdl', sourceText: raw, name: `${archMatch[2]}.${archMatch[1]}`, imports: [] };
        // process(sensitivity_list)
        const procMatch = src.match(/^(\w+)\s*:\s*process\s*\(([^)]*)\)/i);
        if (procMatch) return { nkind: 'NormFn', sourceLang: 'vhdl', sourceText: raw, name: procMatch[1], params: procMatch[2].split(',').map(p => p.trim()).filter(Boolean), body: '...' };
        // signal name : Type;
        const sigMatch = src.match(/^signal\s+(\w+)\s*:\s*(\w+)/i);
        if (sigMatch) return { nkind: 'NormBind', sourceLang: 'vhdl', sourceText: raw, name: sigMatch[1], mutable: true, value: sigMatch[2] };
        // library name; use name.all;
        const libMatch = src.match(/^(?:library|use)\s+([\w.]+)/i);
        if (libMatch) return { nkind: 'NormModule', sourceLang: 'vhdl', sourceText: raw, name: libMatch[1], imports: [libMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'vhdl', sourceText: raw, expression: src };
    }

    // ── MATLAB ──────────────────────────────────────────────────────────
    private normalizeMatlab(src: string, raw: string): NormalizedNode {
        // function [out] = name(params)
        const fnMatch = src.match(/^function\s+(?:\[?[\w,\s]*\]?\s*=\s*)?(\w+)\s*\(([^)]*)\)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'matlab', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim()).filter(Boolean), body: '...' };
        // classdef Name < Base
        const classMatch = src.match(/^classdef\s+(\w+)(?:\s*<\s*\w+)?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'matlab', sourceText: raw, name: classMatch[1], fields: [] };
        // name = value;
        const bindMatch = src.match(/^(\w+)\s*=\s*(.*?)\s*;?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'matlab', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        // for i = start:end ... end
        const forMatch = src.match(/^for\s+(\w+)\s*=\s*(.+)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'matlab', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: '...' };
        return { nkind: 'NormExpr', sourceLang: 'matlab', sourceText: raw, expression: src };
    }

    // ── Pascal/Delphi ───────────────────────────────────────────────────
    private normalizePascal(src: string, raw: string): NormalizedNode {
        // procedure Name(params);
        const procMatch = src.match(/^procedure\s+(\w+)\s*(?:\(([^)]*)\))?\s*;?\s*(.*)/i);
        if (procMatch) return { nkind: 'NormFn', sourceLang: 'pascal', sourceText: raw, name: procMatch[1], params: (procMatch[2] || '').split(/[;,]/).map(p => p.trim().split(/[:\s]+/).pop() || '').filter(Boolean), body: procMatch[3] || '...' };
        // function Name(params): RetType;
        const fnMatch = src.match(/^function\s+(\w+)\s*(?:\(([^)]*)\))?\s*:\s*\w+\s*;?\s*(.*)/i);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'pascal', sourceText: raw, name: fnMatch[1], params: (fnMatch[2] || '').split(/[;,]/).map(p => p.trim().split(/[:\s]+/).pop() || '').filter(Boolean), body: fnMatch[3] || '...' };
        // type Name = class/record ... end
        const typeMatch = src.match(/^type\s+(\w+)\s*=\s*(?:class|record|object)/i);
        if (typeMatch) return { nkind: 'NormStruct', sourceLang: 'pascal', sourceText: raw, name: typeMatch[1], fields: [] };
        // uses Unit1, Unit2;
        const usesMatch = src.match(/^uses\s+([\w,\s]+)\s*;/i);
        if (usesMatch) return { nkind: 'NormModule', sourceLang: 'pascal', sourceText: raw, name: usesMatch[1].split(',')[0].trim(), imports: usesMatch[1].split(',').map(u => u.trim()) };
        // var Name: Type = Value;
        const bindMatch = src.match(/^(?:var|const)\s+(\w+)\s*:\s*\w+\s*=\s*(.*?)\s*;?\s*$/i);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'pascal', sourceText: raw, name: bindMatch[1], mutable: src.match(/^var/i) !== null, value: bindMatch[2] };
        // for i := start to end do
        const forMatch = src.match(/^for\s+(\w+)\s*:=\s*(\d+)\s+(?:to|downto)\s+(\d+)\s+do/i);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'pascal', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: `${forMatch[2]}..${forMatch[3]}`, body: '...' };
        return { nkind: 'NormExpr', sourceLang: 'pascal', sourceText: raw, expression: src };
    }

    // ── Perl ────────────────────────────────────────────────────────────
    private normalizePerl(src: string, raw: string): NormalizedNode {
        // sub name { body }
        const fnMatch = src.match(/^sub\s+(\w+)\s*(?:\(([^)]*)\))?\s*\{?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'perl', sourceText: raw, name: fnMatch[1], params: (fnMatch[2] || '').split(',').map(p => p.trim()).filter(Boolean), body: fnMatch[3] || '...' };
        // package Name;
        const pkgMatch = src.match(/^package\s+([\w:]+)\s*;/);
        if (pkgMatch) return { nkind: 'NormModule', sourceLang: 'perl', sourceText: raw, name: pkgMatch[1], imports: [] };
        // use Module;
        const useMatch = src.match(/^use\s+([\w:]+)/);
        if (useMatch) return { nkind: 'NormModule', sourceLang: 'perl', sourceText: raw, name: useMatch[1], imports: [useMatch[1]] };
        // my $name = value;
        const bindMatch = src.match(/^my\s+([\$@%]\w+)\s*=\s*(.*?)\s*;?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'perl', sourceText: raw, name: bindMatch[1].replace(/^[\$@%]/, ''), mutable: true, value: bindMatch[2] };
        // foreach my $x (@arr) { ... }
        const forMatch = src.match(/^(?:for|foreach)\s+(?:my\s+)?\$(\w+)\s*\((.+?)\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'perl', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'perl', sourceText: raw, expression: src };
    }

    // ── Apex ────────────────────────────────────────────────────────────
    private normalizeApex(src: string, raw: string): NormalizedNode {
        // access RetType name(params) { body }
        const fnMatch = src.match(/^(?:(?:public|private|protected|global|static|virtual|override|testmethod)\s+)*(\w+)\s+(\w+)\s*\(([^)]*)\)\s*\{?\s*(.*)/);
        if (fnMatch && !['class', 'interface', 'enum', 'trigger'].includes(fnMatch[1])) return { nkind: 'NormFn', sourceLang: 'apex', sourceText: raw, name: fnMatch[2], params: fnMatch[3].split(',').map(p => p.trim().split(/\s+/).pop() || '').filter(Boolean), body: this.unifyBody(fnMatch[4] || '...') };
        // public class Name { ... }
        const classMatch = src.match(/^(?:(?:public|private|protected|global|virtual|abstract|with\s+sharing|without\s+sharing)\s+)*class\s+(\w+)(?:\s+(?:extends|implements)\s+[^{]+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'apex', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        return { nkind: 'NormExpr', sourceLang: 'apex', sourceText: raw, expression: src };
    }

    // ── Mojo ────────────────────────────────────────────────────────────
    private normalizeMojo(src: string, raw: string): NormalizedNode {
        // fn name(params) -> RetType: body
        const fnMatch = src.match(/^(?:fn|def)\s+(\w+)\s*\(([^)]*)\)\s*(?:->.*?)?\s*:\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'mojo', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // struct Name: ...
        const structMatch = src.match(/^struct\s+(\w+)(?:\(.*?\))?\s*:\s*(.*)/);
        if (structMatch) return { nkind: 'NormStruct', sourceLang: 'mojo', sourceText: raw, name: structMatch[1], fields: this.extractFields(structMatch[2]) };
        // let/var name: Type = value
        const bindMatch = src.match(/^(?:let|var|alias)\s+(\w+)\s*(?::\s*\S+)?\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'mojo', sourceText: raw, name: bindMatch[1], mutable: src.startsWith('var'), value: bindMatch[2] };
        // from module import name
        const importMatch = src.match(/^(?:from\s+(\w+)\s+)?import\s+(.+)/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'mojo', sourceText: raw, name: importMatch[1] || importMatch[2].trim(), imports: importMatch[2].split(',').map(s => s.trim()) };
        // for x in collection: body
        const forMatch = src.match(/^for\s+(\w+)\s+in\s+(.+?)\s*:\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'mojo', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'mojo', sourceText: raw, expression: src };
    }

    // ── Lua/Luau ────────────────────────────────────────────────────────
    private normalizeLua(src: string, raw: string): NormalizedNode {
        // function name(params) body end
        const fnMatch = src.match(/^(?:local\s+)?function\s+([\w.:]+)\s*\(([^)]*)\)\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'lua', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: fnMatch[3] || '...' };
        // local name = value
        const bindMatch = src.match(/^local\s+(\w+)\s*(?::\s*\S+)?\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'lua', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        // for i = start, end do ... end
        const numForMatch = src.match(/^for\s+(\w+)\s*=\s*(.+?)\s+do\s*(.*)/);
        if (numForMatch) return { nkind: 'NormLoop', sourceLang: 'lua', sourceText: raw, variant: 'for', binding: numForMatch[1], iterable: numForMatch[2], body: numForMatch[3] || '...' };
        // for k, v in pairs/ipairs(tbl) do ... end
        const genFor = src.match(/^for\s+(\w+)(?:\s*,\s*\w+)?\s+in\s+(.+?)\s+do\s*(.*)/);
        if (genFor) return { nkind: 'NormLoop', sourceLang: 'lua', sourceText: raw, variant: 'for', binding: genFor[1], iterable: genFor[2], body: genFor[3] || '...' };
        // require("name")
        const reqMatch = src.match(/^(?:local\s+\w+\s*=\s*)?require\s*\(\s*['"](.*?)['"]\s*\)/);
        if (reqMatch) return { nkind: 'NormModule', sourceLang: 'lua', sourceText: raw, name: reqMatch[1], imports: [reqMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'lua', sourceText: raw, expression: src };
    }

    // ── Groovy ──────────────────────────────────────────────────────────
    private normalizeGroovy(src: string, raw: string): NormalizedNode {
        // def name(params) { body }
        const fnMatch = src.match(/^(?:(?:public|private|protected|static)\s+)*(?:def\s+)?(\w+)\s*\(([^)]*)\)\s*\{?\s*(.*)/);
        if (fnMatch && !['class', 'interface', 'enum', 'if', 'for', 'while', 'switch'].includes(fnMatch[1])) return { nkind: 'NormFn', sourceLang: 'groovy', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/\s+/).pop() || '').filter(Boolean), body: this.unifyBody(fnMatch[3] || '...') };
        // class Name { ... }
        const classMatch = src.match(/^(?:(?:abstract|final)\s+)?class\s+(\w+)(?:\s+(?:extends|implements)\s+[^{]+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'groovy', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // import pkg.Name
        const importMatch = src.match(/^import\s+([\w.]+)/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'groovy', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        // def name = value
        const bindMatch = src.match(/^(?:def|final|(\w+))\s+(\w+)\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'groovy', sourceText: raw, name: bindMatch[2], mutable: !src.startsWith('final'), value: bindMatch[3] };
        // for (x in collection) { ... }
        const forMatch = src.match(/^for\s*\(\s*(?:def\s+)?(\w+)\s+in\s+(.+?)\s*\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'groovy', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'groovy', sourceText: raw, expression: src };
    }

    // ── Elm ─────────────────────────────────────────────────────────────
    private normalizeElm(src: string, raw: string): NormalizedNode {
        // name : Type -> Type (type annotation)
        const sigMatch = src.match(/^(\w+)\s*:\s*(.+)/);
        if (sigMatch) return { nkind: 'NormExpr', sourceLang: 'elm', sourceText: raw, expression: `typesig ${sigMatch[1]} : ${sigMatch[2]}` };
        // name params = body
        const fnMatch = src.match(/^(\w+)\s+([\w\s]+?)\s*=\s*(.*)/);
        if (fnMatch && !['type', 'module', 'import', 'port', 'let', 'if'].includes(fnMatch[1])) return { nkind: 'NormFn', sourceLang: 'elm', sourceText: raw, name: fnMatch[1], params: fnMatch[2].trim().split(/\s+/), body: fnMatch[3] };
        // type alias Name = { ... }
        const typeMatch = src.match(/^type\s+(?:alias\s+)?(\w+)(?:\s+\w+)*\s*=\s*(.*)/);
        if (typeMatch) return { nkind: 'NormStruct', sourceLang: 'elm', sourceText: raw, name: typeMatch[1], fields: [] };
        // module Name exposing (..)
        const modMatch = src.match(/^(?:port\s+)?module\s+([\w.]+)/);
        if (modMatch) return { nkind: 'NormModule', sourceLang: 'elm', sourceText: raw, name: modMatch[1], imports: [] };
        // import Module
        const importMatch = src.match(/^import\s+([\w.]+)/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'elm', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        return { nkind: 'NormExpr', sourceLang: 'elm', sourceText: raw, expression: src };
    }

    // ── Nim ─────────────────────────────────────────────────────────────
    private normalizeNim(src: string, raw: string): NormalizedNode {
        // proc name(params): RetType = body
        const fnMatch = src.match(/^proc\s+(\w+)\s*(?:\*\s*)?\(([^)]*)\)\s*(?::\s*\w+)?\s*=?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'nim', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(/[,;]/).map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: fnMatch[3] || '...' };
        // func name(params): RetType = body
        const funcMatch = src.match(/^func\s+(\w+)\s*(?:\*\s*)?\(([^)]*)\)\s*(?::\s*\w+)?\s*=?\s*(.*)/);
        if (funcMatch) return { nkind: 'NormFn', sourceLang: 'nim', sourceText: raw, name: funcMatch[1], params: funcMatch[2].split(/[,;]/).map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: funcMatch[3] || '...' };
        // type Name = object ... or type Name = ref object ...
        const typeMatch = src.match(/^type\s+(\w+)\s*(?:\*\s*)?=\s*(?:ref\s+)?object\s*(.*)/);
        if (typeMatch) return { nkind: 'NormStruct', sourceLang: 'nim', sourceText: raw, name: typeMatch[1], fields: this.extractFields(typeMatch[2]) };
        // import module
        const importMatch = src.match(/^import\s+(\w+)/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'nim', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        // let/var/const name = value
        const bindMatch = src.match(/^(?:let|var|const)\s+(\w+)\s*(?::\s*\w+)?\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'nim', sourceText: raw, name: bindMatch[1], mutable: src.startsWith('var'), value: bindMatch[2] };
        // for x in collection: body
        const forMatch = src.match(/^for\s+(\w+)\s+in\s+(.+?)\s*:\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'nim', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'nim', sourceText: raw, expression: src };
    }

    // ── Crystal ─────────────────────────────────────────────────────────
    private normalizeCrystal(src: string, raw: string): NormalizedNode {
        // def name(params) : RetType body end
        const fnMatch = src.match(/^def\s+(?:self\.)?(\w+[?!]?)\s*(?:\(([^)]*)\))?\s*(?::\s*\w+)?\s*(.*)/);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'crystal', sourceText: raw, name: fnMatch[1], params: (fnMatch[2] || '').split(',').map(p => p.trim().split(/[:\s]/)[0]).filter(Boolean), body: fnMatch[3] || '...' };
        // class Name < Base ... end
        const classMatch = src.match(/^(?:abstract\s+)?class\s+(\w+)(?:\s*<\s*\w+)?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'crystal', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // struct Name ... end
        const structMatch = src.match(/^struct\s+(\w+)\s*(.*)/);
        if (structMatch) return { nkind: 'NormStruct', sourceLang: 'crystal', sourceText: raw, name: structMatch[1], fields: this.extractFields(structMatch[2]) };
        // require "name"
        const reqMatch = src.match(/^require\s+"([^"]+)"/);
        if (reqMatch) return { nkind: 'NormModule', sourceLang: 'crystal', sourceText: raw, name: reqMatch[1], imports: [reqMatch[1]] };
        // name = value
        const bindMatch = src.match(/^(\w+)\s*=\s*(.*)/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'crystal', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        return { nkind: 'NormExpr', sourceLang: 'crystal', sourceText: raw, expression: src };
    }

    // ── D ───────────────────────────────────────────────────────────────
    private normalizeD(src: string, raw: string): NormalizedNode {
        // RetType name(params) { body }
        const fnMatch = src.match(/^(?:(?:public|private|protected|static|pure|nothrow|@\w+)\s+)*(\w+)\s+(\w+)\s*\(([^)]*)\)\s*\{?\s*(.*)/);
        if (fnMatch && !['class', 'struct', 'interface', 'enum', 'module', 'import', 'if', 'for', 'while'].includes(fnMatch[2])) return { nkind: 'NormFn', sourceLang: 'd', sourceText: raw, name: fnMatch[2], params: fnMatch[3].split(',').map(p => p.trim().split(/\s+/).pop() || '').filter(Boolean), body: this.unifyBody(fnMatch[4] || '...') };
        // class/struct Name { ... }
        const classMatch = src.match(/^(?:class|struct)\s+(\w+)(?:\s*:\s*[^{]+)?\s*\{?\s*(.*)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'd', sourceText: raw, name: classMatch[1], fields: this.extractFields(classMatch[2]) };
        // import std.module;
        const importMatch = src.match(/^import\s+([\w.]+)\s*;/);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'd', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        // auto/Type name = value;
        const bindMatch = src.match(/^(?:auto|immutable|const|(\w+))\s+(\w+)\s*=\s*(.*?)\s*;?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'd', sourceText: raw, name: bindMatch[2], mutable: !src.includes('immutable') && !src.includes('const'), value: bindMatch[3] };
        // foreach (x; collection) { ... }
        const forMatch = src.match(/^foreach\s*\(\s*(\w+)\s*;\s*(.+?)\s*\)\s*\{?\s*(.*)/);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'd', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: forMatch[3] || '...' };
        return { nkind: 'NormExpr', sourceLang: 'd', sourceText: raw, expression: src };
    }

    // ── Visual Basic .NET ───────────────────────────────────────────────
    private normalizeVBNet(src: string, raw: string): NormalizedNode {
        // Sub Name(params) ... End Sub
        const subMatch = src.match(/^(?:(?:Public|Private|Protected|Friend|Shared|Overrides|MustOverride|Overridable)\s+)*Sub\s+(\w+)\s*\(([^)]*)\)\s*(.*)/i);
        if (subMatch) return { nkind: 'NormFn', sourceLang: 'vbnet', sourceText: raw, name: subMatch[1], params: subMatch[2].split(',').map(p => p.trim().split(/\s+As\s+/i)[0].replace(/^ByVal\s+|^ByRef\s+/i, '').trim()).filter(Boolean), body: subMatch[3] || '...' };
        // Function Name(params) As RetType ... End Function
        const fnMatch = src.match(/^(?:(?:Public|Private|Protected|Friend|Shared)\s+)*Function\s+(\w+)\s*\(([^)]*)\)\s*(?:As\s+\w+)?\s*(.*)/i);
        if (fnMatch) return { nkind: 'NormFn', sourceLang: 'vbnet', sourceText: raw, name: fnMatch[1], params: fnMatch[2].split(',').map(p => p.trim().split(/\s+As\s+/i)[0].replace(/^ByVal\s+|^ByRef\s+/i, '').trim()).filter(Boolean), body: fnMatch[3] || '...' };
        // Class Name ... End Class
        const classMatch = src.match(/^(?:(?:Public|Private|Protected|Friend|MustInherit|NotInheritable|Partial)\s+)*Class\s+(\w+)(?:\s+Inherits\s+\w+)?\s*(.*)/i);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'vbnet', sourceText: raw, name: classMatch[1], fields: [] };
        // Imports Namespace
        const importMatch = src.match(/^Imports\s+([\w.]+)/i);
        if (importMatch) return { nkind: 'NormModule', sourceLang: 'vbnet', sourceText: raw, name: importMatch[1], imports: [importMatch[1]] };
        // Dim name As Type = value
        const bindMatch = src.match(/^Dim\s+(\w+)\s*(?:As\s+\w+)?\s*=\s*(.*)/i);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'vbnet', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        // For Each x In collection ... Next
        const forMatch = src.match(/^For\s+Each\s+(\w+)(?:\s+As\s+\w+)?\s+In\s+(.+)/i);
        if (forMatch) return { nkind: 'NormLoop', sourceLang: 'vbnet', sourceText: raw, variant: 'for', binding: forMatch[1], iterable: forMatch[2], body: '...' };
        return { nkind: 'NormExpr', sourceLang: 'vbnet', sourceText: raw, expression: src };
    }

    // ── Smalltalk ───────────────────────────────────────────────────────
    private normalizeSmalltalk(src: string, raw: string): NormalizedNode {
        // ClassName >> methodName  or  methodName: arg1 name2: arg2
        const methMatch = src.match(/^(\w+)\s*>>\s*(\w+[:]?)\s*(.*)/);
        if (methMatch) return { nkind: 'NormFn', sourceLang: 'smalltalk', sourceText: raw, name: `${methMatch[1]}>>${methMatch[2]}`, params: methMatch[3].split(/\s+/).filter(Boolean), body: '...' };
        // #name or name :=
        const bindMatch = src.match(/^(\w+)\s*:=\s*(.*?)\.?\s*$/);
        if (bindMatch) return { nkind: 'NormBind', sourceLang: 'smalltalk', sourceText: raw, name: bindMatch[1], mutable: true, value: bindMatch[2] };
        // Object subclass: #Name
        const classMatch = src.match(/^(\w+)\s+subclass:\s*#(\w+)/);
        if (classMatch) return { nkind: 'NormStruct', sourceLang: 'smalltalk', sourceText: raw, name: classMatch[2], fields: [] };
        return { nkind: 'NormExpr', sourceLang: 'smalltalk', sourceText: raw, expression: src };
    }

    // ── Go field extraction helper ──────────────────────────────────
    private extractGoFields(body: string): Array<{ name: string; type?: string }> {
        if (!body) return [];
        return body.split(/[;\n]/).map(f => f.trim()).filter(Boolean).map(f => {
            const parts = f.split(/\s+/).filter(Boolean);
            return parts.length >= 2
                ? { name: parts[0], type: parts.slice(1).join(' ') }
                : { name: parts[0] };
        });
    }

    // ── SQL field extraction helper ──────────────────────────────────
    private extractSQLFields(body: string): Array<{ name: string; type?: string }> {
        if (!body) return [];
        return body.replace(/\)\s*;?\s*$/, '').split(',').map(f => f.trim()).filter(Boolean).map(f => {
            const parts = f.split(/\s+/).filter(Boolean);
            if (parts.length >= 2 && !['PRIMARY', 'FOREIGN', 'UNIQUE', 'CHECK', 'CONSTRAINT', 'INDEX'].includes(parts[0].toUpperCase())) {
                return { name: parts[0], type: parts[1] };
            }
            return { name: parts[0] };
        }).filter(f => !['PRIMARY', 'FOREIGN', 'UNIQUE', 'CHECK', 'CONSTRAINT', 'INDEX'].includes(f.name.toUpperCase()));
    }

    // ── Field extraction helpers ──────────────────────────────────────

    /** Extract fields from a generic body (comma/semicolon separated name: type or name). */
    private extractFields(body: string): Array<{ name: string; type?: string }> {
        if (!body || body === '...' || body === 'pass') return [];
        return body.split(/[,;]/).map(f => f.trim()).filter(Boolean).map(f => {
            const parts = f.split(/[:\s]+/).filter(Boolean);
            return parts.length >= 2
                ? { name: parts[0], type: parts[1] }
                : { name: parts[0] };
        });
    }

    /** Extract fields from C++ struct body: `type name;` patterns. */
    private extractCppFields(body: string): Array<{ name: string; type?: string }> {
        if (!body) return [];
        return body.split(';').map(f => f.trim()).filter(Boolean).map(f => {
            const parts = f.split(/\s+/).filter(Boolean);
            return parts.length >= 2
                ? { name: parts[parts.length - 1], type: parts.slice(0, -1).join(' ') }
                : { name: parts[0] };
        });
    }

    /** Extract fields from Rust struct body: `name: Type` patterns. */
    private extractRustFields(body: string): Array<{ name: string; type?: string }> {
        if (!body) return [];
        return body.split(',').map(f => f.trim()).filter(Boolean).map(f => {
            const parts = f.split(':').map(p => p.trim());
            return parts.length >= 2
                ? { name: parts[0], type: parts[1] }
                : { name: parts[0] };
        });
    }

    // ── Unified output ──────────────────────────────────────────────────

    /**
     * Unify a body string — normalize language-specific print/log calls
     * into Grey++ universal `print(...)` form, strip trailing semicolons.
     */
    private unifyBody(body: string): string {
        let s = body.trim();
        // console.log(...) → print(...)
        s = s.replace(/console\.log\s*\(/g, 'print(');
        // std::cout << "arg" → print("arg")  |  std::cout << arg → print(arg)
        s = s.replace(/std::cout\s*<<\s*("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|\S+)\s*(?:<<\s*std::endl)?\s*;?/g,
            (_, arg) => `print(${arg.trim()})`);
        // Strip trailing semicolons
        s = s.replace(/;\s*$/, '');
        return s;
    }

    /** Convert a NormalizedNode to Grey++ unified syntax string. */
    toUnified(node: NormalizedNode): string {
        switch (node.nkind) {
            case 'NormBind':
                return `bind ${node.name} = ${node.value}`;
            case 'NormFn':
                return `fn ${node.name ?? ''}(${node.params.join(', ')}) { ${node.body} }`;
            case 'NormLoop':
                if (node.variant === 'for') return `loop ${node.binding ?? '_'} in ${node.iterable ?? '...'} { ${node.body} }`;
                if (node.variant === 'while') return `loop while ${node.condition ?? 'true'} { ${node.body} }`;
                return `loop { ${node.body} }`;
            case 'NormCond':
                return `cond ${node.condition} { ${node.thenBranch} }${node.elseBranch ? ` else { ${node.elseBranch} }` : ''}`;
            case 'NormStruct': {
                const fieldStr = node.fields.map(f => f.type ? `${f.name}: ${f.type}` : f.name).join(', ');
                return `struct ${node.name} { ${fieldStr} }`;
            }
            case 'NormModule':
                return `module ${node.name} { ${node.imports.join(', ')} }`;
            case 'NormExpr':
                return node.expression;
            case 'NormBlock':
                return node.children.map(c => this.toUnified(c)).join('\n');
        }
    }
}
