// ─── Grey++ Runtime Kernel ───────────────────────────────────────────────────
// Modular, single-file runtime that executes every foundation primitive in
// the Grey++ AST.  Each node kind is handled by an isolated private method —
// no shared mutable state between executors, no background services, no
// indexing.
//
// Extend by adding a `private exec<Kind>` method and a case in `exec()`.

import type {
    ASTNode,
    FunctionNode,
    QueryNode,
    InferNode,
    SysNode,
    ModuleNode,
    StorageNode,
    NetNode,
    PipelineNode,
    PipelineStepNode,
    SecNode,
    DistNode,
    DocNode,
    UniNode,
    ReflectNode,
    ArtifactNode,
    AutoNode,
    OrchestrationNode,
    SelfOptNode,
    MemoryNode,
    NarrativeNode,
    UnifyNode,
    CompilerNode,
    ExecMultiNode,
    InteropNode,
    ProgramNode,
    CallExprNode,
    BinaryExprNode,
    ReturnStmtNode,
    IdentifierNode,
    NumberLitNode,
    StringLitNode,
    BoolLitNode,
    ArrayLitNode,
    ObjectLitNode,
    ParamDecl,
} from './grey_ast.js';
import { ReactModule } from './grey_react_module.js';
import { dispatchSys, sysLog, sysThread, sysMessage } from './grey_sys.js';
import { dispatchNet } from './grey_net.js';
import { Coordinator } from './grey_coordination.js';
import { StorageCoordinator } from './grey_storage.js';
import { ProtocolCoordinator } from './grey_protocol.js';
import { dispatchPipelineStep } from './grey_pipeline.js';
import type { PipelineStepResult, PipelineResult } from './grey_pipeline.js';
import { PipelineCoordinator } from './grey_orchestration.js';
import { dispatchSec } from './grey_security.js';
import { TrustCoordinator } from './grey_trust.js';
import { dispatchDist } from './grey_distributed.js';
import { CoordinationManager } from './grey_coordination.js';
import { dispatchDoc } from './grey_doc.js';
import { GreyPDFCoordinator } from './grey_pdf.js';
import { SemanticLayer } from './grey_semantic.js';
import { consolidateUniResults } from './grey_unified.js';
import { LanguageCoordinator } from './grey_language.js';
import { GrammarLayer } from './grey_grammar.js';
import { dispatchReflect } from './grey_reflect.js';
import { MetaCoordinator } from './grey_meta.js';
import { OptimizationLayer } from './grey_opt.js';
import { dispatchArtifact } from './grey_artifact.js';
import { ArtifactCoordinator } from './grey_density.js';
import { RarityLayer } from './grey_rarity.js';
import { dispatchAuto } from './grey_auto.js';
import { TrustBoundaryCoordinator } from './grey_trustbound.js';
import { PolicyLayer } from './grey_policy.js';
import { dispatchOrchestration } from './grey_orchestration.js';
import { FabricCoordinator } from './grey_fabric.js';
import { ExecutionFabric } from './grey_execution.js';
import { dispatchSelfOpt } from './grey_selfopt.js';
import { AdaptCoordinator } from './grey_adapt.js';
import { AdaptiveFabric } from './grey_adaptive.js';
import { dispatchMemory } from './grey_memory.js';
import { PersistCoordinator } from './grey_persist.js';
import { PersistenceLayer } from './grey_persistence.js';
import { dispatchNarrative } from './grey_narrative.js';
import { StoryFabric } from './grey_story.js';
import { dispatchUnify } from './grey_unify.js';
import { UnifyCoordinator } from './grey_unifycoord.js';
import { UnifiedFabric } from './grey_unified.js';
import { dispatchCompiler } from './grey_compiler.js';
import { CompileCoordinator } from './grey_compilecoord.js';
import { BackendFabric } from './grey_backend.js';
import { dispatchExecMulti } from './grey_execmulti.js';
import { MultiExecCoordinator } from './grey_multiexec.js';
import { BackendRunner } from './grey_runner.js';
import { dispatchInterop } from './grey_interop.js';
import { InteropCoordinator } from './grey_interopcoord.js';
import { InteropFabric } from './grey_interfabric.js';
import { Normalizer } from './grey_ast.js';
import type { NormalizedNode } from './grey_ast.js';
import { Translation } from './grey_translate.js';
import type { TranslationOutput } from './grey_translate.js';
import { detectLanguage, detectConstruct } from './grey_grammar.js';

// Re-use the string values directly since const enums are erased at runtime.
const NK = {
    Function: 'Function',
    Query: 'Query',
    Infer: 'Infer',
    Sys: 'Sys',
    Module: 'Module',
    Storage: 'Storage',
    Net: 'Net',
    Pipeline: 'Pipeline',
    PipelineStep: 'PipelineStep',
    Sec: 'Sec',
    Dist: 'Dist',
    Doc: 'Doc',
    Uni: 'Uni',
    Reflect: 'Reflect',
    Artifact: 'Artifact',
    Auto: 'Auto',
    Orchestration: 'Orchestration',
    SelfOpt: 'SelfOpt',
    Memory: 'Memory',
    Narrative: 'Narrative',
    Unify: 'Unify',
    Compiler: 'Compiler',
    ExecMulti: 'ExecMulti',
    Interop: 'Interop',
    Identifier: 'Identifier',
    NumberLit: 'NumberLit',
    StringLit: 'StringLit',
    BoolLit: 'BoolLit',
    BinaryExpr: 'BinaryExpr',
    CallExpr: 'CallExpr',
    ArrayLit: 'ArrayLit',
    ObjectLit: 'ObjectLit',
    ReturnStmt: 'ReturnStmt',
    Program: 'Program',
} as const;

// ─── Return Sentinel ────────────────────────────────────────────────────────
// Used to propagate `return` through the call stack without exceptions.

const RETURN_SYM = Symbol('RETURN');

interface ReturnSignal {
    [RETURN_SYM]: true;
    value: unknown;
}

function returnSignal(value: unknown): ReturnSignal {
    return { [RETURN_SYM]: true, value };
}

function isReturn(v: unknown): v is ReturnSignal {
    return v != null && typeof v === 'object' && (v as any)[RETURN_SYM] === true;
}

// ─── Scope ──────────────────────────────────────────────────────────────────
// Lightweight lexical scope chain.  Each function call gets its own child
// scope so variables never leak across invocations.

class Scope {
    private bindings = new Map<string, unknown>();
    constructor(private parent: Scope | null = null) { }

    define(name: string, value: unknown): void {
        this.bindings.set(name, value);
    }

    get(name: string): unknown {
        if (this.bindings.has(name)) return this.bindings.get(name);
        if (this.parent) return this.parent.get(name);
        return undefined;
    }

    has(name: string): boolean {
        if (this.bindings.has(name)) return true;
        return this.parent?.has(name) ?? false;
    }

    child(): Scope {
        return new Scope(this);
    }
}

// ═══════════════════════════════════════════════════════════════════════════
//  GreyRuntime
// ═══════════════════════════════════════════════════════════════════════════

export class GreyRuntime {
    /** Root scope — built-in globals live here. */
    private rootScope: Scope;

    /** Distributed coordination — in-memory stub. */
    private coordinator: Coordinator;

    /** Storage coordination — routes pdf/db/file operations. */
    private storageCoordinator: StorageCoordinator;

    /** Protocol coordination — routes http/socket/rpc operations. */
    private protocolCoordinator: ProtocolCoordinator;

    /** Pipeline coordination — registers and executes pipelines. */
    private pipelineCoordinator: PipelineCoordinator;

    /** Trust coordination — policy enforcement and audit trail. */
    private trustCoordinator: TrustCoordinator;

    /** Distributed coordination — node registration, consensus, barriers. */
    private coordinationManager: CoordinationManager;

    /** Document coordination — document registration, edit tracking, render pipeline. */
    private pdfCoordinator: GreyPDFCoordinator;

    /** Semantic layer — document structure representation and transforms. */
    private semanticLayer: SemanticLayer;

    /** Language coordinator — unified syntax rule registration and translation. */
    private languageCoordinator: LanguageCoordinator;

    /** Grammar layer — unified AST parsing and semantic collapsing. */
    private grammarLayer: GrammarLayer;

    /** Meta coordinator — collects introspection results from all subsystems. */
    private metaCoordinator: MetaCoordinator;

    /** Optimization layer — parses meta-reports, outputs optimization plans. */
    private optimizationLayer: OptimizationLayer;

    /** Artifact coordinator — tracks artifact outputs and calculates density. */
    private artifactCoordinator: ArtifactCoordinator;

    /** Rarity layer — assigns rarity bands and generates scores. */
    private rarityLayer: RarityLayer;

    /** Trust boundary coordinator — classifies and routes commands. */
    private trustBoundaryCoordinator: TrustBoundaryCoordinator;

    /** Policy layer — evaluates commands against allow/deny/sandbox rules. */
    private policyLayer: PolicyLayer;

    /** Fabric coordinator — registers orchestration pipelines and manages dependencies. */
    private fabricCoordinator: FabricCoordinator;

    /** Execution fabric — unified execution graph and scheduling. */
    private executionFabric: ExecutionFabric;

    /** Adapt coordinator — collects meta-reports and applies adaptive refinements. */
    private adaptCoordinator: AdaptCoordinator;

    /** Adaptive fabric — builds adaptive graphs and produces optimized plans. */
    private adaptiveFabric: AdaptiveFabric;

    /** Persist coordinator — registers and recalls semantic memories. */
    private persistCoordinator: PersistCoordinator;

    /** Persistence layer — persistent graph and consolidated memory state. */
    private persistenceLayer: PersistenceLayer;

    /** Story fabric — narrative graph and recruiter-ready fragment generation. */
    private storyFabric: StoryFabric;

    /** Unify coordinator — maps primitives into universal syntax. */
    private unifyCoordinator: UnifyCoordinator;

    /** Unified fabric — meta-language graph and execution plan generation. */
    private unifiedFabric: UnifiedFabric;

    /** Compile coordinator — maps syntax to IR and manages backend translations. */
    private compileCoordinator: CompileCoordinator;

    /** Backend fabric — compilation target graph and code fragment generation. */
    private backendFabric: BackendFabric;

    /** Multi-exec coordinator — maps backends and dispatches execution. */
    private multiExecCoordinator: MultiExecCoordinator;

    /** Backend runner — execution contexts and stub execution results. */
    private backendRunner: BackendRunner;

    /** Interop coordinator — registers bridges and translation rules. */
    private interopCoordinator: InteropCoordinator;

    /** Interop fabric — cross-backend execution graph and plan generation. */
    private interopFabric: InteropFabric;

    /** Normalizer — converts multi-language source into normalized AST (Stage 18.5). */
    private normalizer: Normalizer;

    /** Translation — converts normalized AST back to target-language code (Stage 18.5). */
    private translation: Translation;

    constructor() {
        this.rootScope = new Scope();
        this.coordinator = new Coordinator();
        this.storageCoordinator = new StorageCoordinator();
        this.protocolCoordinator = new ProtocolCoordinator();
        this.pipelineCoordinator = new PipelineCoordinator();
        this.trustCoordinator = new TrustCoordinator();
        this.coordinationManager = new CoordinationManager();
        this.pdfCoordinator = new GreyPDFCoordinator();
        this.semanticLayer = new SemanticLayer();
        this.languageCoordinator = new LanguageCoordinator();
        this.grammarLayer = new GrammarLayer(this.languageCoordinator);
        this.metaCoordinator = new MetaCoordinator();
        this.optimizationLayer = new OptimizationLayer();
        this.artifactCoordinator = new ArtifactCoordinator();
        this.rarityLayer = new RarityLayer();
        this.trustBoundaryCoordinator = new TrustBoundaryCoordinator();
        this.policyLayer = new PolicyLayer();
        this.fabricCoordinator = new FabricCoordinator();
        this.executionFabric = new ExecutionFabric();
        this.adaptCoordinator = new AdaptCoordinator();
        this.adaptiveFabric = new AdaptiveFabric();
        this.persistCoordinator = new PersistCoordinator();
        this.persistenceLayer = new PersistenceLayer();
        this.storyFabric = new StoryFabric();
        this.unifyCoordinator = new UnifyCoordinator();
        this.unifiedFabric = new UnifiedFabric();
        this.compileCoordinator = new CompileCoordinator();
        this.backendFabric = new BackendFabric();
        this.multiExecCoordinator = new MultiExecCoordinator();
        this.backendRunner = new BackendRunner();
        this.interopCoordinator = new InteropCoordinator();
        this.interopFabric = new InteropFabric();
        this.normalizer = new Normalizer();
        this.translation = new Translation();
        this.seedBuiltins();
        this.seedNetProtocols();
    }

    // ── Public entry point ────────────────────────────────────────────────

    /** Execute an AST node tree starting from the root scope. */
    run(node: ASTNode): unknown {
        return this.exec(node, this.rootScope);
    }

    /** Register a native function that Grey++ code can call. */
    defineGlobal(name: string, value: unknown): void {
        this.rootScope.define(name, value);
    }

    // ── Central dispatcher ────────────────────────────────────────────────
    // Each case delegates to an isolated private method.

    private exec(node: ASTNode, scope: Scope): unknown {
        switch (node.kind) {
            // Foundation primitives
            case NK.Function: return this.execFunction(node as FunctionNode, scope);
            case NK.Query: return this.execQuery(node as QueryNode, scope);
            case NK.Infer: return this.execInfer(node as InferNode, scope);
            case NK.Sys: return this.execSys(node as SysNode, scope);
            case NK.Module: return this.execModule(node as ModuleNode, scope);
            case NK.Storage: return this.execStorage(node as StorageNode, scope);
            case NK.Net: return this.execNet(node as NetNode, scope);
            case NK.Pipeline: return this.execPipeline(node as PipelineNode, scope);
            case NK.Sec: return this.execSec(node as SecNode, scope);
            case NK.Dist: return this.execDist(node as DistNode, scope);
            case NK.Doc: return this.execDoc(node as DocNode, scope);
            case NK.Uni: return this.execUni(node as UniNode, scope);
            case NK.Reflect: return this.execReflect(node as ReflectNode, scope);
            case NK.Artifact: return this.execArtifact(node as ArtifactNode, scope);
            case NK.Auto: return this.execAuto(node as AutoNode, scope);
            case NK.Orchestration: return this.execOrch(node as OrchestrationNode, scope);
            case NK.SelfOpt: return this.execSelfOpt(node as SelfOptNode, scope);
            case NK.Memory: return this.execMemory(node as MemoryNode, scope);
            case NK.Narrative: return this.execNarrative(node as NarrativeNode, scope);
            case NK.Unify: return this.execUnify(node as UnifyNode, scope);
            case NK.Compiler: return this.execCompiler(node as CompilerNode, scope);
            case NK.ExecMulti: return this.execExecMulti(node as ExecMultiNode, scope);
            case NK.Interop: return this.execInterop(node as InteropNode, scope);

            // Expressions
            case NK.Identifier: return this.execIdentifier(node as IdentifierNode, scope);
            case NK.NumberLit: return (node as NumberLitNode).value;
            case NK.StringLit: return (node as StringLitNode).value;
            case NK.BoolLit: return (node as BoolLitNode).value;
            case NK.BinaryExpr: return this.execBinaryExpr(node as BinaryExprNode, scope);
            case NK.CallExpr: return this.execCallExpr(node as CallExprNode, scope);
            case NK.ArrayLit: return this.execArrayLit(node as ArrayLitNode, scope);
            case NK.ObjectLit: return this.execObjectLit(node as ObjectLitNode, scope);

            // Statements
            case NK.ReturnStmt: return this.execReturn(node as ReturnStmtNode, scope);

            // Top-level
            case NK.Program: return this.execProgram(node as ProgramNode, scope);

            default:
                throw new Error(`GreyRuntime: no executor for node kind "${node.kind}"`);
        }
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — FunctionNode
    // ═════════════════════════════════════════════════════════════════════

    /**
     * Registers a named function in the current scope, or returns a callable
     * closure for anonymous lambdas.  Each invocation creates an isolated
     * child scope so parameters and locals never leak.
     */
    private execFunction(node: FunctionNode, scope: Scope): unknown {
        const closure = (...args: unknown[]): unknown => {
            const callScope = scope.child();

            // Bind parameters — fall back to declared defaults.
            node.params.forEach((p: ParamDecl, i: number) => {
                const val = i < args.length ? args[i] : this.execDefault(p, scope);
                callScope.define(p.name, val);
            });

            // Execute body statements.
            let result: unknown;
            for (const stmt of node.body) {
                result = this.exec(stmt, callScope);
                if (isReturn(result)) return result.value;
            }
            return result;
        };

        if (node.name) {
            scope.define(node.name, closure);
            return undefined; // named declaration — no value produced
        }
        return closure; // anonymous — return the callable
    }

    /** Resolve a parameter's default value (if any). */
    private execDefault(p: ParamDecl, scope: Scope): unknown {
        return p.defaultValue ? this.exec(p.defaultValue, scope) : undefined;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — QueryNode
    // ═════════════════════════════════════════════════════════════════════

    /**
     * Stub SQL-style query executor.  Returns mock rows that echo the query
     * structure so the pipeline can be validated end-to-end.  Replace this
     * method body with a real datasource adapter in Stage 2.
     */
    private execQuery(node: QueryNode, _scope: Scope): unknown {
        const columns = node.select.columns;
        const table = node.from?.table ?? '<inline>';
        const alias = node.from?.alias;
        const hasWhere = node.where != null;

        // Build a human-readable summary.
        const summary = `SELECT ${columns.join(', ')} FROM ${table}${alias ? ` AS ${alias}` : ''}${hasWhere ? ' WHERE <condition>' : ''}`;

        // Return mock result set.
        return {
            _type: 'QueryResult',
            sql: summary,
            rows: [
                Object.fromEntries(columns.map(c => [c === '*' ? '_all' : c, `<mock:${table}.${c}>`])),
            ],
            meta: { stub: true },
        };
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — InferNode
    // ═════════════════════════════════════════════════════════════════════

    /**
     * Stub AI inference executor.  Evaluates the input expression, resolves
     * options, and returns a structured result with a placeholder prediction.
     * No external model integration — swap in a real client in Stage 2.
     */
    private execInfer(node: InferNode, scope: Scope): unknown {
        const input = this.exec(node.input, scope);

        // Resolve option values (if any).
        const resolvedOptions: Record<string, unknown> = {};
        if (node.options) {
            for (const [key, expr] of Object.entries(node.options)) {
                resolvedOptions[key] = this.exec(expr, scope);
            }
        }

        return {
            _type: 'InferResult',
            model: node.model,
            input,
            options: resolvedOptions,
            prediction: 'AI inference result',
            meta: { stub: true },
        };
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — SysNode
    // ═════════════════════════════════════════════════════════════════════

    // ─── runSys — public entry for SysNode execution ────────────────────

    /**
     * Execute a SysNode.  Resolves arguments and flags in the current scope,
     * then dispatches to an isolated private method per primitive.
     */
    runSys(node: SysNode): unknown {
        return this.execSys(node, this.rootScope);
    }

    /**
     * Internal SysNode dispatcher.  Each op is handled by its own private
     * method so primitives stay modular and testable in isolation.
     */
    private execSys(node: SysNode, scope: Scope): unknown {
        const args = node.args.map(a => this.exec(a, scope));

        // Resolve flags.
        const flags: Record<string, unknown> = {};
        if (node.flags) {
            for (const [k, v] of Object.entries(node.flags)) {
                flags[k] = this.exec(v, scope);
            }
        }

        switch (node.op) {
            case 'log': return this.execSysLog(args);
            case 'thread': return this.execSysThread(args, flags);
            case 'message': return this.execSysMessage(args, flags);
            default: return dispatchSys(node.op, args, flags);
        }
    }

    // ── sys.log  —  delegate to grey_sys executor ───────────────────────
    private execSysLog(args: unknown[]): unknown {
        return sysLog(args);
    }

    // ── sys.thread  —  register in coordinator, then stub ───────────────
    private execSysThread(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = sysThread(args, flags);
        // Auto-register the thread in the coordinator so it's trackable.
        this.coordinator.register(result.threadId as string, (flags['label'] as string) ?? undefined);
        this.coordinator.setState(result.threadId as string, 'running');
        return result;
    }

    // ── sys.message  —  route through coordinator ───────────────────────
    private execSysMessage(args: unknown[], flags: Record<string, unknown>): unknown {
        const target = (flags['target'] as string) ?? String(args[0] ?? 'unknown');
        const payload = (flags['payload'] as unknown) ?? args[1] ?? null;
        const from = (flags['from'] as string) ?? 'repl';

        // Record the message in the coordinator.
        this.coordinator.send(from, target, payload);

        return sysMessage(args, flags);
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — StorageNode
    // ═════════════════════════════════════════════════════════════════════

    /**
     * Public entry point for storage operations.
     */
    runStorage(node: StorageNode): unknown {
        return this.execStorage(node, this.rootScope);
    }

    /**
     * Internal StorageNode dispatcher.  Resolves arguments and flags,
     * then routes through the StorageCoordinator.
     */
    private execStorage(node: StorageNode, scope: Scope): unknown {
        const args = node.args.map(a => this.exec(a, scope));

        const flags: Record<string, unknown> = {};
        if (node.flags) {
            for (const [k, v] of Object.entries(node.flags)) {
                flags[k] = this.exec(v, scope);
            }
        }

        switch (node.backend) {
            case 'pdf': return this.execStoragePdf(node.op, args, flags);
            case 'db': return this.execStorageDb(node.op, args, flags);
            case 'file': return this.execStorageFile(node.op, args, flags);
            default: return this.storageCoordinator.dispatch(node.backend, node.op, args, flags);
        }
    }

    // ── storage pdf ─────────────────────────────────────────────────────
    private execStoragePdf(op: string, args: unknown[], flags: Record<string, unknown>): unknown {
        return this.storageCoordinator.dispatch('pdf', op as any, args, flags);
    }

    // ── storage db  (stub) ──────────────────────────────────────────────
    private execStorageDb(op: string, args: unknown[], flags: Record<string, unknown>): unknown {
        return this.storageCoordinator.dispatch('db', op as any, args, flags);
    }

    // ── storage file (stub) ─────────────────────────────────────────────
    private execStorageFile(op: string, args: unknown[], flags: Record<string, unknown>): unknown {
        return this.storageCoordinator.dispatch('file', op as any, args, flags);
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — ModuleNode
    // ═════════════════════════════════════════════════════════════════════

    /**
     * Framework-adapter executor.  Each framework target is handled by its
     * own isolated adapter — no shared state between adapters, no background
     * services.
     *
     *   • "react"  → delegates to ReactModule for VNode / HTML / JSX output.
     *   • others   → returns a placeholder ModuleDescriptor.
     */
    private execModule(node: ModuleNode, scope: Scope): unknown {
        // ── React adapter ────────────────────────────────────────────────
        if (node.framework === 'react') {
            return this.execReactModule(node);
        }

        // ── Generic / placeholder adapter ────────────────────────────────
        return this.execGenericModule(node, scope);
    }

    /**
     * React-specific module execution.  Runs entirely inside the
     * ReactModule adapter — the runtime never touches React internals.
     */
    private execReactModule(node: ModuleNode): unknown {
        const adapter = new ReactModule(node);
        const vdom = adapter.render();
        const html = adapter.toHTML();
        const jsx = adapter.toComponentSource();

        return {
            _type: 'ReactModuleResult',
            name: node.name,
            framework: 'react',
            vdom,
            html,
            jsx,
        };
    }

    /**
     * Generic module executor for frameworks without a dedicated adapter.
     * Evaluates exports in an isolated child scope and returns a descriptor
     * with placeholder output.
     */
    private execGenericModule(node: ModuleNode, scope: Scope): unknown {
        const moduleScope = scope.child();

        const exportedValues: Record<string, unknown> = {};
        for (const exp of node.exports) {
            this.exec(exp, moduleScope);
            if (exp.kind === NK.Function && (exp as FunctionNode).name) {
                exportedValues[(exp as FunctionNode).name!] = moduleScope.get((exp as FunctionNode).name!);
            }
        }

        return {
            _type: 'ModuleDescriptor',
            name: node.name,
            framework: node.framework ?? null,
            imports: node.imports ?? [],
            exports: exportedValues,
            meta: { stub: true, note: `No dedicated adapter for "${node.framework ?? 'unknown'}" — placeholder output.` },
        };
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — NetNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runNet — public entry for NetNode execution ─────────────────────

    /**
     * Execute a NetNode.  Resolves arguments and flags in the current scope,
     * then dispatches to an isolated private method per primitive.
     */
    runNet(node: NetNode): unknown {
        return this.execNet(node, this.rootScope);
    }

    /**
     * Internal NetNode dispatcher.  Each primitive is handled by its own
     * private method so primitives stay modular and testable in isolation.
     */
    private execNet(node: NetNode, scope: Scope): unknown {
        const args = node.args.map(a => this.exec(a, scope));

        const flags: Record<string, unknown> = {};
        if (node.flags) {
            for (const [k, v] of Object.entries(node.flags)) {
                flags[k] = this.exec(v, scope);
            }
        }

        switch (node.primitive) {
            case 'http': return this.execNetHttp(node.op, args, flags);
            case 'socket': return this.execNetSocket(node.op, args, flags);
            case 'rpc': return this.execNetRpc(node.op, args, flags);
            default: return dispatchNet(node.primitive, node.op, args, flags);
        }
    }

    // ── net.http ─────────────────────────────────────────────────────────
    private execNetHttp(op: string, args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchNet('http', op, args, flags);
        // Route through the protocol coordinator for tracking.
        this.protocolCoordinator.route('http', op, args, flags);
        return result;
    }

    // ── net.socket ───────────────────────────────────────────────────────
    private execNetSocket(op: string, args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchNet('socket', op, args, flags);
        // Track connection state.
        if (op === 'connect') {
            const host = String(flags['host'] ?? args[0] ?? 'localhost');
            const port = String(flags['port'] ?? args[1] ?? '0');
            this.protocolCoordinator.openConnection('socket', `${host}:${port}`);
        }
        this.protocolCoordinator.route('socket', op, args, flags);
        return result;
    }

    // ── net.rpc ──────────────────────────────────────────────────────────
    private execNetRpc(op: string, args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchNet('rpc', op, args, flags);
        this.protocolCoordinator.route('rpc', op, args, flags);
        return result;
    }

    // ── Seed default protocol handlers ──────────────────────────────────
    private seedNetProtocols(): void {
        // Register pass-through handlers so the ProtocolCoordinator can
        // route without error.  Real adapters replace these in later stages.
        this.protocolCoordinator.registerHandler('http', (op, args, flags) =>
            dispatchNet('http', op, args, flags));
        this.protocolCoordinator.registerHandler('socket', (op, args, flags) =>
            dispatchNet('socket', op, args, flags));
        this.protocolCoordinator.registerHandler('rpc', (op, args, flags) =>
            dispatchNet('rpc', op, args, flags));
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — PipelineNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runPipeline — public entry for PipelineNode execution ───────────

    /**
     * Execute a PipelineNode.  Resolves each step's arguments and flags,
     * then runs them sequentially, threading output → input between steps.
     * Returns a consolidated PipelineResult.
     */
    runPipeline(node: PipelineNode): unknown {
        return this.execPipeline(node, this.rootScope);
    }

    /**
     * Internal PipelineNode executor.  Each step is dispatched to its own
     * private method based on stepKind.
     */
    private execPipeline(node: PipelineNode, scope: Scope): unknown {
        const stepDefs = node.steps.map(step => {
            const args = step.args.map(a => this.exec(a, scope));
            const flags: Record<string, unknown> = {};
            if (step.flags) {
                for (const [k, v] of Object.entries(step.flags)) {
                    flags[k] = this.exec(v, scope);
                }
            }
            return {
                stepKind: step.stepKind,
                args,
                flags,
                primitive: step.primitive,
                op: step.op,
            };
        });

        const stepResults: PipelineStepResult[] = [];
        let prevOutput: unknown = null;

        for (let i = 0; i < stepDefs.length; i++) {
            const def = stepDefs[i];
            let result: PipelineStepResult;

            switch (def.stepKind) {
                case 'infer':
                    result = this.execPipelineInfer(i, def.args, def.flags, prevOutput);
                    break;
                case 'store':
                    result = this.execPipelineStore(i, def.args, def.flags, prevOutput);
                    break;
                case 'net':
                    result = this.execPipelineNet(i, def.args, def.flags, prevOutput, def.primitive, def.op);
                    break;
                default:
                    result = dispatchPipelineStep(def.stepKind, i, def.args, def.flags, prevOutput, def.primitive, def.op);
                    break;
            }

            stepResults.push(result);
            prevOutput = result.output;
        }

        // If the pipeline has a name, register it in the coordinator.
        if (node.name) {
            this.pipelineCoordinator.register(node.name, stepDefs);
        }

        const pipelineResult: PipelineResult = {
            _type: 'PipelineResult',
            name: node.name,
            stepCount: stepDefs.length,
            steps: stepResults,
            finalOutput: prevOutput,
            meta: { stub: true },
        };

        return pipelineResult;
    }

    // ── pipeline.infer ──────────────────────────────────────────────────
    private execPipelineInfer(
        stepIndex: number, args: unknown[], flags: Record<string, unknown>, prevOutput: unknown,
    ): PipelineStepResult {
        return dispatchPipelineStep('infer', stepIndex, args, flags, prevOutput);
    }

    // ── pipeline.store ──────────────────────────────────────────────────
    private execPipelineStore(
        stepIndex: number, args: unknown[], flags: Record<string, unknown>, prevOutput: unknown,
    ): PipelineStepResult {
        return dispatchPipelineStep('store', stepIndex, args, flags, prevOutput);
    }

    // ── pipeline.net ────────────────────────────────────────────────────
    private execPipelineNet(
        stepIndex: number, args: unknown[], flags: Record<string, unknown>, prevOutput: unknown,
        primitive?: string, op?: string,
    ): PipelineStepResult {
        return dispatchPipelineStep('net', stepIndex, args, flags, prevOutput, primitive, op);
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — SecNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runSec — public entry for SecNode execution ─────────────────────

    /**
     * Execute a SecNode.  Resolves arguments and flags in the current scope,
     * then dispatches to an isolated private method per primitive.
     */
    runSec(node: SecNode): unknown {
        return this.execSec(node, this.rootScope);
    }

    /**
     * Internal SecNode dispatcher.  Each primitive is handled by its own
     * private method so primitives stay modular and testable in isolation.
     */
    private execSec(node: SecNode, scope: Scope): unknown {
        const args = node.args.map(a => this.exec(a, scope));

        const flags: Record<string, unknown> = {};
        if (node.flags) {
            for (const [k, v] of Object.entries(node.flags)) {
                flags[k] = this.exec(v, scope);
            }
        }

        switch (node.primitive) {
            case 'perm': return this.execSecPerm(args, flags);
            case 'sandbox': return this.execSecSandbox(args, flags);
            case 'audit': return this.execSecAudit(args, flags);
            default: return dispatchSec(node.primitive, args, flags);
        }
    }

    // ── sec.perm ─────────────────────────────────────────────────────────
    private execSecPerm(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchSec('perm', args, flags);
        // Also enforce through TrustCoordinator if policies are registered.
        const user = String(flags['user'] ?? args[0] ?? 'anonymous');
        const action = String(flags['action'] ?? args[1] ?? '*');
        const resource = flags['resource'] ? String(flags['resource']) : undefined;
        const decision = this.trustCoordinator.enforce({ user, action, resource });
        return { ...result, trustDecision: decision };
    }

    // ── sec.sandbox ──────────────────────────────────────────────────────
    private execSecSandbox(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchSec('sandbox', args, flags);
        // Record sandbox execution in audit trail.
        const command = String(args[0] ?? flags['cmd'] ?? '<none>');
        this.trustCoordinator.recordAudit({
            user: 'sandbox',
            action: 'sandbox.exec',
            resource: command,
            message: `Sandbox executed: ${command}`,
            level: 'info',
        });
        return result;
    }

    // ── sec.audit ────────────────────────────────────────────────────────
    private execSecAudit(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchSec('audit', args, flags);
        // Also persist through TrustCoordinator.
        const message = String(flags['message'] ?? args[0] ?? '<no message>');
        const level = (flags['level'] as 'info' | 'warn' | 'error' | 'critical') ?? 'info';
        const user = String(flags['user'] ?? 'system');
        this.trustCoordinator.recordAudit({
            user,
            action: 'audit.log',
            message,
            level,
        });
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — DistNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runDist — public entry for DistNode execution ───────────────────

    /**
     * Execute a DistNode.  Resolves arguments and flags in the current scope,
     * then dispatches to an isolated private method per primitive.
     */
    runDist(node: DistNode): unknown {
        return this.execDist(node, this.rootScope);
    }

    /**
     * Internal DistNode dispatcher.  Each primitive is handled by its own
     * private method so primitives stay modular and testable in isolation.
     */
    private execDist(node: DistNode, scope: Scope): unknown {
        const args = node.args.map(a => this.exec(a, scope));

        const flags: Record<string, unknown> = {};
        if (node.flags) {
            for (const [k, v] of Object.entries(node.flags)) {
                flags[k] = this.exec(v, scope);
            }
        }

        switch (node.primitive) {
            case 'register': return this.execDistRegister(args, flags);
            case 'consensus': return this.execDistConsensus(args, flags);
            case 'sync': return this.execDistSync(args, flags);
            default: return dispatchDist(node.primitive, args, flags);
        }
    }

    // ── dist.register ────────────────────────────────────────────────────
    private execDistRegister(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchDist('register', args, flags);
        // Also register in the CoordinationManager for cluster tracking.
        const nodeName = String(flags['node'] ?? args[0] ?? 'unknown');
        const role = String(flags['role'] ?? 'peer');
        this.coordinationManager.registerNode(nodeName, role);
        return result;
    }

    // ── dist.consensus ───────────────────────────────────────────────────
    private execDistConsensus(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchDist('consensus', args, flags);
        // Also run consensus through the CoordinationManager.
        const proposal = String(flags['proposal'] ?? args[0] ?? '<no proposal>');
        const quorum = flags['quorum'] ? Number(flags['quorum']) : undefined;
        const round = this.coordinationManager.startConsensus(proposal, quorum);
        return { ...result, coordinationRound: round };
    }

    // ── dist.sync ────────────────────────────────────────────────────────
    private execDistSync(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchDist('sync', args, flags);
        // Also track barrier through the CoordinationManager.
        const barrier = String(flags['barrier'] ?? args[0] ?? 'default');
        const nodes = flags['nodes'] ? Number(flags['nodes']) : undefined;
        const nodeName = String(flags['node'] ?? 'repl');
        const barrierState = this.coordinationManager.arriveAtBarrier(barrier, nodeName, nodes);
        return { ...result, coordinationBarrier: barrierState };
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — DocNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runDoc — public entry for DocNode execution ─────────────────────

    /**
     * Execute a DocNode.  Resolves arguments and flags in the current scope,
     * then dispatches to an isolated private method per primitive.
     */
    runDoc(node: DocNode): unknown {
        return this.execDoc(node, this.rootScope);
    }

    /**
     * Internal DocNode dispatcher.  Each primitive is handled by its own
     * private method so primitives stay modular and testable in isolation.
     */
    private execDoc(node: DocNode, scope: Scope): unknown {
        const args = node.args.map(a => this.exec(a, scope));

        const flags: Record<string, unknown> = {};
        if (node.flags) {
            for (const [k, v] of Object.entries(node.flags)) {
                flags[k] = this.exec(v, scope);
            }
        }

        switch (node.primitive) {
            case 'parse': return this.execDocParse(args, flags);
            case 'render': return this.execDocRender(args, flags);
            case 'edit': return this.execDocEdit(args, flags);
            default: return dispatchDoc(node.primitive, args, flags);
        }
    }

    // ── doc.parse ─────────────────────────────────────────────────────────
    private execDocParse(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchDoc('parse', args, flags);
        // Register the document in the PDF coordinator.
        const file = String(flags['file'] ?? args[0] ?? 'unknown.pdf');
        const docName = file.replace(/\.[^.]+$/, '');
        this.pdfCoordinator.registerDocument(docName, { file, parsedAt: new Date().toISOString() });
        // Build a semantic tree for the document.
        this.semanticLayer.buildFromDocument(docName);
        return result;
    }

    // ── doc.render ────────────────────────────────────────────────────────
    private execDocRender(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchDoc('render', args, flags);
        // Run the document through the PDF coordinator's render pipeline.
        const docName = String(flags['doc'] ?? args[0] ?? 'unknown');
        const pipelineResult = this.pdfCoordinator.manageRenderPipeline(docName);
        return { ...result, renderPipeline: pipelineResult };
    }

    // ── doc.edit ──────────────────────────────────────────────────────────
    private execDocEdit(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchDoc('edit', args, flags);
        // Track the edit in the PDF coordinator.
        const docName = String(flags['doc'] ?? args[0] ?? 'unknown');
        const target = String(flags['target'] ?? args[1] ?? 'node.0');
        const value = String(flags['value'] ?? args[2] ?? '');
        this.pdfCoordinator.trackEdit(docName, 'edit', target, value);
        // Also apply a semantic transform.
        const targetKind = target.split('.')[0];
        this.semanticLayer.applyTransform({
            name: `doc-edit:${target}`,
            target: targetKind,
            action: 'replace',
            value,
        });
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — UniNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runUni — public entry for UniNode execution ─────────────────────

    /**
     * Execute a UniNode (do { … } block).  Each child step is dispatched
     * through the normal exec() path, and results are consolidated into
     * a single UniResult.
     */
    runUni(node: UniNode): unknown {
        return this.execUni(node, this.rootScope);
    }

    /**
     * Internal UniNode executor.  Iterates over each step, executes it
     * via the central dispatcher, and collects results for consolidation.
     */
    private execUni(node: UniNode, scope: Scope): unknown {
        const stepResults: { kind: string; result: unknown }[] = [];

        for (const step of node.steps) {
            const kind = String(step.kind);
            try {
                const result = this.exec(step, scope);
                stepResults.push({ kind, result });
            } catch (err: any) {
                stepResults.push({
                    kind,
                    result: { _type: 'UniError', error: err.message, step: kind },
                });
            }
        }

        return consolidateUniResults(stepResults, node.label);
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — ReflectNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runReflect — public entry for ReflectNode execution ─────────────

    /**
     * Execute a ReflectNode.  Resolves arguments and flags in the current
     * scope, then dispatches to an isolated private method per primitive.
     */
    runReflect(node: ReflectNode): unknown {
        return this.execReflect(node, this.rootScope);
    }

    /**
     * Internal ReflectNode dispatcher.  Each primitive is handled by its
     * own private method so reflection stays modular and testable.
     */
    private execReflect(node: ReflectNode, scope: Scope): unknown {
        const args = node.args.map(a => this.exec(a, scope));

        const flags: Record<string, unknown> = {};
        if (node.flags) {
            for (const [k, v] of Object.entries(node.flags)) {
                flags[k] = this.exec(v, scope);
            }
        }

        switch (node.primitive) {
            case 'pipeline': return this.execReflectPipeline(args, flags);
            case 'grammar': return this.execReflectGrammar(args, flags);
            case 'trust': return this.execReflectTrust(args, flags);
            case 'dist': return this.execReflectDist(args, flags);
            default: return dispatchReflect(node.primitive, args, flags);
        }
    }

    // ── reflect.pipeline ─────────────────────────────────────────────────
    private execReflectPipeline(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchReflect('pipeline', args, flags);
        this.metaCoordinator.collect('pipeline', 'pipeline', result);
        return result;
    }

    // ── reflect.grammar ──────────────────────────────────────────────────
    private execReflectGrammar(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchReflect('grammar', args, flags);
        this.metaCoordinator.collect('grammar', 'grammar', result);
        return result;
    }

    // ── reflect.trust ────────────────────────────────────────────────────
    private execReflectTrust(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchReflect('trust', args, flags);
        // Enrich with live trust snapshot.
        const trustSnap = this.trustCoordinator.snapshot();
        const enriched = { ...result, policies: trustSnap.policies, auditCount: trustSnap.auditCount };
        this.metaCoordinator.collect('trust', 'trust', enriched);
        return enriched;
    }

    // ── reflect.dist ─────────────────────────────────────────────────────
    private execReflectDist(args: unknown[], flags: Record<string, unknown>): unknown {
        const result = dispatchReflect('dist', args, flags);
        this.metaCoordinator.collect('dist', 'dist', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — ArtifactNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runArtifact — public entry for ArtifactNode execution ───────────

    /**
     * Execute an ArtifactNode.  Resolves arguments and flags in the current
     * scope, then dispatches to the artifact handler per primitive.
     */
    runArtifact(node: ArtifactNode): unknown {
        return this.execArtifact(node, this.rootScope);
    }

    /**
     * Internal ArtifactNode dispatcher.  Routes to density, rarity, or proof
     * handlers via the dispatchArtifact function, passing coordinating layers.
     */
    private execArtifact(node: ArtifactNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'density': return this.execArtifactDensity(node, scope);
            case 'rarity': return this.execArtifactRarity(node, scope);
            case 'proof': return this.execArtifactProof(node, scope);
            default:
                return dispatchArtifact(node, (n) => this.exec(n, scope), {
                    artifactCoordinator: this.artifactCoordinator,
                    rarityLayer: this.rarityLayer,
                });
        }
    }

    // ── artifact.density ─────────────────────────────────────────────────
    private execArtifactDensity(node: ArtifactNode, scope: Scope): unknown {
        const result = dispatchArtifact(node, (n) => this.exec(n, scope), {
            artifactCoordinator: this.artifactCoordinator,
        });
        this.metaCoordinator.collect('artifact', 'density', result);
        return result;
    }

    // ── artifact.rarity ──────────────────────────────────────────────────
    private execArtifactRarity(node: ArtifactNode, scope: Scope): unknown {
        const result = dispatchArtifact(node, (n) => this.exec(n, scope), {
            rarityLayer: this.rarityLayer,
        });
        this.metaCoordinator.collect('artifact', 'rarity', result);
        return result;
    }

    // ── artifact.proof ───────────────────────────────────────────────────
    private execArtifactProof(node: ArtifactNode, scope: Scope): unknown {
        const result = dispatchArtifact(node, (n) => this.exec(n, scope), {
            artifactCoordinator: this.artifactCoordinator,
        });
        this.metaCoordinator.collect('artifact', 'proof', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — AutoNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runAuto — public entry for AutoNode execution ───────────────────

    /**
     * Execute an AutoNode.  Resolves arguments and flags in the current
     * scope, then dispatches to the auto-approval handler per primitive.
     */
    runAuto(node: AutoNode): unknown {
        return this.execAuto(node, this.rootScope);
    }

    /**
     * Internal AutoNode dispatcher.  Routes to safe, sandbox, or prompt
     * handlers via the dispatchAuto function, passing trust boundary and
     * policy layers.
     */
    private execAuto(node: AutoNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'safe': return this.execAutoSafe(node, scope);
            case 'sandbox': return this.execAutoSandbox(node, scope);
            case 'prompt': return this.execAutoPrompt(node, scope);
            default:
                return dispatchAuto(node, (n) => this.exec(n, scope), {
                    trustBoundary: this.trustBoundaryCoordinator,
                    policyLayer: this.policyLayer,
                });
        }
    }

    // ── auto.safe ────────────────────────────────────────────────────────
    private execAutoSafe(node: AutoNode, scope: Scope): unknown {
        const result = dispatchAuto(node, (n) => this.exec(n, scope), {
            trustBoundary: this.trustBoundaryCoordinator,
            policyLayer: this.policyLayer,
        });
        // Record in trust coordinator audit trail
        this.trustCoordinator.recordAudit({
            user: 'auto',
            action: 'auto.safe',
            resource: (result as any).command,
            message: (result as any).message,
            level: 'info',
        });
        this.metaCoordinator.collect('auto', 'safe', result);
        return result;
    }

    // ── auto.sandbox ─────────────────────────────────────────────────────
    private execAutoSandbox(node: AutoNode, scope: Scope): unknown {
        const result = dispatchAuto(node, (n) => this.exec(n, scope), {
            trustBoundary: this.trustBoundaryCoordinator,
            policyLayer: this.policyLayer,
        });
        this.trustCoordinator.recordAudit({
            user: 'auto',
            action: 'auto.sandbox',
            resource: (result as any).command,
            message: (result as any).message,
            level: 'warn',
        });
        this.metaCoordinator.collect('auto', 'sandbox', result);
        return result;
    }

    // ── auto.prompt ──────────────────────────────────────────────────────
    private execAutoPrompt(node: AutoNode, scope: Scope): unknown {
        const result = dispatchAuto(node, (n) => this.exec(n, scope), {
            trustBoundary: this.trustBoundaryCoordinator,
            policyLayer: this.policyLayer,
        });
        this.trustCoordinator.recordAudit({
            user: 'auto',
            action: 'auto.prompt',
            resource: (result as any).command,
            message: (result as any).message,
            level: 'warn',
        });
        this.metaCoordinator.collect('auto', 'prompt', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — OrchestrationNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runOrch — public entry for OrchestrationNode execution ──────────

    /**
     * Execute an OrchestrationNode.  Resolves arguments and flags in the
     * current scope, then dispatches to the orchestration handler per
     * primitive.
     */
    runOrch(node: OrchestrationNode): unknown {
        return this.execOrch(node, this.rootScope);
    }

    /**
     * Internal OrchestrationNode dispatcher.  Routes to pipeline, cross,
     * or global handlers via the dispatchOrchestration function, passing
     * fabric coordinator and execution fabric.
     */
    private execOrch(node: OrchestrationNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'pipeline': return this.execOrchPipeline(node, scope);
            case 'cross': return this.execOrchCross(node, scope);
            case 'global': return this.execOrchGlobal(node, scope);
            default:
                return dispatchOrchestration(node, (n) => this.exec(n, scope), {
                    fabricCoordinator: this.fabricCoordinator,
                    executionFabric: this.executionFabric,
                });
        }
    }

    // ── orch.pipeline ────────────────────────────────────────────────────
    private execOrchPipeline(node: OrchestrationNode, scope: Scope): unknown {
        const result = dispatchOrchestration(node, (n) => this.exec(n, scope), {
            fabricCoordinator: this.fabricCoordinator,
            executionFabric: this.executionFabric,
        });
        this.metaCoordinator.collect('orch', 'pipeline', result);
        return result;
    }

    // ── orch.cross ───────────────────────────────────────────────────────
    private execOrchCross(node: OrchestrationNode, scope: Scope): unknown {
        const result = dispatchOrchestration(node, (n) => this.exec(n, scope), {
            fabricCoordinator: this.fabricCoordinator,
        });
        this.metaCoordinator.collect('orch', 'cross', result);
        return result;
    }

    // ── orch.global ──────────────────────────────────────────────────────
    private execOrchGlobal(node: OrchestrationNode, scope: Scope): unknown {
        const result = dispatchOrchestration(node, (n) => this.exec(n, scope), {
            fabricCoordinator: this.fabricCoordinator,
            executionFabric: this.executionFabric,
        });
        this.metaCoordinator.collect('orch', 'global', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — SelfOptNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runSelfOpt — public entry for SelfOptNode execution ─────────────

    /**
     * Execute a SelfOptNode.  Resolves arguments and flags in the current
     * scope, then dispatches to the self-optimization handler per primitive.
     */
    runSelfOpt(node: SelfOptNode): unknown {
        return this.execSelfOpt(node, this.rootScope);
    }

    /**
     * Internal SelfOptNode dispatcher.  Routes to pipeline, trust, or
     * artifact handlers via the dispatchSelfOpt function, passing adapt
     * coordinator and adaptive fabric.
     */
    private execSelfOpt(node: SelfOptNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'pipeline': return this.execSelfOptPipeline(node, scope);
            case 'trust': return this.execSelfOptTrust(node, scope);
            case 'artifact': return this.execSelfOptArtifact(node, scope);
            default:
                return dispatchSelfOpt(node, (n) => this.exec(n, scope), {
                    adaptCoordinator: this.adaptCoordinator,
                    adaptiveFabric: this.adaptiveFabric,
                });
        }
    }

    // ── selfopt.pipeline ─────────────────────────────────────────────────
    private execSelfOptPipeline(node: SelfOptNode, scope: Scope): unknown {
        const result = dispatchSelfOpt(node, (n) => this.exec(n, scope), {
            adaptCoordinator: this.adaptCoordinator,
            adaptiveFabric: this.adaptiveFabric,
        });
        this.metaCoordinator.collect('selfopt', 'pipeline', result);
        return result;
    }

    // ── selfopt.trust ────────────────────────────────────────────────────
    private execSelfOptTrust(node: SelfOptNode, scope: Scope): unknown {
        const result = dispatchSelfOpt(node, (n) => this.exec(n, scope), {
            adaptCoordinator: this.adaptCoordinator,
        });
        this.metaCoordinator.collect('selfopt', 'trust', result);
        return result;
    }

    // ── selfopt.artifact ─────────────────────────────────────────────────
    private execSelfOptArtifact(node: SelfOptNode, scope: Scope): unknown {
        const result = dispatchSelfOpt(node, (n) => this.exec(n, scope), {
            adaptCoordinator: this.adaptCoordinator,
            adaptiveFabric: this.adaptiveFabric,
        });
        this.metaCoordinator.collect('selfopt', 'artifact', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — MemoryNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runMemory — public entry for MemoryNode execution ───────────────

    /**
     * Execute a MemoryNode.  Resolves arguments and flags in the current
     * scope, then dispatches to the memory handler per primitive.
     */
    runMemory(node: MemoryNode): unknown {
        return this.execMemory(node, this.rootScope);
    }

    /**
     * Internal MemoryNode dispatcher.  Routes to store, recall, or
     * evolve handlers via the dispatchMemory function, passing persist
     * coordinator and persistence layer.
     */
    private execMemory(node: MemoryNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'store': return this.execMemoryStore(node, scope);
            case 'recall': return this.execMemoryRecall(node, scope);
            case 'evolve': return this.execMemoryEvolve(node, scope);
            default:
                return dispatchMemory(node, (n) => this.exec(n, scope), {
                    persistCoordinator: this.persistCoordinator,
                    persistenceLayer: this.persistenceLayer,
                });
        }
    }

    // ── memory.store ─────────────────────────────────────────────────────
    private execMemoryStore(node: MemoryNode, scope: Scope): unknown {
        const result = dispatchMemory(node, (n) => this.exec(n, scope), {
            persistCoordinator: this.persistCoordinator,
            persistenceLayer: this.persistenceLayer,
        });
        this.metaCoordinator.collect('memory', 'store', result);
        return result;
    }

    // ── memory.recall ────────────────────────────────────────────────────
    private execMemoryRecall(node: MemoryNode, scope: Scope): unknown {
        const result = dispatchMemory(node, (n) => this.exec(n, scope), {
            persistCoordinator: this.persistCoordinator,
        });
        this.metaCoordinator.collect('memory', 'recall', result);
        return result;
    }

    // ── memory.evolve ────────────────────────────────────────────────────
    private execMemoryEvolve(node: MemoryNode, scope: Scope): unknown {
        const result = dispatchMemory(node, (n) => this.exec(n, scope), {
            persistCoordinator: this.persistCoordinator,
            persistenceLayer: this.persistenceLayer,
        });
        this.metaCoordinator.collect('memory', 'evolve', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — NarrativeNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runNarrative — public entry for NarrativeNode execution ─────────

    /**
     * Execute a NarrativeNode.  Resolves arguments and flags in the current
     * scope, then dispatches to the narrative handler per primitive.
     */
    runNarrative(node: NarrativeNode): unknown {
        return this.execNarrative(node, this.rootScope);
    }

    /**
     * Internal NarrativeNode dispatcher.  Routes to proof, story, or
     * log handlers via the dispatchNarrative function, passing meta
     * coordinator and story fabric.
     */
    private execNarrative(node: NarrativeNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'proof': return this.execNarrativeProof(node, scope);
            case 'story': return this.execNarrativeStory(node, scope);
            case 'log': return this.execNarrativeLog(node, scope);
            default:
                return dispatchNarrative(node, (n) => this.exec(n, scope), {
                    metaCoordinator: this.metaCoordinator,
                    storyFabric: this.storyFabric,
                });
        }
    }

    // ── narrative.proof ─────────────────────────────────────────────────
    private execNarrativeProof(node: NarrativeNode, scope: Scope): unknown {
        const result = dispatchNarrative(node, (n) => this.exec(n, scope), {
            metaCoordinator: this.metaCoordinator,
            storyFabric: this.storyFabric,
        });
        this.metaCoordinator.collect('narrative', 'proof', result);
        return result;
    }

    // ── narrative.story ─────────────────────────────────────────────────
    private execNarrativeStory(node: NarrativeNode, scope: Scope): unknown {
        const result = dispatchNarrative(node, (n) => this.exec(n, scope), {
            metaCoordinator: this.metaCoordinator,
            storyFabric: this.storyFabric,
        });
        this.metaCoordinator.collect('narrative', 'story', result);
        return result;
    }

    // ── narrative.log ───────────────────────────────────────────────────
    private execNarrativeLog(node: NarrativeNode, scope: Scope): unknown {
        const result = dispatchNarrative(node, (n) => this.exec(n, scope), {
            metaCoordinator: this.metaCoordinator,
            storyFabric: this.storyFabric,
        });
        this.metaCoordinator.collect('narrative', 'log', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — UnifyNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runUnify — public entry for UnifyNode execution ─────────────────

    /**
     * Execute a UnifyNode.  Resolves arguments and flags in the current
     * scope, then dispatches to the unification handler per primitive.
     */
    runUnify(node: UnifyNode): unknown {
        return this.execUnify(node, this.rootScope);
    }

    /**
     * Internal UnifyNode dispatcher.  Routes to syntax, semantic, or
     * exec handlers via the dispatchUnify function, passing unify
     * coordinator and unified fabric.
     */
    private execUnify(node: UnifyNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'syntax': return this.execUnifySyntax(node, scope);
            case 'semantic': return this.execUnifySemantic(node, scope);
            case 'exec': return this.execUnifyExec(node, scope);
            default:
                return dispatchUnify(node, (n) => this.exec(n, scope), {
                    unifyCoordinator: this.unifyCoordinator,
                    unifiedFabric: this.unifiedFabric,
                });
        }
    }

    // ── unify.syntax ─────────────────────────────────────────────────────
    private execUnifySyntax(node: UnifyNode, scope: Scope): unknown {
        const result = dispatchUnify(node, (n) => this.exec(n, scope), {
            unifyCoordinator: this.unifyCoordinator,
            unifiedFabric: this.unifiedFabric,
        });
        this.metaCoordinator.collect('unify', 'syntax', result);
        return result;
    }

    // ── unify.semantic ───────────────────────────────────────────────────
    private execUnifySemantic(node: UnifyNode, scope: Scope): unknown {
        const result = dispatchUnify(node, (n) => this.exec(n, scope), {
            unifyCoordinator: this.unifyCoordinator,
            unifiedFabric: this.unifiedFabric,
        });
        this.metaCoordinator.collect('unify', 'semantic', result);
        return result;
    }

    // ── unify.exec ───────────────────────────────────────────────────────
    private execUnifyExec(node: UnifyNode, scope: Scope): unknown {
        const result = dispatchUnify(node, (n) => this.exec(n, scope), {
            unifyCoordinator: this.unifyCoordinator,
            unifiedFabric: this.unifiedFabric,
        });
        this.metaCoordinator.collect('unify', 'exec', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — CompilerNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runCompiler — public entry for CompilerNode execution ────────────

    /**
     * Execute a CompilerNode.  Resolves arguments and flags in the current
     * scope, then dispatches to the compiler handler per primitive.
     */
    runCompiler(node: CompilerNode): unknown {
        return this.execCompiler(node, this.rootScope);
    }

    /**
     * Internal CompilerNode dispatcher.  Routes to syntax, backend, or
     * exec handlers via the dispatchCompiler function, passing compile
     * coordinator and backend fabric.
     */
    private execCompiler(node: CompilerNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'syntax': return this.execCompilerSyntax(node, scope);
            case 'backend': return this.execCompilerBackend(node, scope);
            case 'exec': return this.execCompilerExec(node, scope);
            default:
                return dispatchCompiler(node, (n) => this.exec(n, scope), {
                    compileCoordinator: this.compileCoordinator,
                    backendFabric: this.backendFabric,
                });
        }
    }

    // ── compiler.syntax ─────────────────────────────────────────────────
    private execCompilerSyntax(node: CompilerNode, scope: Scope): unknown {
        const result = dispatchCompiler(node, (n) => this.exec(n, scope), {
            compileCoordinator: this.compileCoordinator,
            backendFabric: this.backendFabric,
        });
        this.metaCoordinator.collect('compiler', 'syntax', result);
        return result;
    }

    // ── compiler.backend ────────────────────────────────────────────────
    private execCompilerBackend(node: CompilerNode, scope: Scope): unknown {
        const result = dispatchCompiler(node, (n) => this.exec(n, scope), {
            compileCoordinator: this.compileCoordinator,
            backendFabric: this.backendFabric,
        });
        this.metaCoordinator.collect('compiler', 'backend', result);
        return result;
    }

    // ── compiler.exec ───────────────────────────────────────────────────
    private execCompilerExec(node: CompilerNode, scope: Scope): unknown {
        const result = dispatchCompiler(node, (n) => this.exec(n, scope), {
            compileCoordinator: this.compileCoordinator,
            backendFabric: this.backendFabric,
        });
        this.metaCoordinator.collect('compiler', 'exec', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — ExecMultiNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runExecMulti — public entry for ExecMultiNode execution ──────────

    /**
     * Execute an ExecMultiNode.  Resolves arguments and flags in the current
     * scope, then dispatches to the multi-backend execution handler per primitive.
     */
    runExecMulti(node: ExecMultiNode): unknown {
        return this.execExecMulti(node, this.rootScope);
    }

    /**
     * Internal ExecMultiNode dispatcher.  Routes to js, cpp, or
     * riscv handlers via the dispatchExecMulti function, passing multi-exec
     * coordinator and backend runner.
     */
    private execExecMulti(node: ExecMultiNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'js': return this.execExecMultiJs(node, scope);
            case 'cpp': return this.execExecMultiCpp(node, scope);
            case 'riscv': return this.execExecMultiRiscv(node, scope);
            default:
                return dispatchExecMulti(node, (n) => this.exec(n, scope), {
                    multiExecCoordinator: this.multiExecCoordinator,
                    backendRunner: this.backendRunner,
                });
        }
    }

    // ── execmulti.js ─────────────────────────────────────────────────────
    private execExecMultiJs(node: ExecMultiNode, scope: Scope): unknown {
        const result = dispatchExecMulti(node, (n) => this.exec(n, scope), {
            multiExecCoordinator: this.multiExecCoordinator,
            backendRunner: this.backendRunner,
        });
        this.metaCoordinator.collect('execmulti', 'js', result);
        return result;
    }

    // ── execmulti.cpp ────────────────────────────────────────────────────
    private execExecMultiCpp(node: ExecMultiNode, scope: Scope): unknown {
        const result = dispatchExecMulti(node, (n) => this.exec(n, scope), {
            multiExecCoordinator: this.multiExecCoordinator,
            backendRunner: this.backendRunner,
        });
        this.metaCoordinator.collect('execmulti', 'cpp', result);
        return result;
    }

    // ── execmulti.riscv ──────────────────────────────────────────────────
    private execExecMultiRiscv(node: ExecMultiNode, scope: Scope): unknown {
        const result = dispatchExecMulti(node, (n) => this.exec(n, scope), {
            multiExecCoordinator: this.multiExecCoordinator,
            backendRunner: this.backendRunner,
        });
        this.metaCoordinator.collect('execmulti', 'riscv', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  FOUNDATION EXECUTOR — InteropNode
    // ═════════════════════════════════════════════════════════════════════

    // ── runInterop — public entry for InteropNode execution ─────────────

    /**
     * Execute an InteropNode.  Resolves arguments and flags in the current
     * scope, then dispatches to the interop handler per primitive.
     */
    runInterop(node: InteropNode): unknown {
        return this.execInterop(node, this.rootScope);
    }

    // ═════════════════════════════════════════════════════════════════════
    //  STAGE 18.5 — UNIFIED AST NORMALIZATION
    // ═════════════════════════════════════════════════════════════════════

    /**
     * Normalize multi-language source into universal Grey++ normalized AST,
     * then optionally translate to a target language.
     *
     * Returns a result containing the normalized node, unified syntax, and
     * optional translation outputs.
     */
    runUnifyAST(source: string, language?: string, targets?: string[]): {
        normalized: NormalizedNode;
        unified: string;
        detectedLang: string;
        detectedConstruct: string | null;
        translations: TranslationOutput[];
    } {
        const lang = language ?? detectLanguage(source);
        const construct = detectConstruct(source, lang);
        const normalized = this.normalizer.normalize(source, lang);
        const unified = this.normalizer.toUnified(normalized);

        const translations: TranslationOutput[] = [];
        if (targets && targets.length > 0) {
            for (const t of targets) {
                translations.push(this.translation.translate(normalized, t as any));
            }
        } else {
            // Default: translate to all targets
            translations.push(...this.translation.translateAll(normalized));
        }

        return { normalized, unified, detectedLang: lang, detectedConstruct: construct, translations };
    }

    /**
     * Internal InteropNode dispatcher.  Routes to data, call, or
     * orch handlers via the dispatchInterop function, passing interop
     * coordinator and interop fabric.
     */
    private execInterop(node: InteropNode, scope: Scope): unknown {
        switch (node.primitive) {
            case 'data': return this.execInteropData(node, scope);
            case 'call': return this.execInteropCall(node, scope);
            case 'orch': return this.execInteropOrch(node, scope);
            default:
                return dispatchInterop(node, (n) => this.exec(n, scope), {
                    interopCoordinator: this.interopCoordinator,
                    interopFabric: this.interopFabric,
                });
        }
    }

    // ── interop.data ─────────────────────────────────────────────────────
    private execInteropData(node: InteropNode, scope: Scope): unknown {
        const result = dispatchInterop(node, (n) => this.exec(n, scope), {
            interopCoordinator: this.interopCoordinator,
            interopFabric: this.interopFabric,
        });
        this.metaCoordinator.collect('interop', 'data', result);
        return result;
    }

    // ── interop.call ─────────────────────────────────────────────────────
    private execInteropCall(node: InteropNode, scope: Scope): unknown {
        const result = dispatchInterop(node, (n) => this.exec(n, scope), {
            interopCoordinator: this.interopCoordinator,
            interopFabric: this.interopFabric,
        });
        this.metaCoordinator.collect('interop', 'call', result);
        return result;
    }

    // ── interop.orch ─────────────────────────────────────────────────────
    private execInteropOrch(node: InteropNode, scope: Scope): unknown {
        const result = dispatchInterop(node, (n) => this.exec(n, scope), {
            interopCoordinator: this.interopCoordinator,
            interopFabric: this.interopFabric,
        });
        this.metaCoordinator.collect('interop', 'orch', result);
        return result;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  EXPRESSION & STATEMENT EXECUTORS
    // ═════════════════════════════════════════════════════════════════════

    private execProgram(node: ProgramNode, scope: Scope): unknown {
        let last: unknown;
        for (const stmt of node.body) {
            last = this.exec(stmt, scope);
            if (isReturn(last)) return (last as ReturnSignal).value;
        }
        return last;
    }

    private execIdentifier(node: IdentifierNode, scope: Scope): unknown {
        if (!scope.has(node.name)) {
            throw new Error(`Undefined variable "${node.name}"`);
        }
        return scope.get(node.name);
    }

    private execBinaryExpr(node: BinaryExprNode, scope: Scope): unknown {
        const left = this.exec(node.left, scope) as any;
        const right = this.exec(node.right, scope) as any;
        switch (node.op) {
            case '+': return left + right;
            case '-': return left - right;
            case '*': return left * right;
            case '/': return left / right;
            case '==': return left == right;
            case '!=': return left != right;
            case '<': return left < right;
            case '>': return left > right;
            case '<=': return left <= right;
            case '>=': return left >= right;
            default: throw new Error(`Unknown operator "${node.op}"`);
        }
    }

    private execCallExpr(node: CallExprNode, scope: Scope): unknown {
        const fn = scope.get(node.callee);
        if (typeof fn !== 'function') {
            throw new Error(`"${node.callee}" is not a function`);
        }
        const args = node.args.map(a => this.exec(a, scope));
        return fn(...args);
    }

    private execReturn(node: ReturnStmtNode, scope: Scope): ReturnSignal {
        return returnSignal(this.exec(node.value, scope));
    }

    private execArrayLit(node: ArrayLitNode, scope: Scope): unknown[] {
        return node.elements.map(el => this.exec(el, scope));
    }

    private execObjectLit(node: ObjectLitNode, scope: Scope): Record<string, unknown> {
        const obj: Record<string, unknown> = {};
        for (const { key, value } of node.entries) {
            obj[key] = this.exec(value, scope);
        }
        return obj;
    }

    // ═════════════════════════════════════════════════════════════════════
    //  BUILT-INS
    // ═════════════════════════════════════════════════════════════════════

    private seedBuiltins(): void {
        this.rootScope.define('print', (...args: unknown[]) => {
            console.log(...args);
            return args[args.length - 1];
        });
    }
}
