// ─── Grey++ Stage 1 REPL ─────────────────────────────────────────────────────
// Minimal REPL that parses `fn` and `query` constructs into grey_ast nodes,
// executes them via GreyRuntime, and prints results.  No background services,
// no heavy indexing — just parse → execute → print.
//
// Usage (interactive):
//   npx tsx src/ast/grey_repl.ts
//
// Usage (programmatic):
//   import { repl } from './grey_repl.js';
//   repl('fn add(a, b) { return a + b }');

import * as readline from 'node:readline';
import { GreyRuntime } from './grey_runtime.js';
import type { ASTNode } from './grey_ast.js';

// ═══════════════════════════════════════════════════════════════════════════
//  MINIMAL PARSER  (Stage 1 — fn + query + expressions only)
// ═══════════════════════════════════════════════════════════════════════════

// ── Token types ─────────────────────────────────────────────────────────────

type TokenType =
    | 'Fn' | 'Query' | 'Select' | 'From' | 'Where' | 'Return'
    | 'Module' | 'Infer' | 'Sys' | 'Storage' | 'Net' | 'Pipeline' | 'Sec' | 'Dist' | 'Doc' | 'Do' | 'Reflect' | 'Artifact' | 'Auto' | 'Orch' | 'SelfOpt' | 'Memory' | 'Narrative' | 'Unify' | 'Compiler' | 'ExecMulti' | 'Interop'
    | 'True' | 'False'
    | 'Number' | 'String' | 'Ident'
    | 'Plus' | 'Minus' | 'Star' | 'Slash'
    | 'Eq' | 'EqEq' | 'BangEq' | 'Lt' | 'Gt' | 'LtEq' | 'GtEq'
    | 'LParen' | 'RParen' | 'LBrace' | 'RBrace'
    | 'LBracket' | 'RBracket'
    | 'Comma' | 'Semi' | 'Colon' | 'Dot'
    | 'EOF';

interface Token { type: TokenType; value: string; }

const KEYWORDS: Record<string, TokenType> = {
    fn: 'Fn', query: 'Query', select: 'Select', from: 'From',
    where: 'Where', return: 'Return', module: 'Module', infer: 'Infer',
    sys: 'Sys', storage: 'Storage', net: 'Net', pipeline: 'Pipeline', sec: 'Sec', dist: 'Dist', doc: 'Doc', do: 'Do', reflect: 'Reflect', artifact: 'Artifact', auto: 'Auto', orch: 'Orch', selfopt: 'SelfOpt', memory: 'Memory', narrative: 'Narrative', unify: 'Unify', compiler: 'Compiler', execmulti: 'ExecMulti', interop: 'Interop', true: 'True', false: 'False',
};

// ── Tokenizer ───────────────────────────────────────────────────────────────

function tokenize(source: string): Token[] {
    const tokens: Token[] = [];
    let i = 0;
    while (i < source.length) {
        const ch = source[i];
        if (/\s/.test(ch)) { i++; continue; }
        if (ch === '/' && source[i + 1] === '/') {
            while (i < source.length && source[i] !== '\n') i++;
            continue;
        }
        if (/[0-9]/.test(ch)) {
            let n = '';
            while (i < source.length && /[0-9.]/.test(source[i])) n += source[i++];
            tokens.push({ type: 'Number', value: n }); continue;
        }
        if (ch === '"' || ch === "'") {
            const q = ch; i++;
            let s = '';
            while (i < source.length && source[i] !== q) {
                if (source[i] === '\\') { i++; s += source[i++]; continue; }
                s += source[i++];
            }
            i++; tokens.push({ type: 'String', value: s }); continue;
        }
        if (/[a-zA-Z_]/.test(ch)) {
            let w = '';
            while (i < source.length && /[a-zA-Z0-9_]/.test(source[i])) w += source[i++];
            tokens.push({ type: KEYWORDS[w] ?? 'Ident', value: w }); continue;
        }
        const two = source.slice(i, i + 2);
        if (two === '==') { tokens.push({ type: 'EqEq', value: two }); i += 2; continue; }
        if (two === '!=') { tokens.push({ type: 'BangEq', value: two }); i += 2; continue; }
        if (two === '<=') { tokens.push({ type: 'LtEq', value: two }); i += 2; continue; }
        if (two === '>=') { tokens.push({ type: 'GtEq', value: two }); i += 2; continue; }

        const SINGLE: Record<string, TokenType> = {
            '+': 'Plus', '-': 'Minus', '*': 'Star', '/': 'Slash',
            '=': 'Eq', '<': 'Lt', '>': 'Gt',
            '(': 'LParen', ')': 'RParen', '{': 'LBrace', '}': 'RBrace',
            '[': 'LBracket', ']': 'RBracket',
            ',': 'Comma', ';': 'Semi', ':': 'Colon', '.': 'Dot',
        };
        if (SINGLE[ch]) { tokens.push({ type: SINGLE[ch], value: ch }); i++; continue; }
        throw new SyntaxError(`Unexpected character '${ch}' at position ${i}`);
    }
    tokens.push({ type: 'EOF', value: '' });
    return tokens;
}

// ── Recursive-descent parser ────────────────────────────────────────────────
// Emits ASTNode objects whose `kind` values are the *string* literals from
// grey_ast.ts (const enums are erased at runtime).

function parseSource(source: string): ASTNode {
    const tokens = tokenize(source);
    let pos = 0;

    const peek = (): Token => tokens[pos];
    const advance = (): Token => tokens[pos++];
    const at = (t: TokenType): boolean => peek().type === t;

    function expect(t: TokenType): Token {
        const tok = advance();
        if (tok.type !== t) throw new SyntaxError(`Expected ${t}, got ${tok.type} ("${tok.value}")`);
        return tok;
    }

    /** True when the current token is a keyword that can start a do-block step.
     *  Used to stop primitive parsers from consuming tokens that belong to the
     *  next step inside a `do { … }` block. */
    const STEP_KEYWORDS: Set<string> = new Set([
        'Net', 'Sec', 'Dist', 'Doc', 'Pipeline', 'Infer', 'Sys', 'Storage', 'Query', 'Fn', 'Do', 'Reflect', 'Artifact', 'Auto', 'Orch', 'SelfOpt', 'Memory', 'Narrative', 'Unify', 'Compiler', 'ExecMulti', 'Interop',
    ]);
    const isStepKeyword = (): boolean => STEP_KEYWORDS.has(peek().type);

    /** Accept an Ident or any keyword token and return its string value. */
    function expectIdentOrKeyword(): string {
        const tok = peek();
        if (tok.type === 'Ident') return advance().value;
        // Accept any keyword token as a plain name in contexts like storage ops.
        if (tok.value && /^[a-zA-Z_]\w*$/.test(tok.value)) return advance().value;
        throw new SyntaxError(`Expected identifier, got ${tok.type} ("${tok.value}")`);
    }

    // ── Program ──
    function parseProgram(): ASTNode {
        const body: ASTNode[] = [];
        while (!at('EOF')) body.push(parseTopLevel());
        return { kind: 'Program' as any, body };
    }

    function parseTopLevel(): ASTNode {
        if (at('Fn')) return parseFn();
        if (at('Query')) return parseQuery();
        if (at('Module')) return parseModule();
        if (at('Infer')) return parseInfer(); if (at('Sys')) return parseSys(); if (at('Storage')) return parseStorage(); if (at('Net')) return parseNet(); if (at('Pipeline')) return parsePipeline(); if (at('Sec')) return parseSec(); if (at('Dist')) return parseDist(); if (at('Doc')) return parseDoc(); if (at('Do')) return parseDo(); if (at('Reflect')) return parseReflect(); if (at('Artifact')) return parseArtifact(); if (at('Auto')) return parseAuto(); if (at('Orch')) return parseOrch(); if (at('SelfOpt')) return parseSelfOpt(); if (at('Memory')) return parseMemory(); if (at('Narrative')) return parseNarrative(); if (at('Unify')) return parseUnify(); if (at('Compiler')) return parseCompiler(); if (at('ExecMulti')) return parseExecMulti(); if (at('Interop')) return parseInterop(); return parseExprStmt();
    }

    // ── fn name(params) { body } ──
    function parseFn(): ASTNode {
        expect('Fn');
        const name = at('Ident') ? advance().value : null;
        expect('LParen');
        const params: { name: string }[] = [];
        while (!at('RParen')) {
            params.push({ name: expect('Ident').value });
            if (at('Comma')) advance();
        }
        expect('RParen');
        const body = parseBlock();
        return { kind: 'Function' as any, name, params, body };
    }

    // ── query { select … from … where … } ──
    function parseQuery(): ASTNode {
        expect('Query');
        expect('LBrace');
        let select: ASTNode | null = null;
        let from: ASTNode | null = null;
        let where: ASTNode | null = null;

        while (!at('RBrace') && !at('EOF')) {
            if (at('Select')) {
                advance();
                const cols: string[] = [];
                cols.push(parseColRef());
                while (at('Comma')) { advance(); cols.push(parseColRef()); }
                select = { kind: 'SelectClause' as any, columns: cols };
            } else if (at('From')) {
                advance();
                const table = expect('Ident').value;
                from = { kind: 'FromClause' as any, table };
            } else if (at('Where')) {
                advance();
                where = { kind: 'WhereClause' as any, condition: parseExpression() };
            } else {
                throw new SyntaxError(`Unexpected token in query: ${peek().value}`);
            }
        }
        expect('RBrace');
        if (!select) throw new SyntaxError('query requires a select clause');
        return { kind: 'Query' as any, select, from, where };
    }

    // ── module <framework> <Name> { exports… } ──
    // Syntax:  module react { div(span("Hello")) }
    //          module django MyAPI { fn getUsers() { ... } }
    function parseModule(): ASTNode {
        expect('Module');

        // Framework target (optional identifier like react, django, spring).
        let framework: string | undefined;
        let name: string;

        // Next token is the framework name or module name.
        const first = expect('Ident').value;

        // If another ident follows before '{', first was framework, second is name.
        if (at('Ident')) {
            framework = first;
            name = advance().value;
        } else {
            // Single identifier — treat it as both framework and name.
            framework = first;
            name = first.charAt(0).toUpperCase() + first.slice(1) + 'Module';
        }

        expect('LBrace');

        // Collect exports — anything inside the braces.
        const exports: ASTNode[] = [];
        while (!at('RBrace') && !at('EOF')) {
            exports.push(parseStatement());
        }
        expect('RBrace');

        return { kind: 'Module' as any, name, framework, exports, imports: [] };
    }

    // ── infer model=<name> <key>=<value>… ──
    // Syntax:  infer model=gpt4 dataset=demo
    //          infer model=basic prompt="explain closures"
    function parseInfer(): ASTNode {
        expect('Infer');

        let model = 'default';
        const optionEntries: { key: string; value: ASTNode }[] = [];
        let inputNode: ASTNode = { kind: 'StringLit' as any, value: '' };

        // Parse key=value pairs.
        // Bare identifiers after '=' are treated as string literals (not var refs).
        while (at('Ident')) {
            const key = advance().value;
            if (at('Eq')) {
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('Ident')) {
                    // Bare ident → treat as string literal.
                    const raw = advance().value;
                    value = { kind: 'StringLit' as any, value: raw };
                } else {
                    value = parsePrimary();
                }
                if (key === 'model') {
                    model = (value as any).value ?? (value as any).name ?? String(value);
                } else if (key === 'prompt' || key === 'input') {
                    inputNode = value;
                } else {
                    optionEntries.push({ key, value });
                }
            }
        }

        // Build options record (ASTNode values keyed by name).
        const options: Record<string, ASTNode> = {};
        for (const { key, value } of optionEntries) {
            options[key] = value;
        }

        return { kind: 'Infer' as any, model, input: inputNode, options };
    }

    // ── sys.<op> <args/flags> ──
    // Syntax:
    //   sys.log "Hello Grey++"
    //   sys.thread { fn compute() { return 42 } }
    //   sys.message target=process1 payload="Ping"
    function parseSys(): ASTNode {
        expect('Sys');
        expect('Dot');

        // The operation name (log, thread, message, env, spawn, …).
        const op = expect('Ident').value;

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect arguments and key=value flags until EOL / EOF / semicolon.
        while (!at('EOF') && !at('Semi') && !at('RBrace')) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('Ident')) {
                    const raw = advance().value;
                    value = { kind: 'StringLit' as any, value: raw };
                } else {
                    value = parsePrimary();
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Block body → wrap in a sub-expression list.
            if (at('LBrace')) {
                const body = parseBlock();
                // Wrap the block as an array literal of statements.
                args.push({ kind: 'ArrayLit' as any, elements: body });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        // Build flags record.
        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Sys' as any,
            op,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── storage <backend> <op> <args/flags> ──
    // Syntax:
    //   storage pdf load "demo.pdf"
    //   storage pdf query "find headings"
    //   storage pdf edit node=1 text="Updated Title"
    //   storage db query "SELECT * FROM users"
    //   storage file load "config.json"
    function parseStorage(): ASTNode {
        expect('Storage');

        // Backend type (pdf, db, file, …).
        // Accept keywords like 'query', 'select' etc. as plain identifiers here.
        const backend = expectIdentOrKeyword();

        // Operation (load, query, edit, list, …).
        const op = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace')) {
            // key=value flag.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('Ident')) {
                    const raw = advance().value;
                    value = { kind: 'StringLit' as any, value: raw };
                } else {
                    value = parsePrimary();
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Storage' as any,
            backend,
            op,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── net.<primitive> <op> <args/flags> ──
    // Syntax:
    //   net.http GET "https://example.com"
    //   net.socket connect host=localhost port=8080
    //   net.rpc call service=math method=add args=[1,2]
    function parseNet(): ASTNode {
        expect('Net');
        expect('Dot');

        // The primitive type (http, socket, rpc, …).
        const primitive = expectIdentOrKeyword();

        // The operation / verb (GET, POST, connect, call, …).
        const op = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else if (at('Ident')) {
                    const raw = advance().value;
                    value = { kind: 'StringLit' as any, value: raw };
                } else {
                    value = parsePrimary();
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Net' as any,
            primitive,
            op,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── sec.<primitive> <args/flags> ──
    // Syntax:
    //   sec.perm user=alice action=net.http
    //   sec.sandbox run "rm -rf testdir"
    //   sec.audit message="Pipeline executed" level=info
    // ── sec.<primitive> <args/flags> ──
    // Syntax:
    //   sec.perm user=alice action=net.http
    //   sec.sandbox run "rm -rf testdir"
    //   sec.audit message="Pipeline executed" level=info
    function parseSec(): ASTNode {
        expect('Sec');
        expect('Dot');

        // The primitive type (perm, sandbox, audit, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values (e.g. net.http).
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Sec' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── dist.<primitive> <args/flags> ──
    // Syntax:
    //   dist.register node=alpha
    //   dist.consensus proposal="upgrade pipeline"
    //   dist.sync barrier="stage8"
    function parseDist(): ASTNode {
        expect('Dist');
        expect('Dot');

        // The primitive type (register, consensus, sync, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Dist' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── doc.<primitive> <args/flags> ──
    // Syntax:
    //   doc.parse file="report.pdf"
    //   doc.render doc="report" format=html
    //   doc.edit doc="report" target=heading.1 value="New Title"
    function parseDoc(): ASTNode {
        expect('Doc');
        expect('Dot');

        // The primitive type (parse, render, edit, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        // Allow step-keyword tokens (e.g. `doc`) when followed by '=' (flag name)
        // but stop when a step keyword starts a new primitive invocation.
        while (!at('EOF') && !at('Semi') && !at('RBrace')
            && (!isStepKeyword() || (pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq'))) {
            // key=value flag — lookahead: any word token followed by '='.
            // Use expectIdentOrKeyword-style check so keywords like 'doc' work as flag names.
            const isWordToken = at('Ident') || (peek().value && /^[a-zA-Z_]\w*$/.test(peek().value));
            if (isWordToken && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values (e.g. heading.1).
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        // Accept ident, keyword, or number after dot (e.g. heading.1).
                        if (at('Number')) {
                            raw += '.' + advance().value;
                        } else {
                            raw += '.' + expectIdentOrKeyword();
                        }
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Doc' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── reflect.<primitive> <args/flags> ──
    // Syntax:
    //   reflect.pipeline id="p1"
    //   reflect.grammar rule="doc.edit"
    //   reflect.trust policy="sec.perm"
    //   reflect.dist cluster="alpha"
    function parseReflect(): ASTNode {
        expect('Reflect');
        expect('Dot');

        // The primitive type (pipeline, grammar, trust, dist, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Reflect' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── do [label] { step; step; … } ──
    // Syntax:
    //   do { net.http GET "https://api.example.com" sec.perm user=alice action=doc.edit doc.edit doc="report" target=heading.1 value="New Title" }
    //   do myBlock { … }

    // ── artifact.<primitive> <args/flags> ──
    // Syntax:
    //   artifact.density target="pipeline1"
    //   artifact.rarity target="doc.edit"
    //   artifact.proof target="dist.consensus"
    function parseArtifact(): ASTNode {
        expect('Artifact');
        expect('Dot');

        // The primitive type (density, rarity, proof, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Artifact' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── auto.<primitive> <args/flags> ──
    // Syntax:
    //   auto.safe command="ls"
    //   auto.sandbox command="rm -rf testdir"
    //   auto.prompt command="git reset --hard"
    function parseAuto(): ASTNode {
        expect('Auto');
        expect('Dot');

        // The primitive type (safe, sandbox, prompt, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Auto' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── orch.<primitive> <args/flags> ──
    // Syntax:
    //   orch.pipeline steps="net.http, doc.edit, dist.consensus"
    //   orch.cross modules="pipeline, security, artifact"
    //   orch.global scenario="civilization-tier coordination"
    function parseOrch(): ASTNode {
        expect('Orch');
        expect('Dot');

        // The primitive type (pipeline, cross, global, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Orchestration' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── selfopt.<primitive> <args/flags> ──
    // Syntax:
    //   selfopt.pipeline target="pipeline1" strategy="latency"
    //   selfopt.trust target="sec.perm" threshold=0.9
    //   selfopt.artifact target="density" mode="aggressive"
    function parseSelfOpt(): ASTNode {
        expect('SelfOpt');
        expect('Dot');

        // The primitive type (pipeline, trust, artifact, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'SelfOpt' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── memory.<primitive> <args/flags> ──
    // Syntax:
    //   memory.store key="pipeline1" value="optimized"
    //   memory.recall key="doc.edit"
    //   memory.evolve key="artifact.rarity"
    function parseMemory(): ASTNode {
        expect('Memory');
        expect('Dot');

        // The primitive type (store, recall, evolve, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Memory' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── narrative.<primitive> <args/flags> ──
    // Syntax:
    //   narrative.proof source="artifact.rarity"
    //   narrative.story theme="civilization-tier orchestration"
    //   narrative.log stage="Stage 16"
    function parseNarrative(): ASTNode {
        expect('Narrative');
        expect('Dot');

        // The primitive type (proof, story, log, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Narrative' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── unify.<primitive> <args/flags> ──
    // Syntax:
    //   unify.syntax modules="pipeline, orchestration, memory"
    //   unify.semantic source="artifact.rarity"
    //   unify.exec scenario="civilization-tier orchestration"
    function parseUnify(): ASTNode {
        expect('Unify');
        expect('Dot');

        // The primitive type (syntax, semantic, exec, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Unify' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── compiler.<primitive> <args/flags> ──
    // Syntax:
    //   compiler.syntax source="pipeline, orchestration"
    //   compiler.backend target="javascript" optimize=true
    //   compiler.exec scenario="full-stack deployment"
    function parseCompiler(): ASTNode {
        expect('Compiler');
        expect('Dot');

        // The primitive type (syntax, backend, exec, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else if (at('True') || at('False')) {
                    value = parsePrimary();
                } else {
                    // Accept any identifier or keyword token as a string value,
                    // and handle dot-separated compound values.
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Compiler' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    function parseExecMulti(): ASTNode {
        expect('ExecMulti');
        expect('Dot');

        // The primitive type (js, cpp, riscv, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else if (at('True') || at('False')) {
                    value = parsePrimary();
                } else {
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'ExecMulti' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // ── interop.<primitive> <args/flags> ──
    // Syntax:
    //   interop.data source="execmulti.js" target="execmulti.cpp"
    //   interop.call source="execmulti.cpp" target="execmulti.riscv"
    //   interop.orch modules="js, cpp, riscv"
    function parseInterop(): ASTNode {
        expect('Interop');
        expect('Dot');

        // The primitive type (data, call, orch, …).
        const primitive = expectIdentOrKeyword();

        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        // Collect positional args and key=value flags.
        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepKeyword()) {
            // key=value flag — lookahead: Ident followed by '='.
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else if (at('True') || at('False')) {
                    value = parsePrimary();
                } else {
                    let raw = expectIdentOrKeyword();
                    while (at('Dot') && pos + 1 < tokens.length) {
                        advance(); // consume '.'
                        raw += '.' + expectIdentOrKeyword();
                    }
                    value = { kind: 'StringLit' as any, value: raw };
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument.
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }

        return {
            kind: 'Interop' as any,
            primitive,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    function parseDo(): ASTNode {
        expect('Do');

        // Optional label before the opening brace.
        let label: string | undefined;
        if (at('Ident')) {
            label = advance().value;
        }

        expect('LBrace');

        // Parse inner statements — each one is a primitive invocation.
        const steps: ASTNode[] = [];
        while (!at('RBrace') && !at('EOF')) {
            steps.push(parseDoStep());
        }
        expect('RBrace');

        return {
            kind: 'Uni' as any,
            label,
            steps,
        };
    }

    // Parse a single step inside a do-block.
    // Reuses the existing primitive parsers (net, sec, dist, doc, pipeline, infer, sys, etc.).
    function parseDoStep(): ASTNode {
        if (at('Net')) return parseNet();
        if (at('Sec')) return parseSec();
        if (at('Dist')) return parseDist();
        if (at('Doc')) return parseDoc();
        if (at('Pipeline')) return parsePipeline();
        if (at('Infer')) return parseInfer();
        if (at('Sys')) return parseSys();
        if (at('Storage')) return parseStorage();
        if (at('Query')) return parseQuery();
        if (at('Fn')) return parseFn();
        if (at('Reflect')) return parseReflect();
        if (at('Artifact')) return parseArtifact();
        if (at('Auto')) return parseAuto();
        if (at('Orch')) return parseOrch();
        if (at('SelfOpt')) return parseSelfOpt();
        if (at('Memory')) return parseMemory();
        if (at('Narrative')) return parseNarrative();
        if (at('Unify')) return parseUnify();
        if (at('Compiler')) return parseCompiler();
        if (at('ExecMulti')) return parseExecMulti();
        if (at('Interop')) return parseInterop();
        // Fallback: treat as expression statement.
        return parseExprStmt();
    }

    // ── pipeline { step; step; … } ──
    // Syntax:
    //   pipeline {
    //     infer model=basic dataset=demo
    //     store backend=pdf file="demo.pdf"
    //     net.http POST "https://api.example.com"
    //   }
    //   pipeline myPipeline { ... }
    function parsePipeline(): ASTNode {
        expect('Pipeline');

        // Optional pipeline name before the opening brace.
        let name: string | undefined;
        if (at('Ident')) {
            name = advance().value;
        }

        expect('LBrace');

        const steps: ASTNode[] = [];

        while (!at('RBrace') && !at('EOF')) {
            steps.push(parsePipelineStep());
        }
        expect('RBrace');

        return {
            kind: 'Pipeline' as any,
            name,
            steps,
        };
    }

    // Parse a single pipeline step (infer, store, or net.*)
    function parsePipelineStep(): ASTNode {
        // ── infer step ──
        if (at('Infer')) {
            advance(); // consume 'infer'
            const { args, flags } = parseFlagsAndArgs();
            return {
                kind: 'PipelineStep' as any,
                stepKind: 'infer',
                args,
                flags: Object.keys(flags).length > 0 ? flags : undefined,
            };
        }

        // ── store step ──
        if (at('Storage')) {
            advance(); // consume 'storage' keyword (used as 'store')
            const { args, flags } = parseFlagsAndArgs();
            return {
                kind: 'PipelineStep' as any,
                stepKind: 'store',
                args,
                flags: Object.keys(flags).length > 0 ? flags : undefined,
            };
        }

        // Also accept bare 'store' as an identifier
        if (at('Ident') && peek().value === 'store') {
            advance(); // consume 'store'
            const { args, flags } = parseFlagsAndArgs();
            return {
                kind: 'PipelineStep' as any,
                stepKind: 'store',
                args,
                flags: Object.keys(flags).length > 0 ? flags : undefined,
            };
        }

        // ── net step: net.<primitive> <op> <args/flags> ──
        if (at('Net')) {
            advance(); // consume 'net'
            expect('Dot');
            const primitive = expectIdentOrKeyword();
            const op = expectIdentOrKeyword();
            const { args, flags } = parseFlagsAndArgs();
            return {
                kind: 'PipelineStep' as any,
                stepKind: 'net',
                primitive,
                op,
                args,
                flags: Object.keys(flags).length > 0 ? flags : undefined,
            };
        }

        // ── generic step: <identifier> key=val … ──
        const stepKind = expectIdentOrKeyword();
        const { args, flags } = parseFlagsAndArgs();
        return {
            kind: 'PipelineStep' as any,
            stepKind,
            args,
            flags: Object.keys(flags).length > 0 ? flags : undefined,
        };
    }

    // Helper: parse mixed key=value flags and positional args until line end.
    function parseFlagsAndArgs(): { args: ASTNode[]; flags: Record<string, ASTNode> } {
        const args: ASTNode[] = [];
        const flagEntries: { key: string; value: ASTNode }[] = [];

        while (!at('EOF') && !at('Semi') && !at('RBrace') && !isStepStart()) {
            // key=value flag
            if (at('Ident') && pos + 1 < tokens.length && tokens[pos + 1].type === 'Eq') {
                const key = advance().value;
                advance(); // consume '='
                let value: ASTNode;
                if (at('String') || at('Number')) {
                    value = parsePrimary();
                } else if (at('LBracket')) {
                    value = parsePrimary();
                } else if (at('Ident')) {
                    const raw = advance().value;
                    value = { kind: 'StringLit' as any, value: raw };
                } else {
                    value = parsePrimary();
                }
                flagEntries.push({ key, value });
                continue;
            }

            // Positional argument
            args.push(parsePrimary());
        }
        if (at('Semi')) advance();

        const flags: Record<string, ASTNode> = {};
        for (const { key, value } of flagEntries) {
            flags[key] = value;
        }
        return { args, flags };
    }

    // Check if current token starts a new pipeline step.
    function isStepStart(): boolean {
        const t = peek();
        if (t.type === 'Infer' || t.type === 'Storage' || t.type === 'Net') return true;
        if (t.type === 'Ident' && t.value === 'store') return true;
        return false;
    }

    function parseColRef(): string {
        if (at('Star')) { advance(); return '*'; }
        return expect('Ident').value;
    }

    // ── Block { stmt; … } ──
    function parseBlock(): ASTNode[] {
        expect('LBrace');
        const stmts: ASTNode[] = [];
        while (!at('RBrace') && !at('EOF')) stmts.push(parseStatement());
        expect('RBrace');
        return stmts;
    }

    function parseStatement(): ASTNode {
        if (at('Return')) return parseReturn();
        if (at('Fn')) return parseFn();
        if (at('Query')) return parseQuery();
        if (at('Module')) return parseModule();
        if (at('Infer')) return parseInfer(); if (at('Sys')) return parseSys(); if (at('Storage')) return parseStorage(); if (at('Net')) return parseNet(); if (at('Pipeline')) return parsePipeline(); if (at('Sec')) return parseSec(); if (at('Dist')) return parseDist(); if (at('Doc')) return parseDoc(); if (at('Do')) return parseDo(); if (at('Reflect')) return parseReflect(); if (at('Artifact')) return parseArtifact(); if (at('Auto')) return parseAuto(); if (at('Orch')) return parseOrch(); if (at('SelfOpt')) return parseSelfOpt(); if (at('Memory')) return parseMemory(); if (at('Narrative')) return parseNarrative(); if (at('Unify')) return parseUnify(); if (at('Compiler')) return parseCompiler(); if (at('ExecMulti')) return parseExecMulti(); if (at('Interop')) return parseInterop(); return parseExprStmt();
    }

    function parseReturn(): ASTNode {
        expect('Return');
        const value = parseExpression();
        if (at('Semi')) advance();
        return { kind: 'ReturnStmt' as any, value };
    }

    function parseExprStmt(): ASTNode {
        const expr = parseExpression();
        if (at('Semi')) advance();
        return expr;
    }

    // ── Expressions ──
    function parseExpression(): ASTNode { return parseComparison(); }

    function parseComparison(): ASTNode {
        let left = parseAdditive();
        while (at('EqEq') || at('BangEq') || at('Lt') || at('Gt') || at('LtEq') || at('GtEq')) {
            const op = advance().value;
            left = { kind: 'BinaryExpr' as any, op, left, right: parseAdditive() };
        }
        return left;
    }

    function parseAdditive(): ASTNode {
        let left = parseMultiplicative();
        while (at('Plus') || at('Minus')) {
            const op = advance().value;
            left = { kind: 'BinaryExpr' as any, op, left, right: parseMultiplicative() };
        }
        return left;
    }

    function parseMultiplicative(): ASTNode {
        let left = parsePrimary();
        while (at('Star') || at('Slash')) {
            const op = advance().value;
            left = { kind: 'BinaryExpr' as any, op, left, right: parsePrimary() };
        }
        return left;
    }

    function parsePrimary(): ASTNode {
        if (at('Number')) return { kind: 'NumberLit' as any, value: Number(advance().value) };
        if (at('String')) return { kind: 'StringLit' as any, value: advance().value };
        if (at('True')) { advance(); return { kind: 'BoolLit' as any, value: true }; }
        if (at('False')) { advance(); return { kind: 'BoolLit' as any, value: false }; }

        if (at('Ident')) {
            const name = advance().value;
            if (at('LParen')) {
                advance();
                const args: ASTNode[] = [];
                while (!at('RParen') && !at('EOF')) {
                    args.push(parseExpression());
                    if (at('Comma')) advance();
                }
                expect('RParen');
                return { kind: 'CallExpr' as any, callee: name, args };
            }
            return { kind: 'Identifier' as any, name };
        }

        if (at('LParen')) {
            advance();
            const expr = parseExpression();
            expect('RParen');
            return expr;
        }

        if (at('LBracket')) {
            advance();
            const elements: ASTNode[] = [];
            while (!at('RBracket') && !at('EOF')) {
                elements.push(parseExpression());
                if (at('Comma')) advance();
            }
            expect('RBracket');
            return { kind: 'ArrayLit' as any, elements };
        }

        if (at('LBrace')) {
            return parseObjectLiteral();
        }

        throw new SyntaxError(`Unexpected token: ${peek().type} ("${peek().value}")`);
    }

    function parseObjectLiteral(): ASTNode {
        expect('LBrace');
        const entries: { key: string; value: ASTNode }[] = [];
        while (!at('RBrace') && !at('EOF')) {
            let key: string;
            if (at('Ident')) key = advance().value;
            else if (at('String')) key = advance().value;
            else throw new SyntaxError(`Expected property name, got ${peek().type}`);
            expect('Colon');
            entries.push({ key, value: parseExpression() });
            if (at('Comma')) advance();
        }
        expect('RBrace');
        return { kind: 'ObjectLit' as any, entries };
    }

    return parseProgram();
}

// ═══════════════════════════════════════════════════════════════════════════
//  REPL FUNCTION
// ═══════════════════════════════════════════════════════════════════════════

const runtime = new GreyRuntime();

/**
 * Parse a Grey++ source string, execute it, and return the result.
 * State (defined functions, globals) persists across calls within the
 * same process.
 *
 * Stage 18.5: prefix with `normalize ` to invoke the normalizer.
 *   normalize def hello(name): print(name)
 *   normalize let x = 42
 *   normalize fn add(a, b) -> i32 { a + b }
 */
export function repl(input: string): unknown {
    const trimmed = input.trim();

    // ── Stage 18.5: normalize command ──
    if (trimmed.startsWith('normalize ')) {
        const source = trimmed.slice('normalize '.length);
        const result = runtime.runUnifyAST(source);

        // Format as the user-facing session output
        const lines: string[] = [];
        lines.push(`Input: ${source}`);
        lines.push(`Output (Grey++): ${result.unified}`);
        for (const t of result.translations) {
            if (t.target === 'grey') continue; // already shown as unified
            const label = t.target === 'javascript' ? 'JS'
                : t.target === 'python' ? 'Python'
                    : t.target === 'cpp' ? 'C++'
                        : t.target === 'rust' ? 'Rust'
                            : t.target;
            lines.push(`Output (${label}): ${t.code}`);
        }

        return {
            _type: 'NormalizeResult',
            _display: lines.join('\n'),
            detectedLang: result.detectedLang,
            detectedConstruct: result.detectedConstruct,
            normalized: result.normalized,
            unified: result.unified,
            translations: result.translations.map(t => ({
                target: t.target,
                code: t.code,
            })),
        };
    }

    const ast = parseSource(trimmed);
    return runtime.run(ast);
}

// ═══════════════════════════════════════════════════════════════════════════
//  INTERACTIVE LOOP  (runs when executed directly)
// ═══════════════════════════════════════════════════════════════════════════

function formatResult(value: unknown): string {
    if (value === undefined) return '';
    if (typeof value === 'object' && value !== null) {
        // Stage 18.5: pretty-print normalize results
        if ((value as any)._type === 'NormalizeResult' && (value as any)._display) {
            return (value as any)._display;
        }
        return JSON.stringify(value, null, 2);
    }
    return String(value);
}

function bracesBalanced(src: string): boolean {
    let depth = 0;
    for (const ch of src) {
        if (ch === '{' || ch === '[') depth++;
        if (ch === '}' || ch === ']') depth--;
        if (depth < 0) return true;
    }
    return depth === 0;
}

function startInteractive(): void {
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: 'grey++ > ',
    });

    console.log('Grey++ v0.21.0  —  Stage 21 TypeScript REPL');
    console.log('Supports: fn, query, module, infer, sys, storage, net, pipeline, sec, dist, doc, do, reflect, artifact, auto, orch, selfopt, memory, narrative, unify, compiler, execmulti, interop, expressions.  Type .exit to quit.\n');
    rl.prompt();

    let buffer = '';

    rl.on('line', (line: string) => {
        const trimmed = line.trim();
        if (trimmed === '.exit') { rl.close(); return; }

        if (trimmed === '.ast') {
            if (buffer) {
                try { console.dir(parseSource(buffer), { depth: null }); }
                catch (e: any) { console.error(e.message); }
                buffer = '';
            }
            rl.prompt();
            return;
        }

        buffer += (buffer ? '\n' : '') + line;

        if (!bracesBalanced(buffer)) {
            process.stdout.write('  ... > ');
            return;
        }

        try {
            const result = repl(buffer);
            const formatted = formatResult(result);
            if (formatted) console.log(formatted);
        } catch (err: any) {
            console.error(`Error: ${err.message}`);
        }

        buffer = '';
        rl.prompt();
    });

    rl.on('close', () => {
        console.log('\nGoodbye.');
        process.exit(0);
    });
}

// ── Self-test: run a batch of commands if invoked directly ──────────────────

function selfTest(): void {
    console.log('═══ Grey++ REPL Self-Test ═══\n');

    const commands = [
        '2 + 3',
        '"hello" + " world"',
        'fn add(a, b) { return a + b }',
        'add(10, 20)',
        'fn double(x) { return x * 2 }',
        'double(21)',
        'query { select name, age from users }',
        'query { select * from orders where status == "active" }',
        'fn outer(x) { fn inner(y) { return x + y } return inner(10) }',
        'outer(5)',
        'module react { div(span("Hello Grey++")) }',
        'module django UserAPI { fn getUsers() { return "users" } }',
        'infer model=basic dataset=demo',
        'infer model=gpt4 prompt="explain closures" temperature=0.7',
        'sys.log "Hello Grey++"',
        'sys.thread label=compute',
        'sys.message target=process1 payload="Ping"',
        'storage pdf load "demo.pdf"',
        'storage pdf query "heading"',
        'storage pdf edit node=2 text="Updated Title"',
        'storage db query "SELECT * FROM users"',
        'storage file load "config.json"',
        'net.http GET "https://example.com"',
        'net.http POST "https://api.example.com/data" body="payload"',
        'net.socket connect host=localhost port=8080',
        'net.rpc call service=math method=add args=[1,2]',
        'pipeline { infer model=basic dataset=demo store backend=pdf file="demo.pdf" net.http POST "https://api.example.com" }',
        'sec.perm user=alice action=net.http',
        'sec.sandbox "rm -rf testdir"',
        'sec.audit message="Pipeline executed" level=info',
        'dist.register node=alpha',
        'dist.consensus proposal="upgrade pipeline"',
        'dist.sync barrier="stage8"',
        'doc.parse file="report.pdf"',
        'doc.render doc="report" format=html',
        'doc.edit doc="report" target=heading.1 value="New Title"',
        'do { net.http GET "https://api.example.com" sec.perm user=alice action=doc.edit doc.edit doc="report" target=heading.1 value="New Title" }',
        'reflect.pipeline id="p1"',
        'reflect.grammar rule="doc.edit"',
        'reflect.trust policy="sec.perm"',
        'reflect.dist cluster="alpha"',
        'artifact.density target="pipeline1"',
        'artifact.rarity target="doc.edit"',
        'artifact.proof target="dist.consensus"',
        'auto.safe command="ls"',
        'auto.sandbox command="rm -rf testdir"',
        'auto.prompt command="git reset --hard"',
        'orch.pipeline steps="net.http, doc.edit, dist.consensus"',
        'orch.cross modules="pipeline, security, artifact"',
        'orch.global scenario="civilization-tier coordination"',
        'selfopt.pipeline target="pipeline1" strategy="latency"',
        'selfopt.trust target="sec.perm" threshold=0.9',
        'selfopt.artifact target="density" mode="aggressive"',
        'memory.store key="pipeline1" value="optimized"',
        'memory.recall key="doc.edit"',
        'memory.evolve key="artifact.rarity"',
        'narrative.proof source="artifact.rarity"',
        'narrative.story theme="civilization-tier orchestration"',
        'narrative.log stage="Stage 16"',
        'unify.syntax modules="pipeline, orchestration, memory"',
        'unify.semantic source="artifact.rarity"',
        'unify.exec scenario="civilization-tier orchestration"',
        'compiler.syntax source="pipeline, orchestration"',
        'compiler.backend target="javascript" optimize=true',
        'compiler.exec scenario="full-stack deployment"',
        'execmulti.js program="console.log(\'Hello Grey++\')"',
        'execmulti.cpp program="int main(){return 0;}"',
        'execmulti.riscv program="addi x1, x0, 5"',
        'interop.data source="execmulti.js" target="execmulti.cpp"',
        'interop.call source="execmulti.cpp" target="execmulti.riscv"',
        'interop.orch modules="js, cpp, riscv"',
        // Stage 18.5: normalize commands — exact spec examples
        'normalize def hello(): print("hi")',
        'normalize function hello(){ console.log("hi"); }',
        'normalize void hello(){ std::cout << "hi"; }',
        'normalize for i in range(10): print(i)',
        'normalize for(let i=0;i<10;i++){console.log(i);}',
        'normalize let x = 42',
        'normalize if x > 5: print("big")',
        'normalize bind result = compute()',
        'normalize fn add(a, b) { return a + b }',
        'print("all tests passed")',
    ];

    for (const cmd of commands) {
        console.log(`grey++ > ${cmd}`);
        try {
            const result = repl(cmd);
            const formatted = formatResult(result);
            if (formatted) console.log(formatted);
        } catch (err: any) {
            console.error(`Error: ${err.message}`);
        }
        console.log();
    }
}

// ── Entry point ─────────────────────────────────────────────────────────────

const args = process.argv.slice(2);

if (args.includes('--test')) {
    selfTest();
} else if (process.stdin.isTTY) {
    startInteractive();
} else {
    // Piped input — read all then evaluate line-by-line.
    let piped = '';
    process.stdin.setEncoding('utf8');
    process.stdin.on('data', (chunk: string) => { piped += chunk; });
    process.stdin.on('end', () => {
        for (const line of piped.split('\n').filter(l => l.trim())) {
            try {
                const result = repl(line);
                const formatted = formatResult(result);
                if (formatted) console.log(formatted);
            } catch (err: any) {
                console.error(`Error: ${err.message}`);
            }
        }
    });
}
