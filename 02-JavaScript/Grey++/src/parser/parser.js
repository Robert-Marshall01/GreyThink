// ─── Grey++ Parser ───────────────────────────────────────────────────────────
// Recursive-descent parser that turns a token stream into the universal AST.
// Stage 1 supports two top-level constructs: `fn` and `query`.

import { tokenize, TokenType } from './lexer.js';
import * as AST from '../ast/types.js';
import { NodeKind } from '../ast/types.js';

export function parse(source) {
    const tokens = tokenize(source);
    let pos = 0;

    // ── Helpers ──────────────────────────────────────────────────────────────
    const peek = () => tokens[pos];
    const advance = () => tokens[pos++];
    const at = (t) => peek().type === t;

    // Set of keyword token types that can also be used as identifiers
    const KEYWORD_IDENT = new Set([
        TokenType.Fn, TokenType.Query, TokenType.Select,
        TokenType.From, TokenType.Where, TokenType.Return,
        TokenType.True, TokenType.False,
    ]);

    /** Accept an identifier OR a keyword-as-identifier */
    function expectIdent() {
        const tok = peek();
        if (tok.type === TokenType.Ident || KEYWORD_IDENT.has(tok.type)) {
            advance();
            return tok;
        }
        throw new SyntaxError(`Expected identifier, got ${tok.type} ("${tok.value}")`);
    }

    function atIdent() {
        return at(TokenType.Ident) || KEYWORD_IDENT.has(peek().type);
    }

    function expect(type) {
        const tok = advance();
        if (tok.type !== type) {
            throw new SyntaxError(`Expected ${type}, got ${tok.type} ("${tok.value}")`);
        }
        return tok;
    }

    // ── Top-level program ──────────────────────────────────────────────────
    function parseProgram() {
        const body = [];
        while (!at(TokenType.EOF)) {
            body.push(parseTopLevel());
        }
        return AST.Program(body);
    }

    function parseTopLevel() {
        if (at(TokenType.Fn)) {
            // fn name(…) is a declaration; fn(…) is an anonymous expression
            const next = tokens[pos + 1];
            if (next && next.type !== TokenType.LParen) return parseFnDecl();
        }
        if (at(TokenType.Query)) return parseQuery();
        // Allow bare expressions / statements at top level (handy in the REPL)
        return parseExpressionStatement();
    }

    // ── fn name(params) { body } ──────────────────────────────────────────
    function parseFnDecl() {
        expect(TokenType.Fn);
        const name = expectIdent().value;
        expect(TokenType.LParen);

        const params = [];
        while (!at(TokenType.RParen)) {
            params.push(expectIdent().value);
            if (at(TokenType.Comma)) advance();
        }
        expect(TokenType.RParen);
        const body = parseBlock();
        return AST.FnDecl(name, params, body);
    }

    // ── query { select ... from ... where ... } ──────────────────────────
    function parseQuery() {
        expect(TokenType.Query);
        expect(TokenType.LBrace);

        let select = null;
        let from = null;
        let where = null;

        while (!at(TokenType.RBrace) && !at(TokenType.EOF)) {
            if (at(TokenType.Select)) {
                advance();
                const cols = [];
                cols.push(parseColumnRef());
                while (at(TokenType.Comma)) { advance(); cols.push(parseColumnRef()); }
                select = AST.SelectClause(cols);
            } else if (at(TokenType.From)) {
                advance();
                from = AST.FromClause(expect(TokenType.Ident).value);
            } else if (at(TokenType.Where)) {
                advance();
                where = AST.WhereClause(parseExpression());
            } else {
                throw new SyntaxError(`Unexpected token in query: ${peek().value}`);
            }
        }
        expect(TokenType.RBrace);

        if (!select) throw new SyntaxError('query requires a select clause');
        return AST.Query({ select, from, where });
    }

    function parseColumnRef() {
        if (at(TokenType.Star)) { advance(); return '*'; }
        return expect(TokenType.Ident).value;
    }

    // ── Block { stmt; stmt; ... } ─────────────────────────────────────────
    function parseBlock() {
        expect(TokenType.LBrace);
        const stmts = [];
        while (!at(TokenType.RBrace) && !at(TokenType.EOF)) {
            stmts.push(parseStatement());
        }
        expect(TokenType.RBrace);
        return stmts;
    }

    // ── Statements ────────────────────────────────────────────────────────
    function parseStatement() {
        if (at(TokenType.Return)) return parseReturn();
        if (at(TokenType.Fn)) {
            // fn name(…) is a declaration; fn(…) is an anonymous expression statement
            const next = tokens[pos + 1];
            if (next && next.type !== TokenType.LParen) return parseFnDecl();
        }
        if (at(TokenType.Query)) return parseQuery();
        return parseExpressionStatement();
    }

    function parseReturn() {
        expect(TokenType.Return);
        const value = parseExpression();
        if (at(TokenType.Semi)) advance();
        return AST.ReturnStmt(value);
    }

    function parseExpressionStatement() {
        const expr = parseExpression();
        if (at(TokenType.Semi)) advance();
        return expr;
    }

    // ── Expressions (precedence climbing) ─────────────────────────────────
    function parseExpression() {
        return parseComparison();
    }

    function parseComparison() {
        let left = parseAdditive();
        while (
            at(TokenType.EqEq) || at(TokenType.BangEq) ||
            at(TokenType.Lt) || at(TokenType.Gt) ||
            at(TokenType.LtEq) || at(TokenType.GtEq)
        ) {
            const op = advance().value;
            left = AST.BinaryExpr(op, left, parseAdditive());
        }
        return left;
    }

    function parseAdditive() {
        let left = parseMultiplicative();
        while (at(TokenType.Plus) || at(TokenType.Minus)) {
            const op = advance().value;
            left = AST.BinaryExpr(op, left, parseMultiplicative());
        }
        return left;
    }

    function parseMultiplicative() {
        let left = parsePostfix();
        while (at(TokenType.Star) || at(TokenType.Slash)) {
            const op = advance().value;
            left = AST.BinaryExpr(op, left, parsePostfix());
        }
        return left;
    }

    // ── Postfix: chained calls  expr(args)(args)… ─────────────────────────
    function parsePostfix() {
        let expr = parseUnary();
        while (at(TokenType.LParen)) {
            advance();
            const args = [];
            while (!at(TokenType.RParen) && !at(TokenType.EOF)) {
                args.push(parseExpression());
                if (at(TokenType.Comma)) advance();
            }
            expect(TokenType.RParen);
            // Keep simple string callee for plain name(…) calls
            if (expr.kind === NodeKind.Identifier) {
                expr = AST.CallExpr(expr.name, args);
            } else {
                expr = AST.CallExpr(expr, args);
            }
        }
        return expr;
    }

    // ── Unary: -expr, !expr ───────────────────────────────────────────────
    function parseUnary() {
        if (at(TokenType.Minus)) {
            advance();
            const operand = parseUnary();
            return AST.BinaryExpr('-', AST.NumberLit(0), operand);
        }
        return parsePrimary();
    }

    function parsePrimary() {
        // ── number ──
        if (at(TokenType.Number)) {
            return AST.NumberLit(Number(advance().value));
        }
        // ── string ──
        if (at(TokenType.String)) {
            return AST.StringLit(advance().value);
        }
        // ── bool ──
        if (at(TokenType.True)) { advance(); return AST.BoolLit(true); }
        if (at(TokenType.False)) { advance(); return AST.BoolLit(false); }
        // ── identifier (calls are handled by parsePostfix) ──
        if (at(TokenType.Ident) || at(TokenType.From) || at(TokenType.Select) || at(TokenType.Where)) {
            return AST.Identifier(advance().value);
        }
        // ── anonymous function expression: fn(params) { body } ──
        if (at(TokenType.Fn)) {
            return parseAnonFn();
        }
        // ── parenthesised expression ──
        if (at(TokenType.LParen)) {
            advance();
            const expr = parseExpression();
            expect(TokenType.RParen);
            return expr;
        }
        // ── array literal ──
        if (at(TokenType.LBracket)) {
            advance();
            const elements = [];
            while (!at(TokenType.RBracket) && !at(TokenType.EOF)) {
                elements.push(parseExpression());
                if (at(TokenType.Comma)) advance();
            }
            expect(TokenType.RBracket);
            return AST.ArrayLit(elements);
        }
        // ── object literal (only at expression level, not top-level block) ──
        if (at(TokenType.LBrace)) {
            return parseObjectLiteral();
        }

        throw new SyntaxError(`Unexpected token: ${peek().type} ("${peek().value}")`);
    }

    // ── fn(params) { body }  (anonymous / lambda) ─────────────────────────
    function parseAnonFn() {
        expect(TokenType.Fn);
        expect(TokenType.LParen);
        const params = [];
        while (!at(TokenType.RParen)) {
            params.push(expectIdent().value);
            if (at(TokenType.Comma)) advance();
        }
        expect(TokenType.RParen);
        const body = parseBlock();
        return AST.FnDecl(null, params, body);
    }

    function parseObjectLiteral() {
        expect(TokenType.LBrace);
        const entries = [];
        while (!at(TokenType.RBrace) && !at(TokenType.EOF)) {
            let key;
            if (atIdent()) {
                key = advance().value;
            } else if (at(TokenType.String)) {
                key = advance().value;
            } else {
                throw new SyntaxError(`Expected property name, got ${peek().type}`);
            }
            expect(TokenType.Colon);
            const value = parseExpression();
            entries.push({ key, value });
            if (at(TokenType.Comma)) advance();
        }
        expect(TokenType.RBrace);
        return AST.ObjectLit(entries);
    }

    return parseProgram();
}
