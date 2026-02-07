"""
Recursive Descent Parser for the Grey language.

Consumes a token stream from the Lexer and builds an AST.
Enforces grammar rules and detects syntax errors.

Grey Grammar (simplified EBNF):
    program     = (declaration | statement)* EOF
    declaration = var_decl | fn_decl | struct_decl | enum_decl
                | impl_decl | trait_decl | import_decl | type_alias
    var_decl    = ("let" ["mut"] | "const") IDENT [":" type] "=" expr ";"
    fn_decl     = ["pub"] "fn" IDENT "(" params ")" ["->" type] block
    struct_decl = ["pub"] "struct" IDENT "{" struct_fields "}"
    enum_decl   = ["pub"] "enum" IDENT "{" enum_variants "}"
    impl_decl   = "impl" [IDENT "for"] IDENT "{" fn_decl* "}"
    trait_decl  = ["pub"] "trait" IDENT "{" fn_signature* "}"
    import_decl = "import" IDENT ("." IDENT)* ";" | "from" ... "import" ...
    type_alias  = "type" IDENT "=" type ";"
    statement   = if_stmt | while_stmt | for_stmt | return_stmt
                | break_stmt | continue_stmt | match_stmt
                | expr_stmt | block
    expr        = assignment
    assignment  = or_expr (("=" | "+=" | ...) assignment)?
    or_expr     = and_expr ("or" and_expr)*
    and_expr    = equality ("and" equality)*
    equality    = comparison (("==" | "!=") comparison)*
    comparison  = addition (("<" | ">" | "<=" | ">=") addition)*
    addition    = mult (("+" | "-") mult)*
    mult        = unary (("*" | "/" | "%") unary)*
    unary       = ("-" | "not" | "~") unary | power
    power       = call ("**" unary)?
    call        = primary ( "(" args ")" | "[" expr "]" | "." IDENT )*
    primary     = INT | FLOAT | STRING | BOOL | NIL | IDENT
                | "(" expr ")" | "[" elements "]" | struct_literal
"""

from typing import Optional
from .tokens import Token, TokenType
from .ast_nodes import *


class ParseError(Exception):
    """Raised for syntax errors during parsing."""

    def __init__(self, message: str, token: Token):
        self.token = token
        loc = token.location
        super().__init__(f"{loc}: Syntax error: {message}")


class Parser:
    """
    Recursive descent parser for Grey.

    Usage:
        parser = Parser(tokens)
        program = parser.parse()
    """

    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.pos = 0
        self.errors: list[ParseError] = []

    # ── Token Helpers ────────────────────────────────────────

    def _current(self) -> Token:
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        return self.tokens[-1]  # EOF

    def _peek(self, offset: int = 1) -> Token:
        idx = self.pos + offset
        if idx < len(self.tokens):
            return self.tokens[idx]
        return self.tokens[-1]

    def _at_end(self) -> bool:
        return self._current().type == TokenType.EOF

    def _check(self, *types: TokenType) -> bool:
        return self._current().type in types

    def _advance(self) -> Token:
        token = self._current()
        if not self._at_end():
            self.pos += 1
        return token

    def _match(self, *types: TokenType) -> Optional[Token]:
        if self._current().type in types:
            return self._advance()
        return None

    def _expect(self, ttype: TokenType, message: str = "") -> Token:
        if self._current().type == ttype:
            return self._advance()
        if not message:
            message = f"Expected {ttype.name}, got {self._current().type.name}"
        raise self._error(message)

    def _error(self, message: str) -> ParseError:
        err = ParseError(message, self._current())
        self.errors.append(err)
        return err

    def _synchronize(self):
        """Skip tokens until we find a statement boundary (error recovery)."""
        self._advance()
        while not self._at_end():
            if self.tokens[self.pos - 1].type == TokenType.SEMICOLON:
                return
            if self._current().type in (
                TokenType.FN, TokenType.LET, TokenType.CONST,
                TokenType.STRUCT, TokenType.ENUM, TokenType.IMPL,
                TokenType.TRAIT, TokenType.IF, TokenType.WHILE,
                TokenType.FOR, TokenType.RETURN, TokenType.IMPORT,
            ):
                return
            self._advance()

    # ══════════════════════════════════════════════════════════
    #  Top-level Parsing
    # ══════════════════════════════════════════════════════════

    def parse(self) -> Program:
        """Parse the entire token stream into a Program AST."""
        program = Program(location=self._current().location)
        program.declarations = []
        program.statements = []

        while not self._at_end():
            try:
                node = self._parse_top_level()
                if isinstance(node, Declaration):
                    program.declarations.append(node)
                else:
                    program.statements.append(node)
            except ParseError:
                self._synchronize()

        return program

    def _parse_top_level(self) -> ASTNode:
        """Parse a top-level declaration or statement."""
        # Check for pub modifier
        if self._check(TokenType.PUB):
            return self._parse_pub_declaration()

        tt = self._current().type

        if tt == TokenType.LET:
            return self._parse_var_decl()
        if tt == TokenType.CONST:
            return self._parse_const_decl()
        if tt == TokenType.FN:
            return self._parse_fn_decl()
        if tt == TokenType.STRUCT:
            return self._parse_struct_decl()
        if tt == TokenType.ENUM:
            return self._parse_enum_decl()
        if tt == TokenType.IMPL:
            return self._parse_impl_decl()
        if tt == TokenType.TRAIT:
            return self._parse_trait_decl()
        if tt == TokenType.IMPORT:
            return self._parse_import_decl()
        if tt == TokenType.FROM:
            return self._parse_from_import_decl()
        if tt == TokenType.TYPE:
            return self._parse_type_alias()

        return self._parse_statement()

    def _parse_pub_declaration(self) -> Declaration:
        self._advance()  # consume 'pub'
        tt = self._current().type
        if tt == TokenType.FN:
            decl = self._parse_fn_decl()
            decl.is_public = True
            return decl
        elif tt == TokenType.STRUCT:
            decl = self._parse_struct_decl()
            decl.is_public = True
            return decl
        elif tt == TokenType.ENUM:
            decl = self._parse_enum_decl()
            decl.is_public = True
            return decl
        elif tt == TokenType.TRAIT:
            decl = self._parse_trait_decl()
            decl.is_public = True
            return decl
        else:
            raise self._error("Expected declaration after 'pub'")

    # ══════════════════════════════════════════════════════════
    #  Declarations
    # ══════════════════════════════════════════════════════════

    def _parse_var_decl(self) -> VarDecl:
        """let [mut] name [: type] = expr ;"""
        loc = self._current().location
        self._expect(TokenType.LET)

        mutable = bool(self._match(TokenType.MUT))

        name_tok = self._expect(TokenType.IDENTIFIER, "Expected variable name")
        name = name_tok.value

        type_ann = None
        if self._match(TokenType.COLON):
            type_ann = self._parse_type()

        initializer = None
        if self._match(TokenType.ASSIGN):
            initializer = self._parse_expression()

        self._expect(TokenType.SEMICOLON, "Expected ';' after variable declaration")

        return VarDecl(
            location=loc, name=name, type_annotation=type_ann,
            initializer=initializer, mutable=mutable, is_const=False
        )

    def _parse_const_decl(self) -> VarDecl:
        """const NAME = expr ;"""
        loc = self._current().location
        self._expect(TokenType.CONST)

        name_tok = self._expect(TokenType.IDENTIFIER, "Expected constant name")
        name = name_tok.value

        type_ann = None
        if self._match(TokenType.COLON):
            type_ann = self._parse_type()

        self._expect(TokenType.ASSIGN, "Expected '=' in constant declaration")
        initializer = self._parse_expression()

        self._expect(TokenType.SEMICOLON, "Expected ';' after constant declaration")

        return VarDecl(
            location=loc, name=name, type_annotation=type_ann,
            initializer=initializer, mutable=False, is_const=True
        )

    def _parse_fn_decl(self, is_method: bool = False) -> FnDecl:
        """fn name(params) [-> type] block"""
        loc = self._current().location
        self._expect(TokenType.FN)

        name_tok = self._expect(TokenType.IDENTIFIER, "Expected function name")
        name = name_tok.value

        self._expect(TokenType.LPAREN, "Expected '(' after function name")
        params = self._parse_params()
        self._expect(TokenType.RPAREN, "Expected ')' after parameters")

        return_type = None
        if self._match(TokenType.ARROW):
            return_type = self._parse_type()

        body = self._parse_block()

        return FnDecl(
            location=loc, name=name, params=params,
            return_type=return_type, body=body,
            is_public=False, is_method=is_method
        )

    def _parse_params(self) -> list[Parameter]:
        params = []
        if self._check(TokenType.RPAREN):
            return params

        params.append(self._parse_param())
        while self._match(TokenType.COMMA):
            if self._check(TokenType.RPAREN):
                break
            params.append(self._parse_param())

        return params

    def _parse_param(self) -> Parameter:
        loc = self._current().location

        # Handle 'self' parameter for methods
        if self._check(TokenType.SELF):
            self._advance()
            return Parameter(location=loc, name="self", type_annotation=SimpleType(name="self"))

        name_tok = self._expect(TokenType.IDENTIFIER, "Expected parameter name")
        self._expect(TokenType.COLON, "Expected ':' after parameter name")
        type_ann = self._parse_type()

        default = None
        if self._match(TokenType.ASSIGN):
            default = self._parse_expression()

        return Parameter(
            location=loc, name=name_tok.value,
            type_annotation=type_ann, default_value=default
        )

    def _parse_struct_decl(self) -> StructDecl:
        """struct Name { field: type, ... }"""
        loc = self._current().location
        self._expect(TokenType.STRUCT)
        name_tok = self._expect(TokenType.IDENTIFIER, "Expected struct name")

        self._expect(TokenType.LBRACE, "Expected '{' after struct name")

        fields = []
        while not self._check(TokenType.RBRACE) and not self._at_end():
            is_pub = bool(self._match(TokenType.PUB))
            field_name = self._expect(TokenType.IDENTIFIER, "Expected field name")
            self._expect(TokenType.COLON, "Expected ':' after field name")
            field_type = self._parse_type()
            fields.append(StructField(
                location=field_name.location,
                name=field_name.value,
                type_annotation=field_type,
                is_public=is_pub
            ))
            if not self._match(TokenType.COMMA):
                break

        self._expect(TokenType.RBRACE, "Expected '}' after struct fields")

        return StructDecl(location=loc, name=name_tok.value, fields=fields)

    def _parse_enum_decl(self) -> EnumDecl:
        """enum Name { Variant1, Variant2(type), ... }"""
        loc = self._current().location
        self._expect(TokenType.ENUM)
        name_tok = self._expect(TokenType.IDENTIFIER, "Expected enum name")

        self._expect(TokenType.LBRACE, "Expected '{' after enum name")

        variants = []
        while not self._check(TokenType.RBRACE) and not self._at_end():
            var_name = self._expect(TokenType.IDENTIFIER, "Expected variant name")
            var_fields = []
            if self._match(TokenType.LPAREN):
                if not self._check(TokenType.RPAREN):
                    var_fields.append(self._parse_type())
                    while self._match(TokenType.COMMA):
                        var_fields.append(self._parse_type())
                self._expect(TokenType.RPAREN, "Expected ')' after variant fields")
            variants.append(EnumVariant(
                location=var_name.location,
                name=var_name.value, fields=var_fields
            ))
            if not self._match(TokenType.COMMA):
                break

        self._expect(TokenType.RBRACE, "Expected '}' after enum variants")

        return EnumDecl(location=loc, name=name_tok.value, variants=variants)

    def _parse_impl_decl(self) -> ImplDecl:
        """impl [TraitName for] TypeName { methods... }"""
        loc = self._current().location
        self._expect(TokenType.IMPL)

        first_name = self._expect(TokenType.IDENTIFIER, "Expected type name")

        trait_name = None
        if self._match(TokenType.FOR):
            # impl Trait for Type
            trait_name = first_name.value
            target_tok = self._expect(TokenType.IDENTIFIER, "Expected type name after 'for'")
            target = target_tok.value
        else:
            target = first_name.value

        self._expect(TokenType.LBRACE, "Expected '{' after impl target")

        methods = []
        while not self._check(TokenType.RBRACE) and not self._at_end():
            is_pub = bool(self._match(TokenType.PUB))
            method = self._parse_fn_decl(is_method=True)
            method.is_public = is_pub
            methods.append(method)

        self._expect(TokenType.RBRACE, "Expected '}' after impl block")

        return ImplDecl(
            location=loc, target=target,
            trait_name=trait_name, methods=methods
        )

    def _parse_trait_decl(self) -> TraitDecl:
        """trait Name { fn signatures... }"""
        loc = self._current().location
        self._expect(TokenType.TRAIT)
        name_tok = self._expect(TokenType.IDENTIFIER, "Expected trait name")

        self._expect(TokenType.LBRACE, "Expected '{' after trait name")

        methods = []
        while not self._check(TokenType.RBRACE) and not self._at_end():
            method = self._parse_fn_decl()
            methods.append(method)

        self._expect(TokenType.RBRACE, "Expected '}' after trait methods")

        return TraitDecl(location=loc, name=name_tok.value, methods=methods)

    def _parse_import_decl(self) -> ImportDecl:
        """import module.path ;"""
        loc = self._current().location
        self._expect(TokenType.IMPORT)

        path_parts = [self._expect(TokenType.IDENTIFIER, "Expected module name").value]
        while self._match(TokenType.DOT):
            path_parts.append(
                self._expect(TokenType.IDENTIFIER, "Expected module name").value
            )

        alias = None
        if self._match(TokenType.AS):
            alias = self._expect(TokenType.IDENTIFIER, "Expected alias name").value

        self._expect(TokenType.SEMICOLON, "Expected ';' after import")

        return ImportDecl(
            location=loc, module_path=".".join(path_parts), alias=alias
        )

    def _parse_from_import_decl(self) -> ImportDecl:
        """from module import name1, name2 ;"""
        loc = self._current().location
        self._expect(TokenType.FROM)

        path_parts = [self._expect(TokenType.IDENTIFIER, "Expected module name").value]
        while self._match(TokenType.DOT):
            path_parts.append(
                self._expect(TokenType.IDENTIFIER, "Expected module name").value
            )

        self._expect(TokenType.IMPORT, "Expected 'import' after module path")

        names = []
        name_tok = self._expect(TokenType.IDENTIFIER, "Expected import name")
        alias = None
        if self._match(TokenType.AS):
            alias = self._expect(TokenType.IDENTIFIER).value
        names.append(ImportName(location=name_tok.location, name=name_tok.value, alias=alias))

        while self._match(TokenType.COMMA):
            name_tok = self._expect(TokenType.IDENTIFIER, "Expected import name")
            alias = None
            if self._match(TokenType.AS):
                alias = self._expect(TokenType.IDENTIFIER).value
            names.append(ImportName(location=name_tok.location, name=name_tok.value, alias=alias))

        self._expect(TokenType.SEMICOLON, "Expected ';' after import")

        return ImportDecl(
            location=loc, module_path=".".join(path_parts), names=names
        )

    def _parse_type_alias(self) -> TypeAlias:
        """type Name = Type ;"""
        loc = self._current().location
        self._expect(TokenType.TYPE)
        name_tok = self._expect(TokenType.IDENTIFIER, "Expected type name")
        self._expect(TokenType.ASSIGN, "Expected '=' after type name")
        aliased = self._parse_type()
        self._expect(TokenType.SEMICOLON, "Expected ';' after type alias")
        return TypeAlias(location=loc, name=name_tok.value, aliased_type=aliased)

    # ══════════════════════════════════════════════════════════
    #  Type Parsing
    # ══════════════════════════════════════════════════════════

    def _parse_type(self) -> GreyType:
        """Parse a type annotation."""
        loc = self._current().location

        # Array type: [int]
        if self._match(TokenType.LBRACKET):
            elem = self._parse_type()
            self._expect(TokenType.RBRACKET, "Expected ']' in array type")
            base = ArrayType(location=loc, element_type=elem)
        # Function type: fn(int, int) -> int
        elif self._match(TokenType.FN):
            self._expect(TokenType.LPAREN, "Expected '(' in function type")
            param_types = []
            if not self._check(TokenType.RPAREN):
                param_types.append(self._parse_type())
                while self._match(TokenType.COMMA):
                    param_types.append(self._parse_type())
            self._expect(TokenType.RPAREN, "Expected ')' in function type")
            ret = None
            if self._match(TokenType.ARROW):
                ret = self._parse_type()
            base = FunctionType(location=loc, param_types=param_types, return_type=ret)
        # Simple named types: int, float, string, bool, void, or user type
        elif self._check(TokenType.INT_TYPE, TokenType.FLOAT_TYPE,
                         TokenType.STRING_TYPE, TokenType.BOOL_TYPE,
                         TokenType.CHAR_TYPE, TokenType.VOID_TYPE,
                         TokenType.IDENTIFIER):
            tok = self._advance()
            base = SimpleType(location=loc, name=tok.value)
        else:
            raise self._error(f"Expected type, got {self._current().type.name}")

        # Optional type suffix: int?
        if self._match(TokenType.QUESTION):
            base = OptionalType(location=loc, inner_type=base)

        return base

    # ══════════════════════════════════════════════════════════
    #  Statements
    # ══════════════════════════════════════════════════════════

    def _parse_statement(self) -> Statement:
        """Parse a single statement."""
        tt = self._current().type

        if tt == TokenType.IF:
            return self._parse_if_stmt()
        if tt == TokenType.WHILE:
            return self._parse_while_stmt()
        if tt == TokenType.FOR:
            return self._parse_for_stmt()
        if tt == TokenType.RETURN:
            return self._parse_return_stmt()
        if tt == TokenType.BREAK:
            return self._parse_break_stmt()
        if tt == TokenType.CONTINUE:
            return self._parse_continue_stmt()
        if tt == TokenType.MATCH:
            return self._parse_match_stmt()
        if tt == TokenType.LBRACE:
            return self._parse_block()
        if tt == TokenType.LET:
            return self._parse_var_decl()
        if tt == TokenType.CONST:
            return self._parse_const_decl()

        return self._parse_expr_stmt()

    def _parse_block(self) -> Block:
        """{ statements... }"""
        loc = self._current().location
        self._expect(TokenType.LBRACE, "Expected '{'")

        stmts = []
        while not self._check(TokenType.RBRACE) and not self._at_end():
            stmts.append(self._parse_statement())

        self._expect(TokenType.RBRACE, "Expected '}'")
        return Block(location=loc, statements=stmts)

    def _parse_if_stmt(self) -> IfStmt:
        """if cond { ... } [elif cond { ... }]* [else { ... }]"""
        loc = self._current().location
        self._expect(TokenType.IF)

        condition = self._parse_expression()
        then_body = self._parse_block()

        elif_clauses = []
        while self._match(TokenType.ELIF):
            elif_loc = self.tokens[self.pos - 1].location
            elif_cond = self._parse_expression()
            elif_body = self._parse_block()
            elif_clauses.append(ElifClause(
                location=elif_loc, condition=elif_cond, body=elif_body
            ))

        else_body = None
        if self._match(TokenType.ELSE):
            else_body = self._parse_block()

        return IfStmt(
            location=loc, condition=condition,
            then_body=then_body, elif_clauses=elif_clauses,
            else_body=else_body
        )

    def _parse_while_stmt(self) -> WhileStmt:
        """while cond { ... }"""
        loc = self._current().location
        self._expect(TokenType.WHILE)
        condition = self._parse_expression()
        body = self._parse_block()
        return WhileStmt(location=loc, condition=condition, body=body)

    def _parse_for_stmt(self) -> ForStmt:
        """for var in expr { ... }"""
        loc = self._current().location
        self._expect(TokenType.FOR)
        var_tok = self._expect(TokenType.IDENTIFIER, "Expected loop variable")
        self._expect(TokenType.IN, "Expected 'in' in for loop")
        iterable = self._parse_expression()
        body = self._parse_block()
        return ForStmt(
            location=loc, variable=var_tok.value,
            iterable=iterable, body=body
        )

    def _parse_return_stmt(self) -> ReturnStmt:
        """return [expr] ;"""
        loc = self._current().location
        self._expect(TokenType.RETURN)

        value = None
        if not self._check(TokenType.SEMICOLON, TokenType.RBRACE):
            value = self._parse_expression()

        self._match(TokenType.SEMICOLON)  # optional semicolon
        return ReturnStmt(location=loc, value=value)

    def _parse_break_stmt(self) -> BreakStmt:
        loc = self._current().location
        self._expect(TokenType.BREAK)
        self._match(TokenType.SEMICOLON)
        return BreakStmt(location=loc)

    def _parse_continue_stmt(self) -> ContinueStmt:
        loc = self._current().location
        self._expect(TokenType.CONTINUE)
        self._match(TokenType.SEMICOLON)
        return ContinueStmt(location=loc)

    def _parse_match_stmt(self) -> MatchStmt:
        """match expr { pattern => body, ... }"""
        loc = self._current().location
        self._expect(TokenType.MATCH)
        subject = self._parse_expression()

        self._expect(TokenType.LBRACE, "Expected '{' after match expression")

        arms = []
        while not self._check(TokenType.RBRACE) and not self._at_end():
            arm_loc = self._current().location
            pattern = self._parse_expression()
            self._expect(TokenType.FAT_ARROW, "Expected '=>' in match arm")

            if self._check(TokenType.LBRACE):
                body = self._parse_block()
            else:
                body = ExpressionStmt(
                    location=self._current().location,
                    expression=self._parse_expression()
                )

            arms.append(MatchArm(location=arm_loc, pattern=pattern, body=body))

            if not self._match(TokenType.COMMA):
                break

        self._expect(TokenType.RBRACE, "Expected '}' after match arms")

        return MatchStmt(location=loc, subject=subject, arms=arms)

    def _parse_expr_stmt(self) -> ExpressionStmt:
        """expression ;"""
        loc = self._current().location
        expr = self._parse_expression()
        self._match(TokenType.SEMICOLON)  # optional semicolon
        return ExpressionStmt(location=loc, expression=expr)

    # ══════════════════════════════════════════════════════════
    #  Expression Parsing (Pratt / precedence climbing)
    # ══════════════════════════════════════════════════════════

    def _parse_expression(self) -> Expression:
        """Parse an expression (entry point)."""
        return self._parse_assignment()

    def _parse_assignment(self) -> Expression:
        """assignment = or_expr (assign_op assignment)?"""
        expr = self._parse_or()

        if self._check(TokenType.ASSIGN, TokenType.PLUS_ASSIGN,
                       TokenType.MINUS_ASSIGN, TokenType.STAR_ASSIGN,
                       TokenType.SLASH_ASSIGN, TokenType.PERCENT_ASSIGN):
            op_tok = self._advance()
            value = self._parse_assignment()  # right-associative
            return AssignExpr(
                location=op_tok.location,
                target=expr, operator=op_tok.lexeme, value=value
            )

        return expr

    def _parse_or(self) -> Expression:
        """or_expr = and_expr ("or" and_expr)*"""
        expr = self._parse_and()
        while self._match(TokenType.OR):
            right = self._parse_and()
            expr = BinaryExpr(
                location=expr.location,
                left=expr, operator="or", right=right
            )
        return expr

    def _parse_and(self) -> Expression:
        """and_expr = bitwise_or ("and" bitwise_or)*"""
        expr = self._parse_bitwise_or()
        while self._match(TokenType.AND):
            right = self._parse_bitwise_or()
            expr = BinaryExpr(
                location=expr.location,
                left=expr, operator="and", right=right
            )
        return expr

    def _parse_bitwise_or(self) -> Expression:
        expr = self._parse_bitwise_xor()
        while self._match(TokenType.PIPE):
            right = self._parse_bitwise_xor()
            expr = BinaryExpr(
                location=expr.location, left=expr, operator="|", right=right
            )
        return expr

    def _parse_bitwise_xor(self) -> Expression:
        expr = self._parse_bitwise_and()
        while self._match(TokenType.CARET):
            right = self._parse_bitwise_and()
            expr = BinaryExpr(
                location=expr.location, left=expr, operator="^", right=right
            )
        return expr

    def _parse_bitwise_and(self) -> Expression:
        expr = self._parse_equality()
        while self._match(TokenType.AMPERSAND):
            right = self._parse_equality()
            expr = BinaryExpr(
                location=expr.location, left=expr, operator="&", right=right
            )
        return expr

    def _parse_equality(self) -> Expression:
        """equality = comparison (("==" | "!=") comparison)*"""
        expr = self._parse_comparison()
        while self._check(TokenType.EQ, TokenType.NEQ):
            op = self._advance()
            right = self._parse_comparison()
            expr = BinaryExpr(
                location=expr.location,
                left=expr, operator=op.lexeme, right=right
            )
        return expr

    def _parse_comparison(self) -> Expression:
        """comparison = shift (("<" | ">" | "<=" | ">=") shift)*"""
        expr = self._parse_shift()
        while self._check(TokenType.LT, TokenType.GT, TokenType.LTE, TokenType.GTE):
            op = self._advance()
            right = self._parse_shift()
            expr = BinaryExpr(
                location=expr.location,
                left=expr, operator=op.lexeme, right=right
            )
        return expr

    def _parse_shift(self) -> Expression:
        expr = self._parse_addition()
        while self._check(TokenType.SHIFT_LEFT, TokenType.SHIFT_RIGHT):
            op = self._advance()
            right = self._parse_addition()
            expr = BinaryExpr(
                location=expr.location, left=expr, operator=op.lexeme, right=right
            )
        return expr

    def _parse_addition(self) -> Expression:
        """addition = multiplication (("+" | "-") multiplication)*"""
        expr = self._parse_multiplication()
        while self._check(TokenType.PLUS, TokenType.MINUS):
            op = self._advance()
            right = self._parse_multiplication()
            expr = BinaryExpr(
                location=expr.location,
                left=expr, operator=op.lexeme, right=right
            )
        return expr

    def _parse_multiplication(self) -> Expression:
        """multiplication = unary (("*" | "/" | "%") unary)*"""
        expr = self._parse_unary()
        while self._check(TokenType.STAR, TokenType.SLASH, TokenType.PERCENT):
            op = self._advance()
            right = self._parse_unary()
            expr = BinaryExpr(
                location=expr.location,
                left=expr, operator=op.lexeme, right=right
            )
        return expr

    def _parse_unary(self) -> Expression:
        """unary = ("-" | "not" | "~") unary | power"""
        if self._check(TokenType.MINUS, TokenType.NOT, TokenType.TILDE):
            op = self._advance()
            operand = self._parse_unary()
            return UnaryExpr(
                location=op.location,
                operator=op.lexeme, operand=operand, prefix=True
            )
        return self._parse_power()

    def _parse_power(self) -> Expression:
        """power = call ("**" unary)?"""
        expr = self._parse_call()
        if self._match(TokenType.POWER):
            right = self._parse_unary()
            expr = BinaryExpr(
                location=expr.location,
                left=expr, operator="**", right=right
            )
        return expr

    def _parse_call(self) -> Expression:
        """call = primary ("(" args ")" | "[" expr "]" | "." IDENT)*"""
        expr = self._parse_primary()

        while True:
            if self._match(TokenType.LPAREN):
                # Function call
                args = []
                if not self._check(TokenType.RPAREN):
                    args.append(self._parse_expression())
                    while self._match(TokenType.COMMA):
                        if self._check(TokenType.RPAREN):
                            break
                        args.append(self._parse_expression())
                self._expect(TokenType.RPAREN, "Expected ')' after arguments")
                expr = CallExpr(
                    location=expr.location,
                    callee=expr, arguments=args
                )
            elif self._match(TokenType.LBRACKET):
                # Indexing
                index = self._parse_expression()
                self._expect(TokenType.RBRACKET, "Expected ']' after index")
                expr = IndexExpr(
                    location=expr.location,
                    object=expr, index=index
                )
            elif self._match(TokenType.DOT):
                # Member access
                member = self._expect(TokenType.IDENTIFIER, "Expected member name after '.'")
                expr = MemberExpr(
                    location=expr.location,
                    object=expr, member=member.value
                )
            else:
                break

        return expr

    def _parse_primary(self) -> Expression:
        """Parse a primary expression (leaf or grouped)."""
        loc = self._current().location

        # Integer literal
        if self._check(TokenType.INTEGER):
            tok = self._advance()
            lit = IntegerLiteral(location=loc, value=tok.value)
            # Check for range: number..expr
            if self._check(TokenType.DOT_DOT):
                self._advance()
                end = self._parse_addition()
                return RangeExpr(location=loc, start=lit, end=end, inclusive=False)
            return lit

        # Float literal
        if self._check(TokenType.FLOAT):
            tok = self._advance()
            return FloatLiteral(location=loc, value=tok.value)

        # String literal
        if self._check(TokenType.STRING):
            tok = self._advance()
            return StringLiteral(location=loc, value=tok.value)

        # Bool literals
        if self._check(TokenType.BOOL_TRUE):
            self._advance()
            return BoolLiteral(location=loc, value=True)
        if self._check(TokenType.BOOL_FALSE):
            self._advance()
            return BoolLiteral(location=loc, value=False)

        # Nil
        if self._check(TokenType.NIL):
            self._advance()
            return NilLiteral(location=loc)

        # Identifier (may be struct literal: Name { ... })
        if self._check(TokenType.IDENTIFIER):
            tok = self._advance()

            # Check for struct literal: Name { field: value, ... }
            if self._check(TokenType.LBRACE) and self._peek().type == TokenType.IDENTIFIER:
                # Look ahead to see if it's field: value pattern
                if self._peek(2).type == TokenType.COLON:
                    return self._parse_struct_literal(tok)

            ident = Identifier(location=loc, name=tok.value)

            # Check for range: ident..expr
            if self._check(TokenType.DOT_DOT):
                self._advance()
                end = self._parse_addition()
                return RangeExpr(location=loc, start=ident, end=end, inclusive=False)

            return ident

        # Parenthesized expression
        if self._match(TokenType.LPAREN):
            expr = self._parse_expression()
            self._expect(TokenType.RPAREN, "Expected ')'")
            return expr

        # Array literal: [1, 2, 3]
        if self._match(TokenType.LBRACKET):
            elements = []
            if not self._check(TokenType.RBRACKET):
                elements.append(self._parse_expression())
                while self._match(TokenType.COMMA):
                    if self._check(TokenType.RBRACKET):
                        break
                    elements.append(self._parse_expression())
            self._expect(TokenType.RBRACKET, "Expected ']'")
            return ArrayLiteral(location=loc, elements=elements)

        # Lambda: |params| body
        if self._match(TokenType.PIPE):
            params = []
            if not self._check(TokenType.PIPE):
                params.append(self._parse_param())
                while self._match(TokenType.COMMA):
                    params.append(self._parse_param())
            self._expect(TokenType.PIPE, "Expected '|' after lambda parameters")
            if self._check(TokenType.LBRACE):
                body = self._parse_block()
            else:
                body = self._parse_expression()
            return LambdaExpr(location=loc, params=params, body=body)

        raise self._error(
            f"Unexpected token: {self._current().type.name} ({self._current().lexeme!r})"
        )

    def _parse_struct_literal(self, name_tok: Token) -> StructLiteral:
        """Parse struct literal: Name { field: value, ... }"""
        loc = name_tok.location
        self._expect(TokenType.LBRACE)

        fields = []
        while not self._check(TokenType.RBRACE) and not self._at_end():
            field_name = self._expect(TokenType.IDENTIFIER, "Expected field name")
            self._expect(TokenType.COLON, "Expected ':' after field name in struct literal")
            field_value = self._parse_expression()
            fields.append(FieldInit(
                location=field_name.location,
                name=field_name.value, value=field_value
            ))
            if not self._match(TokenType.COMMA):
                break

        self._expect(TokenType.RBRACE, "Expected '}' after struct literal")

        return StructLiteral(location=loc, name=name_tok.value, fields=fields)
