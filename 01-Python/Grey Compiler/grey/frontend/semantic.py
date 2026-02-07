"""
Semantic Analyzer for the Grey language.

Performs:
  - Type checking
  - Scope resolution
  - Symbol table construction
  - Detects semantic errors (undefined variables, type mismatches, etc.)
  - Annotates the AST with type and symbol information
"""

from typing import Optional
from .ast_nodes import *
from .symbols import (
    SymbolTable, Symbol, SymbolKind, TypeInfo,
    INT_TYPE, FLOAT_TYPE, STRING_TYPE, BOOL_TYPE,
    CHAR_TYPE, VOID_TYPE, NIL_TYPE, ANY_TYPE, ERROR_TYPE,
    BUILTIN_TYPES,
)


class SemanticError:
    """A semantic error found during analysis."""

    def __init__(self, message: str, location: Optional[SourceLocation] = None):
        self.message = message
        self.location = location

    def __repr__(self):
        if self.location:
            return f"{self.location}: {self.message}"
        return self.message


class SemanticAnalyzer(ASTVisitor):
    """
    Walks the AST and performs semantic analysis.

    Usage:
        analyzer = SemanticAnalyzer()
        analyzer.analyze(ast)
        if analyzer.errors:
            for err in analyzer.errors:
                print(err)
    """

    def __init__(self):
        self.symbol_table = SymbolTable()
        self.errors: list[SemanticError] = []
        self.warnings: list[SemanticError] = []
        self.current_function: Optional[FnDecl] = None
        self.current_return_type: Optional[TypeInfo] = None
        self.in_loop: int = 0  # nesting depth

    def analyze(self, program: Program) -> SymbolTable:
        """Run semantic analysis on the program. Returns the symbol table."""
        # First pass: register all top-level declarations
        self._register_declarations(program)
        # Second pass: analyze everything
        self.visit_Program(program)
        return self.symbol_table

    def _error(self, message: str, node: Optional[ASTNode] = None):
        loc = node.location if node else None
        self.errors.append(SemanticError(message, loc))

    def _warning(self, message: str, node: Optional[ASTNode] = None):
        loc = node.location if node else None
        self.warnings.append(SemanticError(message, loc))

    # ── Type Resolution ──────────────────────────────────────

    def _resolve_type_annotation(self, type_node: Optional[GreyType]) -> TypeInfo:
        """Convert an AST type node to a TypeInfo."""
        if type_node is None:
            return ANY_TYPE

        if isinstance(type_node, SimpleType):
            resolved = self.symbol_table.resolve_type(type_node.name)
            if resolved:
                return resolved
            self._error(f"Unknown type: '{type_node.name}'", type_node)
            return ERROR_TYPE

        if isinstance(type_node, ArrayType):
            elem = self._resolve_type_annotation(type_node.element_type)
            return TypeInfo(name="array", is_array=True, element_type=elem)

        if isinstance(type_node, FunctionType):
            params = [self._resolve_type_annotation(p) for p in type_node.param_types]
            ret = self._resolve_type_annotation(type_node.return_type)
            return TypeInfo(name="fn", is_function=True,
                            param_types=params, return_type=ret)

        if isinstance(type_node, OptionalType):
            inner = self._resolve_type_annotation(type_node.inner_type)
            return TypeInfo(name=inner.name, is_optional=True)

        return ERROR_TYPE

    def _infer_type(self, expr: Optional[Expression]) -> TypeInfo:
        """Infer the type of an expression."""
        if expr is None:
            return VOID_TYPE

        expr.accept(self)
        return expr.resolved_type or ANY_TYPE

    # ── Declaration Registration (Pass 1) ────────────────────

    def _register_declarations(self, program: Program):
        """First pass: register all top-level names so forward references work."""
        for decl in program.declarations:
            if isinstance(decl, FnDecl):
                params = [self._resolve_type_annotation(p.type_annotation)
                          for p in decl.params]
                ret = self._resolve_type_annotation(decl.return_type)
                fn_type = TypeInfo(name="fn", is_function=True,
                                   param_types=params, return_type=ret)
                self.symbol_table.define(Symbol(
                    decl.name, SymbolKind.FUNCTION, fn_type,
                    defined_at=decl.location, is_public=decl.is_public
                ))

            elif isinstance(decl, StructDecl):
                fields = {}
                for f in decl.fields:
                    fields[f.name] = self._resolve_type_annotation(f.type_annotation)
                struct_type = TypeInfo(name=decl.name, is_struct=True, fields=fields)
                self.symbol_table.define(Symbol(
                    decl.name, SymbolKind.STRUCT, struct_type,
                    defined_at=decl.location, is_public=decl.is_public
                ))

            elif isinstance(decl, EnumDecl):
                variants = {}
                for v in decl.variants:
                    variants[v.name] = [self._resolve_type_annotation(f)
                                         for f in v.fields]
                enum_type = TypeInfo(name=decl.name, is_enum=True, variants=variants)
                self.symbol_table.define(Symbol(
                    decl.name, SymbolKind.ENUM, enum_type,
                    defined_at=decl.location, is_public=decl.is_public
                ))

            elif isinstance(decl, TraitDecl):
                trait_type = TypeInfo(name=decl.name)
                self.symbol_table.define(Symbol(
                    decl.name, SymbolKind.TRAIT, trait_type,
                    defined_at=decl.location, is_public=decl.is_public
                ))

            elif isinstance(decl, TypeAlias):
                aliased = self._resolve_type_annotation(decl.aliased_type)
                self.symbol_table.define(Symbol(
                    decl.name, SymbolKind.TYPE_ALIAS, aliased,
                    defined_at=decl.location
                ))

    # ══════════════════════════════════════════════════════════
    #  Visitor Methods (Pass 2)
    # ══════════════════════════════════════════════════════════

    def visit_Program(self, node: Program):
        for decl in node.declarations:
            decl.accept(self)
        for stmt in node.statements:
            stmt.accept(self)

    # ── Declarations ─────────────────────────────────────────

    def visit_VarDecl(self, node: VarDecl):
        # Determine type
        if node.type_annotation:
            declared_type = self._resolve_type_annotation(node.type_annotation)
        else:
            declared_type = None

        init_type = None
        if node.initializer:
            init_type = self._infer_type(node.initializer)

        if declared_type and init_type:
            if not init_type.is_compatible_with(declared_type):
                self._error(
                    f"Type mismatch: cannot assign {init_type} to {declared_type}",
                    node
                )
            final_type = declared_type
        elif declared_type:
            final_type = declared_type
        elif init_type:
            final_type = init_type
        else:
            self._error("Variable declaration requires type annotation or initializer", node)
            final_type = ERROR_TYPE

        final_type.is_mutable = node.mutable

        # Check for redefinition in current scope
        if self.symbol_table.lookup_current(node.name):
            self._error(f"Variable '{node.name}' already defined in this scope", node)

        sym = Symbol(
            name=node.name,
            kind=SymbolKind.CONSTANT if node.is_const else SymbolKind.VARIABLE,
            type_info=final_type,
            mutable=node.mutable,
            defined_at=node.location,
            is_initialized=node.initializer is not None,
        )
        self.symbol_table.define(sym)
        node.resolved_type = final_type

    def visit_FnDecl(self, node: FnDecl):
        # Function was already registered in pass 1 for top-level
        # But handle nested functions or methods
        sym = self.symbol_table.resolve(node.name)

        self.symbol_table.enter_scope(f"fn:{node.name}")
        prev_fn = self.current_function
        prev_ret = self.current_return_type

        self.current_function = node
        self.current_return_type = self._resolve_type_annotation(node.return_type)

        # Define parameters in function scope
        for param in node.params:
            param_type = self._resolve_type_annotation(param.type_annotation)
            self.symbol_table.define(Symbol(
                name=param.name,
                kind=SymbolKind.PARAMETER,
                type_info=param_type,
                mutable=False,
                defined_at=param.location,
                is_initialized=True,
            ))

        # Analyze body
        if node.body:
            for stmt in node.body.statements:
                stmt.accept(self)

        self.symbol_table.exit_scope()
        self.current_function = prev_fn
        self.current_return_type = prev_ret

    def visit_StructDecl(self, node: StructDecl):
        # Already registered in pass 1
        pass

    def visit_EnumDecl(self, node: EnumDecl):
        # Already registered in pass 1
        pass

    def visit_ImplDecl(self, node: ImplDecl):
        # Resolve the target type
        target_sym = self.symbol_table.resolve(node.target)
        if not target_sym:
            self._error(f"Cannot impl for unknown type '{node.target}'", node)
            return

        self.symbol_table.enter_scope(f"impl:{node.target}")

        for method in node.methods:
            # Register method as a function
            params = [self._resolve_type_annotation(p.type_annotation)
                      for p in method.params]
            ret = self._resolve_type_annotation(method.return_type)
            fn_type = TypeInfo(name="fn", is_function=True,
                               param_types=params, return_type=ret)

            method_name = f"{node.target}.{method.name}"
            self.symbol_table.define(Symbol(
                name=method_name, kind=SymbolKind.METHOD,
                type_info=fn_type, defined_at=method.location,
                is_public=method.is_public
            ))

            # Analyze method body
            method.accept(self)

        self.symbol_table.exit_scope()

    def visit_TraitDecl(self, node: TraitDecl):
        pass  # Already registered

    def visit_ImportDecl(self, node: ImportDecl):
        # For now, just record the import
        self.symbol_table.define(Symbol(
            name=node.alias or node.module_path.split(".")[-1],
            kind=SymbolKind.MODULE,
            type_info=ANY_TYPE,
            defined_at=node.location
        ))

    def visit_TypeAlias(self, node: TypeAlias):
        pass  # Already registered in pass 1

    # ── Statements ───────────────────────────────────────────

    def visit_Block(self, node: Block):
        for stmt in node.statements:
            stmt.accept(self)

    def visit_ExpressionStmt(self, node: ExpressionStmt):
        if node.expression:
            self._infer_type(node.expression)

    def visit_IfStmt(self, node: IfStmt):
        cond_type = self._infer_type(node.condition)
        if cond_type != BOOL_TYPE and cond_type != ANY_TYPE and cond_type != ERROR_TYPE:
            self._warning(f"Condition should be bool, got {cond_type}", node)

        self.symbol_table.enter_scope("if")
        if node.then_body:
            node.then_body.accept(self)
        self.symbol_table.exit_scope()

        for elif_clause in node.elif_clauses:
            c_type = self._infer_type(elif_clause.condition)
            if c_type != BOOL_TYPE and c_type != ANY_TYPE and c_type != ERROR_TYPE:
                self._warning(f"Condition should be bool, got {c_type}", elif_clause)
            self.symbol_table.enter_scope("elif")
            if elif_clause.body:
                elif_clause.body.accept(self)
            self.symbol_table.exit_scope()

        if node.else_body:
            self.symbol_table.enter_scope("else")
            node.else_body.accept(self)
            self.symbol_table.exit_scope()

    def visit_WhileStmt(self, node: WhileStmt):
        cond_type = self._infer_type(node.condition)
        if cond_type != BOOL_TYPE and cond_type != ANY_TYPE and cond_type != ERROR_TYPE:
            self._warning(f"While condition should be bool, got {cond_type}", node)

        self.in_loop += 1
        self.symbol_table.enter_scope("while")
        if node.body:
            node.body.accept(self)
        self.symbol_table.exit_scope()
        self.in_loop -= 1

    def visit_ForStmt(self, node: ForStmt):
        iter_type = self._infer_type(node.iterable)

        # Determine loop variable type
        if iter_type.is_array and iter_type.element_type:
            var_type = iter_type.element_type
        elif isinstance(node.iterable, RangeExpr):
            var_type = INT_TYPE
        else:
            var_type = ANY_TYPE

        self.in_loop += 1
        self.symbol_table.enter_scope("for")
        self.symbol_table.define(Symbol(
            name=node.variable,
            kind=SymbolKind.VARIABLE,
            type_info=var_type,
            mutable=False,
            defined_at=node.location,
            is_initialized=True,
        ))

        if node.body:
            node.body.accept(self)

        self.symbol_table.exit_scope()
        self.in_loop -= 1

    def visit_ReturnStmt(self, node: ReturnStmt):
        if self.current_function is None:
            self._error("'return' outside of function", node)
            return

        if node.value:
            ret_type = self._infer_type(node.value)
            if self.current_return_type and not ret_type.is_compatible_with(self.current_return_type):
                self._error(
                    f"Return type mismatch: expected {self.current_return_type}, got {ret_type}",
                    node
                )
        else:
            if self.current_return_type and self.current_return_type != VOID_TYPE:
                self._error(
                    f"Function expects return type {self.current_return_type}",
                    node
                )

    def visit_BreakStmt(self, node: BreakStmt):
        if self.in_loop == 0:
            self._error("'break' outside of loop", node)

    def visit_ContinueStmt(self, node: ContinueStmt):
        if self.in_loop == 0:
            self._error("'continue' outside of loop", node)

    def visit_MatchStmt(self, node: MatchStmt):
        subject_type = self._infer_type(node.subject)
        for arm in node.arms:
            self.symbol_table.enter_scope("match_arm")
            if arm.pattern:
                arm.pattern.accept(self)
            if arm.body:
                arm.body.accept(self)
            self.symbol_table.exit_scope()

    # ── Expressions ──────────────────────────────────────────

    def visit_IntegerLiteral(self, node: IntegerLiteral):
        node.resolved_type = INT_TYPE

    def visit_FloatLiteral(self, node: FloatLiteral):
        node.resolved_type = FLOAT_TYPE

    def visit_StringLiteral(self, node: StringLiteral):
        node.resolved_type = STRING_TYPE

    def visit_BoolLiteral(self, node: BoolLiteral):
        node.resolved_type = BOOL_TYPE

    def visit_NilLiteral(self, node: NilLiteral):
        node.resolved_type = NIL_TYPE

    def visit_Identifier(self, node: Identifier):
        sym = self.symbol_table.resolve(node.name)
        if sym is None:
            self._error(f"Undefined variable: '{node.name}'", node)
            node.resolved_type = ERROR_TYPE
        else:
            sym.used = True
            node.symbol = sym
            node.resolved_type = sym.type_info

    def visit_BinaryExpr(self, node: BinaryExpr):
        left_type = self._infer_type(node.left)
        right_type = self._infer_type(node.right)

        op = node.operator

        # Arithmetic operators
        if op in ('+', '-', '*', '/', '%', '**'):
            if op == '+' and (left_type == STRING_TYPE or right_type == STRING_TYPE):
                # String concatenation
                node.resolved_type = STRING_TYPE
            elif left_type.is_numeric() and right_type.is_numeric():
                # Numeric promotion: int op float -> float
                if left_type == FLOAT_TYPE or right_type == FLOAT_TYPE:
                    node.resolved_type = FLOAT_TYPE
                elif op == '/':
                    node.resolved_type = FLOAT_TYPE  # Division always returns float
                else:
                    node.resolved_type = INT_TYPE
            elif left_type == ANY_TYPE or right_type == ANY_TYPE:
                node.resolved_type = ANY_TYPE
            elif left_type == ERROR_TYPE or right_type == ERROR_TYPE:
                node.resolved_type = ERROR_TYPE
            else:
                self._error(
                    f"Operator '{op}' not defined for {left_type} and {right_type}",
                    node
                )
                node.resolved_type = ERROR_TYPE

        # Comparison operators
        elif op in ('==', '!=', '<', '>', '<=', '>='):
            node.resolved_type = BOOL_TYPE

        # Logical operators
        elif op in ('and', 'or'):
            node.resolved_type = BOOL_TYPE

        # Bitwise operators
        elif op in ('&', '|', '^', '<<', '>>'):
            if left_type == INT_TYPE and right_type == INT_TYPE:
                node.resolved_type = INT_TYPE
            elif left_type == ANY_TYPE or right_type == ANY_TYPE:
                node.resolved_type = ANY_TYPE
            else:
                self._error(
                    f"Bitwise operator '{op}' requires int operands",
                    node
                )
                node.resolved_type = ERROR_TYPE
        else:
            node.resolved_type = ANY_TYPE

    def visit_UnaryExpr(self, node: UnaryExpr):
        operand_type = self._infer_type(node.operand)

        if node.operator == '-':
            if operand_type.is_numeric() or operand_type == ANY_TYPE:
                node.resolved_type = operand_type
            else:
                self._error(f"Cannot negate type {operand_type}", node)
                node.resolved_type = ERROR_TYPE

        elif node.operator in ('not', '!'):
            node.resolved_type = BOOL_TYPE

        elif node.operator == '~':
            if operand_type == INT_TYPE or operand_type == ANY_TYPE:
                node.resolved_type = INT_TYPE
            else:
                self._error(f"Bitwise NOT requires int, got {operand_type}", node)
                node.resolved_type = ERROR_TYPE
        else:
            node.resolved_type = operand_type

    def visit_CallExpr(self, node: CallExpr):
        callee_type = self._infer_type(node.callee)

        if callee_type.is_function:
            # Check argument count (with some flexibility for builtins)
            if isinstance(node.callee, Identifier):
                sym = self.symbol_table.resolve(node.callee.name)
                if sym and sym.kind != SymbolKind.BUILTIN:
                    expected = len(callee_type.param_types)
                    got = len(node.arguments)
                    if expected != got:
                        self._error(
                            f"Function '{node.callee.name}' expects {expected} arguments, got {got}",
                            node
                        )

            # Type check arguments
            for i, arg in enumerate(node.arguments):
                arg_type = self._infer_type(arg)

            node.resolved_type = callee_type.return_type or VOID_TYPE
        elif callee_type == ANY_TYPE or callee_type == ERROR_TYPE:
            for arg in node.arguments:
                self._infer_type(arg)
            node.resolved_type = ANY_TYPE
        else:
            # Could be a struct constructor
            if isinstance(node.callee, Identifier):
                sym = self.symbol_table.resolve(node.callee.name)
                if sym and sym.kind == SymbolKind.STRUCT:
                    for arg in node.arguments:
                        self._infer_type(arg)
                    node.resolved_type = sym.type_info
                    return

            self._error(f"'{callee_type}' is not callable", node)
            for arg in node.arguments:
                self._infer_type(arg)
            node.resolved_type = ERROR_TYPE

    def visit_IndexExpr(self, node: IndexExpr):
        obj_type = self._infer_type(node.object)
        idx_type = self._infer_type(node.index)

        if obj_type.is_array:
            if idx_type != INT_TYPE and idx_type != ANY_TYPE:
                self._error(f"Array index must be int, got {idx_type}", node)
            node.resolved_type = obj_type.element_type or ANY_TYPE
        elif obj_type == STRING_TYPE:
            node.resolved_type = CHAR_TYPE
        elif obj_type == ANY_TYPE or obj_type == ERROR_TYPE:
            node.resolved_type = ANY_TYPE
        else:
            self._error(f"Cannot index into type {obj_type}", node)
            node.resolved_type = ERROR_TYPE

    def visit_MemberExpr(self, node: MemberExpr):
        obj_type = self._infer_type(node.object)

        if obj_type.is_struct and node.member in obj_type.fields:
            node.resolved_type = obj_type.fields[node.member]
        elif obj_type == ANY_TYPE or obj_type == ERROR_TYPE:
            node.resolved_type = ANY_TYPE
        else:
            # Check for methods
            if isinstance(node.object, Identifier):
                method_name = f"{obj_type.name}.{node.member}"
                sym = self.symbol_table.resolve(method_name)
                if sym:
                    node.resolved_type = sym.type_info
                    return

            self._error(
                f"Type '{obj_type}' has no member '{node.member}'",
                node
            )
            node.resolved_type = ERROR_TYPE

    def visit_AssignExpr(self, node: AssignExpr):
        target_type = self._infer_type(node.target)
        value_type = self._infer_type(node.value)

        # Check mutability
        if isinstance(node.target, Identifier):
            sym = self.symbol_table.resolve(node.target.name)
            if sym:
                if sym.kind == SymbolKind.CONSTANT:
                    self._error(f"Cannot reassign constant '{node.target.name}'", node)
                elif not sym.mutable and sym.is_initialized:
                    self._error(
                        f"Cannot reassign immutable variable '{node.target.name}'. "
                        f"Use 'let mut' to make it mutable.",
                        node
                    )
                sym.is_initialized = True

        if not value_type.is_compatible_with(target_type):
            self._error(
                f"Cannot assign {value_type} to {target_type}",
                node
            )

        node.resolved_type = target_type

    def visit_ArrayLiteral(self, node: ArrayLiteral):
        if not node.elements:
            node.resolved_type = TypeInfo(name="array", is_array=True, element_type=ANY_TYPE)
            return

        elem_type = self._infer_type(node.elements[0])
        for elem in node.elements[1:]:
            t = self._infer_type(elem)
            if not t.is_compatible_with(elem_type):
                self._warning(
                    f"Mixed types in array: {elem_type} and {t}",
                    node
                )

        node.resolved_type = TypeInfo(name="array", is_array=True, element_type=elem_type)

    def visit_RangeExpr(self, node: RangeExpr):
        if node.start:
            start_type = self._infer_type(node.start)
        if node.end:
            end_type = self._infer_type(node.end)
        # Range produces an array-like iterable of int
        node.resolved_type = TypeInfo(name="range", is_array=True, element_type=INT_TYPE)

    def visit_StructLiteral(self, node: StructLiteral):
        sym = self.symbol_table.resolve(node.name)
        if sym is None or sym.kind != SymbolKind.STRUCT:
            self._error(f"Unknown struct type: '{node.name}'", node)
            node.resolved_type = ERROR_TYPE
            return

        struct_type = sym.type_info
        # Check fields
        for field_init in node.fields:
            if field_init.name not in struct_type.fields:
                self._error(
                    f"Struct '{node.name}' has no field '{field_init.name}'",
                    field_init
                )
            else:
                expected = struct_type.fields[field_init.name]
                actual = self._infer_type(field_init.value)
                if not actual.is_compatible_with(expected):
                    self._error(
                        f"Field '{field_init.name}': expected {expected}, got {actual}",
                        field_init
                    )

        node.resolved_type = struct_type

    def visit_LambdaExpr(self, node: LambdaExpr):
        self.symbol_table.enter_scope("lambda")
        param_types = []
        for param in node.params:
            p_type = self._resolve_type_annotation(param.type_annotation)
            param_types.append(p_type)
            self.symbol_table.define(Symbol(
                name=param.name, kind=SymbolKind.PARAMETER,
                type_info=p_type, defined_at=param.location,
                is_initialized=True
            ))

        if node.body:
            if isinstance(node.body, Expression):
                ret_type = self._infer_type(node.body)
            else:
                node.body.accept(self)
                ret_type = VOID_TYPE
        else:
            ret_type = VOID_TYPE

        self.symbol_table.exit_scope()
        node.resolved_type = TypeInfo(
            name="fn", is_function=True,
            param_types=param_types, return_type=ret_type
        )

    def visit_CastExpr(self, node: CastExpr):
        self._infer_type(node.expr)
        node.resolved_type = self._resolve_type_annotation(node.target_type)

    def visit_MatchExpr(self, node: MatchExpr):
        self._infer_type(node.subject)
        result_type = ANY_TYPE
        for arm in node.arms:
            self.symbol_table.enter_scope("match_arm")
            if arm.pattern:
                arm.pattern.accept(self)
            if arm.body:
                arm.body.accept(self)
            self.symbol_table.exit_scope()
        node.resolved_type = result_type

    def visit_IfExpr(self, node: IfExpr):
        self._infer_type(node.condition)
        then_type = self._infer_type(node.then_branch)
        if node.else_branch:
            else_type = self._infer_type(node.else_branch)
        else:
            else_type = VOID_TYPE
        node.resolved_type = then_type

    def visit_FieldInit(self, node: FieldInit):
        if node.value:
            self._infer_type(node.value)
