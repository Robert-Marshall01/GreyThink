"""
Grey Static Analysis Linter.

Performs lint checks on Grey source code beyond what the semantic
analyzer does. Catches style issues and potential bugs:

Rules:
  W001  Unused variable
  W002  Variable shadows outer scope
  W003  Function too long (> 50 lines)
  W004  Too many parameters (> 6)
  W005  Deeply nested code (> 4 levels)
  W006  Empty block
  W007  Unreachable code after return/break/continue
  W008  Comparison to nil (use is_nil)
  W009  Redundant boolean expression
  W010  Missing return in non-void function
  W011  Mutable variable never mutated
  W012  Use of deprecated function
"""

from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Optional

from ..frontend.lexer import Lexer
from ..frontend.parser import Parser
from ..frontend.ast_nodes import (
    ASTVisitor, Program, ASTNode,
    FunctionDeclaration, VariableDeclaration,
    IfStatement, WhileStatement, ForStatement, BlockStatement,
    ReturnStatement, BreakStatement, ContinueStatement,
    BinaryExpression, UnaryExpression, CallExpression,
    Identifier, Assignment, ExpressionStatement,
    StructDeclaration, EnumDeclaration, ImplBlock, TraitDeclaration,
    MatchStatement, ArrayLiteral, LambdaExpression,
)


class Severity(Enum):
    WARNING = "warning"
    ERROR = "error"
    INFO = "info"
    HINT = "hint"


@dataclass
class LintIssue:
    """A single lint finding."""
    rule: str
    message: str
    severity: Severity
    line: int = 0
    column: int = 0
    file: str = ""

    def __repr__(self):
        loc = f"{self.file}:{self.line}:{self.column}" if self.file else f"line {self.line}"
        return f"[{self.rule}] {self.severity.value}: {self.message} ({loc})"


class GreyLinter(ASTVisitor):
    """
    Static analysis linter for Grey source code.

    Usage:
        linter = GreyLinter()
        issues = linter.lint(source)
        for issue in issues:
            print(issue)
    """

    def __init__(
        self,
        max_function_length: int = 50,
        max_params: int = 6,
        max_nesting: int = 4,
    ):
        self.max_function_length = max_function_length
        self.max_params = max_params
        self.max_nesting = max_nesting

        self.issues: list[LintIssue] = []
        self.source_file: str = ""

        # Tracking state
        self._scope_stack: list[dict[str, _VarInfo]] = []
        self._nesting_depth: int = 0
        self._current_function: Optional[str] = None
        self._has_return: bool = False

    def lint(self, source: str, filename: str = "<input>") -> list[LintIssue]:
        """Lint Grey source code and return a list of issues."""
        self.issues = []
        self.source_file = filename
        self._scope_stack = [{}]
        self._nesting_depth = 0

        try:
            lexer = Lexer(source, filename)
            tokens = lexer.scan_tokens()
            if lexer.errors:
                return self.issues  # can't lint unparseable code

            parser = Parser(tokens, filename)
            program = parser.parse()
            if parser.errors:
                return self.issues

            self._visit_program(program)
            self._check_unused_variables()

        except Exception:
            pass  # don't crash the linter

        return self.issues

    def lint_file(self, filepath: str) -> list[LintIssue]:
        """Lint a Grey source file."""
        with open(filepath, "r") as f:
            source = f.read()
        return self.lint(source, filepath)

    # ── AST Visitors ─────────────────────────────────────────

    def _visit_program(self, node: Program):
        for decl in node.declarations:
            self._visit_node(decl)

    def _visit_node(self, node):
        """Dispatch to the appropriate visitor."""
        if node is None:
            return

        if isinstance(node, FunctionDeclaration):
            self._visit_function(node)
        elif isinstance(node, VariableDeclaration):
            self._visit_variable_decl(node)
        elif isinstance(node, IfStatement):
            self._visit_if(node)
        elif isinstance(node, WhileStatement):
            self._visit_while(node)
        elif isinstance(node, ForStatement):
            self._visit_for(node)
        elif isinstance(node, BlockStatement):
            self._visit_block(node)
        elif isinstance(node, ReturnStatement):
            self._visit_return(node)
        elif isinstance(node, BreakStatement):
            self._has_return = True  # unreachable code after
        elif isinstance(node, ContinueStatement):
            self._has_return = True
        elif isinstance(node, ExpressionStatement):
            self._visit_expr(node.expression)
        elif isinstance(node, MatchStatement):
            self._visit_match(node)
        elif isinstance(node, StructDeclaration):
            self._visit_struct(node)
        elif isinstance(node, EnumDeclaration):
            pass
        elif isinstance(node, ImplBlock):
            self._visit_impl(node)
        elif isinstance(node, TraitDeclaration):
            pass

    def _visit_function(self, node: FunctionDeclaration):
        old_fn = self._current_function
        self._current_function = node.name

        # W004: too many parameters
        if len(node.params) > self.max_params:
            self._warn("W004",
                       f"Function '{node.name}' has {len(node.params)} parameters "
                       f"(max {self.max_params})",
                       node)

        # W003: function too long
        if node.body and hasattr(node.body, 'statements'):
            body_lines = self._count_statements(node.body)
            if body_lines > self.max_function_length:
                self._warn("W003",
                           f"Function '{node.name}' is {body_lines} statements long "
                           f"(max {self.max_function_length})",
                           node)

        # Push scope with parameters
        self._push_scope()
        for param in node.params:
            pname = param.name if hasattr(param, 'name') else str(param)
            self._declare_var(pname, node, mutable=False)

        self._has_return = False
        if node.body:
            self._visit_node(node.body)

        # W010: missing return
        if node.return_type and not self._has_return:
            rt = node.return_type
            rt_str = rt.name if hasattr(rt, 'name') else str(rt)
            if rt_str not in ("void", "nil", "None"):
                self._info("W010",
                           f"Function '{node.name}' may not return a value in all paths",
                           node)

        self._check_unused_variables()
        self._pop_scope()
        self._current_function = old_fn

    def _visit_variable_decl(self, node: VariableDeclaration):
        # W002: shadowing
        for scope in reversed(self._scope_stack[:-1]):
            if node.name in scope:
                self._warn("W002",
                           f"Variable '{node.name}' shadows variable in outer scope",
                           node)
                break

        mutable = node.mutable if hasattr(node, 'mutable') else True
        self._declare_var(node.name, node, mutable=mutable)

        if node.initializer:
            self._visit_expr(node.initializer)

    def _visit_if(self, node: IfStatement):
        self._visit_expr(node.condition)

        self._nesting_depth += 1
        if self._nesting_depth > self.max_nesting:
            self._warn("W005",
                       f"Code is nested {self._nesting_depth} levels deep (max {self.max_nesting})",
                       node)

        if node.then_branch:
            self._visit_node(node.then_branch)
        if node.else_branch:
            self._visit_node(node.else_branch)

        self._nesting_depth -= 1

    def _visit_while(self, node: WhileStatement):
        self._visit_expr(node.condition)

        self._nesting_depth += 1
        if self._nesting_depth > self.max_nesting:
            self._warn("W005",
                       f"Code is nested {self._nesting_depth} levels deep",
                       node)

        self._visit_node(node.body)
        self._nesting_depth -= 1

    def _visit_for(self, node: ForStatement):
        self._nesting_depth += 1
        self._push_scope()
        self._declare_var(node.variable, node, mutable=False)

        if node.iterable:
            self._visit_expr(node.iterable)
        self._visit_node(node.body)

        self._check_unused_variables()
        self._pop_scope()
        self._nesting_depth -= 1

    def _visit_block(self, node: BlockStatement):
        # W006: empty block
        if not node.statements:
            self._info("W006", "Empty block", node)
            return

        unreachable = False
        for stmt in node.statements:
            if unreachable:
                self._warn("W007", "Unreachable code", stmt)
                break
            self._visit_node(stmt)
            if isinstance(stmt, (ReturnStatement, BreakStatement, ContinueStatement)):
                unreachable = True

    def _visit_return(self, node: ReturnStatement):
        self._has_return = True
        if node.value:
            self._visit_expr(node.value)

    def _visit_match(self, node: MatchStatement):
        self._visit_expr(node.subject)
        for arm in node.arms:
            if hasattr(arm, 'body'):
                self._visit_node(arm.body)

    def _visit_struct(self, node: StructDeclaration):
        pass  # struct declarations are fine

    def _visit_impl(self, node: ImplBlock):
        for method in node.methods:
            self._visit_node(method)

    # ── Expression Visitors ──────────────────────────────────

    def _visit_expr(self, node):
        """Visit an expression, tracking variable usage."""
        if node is None:
            return

        if isinstance(node, Identifier):
            self._use_var(node.name)

        elif isinstance(node, Assignment):
            self._visit_expr(node.value)
            if hasattr(node, 'target'):
                target = node.target
                if isinstance(target, Identifier):
                    self._mutate_var(target.name)

        elif isinstance(node, BinaryExpression):
            self._visit_expr(node.left)
            self._visit_expr(node.right)

            # W008: comparison to nil
            if hasattr(node, 'operator'):
                op = node.operator
                op_str = op.value if hasattr(op, 'value') else str(op)
                if op_str in ("==", "!="):
                    if self._is_nil_literal(node.left) or self._is_nil_literal(node.right):
                        self._hint("W008",
                                   "Consider using is_nil() instead of comparing to nil",
                                   node)

            # W009: redundant boolean
            if hasattr(node, 'operator'):
                op = node.operator
                op_str = op.value if hasattr(op, 'value') else str(op)
                if op_str in ("==", "!="):
                    if self._is_bool_literal(node.right) or self._is_bool_literal(node.left):
                        self._hint("W009",
                                   "Redundant boolean comparison — simplify the expression",
                                   node)

        elif isinstance(node, UnaryExpression):
            self._visit_expr(node.operand)

        elif isinstance(node, CallExpression):
            self._visit_expr(node.callee)
            for arg in node.arguments:
                self._visit_expr(arg)

            # W012: deprecated function
            if isinstance(node.callee, Identifier):
                deprecated = {"print"}  # example
                # Currently no deprecated functions; placeholder

        elif isinstance(node, ArrayLiteral):
            for elem in node.elements:
                self._visit_expr(elem)

        elif isinstance(node, LambdaExpression):
            self._push_scope()
            for p in node.params:
                pname = p.name if hasattr(p, 'name') else str(p)
                self._declare_var(pname, node, mutable=False)
            self._visit_node(node.body)
            self._check_unused_variables()
            self._pop_scope()

    # ── Scope / Variable Tracking ────────────────────────────

    def _push_scope(self):
        self._scope_stack.append({})

    def _pop_scope(self):
        if len(self._scope_stack) > 1:
            self._scope_stack.pop()

    def _declare_var(self, name: str, node, mutable: bool = True):
        if self._scope_stack:
            self._scope_stack[-1][name] = _VarInfo(
                name=name, node=node, used=False,
                mutable=mutable, mutated=False,
            )

    def _use_var(self, name: str):
        for scope in reversed(self._scope_stack):
            if name in scope:
                scope[name].used = True
                return

    def _mutate_var(self, name: str):
        for scope in reversed(self._scope_stack):
            if name in scope:
                scope[name].mutated = True
                return

    def _check_unused_variables(self):
        """Check for unused variables in the current scope."""
        if not self._scope_stack:
            return
        scope = self._scope_stack[-1]
        for name, info in scope.items():
            if name.startswith("_"):
                continue  # convention: _ prefix means intentionally unused

            if not info.used:
                self._warn("W001", f"Unused variable '{name}'", info.node)

            # W011: mutable but never mutated
            if info.mutable and info.used and not info.mutated:
                self._hint("W011",
                           f"Variable '{name}' is declared mutable but never mutated; "
                           f"consider using 'let' instead of 'mut'",
                           info.node)

    # ── Helpers ──────────────────────────────────────────────

    def _warn(self, rule: str, message: str, node=None):
        line, col = self._node_location(node)
        self.issues.append(LintIssue(rule, message, Severity.WARNING, line, col, self.source_file))

    def _info(self, rule: str, message: str, node=None):
        line, col = self._node_location(node)
        self.issues.append(LintIssue(rule, message, Severity.INFO, line, col, self.source_file))

    def _hint(self, rule: str, message: str, node=None):
        line, col = self._node_location(node)
        self.issues.append(LintIssue(rule, message, Severity.HINT, line, col, self.source_file))

    def _error(self, rule: str, message: str, node=None):
        line, col = self._node_location(node)
        self.issues.append(LintIssue(rule, message, Severity.ERROR, line, col, self.source_file))

    @staticmethod
    def _node_location(node) -> tuple[int, int]:
        if node is None:
            return (0, 0)
        if hasattr(node, 'location') and node.location:
            return (node.location.line, node.location.column)
        if hasattr(node, 'line'):
            return (node.line, 0)
        return (0, 0)

    @staticmethod
    def _is_nil_literal(node) -> bool:
        if hasattr(node, 'value') and node.value is None:
            return True
        if isinstance(node, Identifier) and node.name == "nil":
            return True
        return False

    @staticmethod
    def _is_bool_literal(node) -> bool:
        if hasattr(node, 'value') and isinstance(node.value, bool):
            return True
        if isinstance(node, Identifier) and node.name in ("true", "false"):
            return True
        return False

    @staticmethod
    def _count_statements(block) -> int:
        """Recursively count statements in a block."""
        count = 0
        if hasattr(block, 'statements'):
            for stmt in block.statements:
                count += 1
                if hasattr(stmt, 'body'):
                    count += GreyLinter._count_statements(stmt.body)
                if hasattr(stmt, 'then_branch'):
                    count += GreyLinter._count_statements(stmt.then_branch)
                if hasattr(stmt, 'else_branch') and stmt.else_branch:
                    count += GreyLinter._count_statements(stmt.else_branch)
        return count


@dataclass
class _VarInfo:
    """Internal variable tracking info."""
    name: str
    node: object
    used: bool = False
    mutable: bool = True
    mutated: bool = False
