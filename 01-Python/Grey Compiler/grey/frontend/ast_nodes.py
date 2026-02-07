"""
AST (Abstract Syntax Tree) node definitions for the Grey language.

Each node class represents a syntactic construct in the language.
Nodes are organized into:
  - Expressions (produce values)
  - Statements (perform actions)
  - Declarations (define names)
  - Types (represent type annotations)
  - Program (root node)
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Any, Optional
from .tokens import SourceLocation


# ══════════════════════════════════════════════════════════════
#  Base Classes
# ══════════════════════════════════════════════════════════════

@dataclass
class ASTNode:
    """Base class for all AST nodes."""
    location: Optional[SourceLocation] = field(default=None, repr=False)
    # Annotation fields set during semantic analysis
    resolved_type: Optional[GreyType] = field(default=None, repr=False)

    def accept(self, visitor: 'ASTVisitor'):
        """Visitor pattern dispatch."""
        method_name = f'visit_{type(self).__name__}'
        visitor_method = getattr(visitor, method_name, visitor.generic_visit)
        return visitor_method(self)


@dataclass
class Expression(ASTNode):
    """Base class for all expressions (nodes that produce values)."""
    pass


@dataclass
class Statement(ASTNode):
    """Base class for all statements (nodes that perform actions)."""
    pass


@dataclass
class Declaration(Statement):
    """Base class for all declarations."""
    pass


# ══════════════════════════════════════════════════════════════
#  Type Nodes
# ══════════════════════════════════════════════════════════════

@dataclass
class GreyType(ASTNode):
    """Base class for type annotations."""
    pass


@dataclass
class SimpleType(GreyType):
    """Simple named type: int, float, string, bool, void, or user-defined."""
    name: str = ""


@dataclass
class ArrayType(GreyType):
    """Array type: [int], [string], etc."""
    element_type: Optional[GreyType] = None


@dataclass
class FunctionType(GreyType):
    """Function type: fn(int, int) -> int."""
    param_types: list[GreyType] = field(default_factory=list)
    return_type: Optional[GreyType] = None


@dataclass
class OptionalType(GreyType):
    """Optional type: int?, string?, etc."""
    inner_type: Optional[GreyType] = None


# ══════════════════════════════════════════════════════════════
#  Expression Nodes
# ══════════════════════════════════════════════════════════════

@dataclass
class IntegerLiteral(Expression):
    """Integer literal: 42, 0xFF, 0b1010."""
    value: int = 0


@dataclass
class FloatLiteral(Expression):
    """Float literal: 3.14, 1e10."""
    value: float = 0.0


@dataclass
class StringLiteral(Expression):
    """String literal: "hello world"."""
    value: str = ""


@dataclass
class BoolLiteral(Expression):
    """Boolean literal: true, false."""
    value: bool = False


@dataclass
class NilLiteral(Expression):
    """Nil literal: nil."""
    pass


@dataclass
class Identifier(Expression):
    """Variable or function reference: x, myFunc, Point."""
    name: str = ""
    # Set during semantic analysis
    symbol: Any = field(default=None, repr=False)


@dataclass
class BinaryExpr(Expression):
    """Binary expression: a + b, x == y, etc."""
    left: Optional[Expression] = None
    operator: str = ""
    right: Optional[Expression] = None


@dataclass
class UnaryExpr(Expression):
    """Unary expression: -x, not flag, ~bits."""
    operator: str = ""
    operand: Optional[Expression] = None
    prefix: bool = True  # True for prefix, False for postfix


@dataclass
class CallExpr(Expression):
    """Function call: foo(1, 2, 3)."""
    callee: Optional[Expression] = None
    arguments: list[Expression] = field(default_factory=list)


@dataclass
class IndexExpr(Expression):
    """Array/map indexing: arr[0], map["key"]."""
    object: Optional[Expression] = None
    index: Optional[Expression] = None


@dataclass
class MemberExpr(Expression):
    """Member access: point.x, obj.method."""
    object: Optional[Expression] = None
    member: str = ""


@dataclass
class AssignExpr(Expression):
    """Assignment expression: x = 5, arr[0] = 10."""
    target: Optional[Expression] = None
    operator: str = "="   # =, +=, -=, *=, /=, %=
    value: Optional[Expression] = None


@dataclass
class ArrayLiteral(Expression):
    """Array literal: [1, 2, 3]."""
    elements: list[Expression] = field(default_factory=list)


@dataclass
class RangeExpr(Expression):
    """Range expression: 0..10, 1..=100."""
    start: Optional[Expression] = None
    end: Optional[Expression] = None
    inclusive: bool = False


@dataclass
class IfExpr(Expression):
    """If expression (when used as value): let x = if cond { a } else { b }."""
    condition: Optional[Expression] = None
    then_branch: Optional[Expression] = None
    else_branch: Optional[Expression] = None


@dataclass
class MatchExpr(Expression):
    """Match expression: match value { pattern => expr, ... }."""
    subject: Optional[Expression] = None
    arms: list[MatchArm] = field(default_factory=list)


@dataclass
class MatchArm(ASTNode):
    """A single arm in a match expression."""
    pattern: Optional[Expression] = None
    body: Optional[ASTNode] = None


@dataclass
class LambdaExpr(Expression):
    """Lambda/closure: |x, y| x + y."""
    params: list[Parameter] = field(default_factory=list)
    body: Optional[ASTNode] = None


@dataclass
class CastExpr(Expression):
    """Type cast expression: x as int."""
    expr: Optional[Expression] = None
    target_type: Optional[GreyType] = None


@dataclass
class StructLiteral(Expression):
    """Struct instantiation: Point { x: 1.0, y: 2.0 }."""
    name: str = ""
    fields: list[FieldInit] = field(default_factory=list)


@dataclass
class FieldInit(ASTNode):
    """Field initializer in a struct literal."""
    name: str = ""
    value: Optional[Expression] = None


# ══════════════════════════════════════════════════════════════
#  Statement Nodes
# ══════════════════════════════════════════════════════════════

@dataclass
class ExpressionStmt(Statement):
    """An expression used as a statement."""
    expression: Optional[Expression] = None


@dataclass
class Block(Statement):
    """Block of statements: { stmt1; stmt2; ... }."""
    statements: list[Statement] = field(default_factory=list)


@dataclass
class ReturnStmt(Statement):
    """Return statement: return expr;"""
    value: Optional[Expression] = None


@dataclass
class BreakStmt(Statement):
    """Break statement: break;"""
    pass


@dataclass
class ContinueStmt(Statement):
    """Continue statement: continue;"""
    pass


@dataclass
class IfStmt(Statement):
    """If statement: if cond { ... } elif cond { ... } else { ... }."""
    condition: Optional[Expression] = None
    then_body: Optional[Block] = None
    elif_clauses: list[ElifClause] = field(default_factory=list)
    else_body: Optional[Block] = None


@dataclass
class ElifClause(ASTNode):
    """Elif clause."""
    condition: Optional[Expression] = None
    body: Optional[Block] = None


@dataclass
class WhileStmt(Statement):
    """While loop: while cond { ... }."""
    condition: Optional[Expression] = None
    body: Optional[Block] = None


@dataclass
class ForStmt(Statement):
    """For loop: for item in iterable { ... }."""
    variable: str = ""
    iterable: Optional[Expression] = None
    body: Optional[Block] = None


@dataclass
class MatchStmt(Statement):
    """Match statement (when used as statement)."""
    subject: Optional[Expression] = None
    arms: list[MatchArm] = field(default_factory=list)


# ══════════════════════════════════════════════════════════════
#  Declaration Nodes
# ══════════════════════════════════════════════════════════════

@dataclass
class Parameter(ASTNode):
    """Function parameter: name: type."""
    name: str = ""
    type_annotation: Optional[GreyType] = None
    default_value: Optional[Expression] = None


@dataclass
class VarDecl(Declaration):
    """Variable declaration: let x: int = 42; / let mut y = 10; / const Z = 100;"""
    name: str = ""
    type_annotation: Optional[GreyType] = None
    initializer: Optional[Expression] = None
    mutable: bool = False
    is_const: bool = False


@dataclass
class FnDecl(Declaration):
    """Function declaration: fn name(params) -> return_type { body }."""
    name: str = ""
    params: list[Parameter] = field(default_factory=list)
    return_type: Optional[GreyType] = None
    body: Optional[Block] = None
    is_public: bool = False
    is_method: bool = False


@dataclass
class StructDecl(Declaration):
    """Struct declaration: struct Name { field: type, ... }."""
    name: str = ""
    fields: list[StructField] = field(default_factory=list)
    is_public: bool = False


@dataclass
class StructField(ASTNode):
    """Field in a struct declaration."""
    name: str = ""
    type_annotation: Optional[GreyType] = None
    is_public: bool = False


@dataclass
class EnumDecl(Declaration):
    """Enum declaration: enum Name { Variant1, Variant2(int), ... }."""
    name: str = ""
    variants: list[EnumVariant] = field(default_factory=list)
    is_public: bool = False


@dataclass
class EnumVariant(ASTNode):
    """Variant in an enum declaration."""
    name: str = ""
    fields: list[GreyType] = field(default_factory=list)


@dataclass
class ImplDecl(Declaration):
    """Implementation block: impl TypeName { methods... }."""
    target: str = ""
    trait_name: Optional[str] = None
    methods: list[FnDecl] = field(default_factory=list)


@dataclass
class TraitDecl(Declaration):
    """Trait declaration: trait Name { method signatures... }."""
    name: str = ""
    methods: list[FnDecl] = field(default_factory=list)
    is_public: bool = False


@dataclass
class TypeAlias(Declaration):
    """Type alias: type Name = ExistingType;"""
    name: str = ""
    aliased_type: Optional[GreyType] = None


@dataclass
class ImportDecl(Declaration):
    """Import declaration: import module; / from module import name;"""
    module_path: str = ""
    names: list[ImportName] = field(default_factory=list)
    alias: Optional[str] = None


@dataclass
class ImportName(ASTNode):
    """Individual import name."""
    name: str = ""
    alias: Optional[str] = None


# ══════════════════════════════════════════════════════════════
#  Program (Root Node)
# ══════════════════════════════════════════════════════════════

@dataclass
class Program(ASTNode):
    """Root node: represents an entire Grey program."""
    declarations: list[Declaration] = field(default_factory=list)
    statements: list[Statement] = field(default_factory=list)


# ══════════════════════════════════════════════════════════════
#  AST Visitor Pattern
# ══════════════════════════════════════════════════════════════

class ASTVisitor:
    """
    Base visitor for walking the AST.
    Override visit_<NodeClassName> methods to handle specific nodes.
    """

    def generic_visit(self, node: ASTNode):
        raise NotImplementedError(
            f"No visit method for {type(node).__name__}"
        )

    # Default implementations that recurse
    def visit_Program(self, node: Program):
        for decl in node.declarations:
            decl.accept(self)
        for stmt in node.statements:
            stmt.accept(self)

    def visit_Block(self, node: Block):
        for stmt in node.statements:
            stmt.accept(self)

    def visit_ExpressionStmt(self, node: ExpressionStmt):
        if node.expression:
            node.expression.accept(self)

    def visit_BinaryExpr(self, node: BinaryExpr):
        if node.left:
            node.left.accept(self)
        if node.right:
            node.right.accept(self)

    def visit_UnaryExpr(self, node: UnaryExpr):
        if node.operand:
            node.operand.accept(self)

    def visit_CallExpr(self, node: CallExpr):
        if node.callee:
            node.callee.accept(self)
        for arg in node.arguments:
            arg.accept(self)


class ASTPrinter(ASTVisitor):
    """Pretty-prints an AST for debugging."""

    def __init__(self):
        self.indent = 0
        self.output: list[str] = []

    def _emit(self, text: str):
        self.output.append("  " * self.indent + text)

    def _indent(self):
        self.indent += 1

    def _dedent(self):
        self.indent -= 1

    def print(self, node: ASTNode) -> str:
        self.output = []
        self.indent = 0
        node.accept(self)
        return "\n".join(self.output)

    def visit_Program(self, node: Program):
        self._emit("Program")
        self._indent()
        for decl in node.declarations:
            decl.accept(self)
        for stmt in node.statements:
            stmt.accept(self)
        self._dedent()

    def visit_VarDecl(self, node: VarDecl):
        mut = "mut " if node.mutable else ""
        const = "const " if node.is_const else "let "
        type_str = f": {self._type_str(node.type_annotation)}" if node.type_annotation else ""
        self._emit(f"{const}{mut}{node.name}{type_str}")
        if node.initializer:
            self._indent()
            self._emit("= ")
            node.initializer.accept(self)
            self._dedent()

    def visit_FnDecl(self, node: FnDecl):
        params = ", ".join(
            f"{p.name}: {self._type_str(p.type_annotation)}" for p in node.params
        )
        ret = f" -> {self._type_str(node.return_type)}" if node.return_type else ""
        self._emit(f"fn {node.name}({params}){ret}")
        if node.body:
            self._indent()
            node.body.accept(self)
            self._dedent()

    def visit_StructDecl(self, node: StructDecl):
        self._emit(f"struct {node.name}")
        self._indent()
        for f in node.fields:
            self._emit(f"{f.name}: {self._type_str(f.type_annotation)}")
        self._dedent()

    def visit_Block(self, node: Block):
        self._emit("{")
        self._indent()
        for stmt in node.statements:
            stmt.accept(self)
        self._dedent()
        self._emit("}")

    def visit_IfStmt(self, node: IfStmt):
        self._emit("if")
        self._indent()
        if node.condition:
            node.condition.accept(self)
        self._dedent()
        if node.then_body:
            node.then_body.accept(self)
        for elif_clause in node.elif_clauses:
            self._emit("elif")
            self._indent()
            if elif_clause.condition:
                elif_clause.condition.accept(self)
            self._dedent()
            if elif_clause.body:
                elif_clause.body.accept(self)
        if node.else_body:
            self._emit("else")
            node.else_body.accept(self)

    def visit_WhileStmt(self, node: WhileStmt):
        self._emit("while")
        self._indent()
        if node.condition:
            node.condition.accept(self)
        self._dedent()
        if node.body:
            node.body.accept(self)

    def visit_ForStmt(self, node: ForStmt):
        self._emit(f"for {node.variable} in")
        self._indent()
        if node.iterable:
            node.iterable.accept(self)
        self._dedent()
        if node.body:
            node.body.accept(self)

    def visit_ReturnStmt(self, node: ReturnStmt):
        self._emit("return")
        if node.value:
            self._indent()
            node.value.accept(self)
            self._dedent()

    def visit_BreakStmt(self, node: BreakStmt):
        self._emit("break")

    def visit_ContinueStmt(self, node: ContinueStmt):
        self._emit("continue")

    def visit_ExpressionStmt(self, node: ExpressionStmt):
        if node.expression:
            node.expression.accept(self)

    def visit_IntegerLiteral(self, node: IntegerLiteral):
        self._emit(f"Int({node.value})")

    def visit_FloatLiteral(self, node: FloatLiteral):
        self._emit(f"Float({node.value})")

    def visit_StringLiteral(self, node: StringLiteral):
        self._emit(f"String({node.value!r})")

    def visit_BoolLiteral(self, node: BoolLiteral):
        self._emit(f"Bool({node.value})")

    def visit_NilLiteral(self, node: NilLiteral):
        self._emit("Nil")

    def visit_Identifier(self, node: Identifier):
        self._emit(f"Id({node.name})")

    def visit_BinaryExpr(self, node: BinaryExpr):
        self._emit(f"BinOp({node.operator})")
        self._indent()
        if node.left:
            node.left.accept(self)
        if node.right:
            node.right.accept(self)
        self._dedent()

    def visit_UnaryExpr(self, node: UnaryExpr):
        self._emit(f"UnaryOp({node.operator})")
        self._indent()
        if node.operand:
            node.operand.accept(self)
        self._dedent()

    def visit_CallExpr(self, node: CallExpr):
        self._emit("Call")
        self._indent()
        if node.callee:
            node.callee.accept(self)
        for arg in node.arguments:
            arg.accept(self)
        self._dedent()

    def visit_IndexExpr(self, node: IndexExpr):
        self._emit("Index")
        self._indent()
        if node.object:
            node.object.accept(self)
        if node.index:
            node.index.accept(self)
        self._dedent()

    def visit_MemberExpr(self, node: MemberExpr):
        self._emit(f"Member(.{node.member})")
        self._indent()
        if node.object:
            node.object.accept(self)
        self._dedent()

    def visit_AssignExpr(self, node: AssignExpr):
        self._emit(f"Assign({node.operator})")
        self._indent()
        if node.target:
            node.target.accept(self)
        if node.value:
            node.value.accept(self)
        self._dedent()

    def visit_ArrayLiteral(self, node: ArrayLiteral):
        self._emit("Array")
        self._indent()
        for elem in node.elements:
            elem.accept(self)
        self._dedent()

    def visit_RangeExpr(self, node: RangeExpr):
        self._emit(f"Range({'..=' if node.inclusive else '..'})")
        self._indent()
        if node.start:
            node.start.accept(self)
        if node.end:
            node.end.accept(self)
        self._dedent()

    def _type_str(self, t: Optional[GreyType]) -> str:
        if t is None:
            return "?"
        if isinstance(t, SimpleType):
            return t.name
        if isinstance(t, ArrayType):
            return f"[{self._type_str(t.element_type)}]"
        if isinstance(t, FunctionType):
            params = ", ".join(self._type_str(p) for p in t.param_types)
            ret = self._type_str(t.return_type)
            return f"fn({params}) -> {ret}"
        if isinstance(t, OptionalType):
            return f"{self._type_str(t.inner_type)}?"
        return str(t)


# ══════════════════════════════════════════════════════════════
#  Aliases for convenience / backward compatibility
# ══════════════════════════════════════════════════════════════

# Expression aliases
BinaryExpression = BinaryExpr
UnaryExpression = UnaryExpr
CallExpression = CallExpr
IndexExpression = IndexExpr
MemberExpression = MemberExpr
Assignment = AssignExpr
LambdaExpression = LambdaExpr
NumberLiteral = IntegerLiteral  # generic alias

# Statement aliases
ExpressionStatement = ExpressionStmt
BlockStatement = Block
ReturnStatement = ReturnStmt
BreakStatement = BreakStmt
ContinueStatement = ContinueStmt
IfStatement = IfStmt
WhileStatement = WhileStmt
ForStatement = ForStmt
MatchStatement = MatchStmt

# Declaration aliases
FunctionDeclaration = FnDecl
VariableDeclaration = VarDecl
StructDeclaration = StructDecl
EnumDeclaration = EnumDecl
ImplBlock = ImplDecl
TraitDeclaration = TraitDecl
