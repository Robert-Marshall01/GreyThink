"""
Symbol Table for the Grey language.

Manages scoped symbol information for semantic analysis:
  - Variable declarations and their types
  - Function signatures
  - Struct/enum definitions
  - Scope nesting and lookup
"""

from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Optional


class SymbolKind(Enum):
    """Classification of symbols."""
    VARIABLE = auto()
    FUNCTION = auto()
    PARAMETER = auto()
    STRUCT = auto()
    ENUM = auto()
    TRAIT = auto()
    TYPE_ALIAS = auto()
    ENUM_VARIANT = auto()
    FIELD = auto()
    METHOD = auto()
    MODULE = auto()
    CONSTANT = auto()
    BUILTIN = auto()


@dataclass
class TypeInfo:
    """
    Represents resolved type information.

    This is the internal type representation used by the semantic analyzer
    (distinct from AST GreyType nodes which are syntactic).
    """
    name: str
    is_array: bool = False
    element_type: Optional[TypeInfo] = None
    is_optional: bool = False
    is_function: bool = False
    param_types: list[TypeInfo] = field(default_factory=list)
    return_type: Optional[TypeInfo] = None
    is_struct: bool = False
    fields: dict[str, TypeInfo] = field(default_factory=dict)
    is_enum: bool = False
    variants: dict[str, list[TypeInfo]] = field(default_factory=dict)
    is_generic: bool = False
    is_mutable: bool = False

    # Built-in type constants (created lazily)
    _INT: TypeInfo = None       # type: ignore
    _FLOAT: TypeInfo = None     # type: ignore
    _STRING: TypeInfo = None    # type: ignore
    _BOOL: TypeInfo = None      # type: ignore
    _CHAR: TypeInfo = None      # type: ignore
    _VOID: TypeInfo = None      # type: ignore
    _NIL: TypeInfo = None       # type: ignore
    _ANY: TypeInfo = None       # type: ignore
    _ERROR: TypeInfo = None     # type: ignore

    def __eq__(self, other):
        if not isinstance(other, TypeInfo):
            return False
        if self.name == "any" or other.name == "any":
            return True
        if self.is_array and other.is_array:
            return self.element_type == other.element_type
        if self.is_function and other.is_function:
            return (self.param_types == other.param_types and
                    self.return_type == other.return_type)
        return self.name == other.name

    def __hash__(self):
        return hash(self.name)

    def __repr__(self):
        if self.is_array:
            return f"[{self.element_type}]"
        if self.is_function:
            params = ", ".join(str(p) for p in self.param_types)
            return f"fn({params}) -> {self.return_type}"
        if self.is_optional:
            return f"{self.name}?"
        return self.name

    def is_numeric(self) -> bool:
        return self.name in ("int", "float")

    def is_compatible_with(self, other: TypeInfo) -> bool:
        """Check if this type is assignment-compatible with another."""
        if self == other:
            return True
        # int can be promoted to float
        if self.name == "int" and other.name == "float":
            return True
        # nil is compatible with optional types
        if self.name == "nil" and other.is_optional:
            return True
        # any matches everything
        if self.name == "any" or other.name == "any":
            return True
        # error type is compatible with everything (for error recovery)
        if self.name == "<error>" or other.name == "<error>":
            return True
        return False


# Built-in type singletons
def _make_builtin(name: str) -> TypeInfo:
    return TypeInfo(name=name)


INT_TYPE = _make_builtin("int")
FLOAT_TYPE = _make_builtin("float")
STRING_TYPE = _make_builtin("string")
BOOL_TYPE = _make_builtin("bool")
CHAR_TYPE = _make_builtin("char")
VOID_TYPE = _make_builtin("void")
NIL_TYPE = _make_builtin("nil")
ANY_TYPE = _make_builtin("any")
ERROR_TYPE = _make_builtin("<error>")

BUILTIN_TYPES = {
    "int": INT_TYPE,
    "float": FLOAT_TYPE,
    "string": STRING_TYPE,
    "bool": BOOL_TYPE,
    "char": CHAR_TYPE,
    "void": VOID_TYPE,
    "nil": NIL_TYPE,
    "any": ANY_TYPE,
}


@dataclass
class Symbol:
    """
    A symbol in the symbol table.

    Attributes:
        name:       The identifier name
        kind:       What kind of symbol this is
        type_info:  The resolved type of this symbol
        mutable:    Whether this symbol can be reassigned
        defined_at: Source location of definition
        scope_depth: How deep in scope nesting this was defined
    """
    name: str
    kind: SymbolKind
    type_info: TypeInfo
    mutable: bool = False
    defined_at: Any = None   # SourceLocation
    scope_depth: int = 0
    is_public: bool = False
    is_initialized: bool = False
    used: bool = False
    metadata: dict = field(default_factory=dict)

    def __repr__(self):
        return f"Symbol({self.name}: {self.type_info}, {self.kind.name})"


class Scope:
    """
    A single scope level in the symbol table.

    Scopes form a tree structure with parent/child relationships.
    """

    def __init__(self, parent: Optional[Scope] = None, name: str = ""):
        self.parent = parent
        self.name = name
        self.symbols: dict[str, Symbol] = {}
        self.children: list[Scope] = []
        self.depth = parent.depth + 1 if parent else 0

    def define(self, symbol: Symbol) -> bool:
        """
        Define a new symbol in this scope.
        Returns False if already defined in this scope.
        """
        if symbol.name in self.symbols:
            return False
        symbol.scope_depth = self.depth
        self.symbols[symbol.name] = symbol
        return True

    def lookup(self, name: str) -> Optional[Symbol]:
        """Look up a symbol in this scope only (not parents)."""
        return self.symbols.get(name)

    def resolve(self, name: str) -> Optional[Symbol]:
        """Resolve a symbol, searching up through parent scopes."""
        sym = self.symbols.get(name)
        if sym is not None:
            return sym
        if self.parent is not None:
            return self.parent.resolve(name)
        return None

    def __repr__(self):
        return f"Scope({self.name}, depth={self.depth}, symbols={list(self.symbols.keys())})"


class SymbolTable:
    """
    Manages the scope tree and symbol resolution.

    Usage:
        table = SymbolTable()
        table.enter_scope("function_body")
        table.define(Symbol("x", SymbolKind.VARIABLE, INT_TYPE))
        sym = table.resolve("x")
        table.exit_scope()
    """

    def __init__(self):
        self.global_scope = Scope(name="global")
        self.current_scope = self.global_scope
        self._init_builtins()

    def _init_builtins(self):
        """Register built-in functions and types."""
        builtins = [
            # I/O functions
            Symbol("print", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=VOID_TYPE)),
            Symbol("println", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=VOID_TYPE)),
            Symbol("input", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[STRING_TYPE], return_type=STRING_TYPE)),

            # Type conversion
            Symbol("int", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=INT_TYPE)),
            Symbol("float", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=FLOAT_TYPE)),
            Symbol("str", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=STRING_TYPE)),
            Symbol("bool", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=BOOL_TYPE)),

            # Collection functions
            Symbol("len", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=INT_TYPE)),
            Symbol("push", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE, ANY_TYPE], return_type=VOID_TYPE)),
            Symbol("pop", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=ANY_TYPE)),
            Symbol("append", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE, ANY_TYPE], return_type=VOID_TYPE)),

            # Math functions
            Symbol("abs", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=ANY_TYPE)),
            Symbol("min", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE, ANY_TYPE], return_type=ANY_TYPE)),
            Symbol("max", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE, ANY_TYPE], return_type=ANY_TYPE)),

            # String functions
            Symbol("substr", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[STRING_TYPE, INT_TYPE, INT_TYPE],
                            return_type=STRING_TYPE)),
            Symbol("concat", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[STRING_TYPE, STRING_TYPE],
                            return_type=STRING_TYPE)),

            # Utility
            Symbol("typeof", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[ANY_TYPE], return_type=STRING_TYPE)),
            Symbol("assert", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[BOOL_TYPE], return_type=VOID_TYPE)),
            Symbol("panic", SymbolKind.BUILTIN,
                   TypeInfo(name="fn", is_function=True,
                            param_types=[STRING_TYPE], return_type=VOID_TYPE)),
        ]
        for sym in builtins:
            self.global_scope.define(sym)

    def enter_scope(self, name: str = "") -> Scope:
        """Create and enter a new child scope."""
        child = Scope(parent=self.current_scope, name=name)
        self.current_scope.children.append(child)
        self.current_scope = child
        return child

    def exit_scope(self) -> Scope:
        """Exit the current scope, returning to the parent."""
        exited = self.current_scope
        if self.current_scope.parent is not None:
            self.current_scope = self.current_scope.parent
        return exited

    def define(self, symbol: Symbol) -> bool:
        """Define a symbol in the current scope."""
        return self.current_scope.define(symbol)

    def resolve(self, name: str) -> Optional[Symbol]:
        """Resolve a symbol starting from the current scope."""
        return self.current_scope.resolve(name)

    def lookup_current(self, name: str) -> Optional[Symbol]:
        """Look up a symbol only in the current scope."""
        return self.current_scope.lookup(name)

    def resolve_type(self, name: str) -> Optional[TypeInfo]:
        """Resolve a type name to its TypeInfo."""
        # Check built-in types first
        if name in BUILTIN_TYPES:
            return BUILTIN_TYPES[name]
        # Check symbol table for user-defined types
        sym = self.resolve(name)
        if sym and sym.kind in (SymbolKind.STRUCT, SymbolKind.ENUM,
                                SymbolKind.TYPE_ALIAS, SymbolKind.TRAIT):
            return sym.type_info
        return None

    @property
    def depth(self) -> int:
        return self.current_scope.depth

    def dump(self, scope: Optional[Scope] = None, indent: int = 0) -> str:
        """Dump the symbol table for debugging."""
        scope = scope or self.global_scope
        lines = []
        prefix = "  " * indent
        lines.append(f"{prefix}Scope: {scope.name or '(unnamed)'} (depth={scope.depth})")
        for name, sym in scope.symbols.items():
            lines.append(f"{prefix}  {sym}")
        for child in scope.children:
            lines.append(self.dump(child, indent + 1))
        return "\n".join(lines)
