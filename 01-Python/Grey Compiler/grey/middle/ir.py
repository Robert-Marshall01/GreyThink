"""
Intermediate Representation (IR) for the Grey compiler.

A three-address code style IR using SSA (Static Single Assignment) form.
This is a machine-agnostic representation that sits between the AST
and the backend code generator.

IR Structure:
  - Module: top-level container (list of functions + globals)
  - Function: named function with parameters and basic blocks
  - BasicBlock: sequence of instructions ending in a terminator
  - Instruction: individual operation (arithmetic, load, store, call, etc.)
  - Value: result of an instruction or a constant
"""

from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Optional


# ══════════════════════════════════════════════════════════════
#  IR Types
# ══════════════════════════════════════════════════════════════

class IRType(Enum):
    """IR-level type system (simplified)."""
    I32 = "i32"         # 32-bit integer
    I64 = "i64"         # 64-bit integer
    F64 = "f64"         # 64-bit float
    BOOL = "bool"       # boolean
    STR = "str"         # string (pointer to string data)
    PTR = "ptr"         # generic pointer
    VOID = "void"       # no value
    ARRAY = "array"     # array type
    STRUCT = "struct"   # struct type
    FUNC = "func"       # function pointer
    ANY = "any"         # dynamic type

    def __repr__(self):
        return self.value


# ══════════════════════════════════════════════════════════════
#  IR Values
# ══════════════════════════════════════════════════════════════

@dataclass
class IRValue:
    """Base class for all IR values."""
    name: str = ""
    ir_type: IRType = IRType.ANY

    def __repr__(self):
        return f"%{self.name}"


@dataclass
class IRConst(IRValue):
    """An IR constant value."""
    value: Any = None

    def __repr__(self):
        if self.ir_type == IRType.STR:
            return f'"{self.value}"'
        if self.ir_type == IRType.BOOL:
            return "true" if self.value else "false"
        return str(self.value)


@dataclass
class IRTemp(IRValue):
    """A temporary value (SSA virtual register)."""
    index: int = 0

    def __repr__(self):
        return f"%t{self.index}"


@dataclass
class IRParam(IRValue):
    """A function parameter."""
    param_index: int = 0

    def __repr__(self):
        return f"%arg{self.param_index}"


@dataclass
class IRGlobal(IRValue):
    """A global variable reference."""

    def __repr__(self):
        return f"@{self.name}"


# ══════════════════════════════════════════════════════════════
#  IR Instructions (Three-Address Code)
# ══════════════════════════════════════════════════════════════

class Opcode(Enum):
    """All IR operation codes."""
    # Arithmetic
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    MOD = auto()
    POW = auto()
    NEG = auto()

    # Bitwise
    BIT_AND = auto()
    BIT_OR = auto()
    BIT_XOR = auto()
    BIT_NOT = auto()
    SHL = auto()
    SHR = auto()

    # Comparison
    EQ = auto()
    NEQ = auto()
    LT = auto()
    GT = auto()
    LTE = auto()
    GTE = auto()

    # Logical
    AND = auto()
    OR = auto()
    NOT = auto()

    # Memory
    LOAD = auto()        # load from address
    STORE = auto()       # store to address
    ALLOCA = auto()      # stack allocation
    GET_FIELD = auto()   # struct field access
    SET_FIELD = auto()   # struct field set
    GET_INDEX = auto()   # array index read
    SET_INDEX = auto()   # array index write

    # Control flow (terminators)
    JUMP = auto()        # unconditional jump
    BRANCH = auto()      # conditional branch
    RETURN = auto()      # return from function
    CALL = auto()        # function call
    CALL_BUILTIN = auto()  # built-in function call

    # Type conversion
    INT_TO_FLOAT = auto()
    FLOAT_TO_INT = auto()
    TO_STRING = auto()
    TO_BOOL = auto()

    # Constants
    CONST = auto()       # define a constant

    # Array operations
    ARRAY_NEW = auto()   # create new array
    ARRAY_LEN = auto()   # get array length
    ARRAY_PUSH = auto()  # push element

    # Misc
    PHI = auto()         # SSA phi node
    NOP = auto()         # no operation
    COPY = auto()        # copy value
    LABEL = auto()       # label (pseudo-instruction)


@dataclass
class IRInstruction:
    """
    A single IR instruction.

    Format: result = opcode operand1, operand2, ...
    """
    opcode: Opcode
    result: Optional[IRValue] = None
    operands: list[IRValue] = field(default_factory=list)
    ir_type: IRType = IRType.ANY
    metadata: dict = field(default_factory=dict)

    def __repr__(self):
        result_str = f"{self.result} = " if self.result else ""
        ops_str = ", ".join(str(op) for op in self.operands)
        return f"  {result_str}{self.opcode.name.lower()} {ops_str}"

    @property
    def is_terminator(self) -> bool:
        """Check if this instruction is a block terminator."""
        return self.opcode in (Opcode.JUMP, Opcode.BRANCH, Opcode.RETURN)

    @property
    def has_side_effects(self) -> bool:
        return self.opcode in (
            Opcode.STORE, Opcode.SET_FIELD, Opcode.SET_INDEX,
            Opcode.CALL, Opcode.CALL_BUILTIN,
            Opcode.ARRAY_PUSH, Opcode.RETURN,
        )

    @property
    def is_pure(self) -> bool:
        """Check if this instruction is purely computational (no side effects)."""
        return not self.has_side_effects and not self.is_terminator


# ══════════════════════════════════════════════════════════════
#  Basic Blocks
# ══════════════════════════════════════════════════════════════

@dataclass
class BasicBlock:
    """
    A basic block: a sequence of instructions with a single entry
    and a single exit (terminator instruction).
    """
    label: str = ""
    instructions: list[IRInstruction] = field(default_factory=list)
    predecessors: list[BasicBlock] = field(default_factory=list, repr=False)
    successors: list[BasicBlock] = field(default_factory=list, repr=False)

    def append(self, instr: IRInstruction):
        self.instructions.append(instr)

    @property
    def terminator(self) -> Optional[IRInstruction]:
        if self.instructions and self.instructions[-1].is_terminator:
            return self.instructions[-1]
        return None

    @property
    def is_terminated(self) -> bool:
        return self.terminator is not None

    def __repr__(self):
        lines = [f"{self.label}:"]
        for instr in self.instructions:
            lines.append(str(instr))
        return "\n".join(lines)


# ══════════════════════════════════════════════════════════════
#  Functions
# ══════════════════════════════════════════════════════════════

@dataclass
class IRFunction:
    """
    An IR function containing basic blocks.
    """
    name: str = ""
    params: list[IRParam] = field(default_factory=list)
    return_type: IRType = IRType.VOID
    blocks: list[BasicBlock] = field(default_factory=list)
    locals: dict[str, IRValue] = field(default_factory=dict)
    is_external: bool = False

    @property
    def entry_block(self) -> Optional[BasicBlock]:
        return self.blocks[0] if self.blocks else None

    def add_block(self, label: str) -> BasicBlock:
        block = BasicBlock(label=label)
        self.blocks.append(block)
        return block

    def __repr__(self):
        params_str = ", ".join(
            f"{p}: {p.ir_type.value}" for p in self.params
        )
        lines = [f"fn {self.name}({params_str}) -> {self.return_type.value} {{"]
        for block in self.blocks:
            lines.append(str(block))
        lines.append("}")
        return "\n".join(lines)


# ══════════════════════════════════════════════════════════════
#  Global Variables
# ══════════════════════════════════════════════════════════════

@dataclass
class IRGlobalVar:
    """A global variable definition."""
    name: str = ""
    ir_type: IRType = IRType.ANY
    initializer: Optional[IRConst] = None
    is_const: bool = False

    def __repr__(self):
        const_str = "const " if self.is_const else ""
        init_str = f" = {self.initializer}" if self.initializer else ""
        return f"@{self.name}: {const_str}{self.ir_type.value}{init_str}"


# ══════════════════════════════════════════════════════════════
#  Module (Top-level IR container)
# ══════════════════════════════════════════════════════════════

@dataclass
class IRModule:
    """
    The top-level IR container representing a compilation unit.
    Contains globals and functions.
    """
    name: str = ""
    globals: list[IRGlobalVar] = field(default_factory=list)
    functions: list[IRFunction] = field(default_factory=list)
    string_pool: dict[str, int] = field(default_factory=dict)

    def add_function(self, func: IRFunction):
        self.functions.append(func)

    def add_global(self, glob: IRGlobalVar):
        self.globals.append(glob)

    def intern_string(self, s: str) -> int:
        """Intern a string literal and return its index."""
        if s not in self.string_pool:
            self.string_pool[s] = len(self.string_pool)
        return self.string_pool[s]

    def find_function(self, name: str) -> Optional[IRFunction]:
        for fn in self.functions:
            if fn.name == name:
                return fn
        return None

    def __repr__(self):
        lines = [f"module {self.name}"]
        lines.append("")
        for g in self.globals:
            lines.append(str(g))
        if self.globals:
            lines.append("")
        for fn in self.functions:
            lines.append(str(fn))
            lines.append("")
        return "\n".join(lines)
