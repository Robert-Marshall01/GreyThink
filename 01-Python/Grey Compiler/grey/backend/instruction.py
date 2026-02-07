"""
Grey VM Instruction Set.

Defines the bytecode instruction set for the Grey Virtual Machine.
This is a register-based VM (like Lua/Dalvik) rather than stack-based
(like JVM/CPython), offering better performance characteristics.

Instruction format:
  [opcode: 8 bits] [A: 8 bits] [B: 8 bits] [C: 8 bits]
  or
  [opcode: 8 bits] [A: 8 bits] [Bx: 16 bits]  (for larger immediate values)
"""

from enum import IntEnum, auto
from dataclasses import dataclass, field
from typing import Any, Optional


class VMOpcode(IntEnum):
    """
    Bytecode opcodes for the Grey VM.

    Naming convention: CATEGORY_ACTION
    Register arguments: A = destination, B/C = source operands
    """

    # ── Loads & Stores ───────────────────────────────────────
    LOAD_CONST = 0x01       # A = constants[Bx]
    LOAD_NIL = 0x02         # A = nil
    LOAD_TRUE = 0x03        # A = true
    LOAD_FALSE = 0x04       # A = false
    LOAD_LOCAL = 0x05       # A = locals[B]
    STORE_LOCAL = 0x06      # locals[B] = A
    LOAD_GLOBAL = 0x07      # A = globals[Bx]
    STORE_GLOBAL = 0x08     # globals[Bx] = A
    LOAD_UPVALUE = 0x09     # A = upvalues[B]  (for closures)
    STORE_UPVALUE = 0x0A    # upvalues[B] = A

    # ── Arithmetic ───────────────────────────────────────────
    ADD = 0x10              # A = B + C
    SUB = 0x11              # A = B - C
    MUL = 0x12              # A = B * C
    DIV = 0x13              # A = B / C
    MOD = 0x14              # A = B % C
    POW = 0x15              # A = B ** C
    NEG = 0x16              # A = -B
    FLOOR_DIV = 0x17        # A = B // C

    # ── Bitwise ──────────────────────────────────────────────
    BIT_AND = 0x20          # A = B & C
    BIT_OR = 0x21           # A = B | C
    BIT_XOR = 0x22          # A = B ^ C
    BIT_NOT = 0x23          # A = ~B
    SHL = 0x24              # A = B << C
    SHR = 0x25              # A = B >> C

    # ── Comparison ───────────────────────────────────────────
    EQ = 0x30               # A = (B == C)
    NEQ = 0x31              # A = (B != C)
    LT = 0x32               # A = (B < C)
    GT = 0x33               # A = (B > C)
    LTE = 0x34              # A = (B <= C)
    GTE = 0x35              # A = (B >= C)

    # ── Logical ──────────────────────────────────────────────
    NOT = 0x40              # A = not B
    AND = 0x41              # A = B and C
    OR = 0x42               # A = B or C

    # ── Control Flow ─────────────────────────────────────────
    JUMP = 0x50             # PC += sBx (signed offset)
    JUMP_IF_TRUE = 0x51     # if A: PC += sBx
    JUMP_IF_FALSE = 0x52    # if not A: PC += sBx
    CALL = 0x53             # A = call(B, C args)  B=function, C=arg count
    RETURN = 0x54           # return A (or void)
    RETURN_NONE = 0x55      # return (no value)

    # ── Array Operations ─────────────────────────────────────
    ARRAY_NEW = 0x60        # A = new array(size=B)
    ARRAY_GET = 0x61        # A = B[C]
    ARRAY_SET = 0x62        # A[B] = C
    ARRAY_LEN = 0x63        # A = len(B)
    ARRAY_PUSH = 0x64       # push(A, B)
    ARRAY_POP = 0x65        # A = pop(B)

    # ── Struct Operations ────────────────────────────────────
    STRUCT_NEW = 0x70       # A = new struct(type_id=Bx)
    FIELD_GET = 0x71        # A = B.field[C]
    FIELD_SET = 0x72        # A.field[B] = C

    # ── String Operations ────────────────────────────────────
    CONCAT = 0x80           # A = B + C (string concat)
    STR_LEN = 0x81          # A = len(B)

    # ── Type Operations ──────────────────────────────────────
    TYPE_CHECK = 0x90       # A = isinstance(B, type_id=C)
    CAST = 0x91             # A = cast(B, type_id=C)
    TO_STRING = 0x92        # A = str(B)
    TO_INT = 0x93           # A = int(B)
    TO_FLOAT = 0x94         # A = float(B)

    # ── Closure ──────────────────────────────────────────────
    CLOSURE = 0xA0          # A = closure(fn_index=Bx)
    CLOSE_UPVALUE = 0xA1    # close upvalue at stack[A]

    # ── Built-in Calls ───────────────────────────────────────
    BUILTIN_CALL = 0xB0     # A = builtin(id=B, args=C)
    PRINT = 0xB1            # print(A)
    PRINTLN = 0xB2          # println(A)
    INPUT = 0xB3            # A = input(prompt=B)
    ASSERT = 0xB4           # assert(A, message=B)

    # ── Misc ─────────────────────────────────────────────────
    NOP = 0xF0              # no operation
    HALT = 0xFF             # stop execution
    MOVE = 0xF1             # A = B
    DUP = 0xF2              # A = copy of B


@dataclass
class VMInstruction:
    """
    A single VM bytecode instruction.
    """
    opcode: VMOpcode
    a: int = 0              # destination register or first operand
    b: int = 0              # second operand or immediate
    c: int = 0              # third operand
    bx: int = 0             # 16-bit extended operand (unsigned)
    sbx: int = 0            # 16-bit signed offset (for jumps)
    comment: str = ""       # optional debug comment

    def __post_init__(self):
        """Sync bx/sbx with b/c fields for proper encoding."""
        if self.bx and not (self.b or self.c):
            # bx was set directly — derive b and c from it
            self.b = (self.bx >> 8) & 0xFF
            self.c = self.bx & 0xFF
        elif (self.b or self.c) and not self.bx:
            # b/c were set — derive bx
            self.bx = (self.b << 8) | self.c
        if self.sbx and not (self.b or self.c):
            # sbx was set directly — encode as unsigned 16-bit in b/c
            unsigned = self.sbx & 0xFFFF
            self.b = (unsigned >> 8) & 0xFF
            self.c = unsigned & 0xFF

    def __repr__(self):
        parts = [f"{self.opcode.name:20s}"]
        if self.opcode in _ABC_OPCODES:
            parts.append(f"R{self.a}, R{self.b}, R{self.c}")
        elif self.opcode in _ABx_OPCODES:
            parts.append(f"R{self.a}, {self.bx}")
        elif self.opcode in _AsBx_OPCODES:
            parts.append(f"R{self.a}, {self.sbx:+d}")
        elif self.opcode in _sBx_OPCODES:
            parts.append(f"{self.sbx:+d}")
        elif self.opcode in _A_OPCODES:
            parts.append(f"R{self.a}")
        elif self.opcode in _AB_OPCODES:
            parts.append(f"R{self.a}, R{self.b}")
        else:
            if self.a or self.b or self.c:
                parts.append(f"R{self.a}, R{self.b}, R{self.c}")

        result = " ".join(parts)
        if self.comment:
            result += f"  ; {self.comment}"
        return result

    def encode(self) -> int:
        """Encode instruction into a 32-bit integer."""
        return (self.opcode << 24) | (self.a << 16) | (self.b << 8) | self.c

    @staticmethod
    def decode(word: int) -> 'VMInstruction':
        """Decode a 32-bit integer into an instruction."""
        opcode = VMOpcode((word >> 24) & 0xFF)
        a = (word >> 16) & 0xFF
        b = (word >> 8) & 0xFF
        c = word & 0xFF
        return VMInstruction(opcode=opcode, a=a, b=b, c=c, bx=(b << 8) | c)


# Categorize opcodes by their operand format
_ABC_OPCODES = {
    VMOpcode.ADD, VMOpcode.SUB, VMOpcode.MUL, VMOpcode.DIV,
    VMOpcode.MOD, VMOpcode.POW, VMOpcode.FLOOR_DIV,
    VMOpcode.BIT_AND, VMOpcode.BIT_OR, VMOpcode.BIT_XOR,
    VMOpcode.SHL, VMOpcode.SHR,
    VMOpcode.EQ, VMOpcode.NEQ, VMOpcode.LT, VMOpcode.GT,
    VMOpcode.LTE, VMOpcode.GTE,
    VMOpcode.AND, VMOpcode.OR,
    VMOpcode.ARRAY_GET, VMOpcode.ARRAY_SET,
    VMOpcode.FIELD_GET, VMOpcode.FIELD_SET,
    VMOpcode.CALL,
}

_AB_OPCODES = {
    VMOpcode.NEG, VMOpcode.BIT_NOT, VMOpcode.NOT,
    VMOpcode.MOVE, VMOpcode.DUP,
    VMOpcode.ARRAY_NEW, VMOpcode.ARRAY_LEN, VMOpcode.ARRAY_PUSH, VMOpcode.ARRAY_POP,
    VMOpcode.STR_LEN, VMOpcode.CONCAT,
    VMOpcode.STORE_LOCAL, VMOpcode.LOAD_LOCAL,
    VMOpcode.TO_STRING, VMOpcode.TO_INT, VMOpcode.TO_FLOAT,
}

_ABx_OPCODES = {
    VMOpcode.LOAD_CONST, VMOpcode.LOAD_GLOBAL, VMOpcode.STORE_GLOBAL,
    VMOpcode.CLOSURE, VMOpcode.STRUCT_NEW,
}

_AsBx_OPCODES = {
    VMOpcode.JUMP_IF_TRUE, VMOpcode.JUMP_IF_FALSE,
}

_sBx_OPCODES = {
    VMOpcode.JUMP,
}

_A_OPCODES = {
    VMOpcode.LOAD_NIL, VMOpcode.LOAD_TRUE, VMOpcode.LOAD_FALSE,
    VMOpcode.RETURN, VMOpcode.RETURN_NONE,
    VMOpcode.PRINT, VMOpcode.PRINTLN,
    VMOpcode.HALT,
}


# ══════════════════════════════════════════════════════════════
#  Bytecode Chunk (compiled function)
# ══════════════════════════════════════════════════════════════

@dataclass
class Chunk:
    """
    A compiled function/code block.
    Contains the instruction sequence, constant pool, and debug info.
    """
    name: str = ""
    instructions: list[VMInstruction] = field(default_factory=list)
    constants: list[Any] = field(default_factory=list)
    local_count: int = 0
    param_count: int = 0
    upvalue_count: int = 0
    max_registers: int = 256
    # Debug info
    line_numbers: list[int] = field(default_factory=list)
    local_names: list[str] = field(default_factory=list)
    source_file: str = ""

    def add_constant(self, value: Any) -> int:
        """Add a constant to the pool and return its index."""
        # Dedup constants
        for i, existing in enumerate(self.constants):
            if existing == value and type(existing) == type(value):
                return i
        idx = len(self.constants)
        self.constants.append(value)
        return idx

    def emit(self, instr: VMInstruction, line: int = 0):
        """Emit an instruction."""
        self.instructions.append(instr)
        self.line_numbers.append(line)

    def __repr__(self):
        lines = [f"=== Chunk: {self.name} ==="]
        lines.append(f"  Constants: {self.constants}")
        lines.append(f"  Locals: {self.local_count}, Params: {self.param_count}")
        lines.append(f"  Instructions:")
        for i, instr in enumerate(self.instructions):
            line_no = self.line_numbers[i] if i < len(self.line_numbers) else 0
            lines.append(f"    {i:4d}  [{line_no:3d}]  {instr}")
        return "\n".join(lines)


@dataclass
class CompiledProgram:
    """
    The complete compiled output - all chunks plus metadata.
    """
    chunks: list[Chunk] = field(default_factory=list)
    main_chunk_index: int = 0
    global_names: list[str] = field(default_factory=list)
    string_table: list[str] = field(default_factory=list)
    version: str = "1.0"

    def add_chunk(self, chunk: Chunk) -> int:
        idx = len(self.chunks)
        self.chunks.append(chunk)
        return idx

    def __repr__(self):
        lines = [f"CompiledProgram v{self.version}"]
        lines.append(f"  Main chunk: {self.main_chunk_index}")
        lines.append(f"  Global names: {self.global_names}")
        lines.append(f"  Chunks: {len(self.chunks)}")
        for chunk in self.chunks:
            lines.append("")
            lines.append(str(chunk))
        return "\n".join(lines)
