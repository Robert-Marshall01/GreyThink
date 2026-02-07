"""
x86-64 Native Code Generator for the Grey compiler.

Translates Grey IR directly to x86-64 assembly (NASM syntax).
Targets the System V AMD64 ABI on Linux or the Windows x64 calling
convention, and links against the C runtime for I/O and memory.

This is a *real* native backend — it produces .asm files that can be
assembled with NASM and linked with GCC/LD to produce actual executables.
"""

from __future__ import annotations
import os
import platform
from dataclasses import dataclass, field
from typing import Optional

from ..middle.ir import (
    IRModule, IRFunction, BasicBlock, IRInstruction,
    IRValue, IRConst, IRTemp, IRParam, IRGlobal,
    Opcode as IROp, IRType,
)


# ═══════════════════════════════════════════════════════════════
#  Platform detection
# ═══════════════════════════════════════════════════════════════

IS_WINDOWS = platform.system() == "Windows"

# Windows x64: RCX, RDX, R8, R9   (shadow space 32 bytes)
# System V:    RDI, RSI, RDX, RCX, R8, R9
WIN_ARG_REGS = ["rcx", "rdx", "r8", "r9"]
SYSV_ARG_REGS = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

# Floating-point argument registers
WIN_FP_ARG_REGS = ["xmm0", "xmm1", "xmm2", "xmm3"]
SYSV_FP_ARG_REGS = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"]

# Callee-saved registers we may use for locals
CALLEE_SAVED = ["rbx", "r12", "r13", "r14", "r15"]

# Scratch registers for intermediate computation
SCRATCH = ["r10", "r11"]


@dataclass
class StackSlot:
    """Tracks a variable's position on the stack (offset from RBP)."""
    name: str
    offset: int          # negative offset from rbp, e.g. -8, -16 …
    ir_type: IRType = IRType.I64
    is_param: bool = False


class X86_64CodeGenerator:
    """
    Generates x86-64 assembly (NASM syntax) from Grey IR.

    Usage:
        gen = X86_64CodeGenerator()
        asm_text = gen.generate(ir_module)
        gen.write("output.asm")
    """

    def __init__(self, *, target_os: str = "auto"):
        if target_os == "auto":
            self.windows = IS_WINDOWS
        else:
            self.windows = target_os.lower() == "windows"

        self.arg_regs = WIN_ARG_REGS if self.windows else SYSV_ARG_REGS
        self.fp_arg_regs = WIN_FP_ARG_REGS if self.windows else SYSV_FP_ARG_REGS
        self.shadow_space = 32 if self.windows else 0

        # Output buffers
        self._data: list[str] = []          # .data section
        self._rodata: list[str] = []        # .rodata / .rdata section
        self._bss: list[str] = []           # .bss section
        self._text: list[str] = []          # .text section

        # Book-keeping
        self._string_literals: dict[str, str] = {}   # value → label
        self._float_literals: dict[float, str] = {}   # value → label
        self._str_counter = 0
        self._flt_counter = 0
        self._label_counter = 0
        self._globals: dict[str, str] = {}  # ir name → asm label

        # Per-function state
        self._stack_slots: dict[str, StackSlot] = {}
        self._stack_size: int = 0
        self._current_fn: Optional[IRFunction] = None

    # ──────────────────────────────────────────────────────────
    #  Public API
    # ──────────────────────────────────────────────────────────

    def generate(self, module: IRModule) -> str:
        """Generate complete x86-64 NASM assembly for the module."""
        self._data.clear()
        self._rodata.clear()
        self._bss.clear()
        self._text.clear()
        self._string_literals.clear()
        self._float_literals.clear()

        # Pre-scan: intern all string literals used in the module
        self._prescan_strings(module)

        # Pre-scan: intern all float literals
        self._prescan_floats(module)

        # Generate global variables
        for gv in module.globals:
            self._gen_global_var(gv)

        # Generate each function
        for fn in module.functions:
            self._gen_function(fn)

        return self._assemble_output(module.name)

    def write(self, path: str, asm_text: str):
        """Write assembly text to a file."""
        with open(path, "w", encoding="utf-8") as f:
            f.write(asm_text)

    # ──────────────────────────────────────────────────────────
    #  Output assembly construction
    # ──────────────────────────────────────────────────────────

    def _assemble_output(self, module_name: str) -> str:
        lines: list[str] = []
        lines.append(f"; ===================================================")
        lines.append(f";  Grey Compiler -- x86-64 Assembly Output")
        lines.append(f";  Module: {module_name}")
        lines.append(f";  Target: {'Windows x64' if self.windows else 'System V AMD64'}")
        lines.append(f";  Assembler: NASM")
        lines.append(f"; ===================================================")
        lines.append("")

        # External C runtime symbols
        lines.append("; -- External C runtime symbols --")
        lines.append("extern printf")
        lines.append("extern puts")
        lines.append("extern scanf")
        lines.append("extern malloc")
        lines.append("extern free")
        lines.append("extern exit")
        lines.append("extern fflush")
        lines.append("")

        if self.windows:
            lines.append("default rel")
            lines.append("")

        # ── Data section ──
        ro_section = "section .rdata" if self.windows else "section .rodata"
        lines.append(f"{ro_section}")
        # Built-in format strings
        lines.append("    __fmt_int:    db \"%lld\", 0")
        lines.append("    __fmt_intln:  db \"%lld\", 10, 0")
        lines.append("    __fmt_float:  db \"%g\", 0")
        lines.append("    __fmt_floatln:db \"%g\", 10, 0")
        lines.append("    __fmt_str:    db \"%s\", 0")
        lines.append("    __fmt_strln:  db \"%s\", 10, 0")
        lines.append("    __fmt_true:   db \"true\", 0")
        lines.append("    __fmt_false:  db \"false\", 0")
        lines.append("    __fmt_nil:    db \"nil\", 0")
        lines.append("    __fmt_newline:db 10, 0")
        lines.append("    __fmt_scanf_int: db \"%lld\", 0")
        lines.append("    __fmt_scanf_str: db \"%255s\", 0")
        lines.append("")
        for line in self._rodata:
            lines.append(line)
        lines.append("")

        # ── Data section (mutable) ──
        if self._data:
            lines.append("section .data")
            for line in self._data:
                lines.append(line)
            lines.append("")

        # ── BSS section ──
        if self._bss:
            lines.append("section .bss")
            for line in self._bss:
                lines.append(line)
            lines.append("")

        # ── Text section ──
        lines.append("section .text")
        if self.windows:
            lines.append("global main")
        else:
            lines.append("global main")
        lines.append("")
        for line in self._text:
            lines.append(line)

        lines.append("")
        return "\n".join(lines)

    # ──────────────────────────────────────────────────────────
    #  String & float literal management
    # ──────────────────────────────────────────────────────────

    def _intern_string(self, value: str) -> str:
        """Intern a string literal, return its label."""
        if value in self._string_literals:
            return self._string_literals[value]
        label = f"__str_{self._str_counter}"
        self._str_counter += 1
        # Escape the string for NASM
        escaped = self._nasm_escape(value)
        self._rodata.append(f"    {label}: db {escaped}, 0")
        self._string_literals[value] = label
        return label

    def _intern_float(self, value: float) -> str:
        """Intern a float constant, return its label."""
        if value in self._float_literals:
            return self._float_literals[value]
        label = f"__flt_{self._flt_counter}"
        self._flt_counter += 1
        self._rodata.append(f"    {label}: dq {value!r}")
        self._float_literals[value] = label
        return label

    def _nasm_escape(self, s: str) -> str:
        """Convert a Python string to NASM db operands."""
        parts: list[str] = []
        current: list[str] = []
        for ch in s:
            code = ord(ch)
            if 32 <= code < 127 and ch not in ('"', "'", "\\"):
                current.append(ch)
            else:
                if current:
                    parts.append('"' + "".join(current) + '"')
                    current = []
                parts.append(str(code))
        if current:
            parts.append('"' + "".join(current) + '"')
        return ", ".join(parts) if parts else '""'

    def _new_label(self, prefix: str = "L") -> str:
        self._label_counter += 1
        return f".{prefix}_{self._label_counter}"

    # ──────────────────────────────────────────────────────────
    #  Pre-scan passes
    # ──────────────────────────────────────────────────────────

    def _prescan_strings(self, module: IRModule):
        for fn in module.functions:
            for block in fn.blocks:
                for instr in block.instructions:
                    for op in instr.operands:
                        if isinstance(op, IRConst) and op.ir_type == IRType.STR and isinstance(op.value, str):
                            self._intern_string(op.value)
                    if instr.result and isinstance(instr.result, IRConst) and instr.result.ir_type == IRType.STR:
                        self._intern_string(instr.result.value)

    def _prescan_floats(self, module: IRModule):
        for fn in module.functions:
            for block in fn.blocks:
                for instr in block.instructions:
                    for op in instr.operands:
                        if isinstance(op, IRConst) and op.ir_type == IRType.F64 and isinstance(op.value, float):
                            self._intern_float(op.value)

    # ──────────────────────────────────────────────────────────
    #  Global variable generation
    # ──────────────────────────────────────────────────────────

    def _gen_global_var(self, gv):
        label = f"_grey_{gv.name}"
        self._globals[gv.name] = label
        if gv.initializer and gv.initializer.value is not None:
            if gv.ir_type in (IRType.I32, IRType.I64, IRType.BOOL):
                self._data.append(f"    {label}: dq {gv.initializer.value}")
            elif gv.ir_type == IRType.F64:
                self._data.append(f"    {label}: dq {gv.initializer.value!r}")
            elif gv.ir_type == IRType.STR:
                sl = self._intern_string(str(gv.initializer.value))
                self._data.append(f"    {label}: dq {sl}")
            else:
                self._data.append(f"    {label}: dq 0")
        else:
            self._bss.append(f"    {label}: resq 1")

    # ══════════════════════════════════════════════════════════
    #  Function generation
    # ══════════════════════════════════════════════════════════

    def _gen_function(self, fn: IRFunction):
        self._current_fn = fn
        self._stack_slots = {}

        # Determine the assembly label
        if fn.name == "__main__":
            asm_label = "main"
        else:
            asm_label = f"_grey_{fn.name.replace('.', '_')}"

        # Compute stack layout: reserve space for all IR temps + params
        self._compute_stack_layout(fn)

        self._text.append(f"; ── function {fn.name} ──")
        if fn.name != "__main__":
            self._text.append(f"global {asm_label}")
        self._text.append(f"{asm_label}:")

        # Prologue
        self._text.append("    push rbp")
        self._text.append("    mov rbp, rsp")

        # Align stack to 16 bytes
        alloc = self._stack_size
        if alloc % 16 != 0:
            alloc += 16 - (alloc % 16)
        if alloc > 0:
            self._text.append(f"    sub rsp, {alloc}")

        # Save callee-saved registers we use
        used_callee = self._callee_saved_in_use(fn)
        for reg in used_callee:
            self._text.append(f"    push {reg}")

        # Store incoming parameters
        for i, param in enumerate(fn.params):
            slot = self._get_slot(str(param))
            if i < len(self.arg_regs):
                self._text.append(f"    mov [rbp{slot.offset:+d}], {self.arg_regs[i]}  ; param {param.name}")
            else:
                # Parameter passed on stack — at [rbp+16+8*(i-len(arg_regs))]
                stack_off = 16 + 8 * (i - len(self.arg_regs))
                if self.windows:
                    stack_off += self.shadow_space
                self._text.append(f"    mov rax, [rbp+{stack_off}]")
                self._text.append(f"    mov [rbp{slot.offset:+d}], rax  ; param {param.name}")

        # Generate basic blocks
        for block in fn.blocks:
            self._gen_block(block, asm_label)

        # If last block has no return, add one
        if fn.blocks and not fn.blocks[-1].is_terminated:
            self._emit_epilogue(used_callee, alloc)

        self._text.append("")

    def _compute_stack_layout(self, fn: IRFunction):
        """
        Assign a stack slot (offset from rbp) to every IR value used
        in the function (params, temps, locals).
        """
        self._stack_slots = {}
        offset = 0

        # Parameters get the first slots
        for param in fn.params:
            offset -= 8
            self._stack_slots[str(param)] = StackSlot(
                name=param.name, offset=offset,
                ir_type=param.ir_type, is_param=True,
            )

        # Collect all results and operands
        seen: set[str] = set(self._stack_slots.keys())
        for block in fn.blocks:
            for instr in block.instructions:
                if instr.result:
                    key = str(instr.result)
                    if key not in seen:
                        seen.add(key)
                        offset -= 8
                        self._stack_slots[key] = StackSlot(
                            name=key, offset=offset,
                            ir_type=instr.result.ir_type,
                        )
                for op in instr.operands:
                    if isinstance(op, (IRTemp, IRParam)):
                        key = str(op)
                        if key not in seen:
                            seen.add(key)
                            offset -= 8
                            self._stack_slots[key] = StackSlot(
                                name=key, offset=offset,
                                ir_type=op.ir_type,
                            )

        self._stack_size = -offset  # positive

    def _get_slot(self, key: str) -> StackSlot:
        if key not in self._stack_slots:
            self._stack_size += 8
            self._stack_slots[key] = StackSlot(
                name=key, offset=-self._stack_size, ir_type=IRType.I64,
            )
        return self._stack_slots[key]

    def _callee_saved_in_use(self, fn: IRFunction) -> list[str]:
        # For simplicity we don't use callee-saved GPRs for locals (we use
        # the stack), so nothing to save beyond rbp which is already handled.
        return []

    def _emit_epilogue(self, used_callee: list[str], alloc: int):
        for reg in reversed(used_callee):
            self._text.append(f"    pop {reg}")
        self._text.append("    mov rsp, rbp")
        self._text.append("    pop rbp")
        self._text.append("    ret")

    # ──────────────────────────────────────────────────────────
    #  Basic block & instruction generation
    # ──────────────────────────────────────────────────────────

    def _gen_block(self, block: BasicBlock, fn_label: str):
        self._text.append(f".{block.label}:")
        for instr in block.instructions:
            self._gen_instr(instr, fn_label)

    def _gen_instr(self, instr: IRInstruction, fn_label: str):
        """Emit x86-64 for one IR instruction."""
        op = instr.opcode

        # ── Arithmetic ──
        if op in (IROp.ADD, IROp.SUB, IROp.MUL):
            self._gen_arith(instr)
        elif op == IROp.DIV:
            self._gen_div(instr)
        elif op == IROp.MOD:
            self._gen_mod(instr)
        elif op == IROp.NEG:
            self._gen_neg(instr)
        elif op == IROp.POW:
            self._gen_pow(instr)

        # ── Bitwise ──
        elif op in (IROp.BIT_AND, IROp.BIT_OR, IROp.BIT_XOR):
            self._gen_bitwise(instr)
        elif op == IROp.BIT_NOT:
            self._gen_bit_not(instr)
        elif op in (IROp.SHL, IROp.SHR):
            self._gen_shift(instr)

        # ── Comparison ──
        elif op in (IROp.EQ, IROp.NEQ, IROp.LT, IROp.GT, IROp.LTE, IROp.GTE):
            self._gen_comparison(instr)

        # ── Logical ──
        elif op in (IROp.AND, IROp.OR):
            self._gen_logical(instr)
        elif op == IROp.NOT:
            self._gen_not(instr)

        # ── Memory ──
        elif op == IROp.ALLOCA:
            self._gen_alloca(instr)
        elif op == IROp.LOAD:
            self._gen_load(instr)
        elif op == IROp.STORE:
            self._gen_store(instr)
        elif op == IROp.CONST:
            self._gen_const(instr)
        elif op == IROp.COPY:
            self._gen_copy(instr)

        # ── Control flow ──
        elif op == IROp.JUMP:
            self._gen_jump(instr)
        elif op == IROp.BRANCH:
            self._gen_branch(instr)
        elif op == IROp.RETURN:
            self._gen_return(instr, fn_label)
        elif op == IROp.CALL:
            self._gen_call(instr)
        elif op == IROp.CALL_BUILTIN:
            self._gen_call_builtin(instr)

        # ── NOP ──
        elif op == IROp.NOP:
            self._text.append("    nop")

    # ══════════════════════════════════════════════════════════
    #  Value loading helpers
    # ══════════════════════════════════════════════════════════

    def _load_to_reg(self, value: IRValue, reg: str = "rax") -> str:
        """Load an IR value into the specified x86 register. Returns the reg."""
        if isinstance(value, IRConst):
            return self._load_const_to_reg(value, reg)
        key = str(value)
        slot = self._get_slot(key)
        self._text.append(f"    mov {reg}, [rbp{slot.offset:+d}]  ; load {key}")
        return reg

    def _load_const_to_reg(self, c: IRConst, reg: str = "rax") -> str:
        if c.value is None:
            self._text.append(f"    xor {reg}, {reg}  ; nil")
        elif isinstance(c.value, bool):
            val = 1 if c.value else 0
            self._text.append(f"    mov {reg}, {val}  ; {'true' if c.value else 'false'}")
        elif isinstance(c.value, int):
            if -2**31 <= c.value < 2**31:
                self._text.append(f"    mov {reg}, {c.value}")
            else:
                self._text.append(f"    mov {reg}, {c.value}")
        elif isinstance(c.value, float):
            label = self._intern_float(c.value)
            # For float we load into rax as a bit pattern (or use xmm)
            self._text.append(f"    mov {reg}, [rel {label}]  ; float {c.value}")
        elif isinstance(c.value, str):
            label = self._intern_string(c.value)
            self._text.append(f"    lea {reg}, [rel {label}]  ; str \"{c.value[:30]}\"")
        else:
            self._text.append(f"    xor {reg}, {reg}  ; unknown const")
        return reg

    def _store_from_reg(self, reg: str, result: IRValue):
        """Store a register value into the result's stack slot."""
        if result is None:
            return
        key = str(result)
        slot = self._get_slot(key)
        self._text.append(f"    mov [rbp{slot.offset:+d}], {reg}  ; store {key}")

    # ══════════════════════════════════════════════════════════
    #  Arithmetic
    # ══════════════════════════════════════════════════════════

    def _gen_arith(self, instr: IRInstruction):
        """ADD, SUB, MUL — all follow the same pattern."""
        left, right = instr.operands[0], instr.operands[1]

        # Check for float arithmetic
        if self._is_float_op(left, right, instr):
            self._gen_float_arith(instr)
            return

        self._load_to_reg(left, "rax")
        self._load_to_reg(right, "rcx")

        op_map = {IROp.ADD: "add", IROp.SUB: "sub"}
        if instr.opcode in op_map:
            asm_op = op_map[instr.opcode]
            self._text.append(f"    {asm_op} rax, rcx")
        elif instr.opcode == IROp.MUL:
            self._text.append("    imul rax, rcx")

        self._store_from_reg("rax", instr.result)

    def _gen_float_arith(self, instr: IRInstruction):
        """Float arithmetic using SSE2."""
        left, right = instr.operands[0], instr.operands[1]

        # Load left into xmm0
        if isinstance(left, IRConst) and isinstance(left.value, float):
            label = self._intern_float(left.value)
            self._text.append(f"    movsd xmm0, [rel {label}]")
        else:
            self._load_to_reg(left, "rax")
            self._text.append("    movq xmm0, rax")

        # Load right into xmm1
        if isinstance(right, IRConst) and isinstance(right.value, float):
            label = self._intern_float(right.value)
            self._text.append(f"    movsd xmm1, [rel {label}]")
        else:
            self._load_to_reg(right, "rax")
            self._text.append("    movq xmm1, rax")

        op_map = {IROp.ADD: "addsd", IROp.SUB: "subsd", IROp.MUL: "mulsd", IROp.DIV: "divsd"}
        asm_op = op_map.get(instr.opcode, "addsd")
        self._text.append(f"    {asm_op} xmm0, xmm1")
        self._text.append("    movq rax, xmm0")
        self._store_from_reg("rax", instr.result)

    def _gen_div(self, instr: IRInstruction):
        if self._is_float_op(instr.operands[0], instr.operands[1], instr):
            self._gen_float_arith(instr)
            return
        self._load_to_reg(instr.operands[0], "rax")
        self._load_to_reg(instr.operands[1], "rcx")
        self._text.append("    cqo")           # sign-extend RAX into RDX:RAX
        self._text.append("    idiv rcx")      # RAX = quotient, RDX = remainder
        self._store_from_reg("rax", instr.result)

    def _gen_mod(self, instr: IRInstruction):
        self._load_to_reg(instr.operands[0], "rax")
        self._load_to_reg(instr.operands[1], "rcx")
        self._text.append("    cqo")
        self._text.append("    idiv rcx")
        self._store_from_reg("rdx", instr.result)  # remainder in RDX

    def _gen_neg(self, instr: IRInstruction):
        self._load_to_reg(instr.operands[0], "rax")
        self._text.append("    neg rax")
        self._store_from_reg("rax", instr.result)

    def _gen_pow(self, instr: IRInstruction):
        """Integer power: simple loop-based exponentiation."""
        self._load_to_reg(instr.operands[0], "rcx")   # base
        self._load_to_reg(instr.operands[1], "rdx")    # exponent
        lbl_loop = self._new_label("pow_loop")
        lbl_done = self._new_label("pow_done")
        self._text.append("    mov rax, 1          ; result = 1")
        self._text.append(f"{lbl_loop}:")
        self._text.append("    test rdx, rdx")
        self._text.append(f"    jz {lbl_done}")
        self._text.append("    imul rax, rcx       ; result *= base")
        self._text.append("    dec rdx")
        self._text.append(f"    jmp {lbl_loop}")
        self._text.append(f"{lbl_done}:")
        self._store_from_reg("rax", instr.result)

    # ══════════════════════════════════════════════════════════
    #  Bitwise
    # ══════════════════════════════════════════════════════════

    def _gen_bitwise(self, instr: IRInstruction):
        self._load_to_reg(instr.operands[0], "rax")
        self._load_to_reg(instr.operands[1], "rcx")
        op_map = {IROp.BIT_AND: "and", IROp.BIT_OR: "or", IROp.BIT_XOR: "xor"}
        self._text.append(f"    {op_map[instr.opcode]} rax, rcx")
        self._store_from_reg("rax", instr.result)

    def _gen_bit_not(self, instr: IRInstruction):
        self._load_to_reg(instr.operands[0], "rax")
        self._text.append("    not rax")
        self._store_from_reg("rax", instr.result)

    def _gen_shift(self, instr: IRInstruction):
        self._load_to_reg(instr.operands[0], "rax")
        self._load_to_reg(instr.operands[1], "rcx")
        op = "shl" if instr.opcode == IROp.SHL else "sar"
        self._text.append(f"    {op} rax, cl")  # shift amount must be in CL
        self._store_from_reg("rax", instr.result)

    # ══════════════════════════════════════════════════════════
    #  Comparison
    # ══════════════════════════════════════════════════════════

    def _gen_comparison(self, instr: IRInstruction):
        self._load_to_reg(instr.operands[0], "rax")
        self._load_to_reg(instr.operands[1], "rcx")
        self._text.append("    cmp rax, rcx")

        setcc_map = {
            IROp.EQ: "sete", IROp.NEQ: "setne",
            IROp.LT: "setl", IROp.GT: "setg",
            IROp.LTE: "setle", IROp.GTE: "setge",
        }
        setcc = setcc_map[instr.opcode]
        self._text.append(f"    {setcc} al")
        self._text.append("    movzx rax, al")
        self._store_from_reg("rax", instr.result)

    # ══════════════════════════════════════════════════════════
    #  Logical
    # ══════════════════════════════════════════════════════════

    def _gen_logical(self, instr: IRInstruction):
        self._load_to_reg(instr.operands[0], "rax")
        self._text.append("    test rax, rax")      # truthy check
        self._load_to_reg(instr.operands[1], "rcx")
        self._text.append("    test rcx, rcx")

        if instr.opcode == IROp.AND:
            # both must be truthy
            lbl_false = self._new_label("and_false")
            lbl_end = self._new_label("and_end")
            # re-do: load, branch, load, branch
            self._load_to_reg(instr.operands[0], "rax")
            self._text.append("    test rax, rax")
            self._text.append(f"    jz {lbl_false}")
            self._load_to_reg(instr.operands[1], "rax")
            self._text.append("    test rax, rax")
            self._text.append(f"    jz {lbl_false}")
            self._text.append("    mov rax, 1")
            self._text.append(f"    jmp {lbl_end}")
            self._text.append(f"{lbl_false}:")
            self._text.append("    xor rax, rax")
            self._text.append(f"{lbl_end}:")
        else:  # OR
            lbl_true = self._new_label("or_true")
            lbl_end = self._new_label("or_end")
            self._load_to_reg(instr.operands[0], "rax")
            self._text.append("    test rax, rax")
            self._text.append(f"    jnz {lbl_true}")
            self._load_to_reg(instr.operands[1], "rax")
            self._text.append("    test rax, rax")
            self._text.append(f"    jnz {lbl_true}")
            self._text.append("    xor rax, rax")
            self._text.append(f"    jmp {lbl_end}")
            self._text.append(f"{lbl_true}:")
            self._text.append("    mov rax, 1")
            self._text.append(f"{lbl_end}:")

        self._store_from_reg("rax", instr.result)

    def _gen_not(self, instr: IRInstruction):
        self._load_to_reg(instr.operands[0], "rax")
        self._text.append("    test rax, rax")
        self._text.append("    sete al")
        self._text.append("    movzx rax, al")
        self._store_from_reg("rax", instr.result)

    # ══════════════════════════════════════════════════════════
    #  Memory (ALLOCA / LOAD / STORE / COPY / CONST)
    # ══════════════════════════════════════════════════════════

    def _gen_alloca(self, instr: IRInstruction):
        # Stack slot already reserved in layout phase; initialize to 0
        if instr.result:
            key = str(instr.result)
            slot = self._get_slot(key)
            self._text.append(f"    mov qword [rbp{slot.offset:+d}], 0  ; alloca {key}")

    def _gen_load(self, instr: IRInstruction):
        if instr.operands and instr.result:
            src = instr.operands[0]
            src_key = str(src)
            if src_key in self._globals:
                label = self._globals[src_key]
                self._text.append(f"    mov rax, [rel {label}]")
            else:
                src_slot = self._get_slot(src_key)
                self._text.append(f"    mov rax, [rbp{src_slot.offset:+d}]  ; load {src_key}")
            self._store_from_reg("rax", instr.result)

    def _gen_store(self, instr: IRInstruction):
        if len(instr.operands) >= 2:
            dst = instr.operands[0]
            src = instr.operands[1]
            self._load_to_reg(src, "rax")
            dst_key = str(dst)
            if dst_key in self._globals:
                label = self._globals[dst_key]
                self._text.append(f"    mov [rel {label}], rax")
            else:
                dst_slot = self._get_slot(dst_key)
                self._text.append(f"    mov [rbp{dst_slot.offset:+d}], rax  ; store -> {dst_key}")

    def _gen_const(self, instr: IRInstruction):
        if instr.result and instr.operands:
            self._load_to_reg(instr.operands[0], "rax")
            self._store_from_reg("rax", instr.result)

    def _gen_copy(self, instr: IRInstruction):
        if instr.operands and instr.result:
            self._load_to_reg(instr.operands[0], "rax")
            self._store_from_reg("rax", instr.result)

    # ══════════════════════════════════════════════════════════
    #  Control flow
    # ══════════════════════════════════════════════════════════

    def _gen_jump(self, instr: IRInstruction):
        if instr.operands:
            target = instr.operands[0].value
            self._text.append(f"    jmp .{target}")

    def _gen_branch(self, instr: IRInstruction):
        if len(instr.operands) >= 3:
            cond = instr.operands[0]
            true_label = instr.operands[1].value
            false_label = instr.operands[2].value

            self._load_to_reg(cond, "rax")
            self._text.append("    test rax, rax")
            self._text.append(f"    jnz .{true_label}")
            self._text.append(f"    jmp .{false_label}")

    def _gen_return(self, instr: IRInstruction, fn_label: str):
        if instr.operands:
            self._load_to_reg(instr.operands[0], "rax")
        else:
            self._text.append("    xor eax, eax")
        self._text.append("    mov rsp, rbp")
        self._text.append("    pop rbp")
        self._text.append("    ret")

    # ══════════════════════════════════════════════════════════
    #  Function calls
    # ══════════════════════════════════════════════════════════

    def _gen_call(self, instr: IRInstruction):
        """Generate a Grey function call."""
        if not instr.operands:
            return

        callee = instr.operands[0]
        args = instr.operands[1:]

        # Set up shadow space + arguments
        total_stack = self.shadow_space if self.windows else 0
        extra_args = max(0, len(args) - len(self.arg_regs))
        total_stack += extra_args * 8
        # Align to 16 bytes
        if total_stack % 16 != 0:
            total_stack += 8

        if total_stack > 0:
            self._text.append(f"    sub rsp, {total_stack}")

        # Load args into registers (or push remaining)
        for i, arg in enumerate(args):
            if i < len(self.arg_regs):
                self._load_to_reg(arg, self.arg_regs[i])
            else:
                self._load_to_reg(arg, "rax")
                stack_off = self.shadow_space + (i - len(self.arg_regs)) * 8 if self.windows else (i - len(self.arg_regs)) * 8
                self._text.append(f"    mov [rsp+{stack_off}], rax")

        # Call
        if isinstance(callee, IRGlobal):
            fn_label = f"_grey_{callee.name.replace('.', '_')}"
            self._text.append(f"    call {fn_label}")
        else:
            self._load_to_reg(callee, "r10")
            self._text.append("    call r10")

        # Clean up stack
        if total_stack > 0:
            self._text.append(f"    add rsp, {total_stack}")

        # Store result
        if instr.result:
            self._store_from_reg("rax", instr.result)

    def _gen_call_builtin(self, instr: IRInstruction):
        """Generate a built-in function call (maps to C library calls)."""
        if not instr.operands:
            return

        builtin_name = instr.operands[0].value
        args = instr.operands[1:]

        if builtin_name in ("print", "println"):
            self._gen_print(args, newline=(builtin_name == "println"))
        elif builtin_name == "input":
            self._gen_input(args, instr.result)
        elif builtin_name == "assert":
            self._gen_assert(args)
        elif builtin_name == "exit":
            self._gen_exit(args)
        elif builtin_name == "__range__":
            # For native code, we skip dynamic range; just store 0
            if instr.result:
                self._text.append("    xor rax, rax  ; range not supported in native mode")
                self._store_from_reg("rax", instr.result)
        elif builtin_name in ("abs", "min", "max"):
            self._gen_math_builtin(builtin_name, args, instr.result)
        elif builtin_name in ("to_int", "to_float", "to_string"):
            # Simplified: just pass through the value
            if args and instr.result:
                self._load_to_reg(args[0], "rax")
                self._store_from_reg("rax", instr.result)
        elif builtin_name in ("len",):
            # Array/string length — would need runtime support
            if instr.result:
                self._text.append(f"    xor rax, rax  ; len() stub")
                self._store_from_reg("rax", instr.result)
        else:
            # Unknown builtin — emit a comment and zero the result
            self._text.append(f"    ; TODO: builtin '{builtin_name}' not implemented in native backend")
            if instr.result:
                self._text.append("    xor rax, rax")
                self._store_from_reg("rax", instr.result)

    def _setup_call_args(self, args: list[IRValue]):
        """Set up arguments for a function call per calling convention."""
        # Reserve shadow space on Windows
        if self.windows and args:
            self._text.append(f"    sub rsp, {self.shadow_space}  ; shadow space")

        for i, arg in enumerate(args):
            if i < len(self.arg_regs):
                self._load_to_reg(arg, self.arg_regs[i])
            else:
                self._load_to_reg(arg, "rax")
                self._text.append("    push rax")

    def _cleanup_call(self, num_args: int):
        """Clean up stack after call."""
        extra = max(0, num_args - len(self.arg_regs))
        total_cleanup = extra * 8
        if self.windows:
            total_cleanup += self.shadow_space
        if total_cleanup > 0:
            self._text.append(f"    add rsp, {total_cleanup}")

    # ══════════════════════════════════════════════════════════
    #  Built-in implementations
    # ══════════════════════════════════════════════════════════

    def _gen_print(self, args: list[IRValue], newline: bool = False):
        """Generate printf call for print/println."""
        if not args:
            if newline:
                # Just print a newline
                if self.windows:
                    self._text.append(f"    sub rsp, {self.shadow_space}")
                self._text.append(f"    lea {self.arg_regs[0]}, [rel __fmt_newline]")
                self._text.append("    call printf")
                if self.windows:
                    self._text.append(f"    add rsp, {self.shadow_space}")
            return

        arg = args[0]
        fmt_suffix = "ln" if newline else ""

        if isinstance(arg, IRConst):
            if isinstance(arg.value, str):
                label = self._intern_string(arg.value)
                if self.windows:
                    self._text.append(f"    sub rsp, {self.shadow_space}")
                fmt = f"__fmt_str{fmt_suffix}"
                self._text.append(f"    lea {self.arg_regs[0]}, [rel {fmt}]")
                self._text.append(f"    lea {self.arg_regs[1]}, [rel {label}]")
                self._text.append("    xor eax, eax")
                self._text.append("    call printf")
                if self.windows:
                    self._text.append(f"    add rsp, {self.shadow_space}")
            elif isinstance(arg.value, bool):
                lbl = "__fmt_true" if arg.value else "__fmt_false"
                if self.windows:
                    self._text.append(f"    sub rsp, {self.shadow_space}")
                self._text.append(f"    lea {self.arg_regs[0]}, [rel __fmt_strln]")
                self._text.append(f"    lea {self.arg_regs[1]}, [rel {lbl}]")
                self._text.append("    xor eax, eax")
                self._text.append("    call printf")
                if self.windows:
                    self._text.append(f"    add rsp, {self.shadow_space}")
            elif isinstance(arg.value, float):
                label = self._intern_float(arg.value)
                if self.windows:
                    self._text.append(f"    sub rsp, {self.shadow_space}")
                fmt = f"__fmt_float{fmt_suffix}"
                self._text.append(f"    lea {self.arg_regs[0]}, [rel {fmt}]")
                self._text.append(f"    movsd xmm1, [rel {label}]")
                if self.windows:
                    self._text.append(f"    movq {self.arg_regs[1]}, xmm1")
                else:
                    self._text.append("    movsd xmm0, xmm1")
                    self._text.append("    mov eax, 1")  # 1 float arg for varargs
                self._text.append("    call printf")
                if self.windows:
                    self._text.append(f"    add rsp, {self.shadow_space}")
            elif isinstance(arg.value, int):
                if self.windows:
                    self._text.append(f"    sub rsp, {self.shadow_space}")
                fmt = f"__fmt_int{fmt_suffix}"
                self._text.append(f"    lea {self.arg_regs[0]}, [rel {fmt}]")
                self._text.append(f"    mov {self.arg_regs[1]}, {arg.value}")
                self._text.append("    xor eax, eax")
                self._text.append("    call printf")
                if self.windows:
                    self._text.append(f"    add rsp, {self.shadow_space}")
            elif arg.value is None:
                if self.windows:
                    self._text.append(f"    sub rsp, {self.shadow_space}")
                self._text.append(f"    lea {self.arg_regs[0]}, [rel __fmt_strln]")
                self._text.append(f"    lea {self.arg_regs[1]}, [rel __fmt_nil]")
                self._text.append("    xor eax, eax")
                self._text.append("    call printf")
                if self.windows:
                    self._text.append(f"    add rsp, {self.shadow_space}")
        else:
            # Dynamic value — assume integer for now
            self._load_to_reg(arg, "r10")
            if self.windows:
                self._text.append(f"    sub rsp, {self.shadow_space}")
            fmt = f"__fmt_int{fmt_suffix}"
            self._text.append(f"    lea {self.arg_regs[0]}, [rel {fmt}]")
            self._text.append(f"    mov {self.arg_regs[1]}, r10")
            self._text.append("    xor eax, eax")
            self._text.append("    call printf")
            if self.windows:
                self._text.append(f"    add rsp, {self.shadow_space}")

        # Flush stdout
        if self.windows:
            self._text.append(f"    sub rsp, {self.shadow_space}")
        self._text.append(f"    xor {self.arg_regs[0]}, {self.arg_regs[0]}")
        self._text.append("    call fflush")
        if self.windows:
            self._text.append(f"    add rsp, {self.shadow_space}")

    def _gen_input(self, args: list[IRValue], result: Optional[IRValue]):
        """Generate scanf-based input."""
        # Print prompt if given
        if args:
            self._gen_print(args, newline=False)

        # Read integer (simplified)
        if result:
            slot = self._get_slot(str(result))
            self._text.append(f"    mov qword [rbp{slot.offset:+d}], 0")
            if self.windows:
                self._text.append(f"    sub rsp, {self.shadow_space}")
            self._text.append(f"    lea {self.arg_regs[0]}, [rel __fmt_scanf_int]")
            self._text.append(f"    lea {self.arg_regs[1]}, [rbp{slot.offset:+d}]")
            self._text.append("    xor eax, eax")
            self._text.append("    call scanf")
            if self.windows:
                self._text.append(f"    add rsp, {self.shadow_space}")

    def _gen_assert(self, args: list[IRValue]):
        """Generate assertion check."""
        if not args:
            return
        cond = args[0]
        lbl_ok = self._new_label("assert_ok")

        self._load_to_reg(cond, "rax")
        self._text.append("    test rax, rax")
        self._text.append(f"    jnz {lbl_ok}")

        # Assertion failed — print message and exit
        msg = "Assertion failed!"
        if len(args) > 1 and isinstance(args[1], IRConst) and isinstance(args[1].value, str):
            msg = args[1].value
        msg_lbl = self._intern_string(msg)

        if self.windows:
            self._text.append(f"    sub rsp, {self.shadow_space}")
        self._text.append(f"    lea {self.arg_regs[0]}, [rel __fmt_strln]")
        self._text.append(f"    lea {self.arg_regs[1]}, [rel {msg_lbl}]")
        self._text.append("    xor eax, eax")
        self._text.append("    call printf")
        if self.windows:
            self._text.append(f"    add rsp, {self.shadow_space}")

        # exit(1)
        if self.windows:
            self._text.append(f"    sub rsp, {self.shadow_space}")
        self._text.append(f"    mov {self.arg_regs[0]}, 1")
        self._text.append("    call exit")
        if self.windows:
            self._text.append(f"    add rsp, {self.shadow_space}")

        self._text.append(f"{lbl_ok}:")

    def _gen_exit(self, args: list[IRValue]):
        if args:
            self._load_to_reg(args[0], self.arg_regs[0])
        else:
            self._text.append(f"    xor {self.arg_regs[0]}, {self.arg_regs[0]}")
        if self.windows:
            self._text.append(f"    sub rsp, {self.shadow_space}")
        self._text.append("    call exit")
        if self.windows:
            self._text.append(f"    add rsp, {self.shadow_space}")

    def _gen_math_builtin(self, name: str, args: list[IRValue], result: Optional[IRValue]):
        if name == "abs" and args:
            self._load_to_reg(args[0], "rax")
            lbl_pos = self._new_label("abs_pos")
            self._text.append("    test rax, rax")
            self._text.append(f"    jns {lbl_pos}")
            self._text.append("    neg rax")
            self._text.append(f"{lbl_pos}:")
            if result:
                self._store_from_reg("rax", result)
        elif name == "min" and len(args) >= 2:
            self._load_to_reg(args[0], "rax")
            self._load_to_reg(args[1], "rcx")
            self._text.append("    cmp rax, rcx")
            self._text.append("    cmovg rax, rcx")
            if result:
                self._store_from_reg("rax", result)
        elif name == "max" and len(args) >= 2:
            self._load_to_reg(args[0], "rax")
            self._load_to_reg(args[1], "rcx")
            self._text.append("    cmp rax, rcx")
            self._text.append("    cmovl rax, rcx")
            if result:
                self._store_from_reg("rax", result)
        else:
            if result:
                self._text.append("    xor rax, rax")
                self._store_from_reg("rax", result)

    # ══════════════════════════════════════════════════════════
    #  Helpers
    # ══════════════════════════════════════════════════════════

    def _is_float_op(self, left: IRValue, right: IRValue, instr: IRInstruction) -> bool:
        """Check if an operation should use floating point."""
        if instr.ir_type == IRType.F64:
            return True
        if isinstance(left, IRConst) and isinstance(left.value, float):
            return True
        if isinstance(right, IRConst) and isinstance(right.value, float):
            return True
        if hasattr(left, 'ir_type') and left.ir_type == IRType.F64:
            return True
        if hasattr(right, 'ir_type') and right.ir_type == IRType.F64:
            return True
        return False
