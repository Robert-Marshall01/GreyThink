"""
Code Generator for the Grey compiler.

Translates optimized IR into Grey VM bytecode.
Handles instruction selection, mapping IR operations to VM opcodes.
"""

from typing import Optional
from ..middle.ir import (
    IRModule, IRFunction, BasicBlock, IRInstruction,
    IRValue, IRConst, IRTemp, IRParam, IRGlobal,
    Opcode as IROp, IRType,
)
from .instruction import VMOpcode, VMInstruction, Chunk, CompiledProgram


class CodeGenerator:
    """
    Generates VM bytecode from IR.

    Usage:
        codegen = CodeGenerator()
        program = codegen.generate(ir_module)
    """

    def __init__(self):
        self.program = CompiledProgram()
        self.current_chunk: Optional[Chunk] = None
        self.register_map: dict[str, int] = {}
        self.next_register: int = 0
        self.label_positions: dict[str, int] = {}
        self.jump_patches: list[tuple[int, str]] = []  # (instruction_index, target_label)

    def generate(self, module: IRModule) -> CompiledProgram:
        """Generate bytecode for the entire IR module."""
        self.program = CompiledProgram()

        for fn in module.functions:
            chunk = self._generate_function(fn)
            idx = self.program.add_chunk(chunk)
            if fn.name == "__main__":
                self.program.main_chunk_index = idx

        # String table
        self.program.string_table = list(module.string_pool.keys())

        return self.program

    def _generate_function(self, fn: IRFunction) -> Chunk:
        """Generate a bytecode chunk for a single function."""
        chunk = Chunk(
            name=fn.name,
            param_count=len(fn.params),
        )
        self.current_chunk = chunk
        self.register_map = {}
        self.next_register = 0
        self.label_positions = {}
        self.jump_patches = []

        # Allocate registers for parameters
        for param in fn.params:
            self._alloc_register(str(param))

        # Generate instructions for each block
        for block in fn.blocks:
            self._gen_block(block)

        # Add HALT at the end of main
        if fn.name == "__main__":
            chunk.emit(VMInstruction(opcode=VMOpcode.HALT))

        # Patch jump targets
        self._patch_jumps()

        chunk.local_count = self.next_register
        chunk.max_registers = max(self.next_register, 256)
        chunk.local_names = [name for name in self.register_map.keys()]

        return chunk

    # ── Register Allocation (Simple) ─────────────────────────

    def _alloc_register(self, name: str) -> int:
        """Allocate a register for a value."""
        if name in self.register_map:
            return self.register_map[name]
        reg = self.next_register
        self.register_map[name] = reg
        self.next_register += 1
        return reg

    def _get_register(self, value: IRValue) -> int:
        """Get the register for a value, allocating if needed."""
        key = str(value)
        return self._alloc_register(key)

    def _load_value(self, value: IRValue) -> int:
        """Load a value into a register, emitting instructions if needed."""
        if isinstance(value, IRConst):
            return self._load_const(value)
        if isinstance(value, IRTemp):
            return self._get_register(value)
        if isinstance(value, IRParam):
            return self._get_register(value)
        if isinstance(value, IRGlobal):
            reg = self._alloc_register(str(value))
            idx = self.current_chunk.add_constant(value.name)
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.LOAD_GLOBAL, a=reg, bx=idx,
                comment=f"load global {value.name}"
            ))
            return reg
        return 0

    def _load_const(self, const: IRConst) -> int:
        """Load a constant into a register."""
        reg = self._alloc_register(f"const_{const.value}_{id(const)}")

        if const.value is None:
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.LOAD_NIL, a=reg
            ))
        elif const.value is True:
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.LOAD_TRUE, a=reg
            ))
        elif const.value is False:
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.LOAD_FALSE, a=reg
            ))
        else:
            idx = self.current_chunk.add_constant(const.value)
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.LOAD_CONST, a=reg, bx=idx,
                comment=f"load {const.value!r}"
            ))

        return reg

    # ── Block Generation ─────────────────────────────────────

    def _gen_block(self, block: BasicBlock):
        """Generate bytecode for a basic block."""
        # Record label position
        self.label_positions[block.label] = len(self.current_chunk.instructions)

        for instr in block.instructions:
            self._gen_instruction(instr)

    def _gen_instruction(self, instr: IRInstruction):
        """Generate bytecode for a single IR instruction."""
        dest_reg = self._get_register(instr.result) if instr.result else 0

        # ── Arithmetic ───────────────────────────────────────
        if instr.opcode == IROp.ADD:
            self._gen_binary(VMOpcode.ADD, dest_reg, instr)
        elif instr.opcode == IROp.SUB:
            self._gen_binary(VMOpcode.SUB, dest_reg, instr)
        elif instr.opcode == IROp.MUL:
            self._gen_binary(VMOpcode.MUL, dest_reg, instr)
        elif instr.opcode == IROp.DIV:
            self._gen_binary(VMOpcode.DIV, dest_reg, instr)
        elif instr.opcode == IROp.MOD:
            self._gen_binary(VMOpcode.MOD, dest_reg, instr)
        elif instr.opcode == IROp.POW:
            self._gen_binary(VMOpcode.POW, dest_reg, instr)
        elif instr.opcode == IROp.NEG:
            b = self._load_value(instr.operands[0])
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.NEG, a=dest_reg, b=b, comment="neg"
            ))

        # ── Bitwise ──────────────────────────────────────────
        elif instr.opcode == IROp.BIT_AND:
            self._gen_binary(VMOpcode.BIT_AND, dest_reg, instr)
        elif instr.opcode == IROp.BIT_OR:
            self._gen_binary(VMOpcode.BIT_OR, dest_reg, instr)
        elif instr.opcode == IROp.BIT_XOR:
            self._gen_binary(VMOpcode.BIT_XOR, dest_reg, instr)
        elif instr.opcode == IROp.BIT_NOT:
            b = self._load_value(instr.operands[0])
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.BIT_NOT, a=dest_reg, b=b
            ))
        elif instr.opcode == IROp.SHL:
            self._gen_binary(VMOpcode.SHL, dest_reg, instr)
        elif instr.opcode == IROp.SHR:
            self._gen_binary(VMOpcode.SHR, dest_reg, instr)

        # ── Comparison ───────────────────────────────────────
        elif instr.opcode == IROp.EQ:
            self._gen_binary(VMOpcode.EQ, dest_reg, instr)
        elif instr.opcode == IROp.NEQ:
            self._gen_binary(VMOpcode.NEQ, dest_reg, instr)
        elif instr.opcode == IROp.LT:
            self._gen_binary(VMOpcode.LT, dest_reg, instr)
        elif instr.opcode == IROp.GT:
            self._gen_binary(VMOpcode.GT, dest_reg, instr)
        elif instr.opcode == IROp.LTE:
            self._gen_binary(VMOpcode.LTE, dest_reg, instr)
        elif instr.opcode == IROp.GTE:
            self._gen_binary(VMOpcode.GTE, dest_reg, instr)

        # ── Logical ──────────────────────────────────────────
        elif instr.opcode == IROp.AND:
            self._gen_binary(VMOpcode.AND, dest_reg, instr)
        elif instr.opcode == IROp.OR:
            self._gen_binary(VMOpcode.OR, dest_reg, instr)
        elif instr.opcode == IROp.NOT:
            b = self._load_value(instr.operands[0])
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.NOT, a=dest_reg, b=b
            ))

        # ── Memory ───────────────────────────────────────────
        elif instr.opcode == IROp.ALLOCA:
            # For struct types, allocate a struct object
            if instr.ir_type == IRType.STRUCT:
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.STRUCT_NEW, a=dest_reg,
                    comment="alloca struct"
                ))
            else:
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.LOAD_NIL, a=dest_reg,
                    comment="alloca"
                ))

        elif instr.opcode == IROp.LOAD:
            if instr.operands:
                src = self._get_register(instr.operands[0])
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.MOVE, a=dest_reg, b=src,
                    comment="load"
                ))

        elif instr.opcode == IROp.STORE:
            if len(instr.operands) >= 2:
                dst = self._get_register(instr.operands[0])
                src = self._load_value(instr.operands[1])
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.MOVE, a=dst, b=src,
                    comment="store"
                ))

        elif instr.opcode == IROp.GET_FIELD:
            if len(instr.operands) >= 2:
                obj = self._load_value(instr.operands[0])
                field_idx = self.current_chunk.add_constant(instr.operands[1].value)
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.FIELD_GET, a=dest_reg, b=obj, c=field_idx,
                    comment=f"get field {instr.operands[1].value}"
                ))

        elif instr.opcode == IROp.SET_FIELD:
            if len(instr.operands) >= 3:
                obj = self._load_value(instr.operands[0])
                field_idx = self.current_chunk.add_constant(instr.operands[1].value)
                val = self._load_value(instr.operands[2])
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.FIELD_SET, a=obj, b=field_idx, c=val,
                    comment=f"set field {instr.operands[1].value}"
                ))

        elif instr.opcode == IROp.GET_INDEX:
            if len(instr.operands) >= 2:
                obj = self._load_value(instr.operands[0])
                idx = self._load_value(instr.operands[1])
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.ARRAY_GET, a=dest_reg, b=obj, c=idx,
                    comment="array get"
                ))

        elif instr.opcode == IROp.SET_INDEX:
            if len(instr.operands) >= 3:
                obj = self._load_value(instr.operands[0])
                idx = self._load_value(instr.operands[1])
                val = self._load_value(instr.operands[2])
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.ARRAY_SET, a=obj, b=idx, c=val,
                    comment="array set"
                ))

        # ── Array Operations ─────────────────────────────────
        elif instr.opcode == IROp.ARRAY_NEW:
            # For array literals, size is 0 (filled by pushes).
            # Use the constant value directly as the size parameter.
            size = 0
            if instr.operands and isinstance(instr.operands[0], IRConst):
                size = instr.operands[0].value or 0
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.ARRAY_NEW, a=dest_reg, b=size,
                comment="array new"
            ))

        elif instr.opcode == IROp.ARRAY_PUSH:
            if len(instr.operands) >= 2:
                arr = self._load_value(instr.operands[0])
                val = self._load_value(instr.operands[1])
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.ARRAY_PUSH, a=arr, b=val,
                    comment="array push"
                ))

        elif instr.opcode == IROp.ARRAY_LEN:
            arr = self._load_value(instr.operands[0])
            self.current_chunk.emit(VMInstruction(
                opcode=VMOpcode.ARRAY_LEN, a=dest_reg, b=arr,
                comment="array len"
            ))

        # ── Control Flow ─────────────────────────────────────
        elif instr.opcode == IROp.JUMP:
            if instr.operands:
                target = instr.operands[0].value
                instr_idx = len(self.current_chunk.instructions)
                self.jump_patches.append((instr_idx, target))
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.JUMP, sbx=0,
                    comment=f"jump {target}"
                ))

        elif instr.opcode == IROp.BRANCH:
            if len(instr.operands) >= 3:
                cond = self._load_value(instr.operands[0])
                true_label = instr.operands[1].value
                false_label = instr.operands[2].value

                # Jump if true to true_label
                instr_idx = len(self.current_chunk.instructions)
                self.jump_patches.append((instr_idx, true_label))
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.JUMP_IF_TRUE, a=cond, sbx=0,
                    comment=f"branch true -> {true_label}"
                ))

                # Fall through or jump to false_label
                instr_idx = len(self.current_chunk.instructions)
                self.jump_patches.append((instr_idx, false_label))
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.JUMP, sbx=0,
                    comment=f"branch false -> {false_label}"
                ))

        elif instr.opcode == IROp.RETURN:
            if instr.operands:
                val_reg = self._load_value(instr.operands[0])
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.RETURN, a=val_reg,
                    comment="return"
                ))
            else:
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.RETURN_NONE,
                    comment="return void"
                ))

        # ── Function Calls ───────────────────────────────────
        elif instr.opcode == IROp.CALL:
            if instr.operands:
                arg_count = len(instr.operands) - 1

                # First, evaluate all argument values
                arg_regs = []
                for arg in instr.operands[1:]:
                    arg_regs.append(self._load_value(arg))

                # Load the function into a fresh register
                fn_reg = self._load_value(instr.operands[0])

                # Move arguments into consecutive registers after fn_reg
                for i, arg_reg in enumerate(arg_regs):
                    target_reg = fn_reg + 1 + i
                    if target_reg != arg_reg:
                        self.current_chunk.emit(VMInstruction(
                            opcode=VMOpcode.MOVE, a=target_reg, b=arg_reg,
                            comment=f"setup arg {i}"
                        ))

                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.CALL, a=dest_reg, b=fn_reg, c=arg_count,
                    comment="call"
                ))

        elif instr.opcode == IROp.CALL_BUILTIN:
            if instr.operands:
                builtin_name = instr.operands[0].value
                arg_count = len(instr.operands) - 1

                # Special handling for print
                if builtin_name == "print" and arg_count == 1:
                    val_reg = self._load_value(instr.operands[1])
                    self.current_chunk.emit(VMInstruction(
                        opcode=VMOpcode.PRINT, a=val_reg,
                        comment="print"
                    ))
                elif builtin_name == "println" and arg_count == 1:
                    val_reg = self._load_value(instr.operands[1])
                    self.current_chunk.emit(VMInstruction(
                        opcode=VMOpcode.PRINTLN, a=val_reg,
                        comment="println"
                    ))
                elif builtin_name == "assert" and arg_count >= 1:
                    cond_reg = self._load_value(instr.operands[1])
                    msg_reg = 0
                    if arg_count >= 2:
                        msg_reg = self._load_value(instr.operands[2])
                    self.current_chunk.emit(VMInstruction(
                        opcode=VMOpcode.ASSERT, a=cond_reg, b=msg_reg,
                        comment="assert"
                    ))
                else:
                    # Generic builtin call - args must be at dest_reg+1, dest_reg+2, ...
                    name_idx = self.current_chunk.add_constant(builtin_name)
                    arg_regs = []
                    for arg in instr.operands[1:]:
                        arg_regs.append(self._load_value(arg))
                    # Move args into consecutive registers after dest_reg
                    for i, arg_reg in enumerate(arg_regs):
                        target_reg = dest_reg + 1 + i
                        if target_reg != arg_reg:
                            self.current_chunk.emit(VMInstruction(
                                opcode=VMOpcode.MOVE, a=target_reg, b=arg_reg,
                                comment=f"setup builtin arg {i}"
                            ))
                    self.current_chunk.emit(VMInstruction(
                        opcode=VMOpcode.BUILTIN_CALL,
                        a=dest_reg, b=name_idx, c=arg_count,
                        comment=f"builtin {builtin_name}"
                    ))

        # ── Copy ─────────────────────────────────────────────
        elif instr.opcode == IROp.COPY:
            if instr.operands:
                src = self._load_value(instr.operands[0])
                self.current_chunk.emit(VMInstruction(
                    opcode=VMOpcode.MOVE, a=dest_reg, b=src,
                    comment="copy"
                ))

        # ── NOP ──────────────────────────────────────────────
        elif instr.opcode == IROp.NOP:
            self.current_chunk.emit(VMInstruction(opcode=VMOpcode.NOP))

    def _gen_binary(self, opcode: VMOpcode, dest: int, instr: IRInstruction):
        """Generate a binary operation instruction."""
        if len(instr.operands) >= 2:
            b = self._load_value(instr.operands[0])
            c = self._load_value(instr.operands[1])
            self.current_chunk.emit(VMInstruction(
                opcode=opcode, a=dest, b=b, c=c,
                comment=instr.opcode.name.lower()
            ))

    def _patch_jumps(self):
        """Patch jump target addresses after all blocks are generated."""
        for instr_idx, target_label in self.jump_patches:
            if target_label in self.label_positions:
                target_pos = self.label_positions[target_label]
                offset = target_pos - instr_idx - 1  # relative offset
                instr = self.current_chunk.instructions[instr_idx]
                instr.sbx = offset
            else:
                # Target not found — should not happen with correct IR
                pass
