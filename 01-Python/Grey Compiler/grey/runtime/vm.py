"""
Grey Virtual Machine.

A register-based VM that executes compiled Grey bytecode.
Implements the full fetch-decode-execute cycle for all VMOpcodes.

Architecture:
  - 256 general-purpose registers per call frame
  - Call stack with frame isolation
  - Constant pool per chunk
  - Built-in function dispatch
  - Runtime type checking
  - Exception / panic propagation
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Optional

from ..backend.instruction import (
    VMOpcode, VMInstruction, Chunk, CompiledProgram,
)
from .memory import (
    MemoryManager, CallFrame, GreyValue, ValueType,
    NIL, TRUE, FALSE,
    make_int, make_float, make_string, make_bool, make_array, make_struct,
)
from .gc import GarbageCollector
from .stdlib import StandardLibrary


class VMError(Exception):
    """Runtime error raised by the Grey VM."""
    pass


class GreyVM:
    """
    The Grey Virtual Machine.

    Usage:
        vm = GreyVM()
        vm.load(compiled_program)
        vm.run()
    """

    def __init__(self, *, trace: bool = False, gc_enabled: bool = True):
        self.memory = MemoryManager()
        self.gc = GarbageCollector(self.memory)
        self.gc.enabled = gc_enabled
        self.stdlib = StandardLibrary()

        self.program: Optional[CompiledProgram] = None
        self.trace = trace
        self.halted = False

        # Per-frame register stacks (list of register lists)
        self._register_stack: list[list[GreyValue]] = []

    # ── Public API ───────────────────────────────────────────

    def load(self, program: CompiledProgram):
        """Load a compiled program."""
        self.program = program
        self.halted = False
        self.memory.globals.clear()
        self.memory.call_stack.clear()
        self._register_stack.clear()

        # Pre-populate global slots
        for gname in program.global_names:
            self.memory.set_global(gname, NIL)

        # Register function chunks as callable FUNCTION values in globals
        for idx, chunk in enumerate(program.chunks):
            if idx != program.main_chunk_index and chunk.name:
                fn_val = GreyValue(ValueType.FUNCTION, {
                    "chunk": idx,
                    "name": chunk.name,
                })
                self.memory.set_global(chunk.name, fn_val)

    def run(self) -> GreyValue:
        """Execute the loaded program starting at the main chunk."""
        if self.program is None:
            raise VMError("No program loaded")

        main_chunk = self.program.main_chunk_index

        # Push the main frame
        regs = [NIL] * 256
        self._register_stack.append(regs)
        self.memory.push_frame(CallFrame(
            chunk_index=main_chunk,
            ip=0,
            base_register=0,
            return_register=0,
            function_name=self.program.chunks[main_chunk].name or "__main__",
        ))

        result = self._execute()
        return result

    # ── Execution Engine ─────────────────────────────────────

    def _execute(self) -> GreyValue:
        """Main execution loop."""
        while not self.halted:
            frame = self.memory.current_frame
            if frame is None:
                break

            chunk = self.program.chunks[frame.chunk_index]

            if frame.ip >= len(chunk.instructions):
                # Implicit return
                return_val = NIL
                self._register_stack.pop()
                self.memory.pop_frame()
                if self.memory.current_frame is not None:
                    parent_regs = self._register_stack[-1]
                    parent_regs[self.memory.current_frame.return_register] = return_val
                else:
                    return return_val
                continue

            instr = chunk.instructions[frame.ip]
            frame.ip += 1

            if self.trace:
                self._trace_instruction(frame, instr)

            result = self._dispatch(instr, chunk, frame)

            if result is not None:
                return result

            # Periodic GC check
            self.gc.maybe_collect()

        return NIL

    def _dispatch(self, instr: VMInstruction, chunk: Chunk, frame: CallFrame) -> Optional[GreyValue]:
        """Dispatch a single instruction. Returns a value only when the program terminates."""
        regs = self._register_stack[-1]
        op = instr.opcode

        # ── Loads & Stores ───────────────────────────────────
        if op == VMOpcode.LOAD_CONST:
            regs[instr.a] = self._to_grey_value(chunk.constants[instr.bx])

        elif op == VMOpcode.LOAD_NIL:
            regs[instr.a] = NIL

        elif op == VMOpcode.LOAD_TRUE:
            regs[instr.a] = TRUE

        elif op == VMOpcode.LOAD_FALSE:
            regs[instr.a] = FALSE

        elif op == VMOpcode.LOAD_LOCAL:
            regs[instr.a] = regs[instr.b]

        elif op == VMOpcode.STORE_LOCAL:
            regs[instr.b] = regs[instr.a]

        elif op == VMOpcode.LOAD_GLOBAL:
            name = self._resolve_global_name(instr.bx, chunk)
            regs[instr.a] = self.memory.get_global(name)

        elif op == VMOpcode.STORE_GLOBAL:
            name = self._resolve_global_name(instr.bx, chunk)
            self.memory.set_global(name, regs[instr.a])

        elif op == VMOpcode.LOAD_UPVALUE:
            # Simplified: upvalues are stored in closure data
            regs[instr.a] = NIL

        elif op == VMOpcode.STORE_UPVALUE:
            pass

        elif op == VMOpcode.MOVE:
            regs[instr.a] = regs[instr.b]

        elif op == VMOpcode.DUP:
            val = regs[instr.b]
            if val.type == ValueType.ARRAY:
                regs[instr.a] = make_array(list(val.data))
            elif val.type == ValueType.STRUCT:
                regs[instr.a] = make_struct(dict(val.data))
            else:
                regs[instr.a] = val

        # ── Arithmetic ───────────────────────────────────────
        elif op == VMOpcode.ADD:
            regs[instr.a] = self._arith_add(regs[instr.b], regs[instr.c])

        elif op == VMOpcode.SUB:
            regs[instr.a] = self._arith_sub(regs[instr.b], regs[instr.c])

        elif op == VMOpcode.MUL:
            regs[instr.a] = self._arith_mul(regs[instr.b], regs[instr.c])

        elif op == VMOpcode.DIV:
            regs[instr.a] = self._arith_div(regs[instr.b], regs[instr.c])

        elif op == VMOpcode.MOD:
            regs[instr.a] = self._arith_mod(regs[instr.b], regs[instr.c])

        elif op == VMOpcode.POW:
            regs[instr.a] = self._arith_pow(regs[instr.b], regs[instr.c])

        elif op == VMOpcode.NEG:
            v = regs[instr.b]
            if v.type == ValueType.INT:
                regs[instr.a] = make_int(-v.data)
            elif v.type == ValueType.FLOAT:
                regs[instr.a] = make_float(-v.data)
            else:
                raise VMError(f"Cannot negate {v.type_name()}")

        elif op == VMOpcode.FLOOR_DIV:
            b, c = regs[instr.b], regs[instr.c]
            bv = self._as_number(b)
            cv = self._as_number(c)
            if cv == 0:
                raise VMError("Division by zero")
            regs[instr.a] = make_int(int(bv // cv))

        # ── Bitwise ──────────────────────────────────────────
        elif op == VMOpcode.BIT_AND:
            regs[instr.a] = make_int(self._as_int(regs[instr.b]) & self._as_int(regs[instr.c]))

        elif op == VMOpcode.BIT_OR:
            regs[instr.a] = make_int(self._as_int(regs[instr.b]) | self._as_int(regs[instr.c]))

        elif op == VMOpcode.BIT_XOR:
            regs[instr.a] = make_int(self._as_int(regs[instr.b]) ^ self._as_int(regs[instr.c]))

        elif op == VMOpcode.BIT_NOT:
            regs[instr.a] = make_int(~self._as_int(regs[instr.b]))

        elif op == VMOpcode.SHL:
            regs[instr.a] = make_int(self._as_int(regs[instr.b]) << self._as_int(regs[instr.c]))

        elif op == VMOpcode.SHR:
            regs[instr.a] = make_int(self._as_int(regs[instr.b]) >> self._as_int(regs[instr.c]))

        # ── Comparison ───────────────────────────────────────
        elif op == VMOpcode.EQ:
            regs[instr.a] = make_bool(self._values_equal(regs[instr.b], regs[instr.c]))

        elif op == VMOpcode.NEQ:
            regs[instr.a] = make_bool(not self._values_equal(regs[instr.b], regs[instr.c]))

        elif op == VMOpcode.LT:
            regs[instr.a] = make_bool(self._compare(regs[instr.b], regs[instr.c]) < 0)

        elif op == VMOpcode.GT:
            regs[instr.a] = make_bool(self._compare(regs[instr.b], regs[instr.c]) > 0)

        elif op == VMOpcode.LTE:
            regs[instr.a] = make_bool(self._compare(regs[instr.b], regs[instr.c]) <= 0)

        elif op == VMOpcode.GTE:
            regs[instr.a] = make_bool(self._compare(regs[instr.b], regs[instr.c]) >= 0)

        # ── Logical ──────────────────────────────────────────
        elif op == VMOpcode.NOT:
            regs[instr.a] = make_bool(not regs[instr.b].is_truthy())

        elif op == VMOpcode.AND:
            b = regs[instr.b]
            regs[instr.a] = regs[instr.c] if b.is_truthy() else b

        elif op == VMOpcode.OR:
            b = regs[instr.b]
            regs[instr.a] = b if b.is_truthy() else regs[instr.c]

        # ── Control Flow ─────────────────────────────────────
        elif op == VMOpcode.JUMP:
            frame.ip += instr.sbx

        elif op == VMOpcode.JUMP_IF_TRUE:
            if regs[instr.a].is_truthy():
                frame.ip += instr.sbx

        elif op == VMOpcode.JUMP_IF_FALSE:
            if not regs[instr.a].is_truthy():
                frame.ip += instr.sbx

        elif op == VMOpcode.CALL:
            self._do_call(instr, regs, frame)

        elif op == VMOpcode.RETURN:
            return_val = regs[instr.a]
            self._register_stack.pop()
            self.memory.pop_frame()
            if self.memory.current_frame is not None:
                parent_regs = self._register_stack[-1]
                parent_frame = self.memory.current_frame
                parent_regs[parent_frame.return_register] = return_val
            else:
                return return_val

        elif op == VMOpcode.RETURN_NONE:
            self._register_stack.pop()
            self.memory.pop_frame()
            if self.memory.current_frame is not None:
                parent_regs = self._register_stack[-1]
                parent_frame = self.memory.current_frame
                parent_regs[parent_frame.return_register] = NIL
            else:
                return NIL

        # ── Array Operations ─────────────────────────────────
        elif op == VMOpcode.ARRAY_NEW:
            size = instr.b
            regs[instr.a] = self.memory.alloc_array(size)

        elif op == VMOpcode.ARRAY_GET:
            arr = regs[instr.b]
            idx = regs[instr.c]
            if arr.type != ValueType.ARRAY:
                raise VMError(f"Cannot index {arr.type_name()}")
            i = self._as_int(idx)
            if i < 0 or i >= len(arr.data):
                raise VMError(f"Array index {i} out of bounds (length {len(arr.data)})")
            regs[instr.a] = arr.data[i]

        elif op == VMOpcode.ARRAY_SET:
            arr = regs[instr.a]
            idx = regs[instr.b]
            if arr.type != ValueType.ARRAY:
                raise VMError(f"Cannot index-assign {arr.type_name()}")
            i = self._as_int(idx)
            if i < 0 or i >= len(arr.data):
                raise VMError(f"Array index {i} out of bounds (length {len(arr.data)})")
            arr.data[i] = regs[instr.c]

        elif op == VMOpcode.ARRAY_LEN:
            arr = regs[instr.b]
            if arr.type != ValueType.ARRAY:
                raise VMError(f"Cannot get length of {arr.type_name()}")
            regs[instr.a] = make_int(len(arr.data))

        elif op == VMOpcode.ARRAY_PUSH:
            arr = regs[instr.a]
            if arr.type != ValueType.ARRAY:
                raise VMError(f"Cannot push to {arr.type_name()}")
            arr.data.append(regs[instr.b])

        elif op == VMOpcode.ARRAY_POP:
            arr = regs[instr.b]
            if arr.type != ValueType.ARRAY:
                raise VMError(f"Cannot pop from {arr.type_name()}")
            if not arr.data:
                raise VMError("Pop from empty array")
            regs[instr.a] = arr.data.pop()

        # ── Struct Operations ────────────────────────────────
        elif op == VMOpcode.STRUCT_NEW:
            regs[instr.a] = self.memory.alloc_struct()

        elif op == VMOpcode.FIELD_GET:
            obj = regs[instr.b]
            if obj.type != ValueType.STRUCT:
                raise VMError(f"Cannot access field of {obj.type_name()}")
            field_name = chunk.constants[instr.c] if instr.c < len(chunk.constants) else str(instr.c)
            if isinstance(field_name, str) and field_name in obj.data:
                regs[instr.a] = obj.data[field_name]
            else:
                regs[instr.a] = NIL

        elif op == VMOpcode.FIELD_SET:
            obj = regs[instr.a]
            if obj.type != ValueType.STRUCT:
                raise VMError(f"Cannot set field of {obj.type_name()}")
            field_name = chunk.constants[instr.b] if instr.b < len(chunk.constants) else str(instr.b)
            obj.data[field_name] = regs[instr.c]

        # ── String Operations ────────────────────────────────
        elif op == VMOpcode.CONCAT:
            b, c = regs[instr.b], regs[instr.c]
            regs[instr.a] = make_string(self._to_display(b) + self._to_display(c))

        elif op == VMOpcode.STR_LEN:
            s = regs[instr.b]
            if s.type != ValueType.STRING:
                raise VMError(f"STR_LEN expects string, got {s.type_name()}")
            regs[instr.a] = make_int(len(s.data))

        # ── Type Operations ──────────────────────────────────
        elif op == VMOpcode.TYPE_CHECK:
            regs[instr.a] = make_bool(regs[instr.b].type.value == instr.c)

        elif op == VMOpcode.CAST:
            regs[instr.a] = regs[instr.b]  # simplified cast

        elif op == VMOpcode.TO_STRING:
            regs[instr.a] = make_string(self._to_display(regs[instr.b]))

        elif op == VMOpcode.TO_INT:
            v = regs[instr.b]
            if v.type == ValueType.INT:
                regs[instr.a] = v
            elif v.type == ValueType.FLOAT:
                regs[instr.a] = make_int(int(v.data))
            elif v.type == ValueType.STRING:
                try:
                    regs[instr.a] = make_int(int(v.data))
                except ValueError:
                    raise VMError(f"Cannot convert '{v.data}' to int")
            else:
                raise VMError(f"Cannot convert {v.type_name()} to int")

        elif op == VMOpcode.TO_FLOAT:
            v = regs[instr.b]
            if v.type == ValueType.FLOAT:
                regs[instr.a] = v
            elif v.type == ValueType.INT:
                regs[instr.a] = make_float(float(v.data))
            elif v.type == ValueType.STRING:
                try:
                    regs[instr.a] = make_float(float(v.data))
                except ValueError:
                    raise VMError(f"Cannot convert '{v.data}' to float")
            else:
                raise VMError(f"Cannot convert {v.type_name()} to float")

        # ── Closure ──────────────────────────────────────────
        elif op == VMOpcode.CLOSURE:
            fn_val = GreyValue(ValueType.CLOSURE, {
                "chunk": instr.bx,
                "upvalues": [],
            })
            regs[instr.a] = fn_val

        elif op == VMOpcode.CLOSE_UPVALUE:
            pass  # simplified

        # ── Built-in Calls ───────────────────────────────────
        elif op == VMOpcode.BUILTIN_CALL:
            builtin_id = instr.b
            arg_count = instr.c
            builtin_name = chunk.constants[builtin_id] if builtin_id < len(chunk.constants) else None
            if isinstance(builtin_name, str) and self.stdlib.has(builtin_name):
                args = [regs[instr.a + 1 + i] for i in range(arg_count)]
                regs[instr.a] = self.stdlib.call(builtin_name, args)
            else:
                raise VMError(f"Unknown builtin: {builtin_name}")

        elif op == VMOpcode.PRINT:
            print(self._to_display(regs[instr.a]), end="")

        elif op == VMOpcode.PRINTLN:
            print(self._to_display(regs[instr.a]))

        elif op == VMOpcode.INPUT:
            prompt = self._to_display(regs[instr.b]) if instr.b != instr.a else ""
            regs[instr.a] = make_string(input(prompt))

        elif op == VMOpcode.ASSERT:
            if not regs[instr.a].is_truthy():
                msg = "Assertion failed"
                if instr.b and instr.b < 256:
                    m = regs[instr.b]
                    if m.type == ValueType.STRING:
                        msg = f"Assertion failed: {m.data}"
                raise VMError(msg)

        # ── Misc ─────────────────────────────────────────────
        elif op == VMOpcode.NOP:
            pass

        elif op == VMOpcode.HALT:
            self.halted = True
            return regs[0]

        else:
            raise VMError(f"Unknown opcode: {op}")

        return None

    # ── Call Handling ─────────────────────────────────────────

    def _do_call(self, instr: VMInstruction, regs: list[GreyValue], frame: CallFrame):
        """Handle CALL instruction: A = call(B, C args)"""
        fn_val = regs[instr.b]
        arg_count = instr.c
        dest_reg = instr.a

        # Collect arguments
        args = [regs[instr.b + 1 + i] for i in range(arg_count)]

        if fn_val.type == ValueType.NATIVE_FN:
            # Call native function
            result = fn_val.data(args)
            regs[dest_reg] = result if result is not None else NIL

        elif fn_val.type == ValueType.FUNCTION:
            # Call Grey function
            chunk_idx = fn_val.data["chunk"]
            target_chunk = self.program.chunks[chunk_idx]

            # Save return info
            frame.return_register = dest_reg

            # Create new register set
            new_regs = [NIL] * 256
            for i, arg in enumerate(args):
                new_regs[i] = arg
            self._register_stack.append(new_regs)

            # Push new frame
            self.memory.push_frame(CallFrame(
                chunk_index=chunk_idx,
                ip=0,
                base_register=0,
                return_register=dest_reg,
                function_name=fn_val.data.get("name", target_chunk.name),
            ))

        elif fn_val.type == ValueType.CLOSURE:
            chunk_idx = fn_val.data["chunk"]
            target_chunk = self.program.chunks[chunk_idx]

            frame.return_register = dest_reg

            new_regs = [NIL] * 256
            for i, arg in enumerate(args):
                new_regs[i] = arg
            self._register_stack.append(new_regs)

            self.memory.push_frame(CallFrame(
                chunk_index=chunk_idx,
                ip=0,
                base_register=0,
                return_register=dest_reg,
                function_name=target_chunk.name,
            ))

        elif fn_val.type == ValueType.STRING:
            # Try stdlib lookup by name
            name = fn_val.data
            if self.stdlib.has(name):
                result = self.stdlib.call(name, args)
                regs[dest_reg] = result if result is not None else NIL
            else:
                raise VMError(f"Cannot call string '{name}' as function")

        else:
            raise VMError(f"Cannot call value of type {fn_val.type_name()}")

    # ── Arithmetic Helpers ───────────────────────────────────

    def _arith_add(self, a: GreyValue, b: GreyValue) -> GreyValue:
        if a.type == ValueType.INT and b.type == ValueType.INT:
            return make_int(a.data + b.data)
        if a.type in (ValueType.INT, ValueType.FLOAT) and b.type in (ValueType.INT, ValueType.FLOAT):
            return make_float(float(a.data) + float(b.data))
        if a.type == ValueType.STRING and b.type == ValueType.STRING:
            return make_string(a.data + b.data)
        if a.type == ValueType.STRING or b.type == ValueType.STRING:
            return make_string(self._to_display(a) + self._to_display(b))
        raise VMError(f"Cannot add {a.type_name()} and {b.type_name()}")

    def _arith_sub(self, a: GreyValue, b: GreyValue) -> GreyValue:
        if a.type == ValueType.INT and b.type == ValueType.INT:
            return make_int(a.data - b.data)
        if a.type in (ValueType.INT, ValueType.FLOAT) and b.type in (ValueType.INT, ValueType.FLOAT):
            return make_float(float(a.data) - float(b.data))
        raise VMError(f"Cannot subtract {b.type_name()} from {a.type_name()}")

    def _arith_mul(self, a: GreyValue, b: GreyValue) -> GreyValue:
        if a.type == ValueType.INT and b.type == ValueType.INT:
            return make_int(a.data * b.data)
        if a.type in (ValueType.INT, ValueType.FLOAT) and b.type in (ValueType.INT, ValueType.FLOAT):
            return make_float(float(a.data) * float(b.data))
        if a.type == ValueType.STRING and b.type == ValueType.INT:
            return make_string(a.data * b.data)
        raise VMError(f"Cannot multiply {a.type_name()} and {b.type_name()}")

    def _arith_div(self, a: GreyValue, b: GreyValue) -> GreyValue:
        bv = self._as_number(b)
        if bv == 0:
            raise VMError("Division by zero")
        av = self._as_number(a)
        if a.type == ValueType.INT and b.type == ValueType.INT and a.data % b.data == 0:
            return make_int(a.data // b.data)
        return make_float(av / bv)

    def _arith_mod(self, a: GreyValue, b: GreyValue) -> GreyValue:
        bv = self._as_number(b)
        if bv == 0:
            raise VMError("Modulo by zero")
        av = self._as_number(a)
        if a.type == ValueType.INT and b.type == ValueType.INT:
            return make_int(a.data % b.data)
        return make_float(av % bv)

    def _arith_pow(self, a: GreyValue, b: GreyValue) -> GreyValue:
        av = self._as_number(a)
        bv = self._as_number(b)
        result = av ** bv
        if isinstance(result, float):
            return make_float(result)
        return make_int(result)

    # ── Value Helpers ────────────────────────────────────────

    def _as_number(self, v: GreyValue) -> int | float:
        if v.type == ValueType.INT:
            return v.data
        if v.type == ValueType.FLOAT:
            return v.data
        raise VMError(f"Expected number, got {v.type_name()}")

    def _as_int(self, v: GreyValue) -> int:
        if v.type == ValueType.INT:
            return v.data
        if v.type == ValueType.FLOAT:
            return int(v.data)
        raise VMError(f"Expected int, got {v.type_name()}")

    def _values_equal(self, a: GreyValue, b: GreyValue) -> bool:
        if a.type == ValueType.NIL and b.type == ValueType.NIL:
            return True
        if a.type == ValueType.NIL or b.type == ValueType.NIL:
            return False
        if a.type != b.type:
            # Allow int/float comparison
            if a.type in (ValueType.INT, ValueType.FLOAT) and b.type in (ValueType.INT, ValueType.FLOAT):
                return float(a.data) == float(b.data)
            return False
        return a.data == b.data

    def _compare(self, a: GreyValue, b: GreyValue) -> int:
        """Compare two values, returning negative/zero/positive."""
        if a.type in (ValueType.INT, ValueType.FLOAT) and b.type in (ValueType.INT, ValueType.FLOAT):
            av, bv = float(a.data), float(b.data)
            return (av > bv) - (av < bv)
        if a.type == ValueType.STRING and b.type == ValueType.STRING:
            return (a.data > b.data) - (a.data < b.data)
        raise VMError(f"Cannot compare {a.type_name()} and {b.type_name()}")

    def _to_grey_value(self, const) -> GreyValue:
        """Convert a Python constant to a GreyValue."""
        if const is None:
            return NIL
        if isinstance(const, bool):
            return make_bool(const)
        if isinstance(const, int):
            return make_int(const)
        if isinstance(const, float):
            return make_float(const)
        if isinstance(const, str):
            return make_string(const)
        return NIL

    def _to_display(self, v: GreyValue) -> str:
        """Display string for a value."""
        if v.type == ValueType.NIL:
            return "nil"
        if v.type == ValueType.BOOL:
            return "true" if v.data else "false"
        if v.type == ValueType.STRING:
            return v.data
        if v.type == ValueType.ARRAY:
            items = ", ".join(self._to_display(x) for x in v.data)
            return f"[{items}]"
        if v.type == ValueType.STRUCT:
            fields = ", ".join(f"{k}: {self._to_display(v2)}" for k, v2 in v.data.items())
            return f"{{{fields}}}"
        return str(v.data)

    def _global_name(self, index: int) -> str:
        """Look up a global name by index."""
        if index < len(self.program.global_names):
            return self.program.global_names[index]
        if index < len(self.program.string_table):
            return self.program.string_table[index]
        return f"__global_{index}"

    def _resolve_global_name(self, bx: int, chunk: 'Chunk') -> str:
        """Resolve a global variable name from the chunk's constant pool.

        The codegen stores the global name as a string constant in the chunk,
        so we look it up there first before falling back to global_names.
        """
        if bx < len(chunk.constants):
            name = chunk.constants[bx]
            if isinstance(name, str):
                return name
        return self._global_name(bx)

    # ── Debug / Trace ────────────────────────────────────────

    def _trace_instruction(self, frame: CallFrame, instr: VMInstruction):
        chunk = self.program.chunks[frame.chunk_index]
        print(f"  [{frame.function_name}:{frame.ip - 1:04d}]  {instr}")

    def stack_trace(self) -> str:
        """Return a human-readable stack trace."""
        lines = ["Stack trace (most recent call last):"]
        for i, frame in enumerate(self.memory.call_stack):
            chunk = self.program.chunks[frame.chunk_index]
            line_no = 0
            if frame.ip - 1 < len(chunk.line_numbers):
                line_no = chunk.line_numbers[frame.ip - 1]
            lines.append(
                f"  Frame {i}: {frame.function_name} "
                f"(chunk {frame.chunk_index}, ip={frame.ip}, line {line_no})"
            )
        return "\n".join(lines)


def _signed16(value: int) -> int:
    """Convert unsigned 16-bit to signed 16-bit."""
    if value >= 0x8000:
        return value - 0x10000
    return value
