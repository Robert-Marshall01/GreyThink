"""
C Code Generator for the Grey compiler.

Translates Grey IR to portable C99 code, which can be compiled by any
C compiler (GCC, Clang, MSVC) to produce a real native executable.
This is the most portable native compilation path.

The generated C code links only against the standard C library.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import Optional

from ..middle.ir import (
    IRModule, IRFunction, BasicBlock, IRInstruction,
    IRValue, IRConst, IRTemp, IRParam, IRGlobal,
    Opcode as IROp, IRType,
)


def _c_type(ir_type: IRType) -> str:
    """Convert IR type to C type string."""
    return {
        IRType.I32:    "int32_t",
        IRType.I64:    "int64_t",
        IRType.F64:    "double",
        IRType.BOOL:   "int64_t",       # use int for bool to unify register representation
        IRType.STR:    "const char*",
        IRType.PTR:    "void*",
        IRType.VOID:   "void",
        IRType.ANY:    "int64_t",        # default to int64 for untyped
        IRType.ARRAY:  "GreyArray*",
        IRType.STRUCT: "GreyStruct*",
        IRType.FUNC:   "void*",
    }.get(ir_type, "int64_t")


class CCodeGenerator:
    """
    Generates C99 code from Grey IR.

    Usage:
        gen = CCodeGenerator()
        c_code = gen.generate(ir_module)
        gen.write("output.c", c_code)
    """

    def __init__(self):
        self._output: list[str] = []
        self._indent: int = 0
        self._string_literals: dict[str, str] = {}
        self._str_counter: int = 0
        self._declared_functions: set[str] = set()
        self._current_fn_name: str = ""

    def generate(self, module: IRModule) -> str:
        """Generate complete C source for the module."""
        self._output = []
        self._string_literals = {}
        self._str_counter = 0
        self._declared_functions = set()

        self._emit_header(module.name)
        self._emit_runtime()
        self._prescan_strings(module)
        self._emit_string_literals()
        self._emit_forward_declarations(module)
        self._emit_globals(module)

        for fn in module.functions:
            self._gen_function(fn)

        return "\n".join(self._output)

    def write(self, path: str, code: str):
        """Write generated C code to a file."""
        with open(path, "w", encoding="utf-8") as f:
            f.write(code)

    # ──────────────────────────────────────────────────────────
    #  Output helpers
    # ──────────────────────────────────────────────────────────

    def _emit(self, line: str = ""):
        prefix = "    " * self._indent
        self._output.append(prefix + line)

    def _indent_inc(self):
        self._indent += 1

    def _indent_dec(self):
        self._indent = max(0, self._indent - 1)

    # ──────────────────────────────────────────────────────────
    #  Header & Runtime
    # ──────────────────────────────────────────────────────────

    def _emit_header(self, module_name: str):
        self._emit(f"/* ======================================================= */")
        self._emit(f"/*  Grey Compiler -- Generated C Code                       */")
        self._emit(f"/*  Module: {module_name:<46s} */")
        self._emit(f"/*  This file was automatically generated. Do not edit.     */")
        self._emit(f"/* ======================================================= */")
        self._emit()
        self._emit("#include <stdio.h>")
        self._emit("#include <stdlib.h>")
        self._emit("#include <stdint.h>")
        self._emit("#include <string.h>")
        self._emit("#include <math.h>")
        self._emit("#include <time.h>")
        self._emit()

    def _emit_runtime(self):
        """Emit a minimal Grey runtime in C."""
        self._emit("/* -- Grey Runtime ----------------------------- */")
        self._emit()
        # Simple dynamic array
        self._emit("typedef struct {")
        self._indent_inc()
        self._emit("int64_t* data;")
        self._emit("int64_t  length;")
        self._emit("int64_t  capacity;")
        self._indent_dec()
        self._emit("} GreyArray;")
        self._emit()
        # Simple key-value struct
        self._emit("typedef struct {")
        self._indent_inc()
        self._emit("const char** keys;")
        self._emit("int64_t*     values;")
        self._emit("int64_t      count;")
        self._indent_dec()
        self._emit("} GreyStruct;")
        self._emit()
        # Array functions
        self._emit("static GreyArray* grey_array_new(int64_t capacity) {")
        self._indent_inc()
        self._emit("GreyArray* arr = (GreyArray*)malloc(sizeof(GreyArray));")
        self._emit("arr->data = (int64_t*)calloc(capacity > 0 ? capacity : 4, sizeof(int64_t));")
        self._emit("arr->length = 0;")
        self._emit("arr->capacity = capacity > 0 ? capacity : 4;")
        self._emit("return arr;")
        self._indent_dec()
        self._emit("}")
        self._emit()
        self._emit("static void grey_array_push(GreyArray* arr, int64_t value) {")
        self._indent_inc()
        self._emit("if (arr->length >= arr->capacity) {")
        self._indent_inc()
        self._emit("arr->capacity *= 2;")
        self._emit("arr->data = (int64_t*)realloc(arr->data, arr->capacity * sizeof(int64_t));")
        self._indent_dec()
        self._emit("}")
        self._emit("arr->data[arr->length++] = value;")
        self._indent_dec()
        self._emit("}")
        self._emit()
        self._emit("static int64_t grey_array_get(GreyArray* arr, int64_t index) {")
        self._indent_inc()
        self._emit("if (index < 0 || index >= arr->length) { fprintf(stderr, \"Index out of bounds\\n\"); exit(1); }")
        self._emit("return arr->data[index];")
        self._indent_dec()
        self._emit("}")
        self._emit()
        self._emit("static void grey_array_set(GreyArray* arr, int64_t index, int64_t value) {")
        self._indent_inc()
        self._emit("if (index < 0 || index >= arr->length) { fprintf(stderr, \"Index out of bounds\\n\"); exit(1); }")
        self._emit("arr->data[index] = value;")
        self._indent_dec()
        self._emit("}")
        self._emit()
        # Struct functions
        self._emit("static GreyStruct* grey_struct_new(int64_t count) {")
        self._indent_inc()
        self._emit("GreyStruct* s = (GreyStruct*)malloc(sizeof(GreyStruct));")
        self._emit("s->keys = (const char**)calloc(count, sizeof(const char*));")
        self._emit("s->values = (int64_t*)calloc(count, sizeof(int64_t));")
        self._emit("s->count = 0;")
        self._emit("return s;")
        self._indent_dec()
        self._emit("}")
        self._emit()
        self._emit("static void grey_struct_set(GreyStruct* s, const char* key, int64_t value) {")
        self._indent_inc()
        self._emit("for (int64_t i = 0; i < s->count; i++) {")
        self._indent_inc()
        self._emit("if (strcmp(s->keys[i], key) == 0) { s->values[i] = value; return; }")
        self._indent_dec()
        self._emit("}")
        self._emit("s->keys[s->count] = key;")
        self._emit("s->values[s->count] = value;")
        self._emit("s->count++;")
        self._indent_dec()
        self._emit("}")
        self._emit()
        self._emit("static int64_t grey_struct_get(GreyStruct* s, const char* key) {")
        self._indent_inc()
        self._emit("for (int64_t i = 0; i < s->count; i++) {")
        self._indent_inc()
        self._emit("if (strcmp(s->keys[i], key) == 0) return s->values[i];")
        self._indent_dec()
        self._emit("}")
        self._emit('fprintf(stderr, "Field not found: %s\\n", key); exit(1);')
        self._emit("return 0;")
        self._indent_dec()
        self._emit("}")
        self._emit()
        # Integer power function
        self._emit("static int64_t grey_pow(int64_t base, int64_t exp) {")
        self._indent_inc()
        self._emit("int64_t result = 1;")
        self._emit("for (int64_t i = 0; i < exp; i++) result *= base;")
        self._emit("return result;")
        self._indent_dec()
        self._emit("}")
        self._emit()
        # Abs
        self._emit("static int64_t grey_abs(int64_t x) { return x < 0 ? -x : x; }")
        self._emit("static int64_t grey_min(int64_t a, int64_t b) { return a < b ? a : b; }")
        self._emit("static int64_t grey_max(int64_t a, int64_t b) { return a > b ? a : b; }")
        self._emit()
        # Range helper
        self._emit("static GreyArray* grey_range(int64_t start, int64_t end, int inclusive) {")
        self._indent_inc()
        self._emit("int64_t count = end - start + (inclusive ? 1 : 0);")
        self._emit("if (count < 0) count = 0;")
        self._emit("GreyArray* arr = grey_array_new(count);")
        self._emit("for (int64_t i = start; inclusive ? i <= end : i < end; i++)")
        self._indent_inc()
        self._emit("grey_array_push(arr, i);")
        self._indent_dec()
        self._emit("return arr;")
        self._indent_dec()
        self._emit("}")
        self._emit()
        self._emit("/* -- End Runtime ------------------------------ */")
        self._emit()

    # ──────────────────────────────────────────────────────────
    #  String literals
    # ──────────────────────────────────────────────────────────

    def _prescan_strings(self, module: IRModule):
        for fn in module.functions:
            for block in fn.blocks:
                for instr in block.instructions:
                    for op in instr.operands:
                        if isinstance(op, IRConst) and isinstance(op.value, str):
                            self._intern_string(op.value)

    def _intern_string(self, value: str) -> str:
        if value in self._string_literals:
            return self._string_literals[value]
        label = f"__grey_str_{self._str_counter}"
        self._str_counter += 1
        self._string_literals[value] = label
        return label

    def _emit_string_literals(self):
        if not self._string_literals:
            return
        self._emit("/* -- String Constants ------------------------- */")
        for value, label in self._string_literals.items():
            escaped = value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n").replace("\t", "\\t")
            self._emit(f'static const char {label}[] = "{escaped}";')
        self._emit()

    # ──────────────────────────────────────────────────────────
    #  Forward declarations & globals
    # ──────────────────────────────────────────────────────────

    def _emit_forward_declarations(self, module: IRModule):
        self._emit("/* -- Forward Declarations --------------------- */")
        for fn in module.functions:
            if fn.name == "__main__":
                continue
            c_name = self._c_fn_name(fn.name)
            ret = _c_type(fn.return_type)
            params = ", ".join(
                f"{_c_type(p.ir_type)} {self._c_var(str(p))}" for p in fn.params
            )
            if not params:
                params = "void"
            self._emit(f"static {ret} {c_name}({params});")
            self._declared_functions.add(fn.name)
        self._emit()

    def _emit_globals(self, module: IRModule):
        if not module.globals:
            return
        self._emit("/* -- Global Variables ------------------------- */")
        for gv in module.globals:
            ct = _c_type(gv.ir_type)
            label = self._c_var(gv.name)
            if gv.initializer and gv.initializer.value is not None:
                val = self._c_literal(gv.initializer)
                self._emit(f"static {ct} {label} = {val};")
            else:
                self._emit(f"static {ct} {label} = 0;")
        self._emit()

    # ══════════════════════════════════════════════════════════
    #  Function generation
    # ══════════════════════════════════════════════════════════

    def _gen_function(self, fn: IRFunction):
        self._current_fn_name = fn.name

        # Determine C function name and signature
        if fn.name == "__main__":
            self._emit("int main(int argc, char** argv) {")
        else:
            c_name = self._c_fn_name(fn.name)
            ret = _c_type(fn.return_type)
            params = ", ".join(
                f"{_c_type(p.ir_type)} {self._c_var(str(p))}" for p in fn.params
            )
            if not params:
                params = "void"
            self._emit(f"static {ret} {c_name}({params}) {{")

        self._indent_inc()

        # Declare all local variables (IR temps)
        locals_seen: set[str] = set()
        for block in fn.blocks:
            for instr in block.instructions:
                if instr.result:
                    key = str(instr.result)
                    if key not in locals_seen and not self._is_param(key, fn):
                        locals_seen.add(key)
                        ct = _c_type(instr.ir_type if instr.ir_type != IRType.ANY else
                                     (instr.result.ir_type if instr.result else IRType.I64))
                        # Most values are int64_t in our simplified model
                        self._emit(f"int64_t {self._c_var(key)} = 0;")

        if locals_seen:
            self._emit()

        # Generate blocks
        for block in fn.blocks:
            self._gen_block(block)

        # __main__ returns 0
        if fn.name == "__main__":
            self._emit("return 0;")

        self._indent_dec()
        self._emit("}")
        self._emit()

    def _gen_block(self, block: BasicBlock):
        self._emit(f"/* block: {block.label} */")
        self._emit(f"{self._c_label(block.label)}:;")

        for instr in block.instructions:
            self._gen_instr(instr)

    def _gen_instr(self, instr: IRInstruction):
        op = instr.opcode

        # ── Arithmetic ──
        if op in (IROp.ADD, IROp.SUB, IROp.MUL, IROp.DIV, IROp.MOD):
            self._gen_binary_op(instr)
        elif op == IROp.POW:
            self._gen_pow(instr)
        elif op == IROp.NEG:
            self._gen_unary(instr, "-")

        # ── Bitwise ──
        elif op in (IROp.BIT_AND, IROp.BIT_OR, IROp.BIT_XOR, IROp.SHL, IROp.SHR):
            self._gen_binary_op(instr)
        elif op == IROp.BIT_NOT:
            self._gen_unary(instr, "~")

        # ── Comparison ──
        elif op in (IROp.EQ, IROp.NEQ, IROp.LT, IROp.GT, IROp.LTE, IROp.GTE):
            self._gen_binary_op(instr)

        # ── Logical ──
        elif op == IROp.AND:
            self._gen_binary_op(instr)
        elif op == IROp.OR:
            self._gen_binary_op(instr)
        elif op == IROp.NOT:
            self._gen_unary(instr, "!")

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

        # ── Array ──
        elif op == IROp.ARRAY_NEW:
            self._gen_array_new(instr)
        elif op == IROp.ARRAY_PUSH:
            self._gen_array_push(instr)
        elif op == IROp.ARRAY_LEN:
            self._gen_array_len(instr)
        elif op == IROp.GET_INDEX:
            self._gen_get_index(instr)
        elif op == IROp.SET_INDEX:
            self._gen_set_index(instr)

        # ── Struct ──
        elif op == IROp.GET_FIELD:
            self._gen_get_field(instr)
        elif op == IROp.SET_FIELD:
            self._gen_set_field(instr)

        # ── Control flow ──
        elif op == IROp.JUMP:
            self._gen_jump(instr)
        elif op == IROp.BRANCH:
            self._gen_branch(instr)
        elif op == IROp.RETURN:
            self._gen_return(instr)
        elif op == IROp.CALL:
            self._gen_call(instr)
        elif op == IROp.CALL_BUILTIN:
            self._gen_call_builtin(instr)

        elif op == IROp.NOP:
            pass  # nothing

    # ──────────────────────────────────────────────────────────
    #  Code generation for specific IR operations
    # ──────────────────────────────────────────────────────────

    _OP_SYMBOL = {
        IROp.ADD: "+", IROp.SUB: "-", IROp.MUL: "*", IROp.DIV: "/", IROp.MOD: "%",
        IROp.BIT_AND: "&", IROp.BIT_OR: "|", IROp.BIT_XOR: "^",
        IROp.SHL: "<<", IROp.SHR: ">>",
        IROp.EQ: "==", IROp.NEQ: "!=",
        IROp.LT: "<", IROp.GT: ">", IROp.LTE: "<=", IROp.GTE: ">=",
        IROp.AND: "&&", IROp.OR: "||",
    }

    def _gen_binary_op(self, instr: IRInstruction):
        if instr.result and len(instr.operands) >= 2:
            dest = self._c_var(str(instr.result))
            left = self._c_value(instr.operands[0])
            right = self._c_value(instr.operands[1])
            sym = self._OP_SYMBOL.get(instr.opcode, "+")
            self._emit(f"{dest} = ({left}) {sym} ({right});")

    def _gen_pow(self, instr: IRInstruction):
        if instr.result and len(instr.operands) >= 2:
            dest = self._c_var(str(instr.result))
            base = self._c_value(instr.operands[0])
            exp = self._c_value(instr.operands[1])
            self._emit(f"{dest} = grey_pow({base}, {exp});")

    def _gen_unary(self, instr: IRInstruction, op: str):
        if instr.result and instr.operands:
            dest = self._c_var(str(instr.result))
            val = self._c_value(instr.operands[0])
            self._emit(f"{dest} = {op}({val});")

    def _gen_alloca(self, instr: IRInstruction):
        # Variable already declared at function top; initialize to 0
        if instr.result:
            if instr.ir_type == IRType.STRUCT:
                name = instr.metadata.get("struct_name", "anon")
                dest = self._c_var(str(instr.result))
                self._emit(f"{dest} = (int64_t)grey_struct_new(16); /* struct {name} */")

    def _gen_load(self, instr: IRInstruction):
        if instr.operands and instr.result:
            dest = self._c_var(str(instr.result))
            src = self._c_var(str(instr.operands[0]))
            self._emit(f"{dest} = {src};")

    def _gen_store(self, instr: IRInstruction):
        if len(instr.operands) >= 2:
            dst = self._c_var(str(instr.operands[0]))
            src = self._c_value(instr.operands[1])
            self._emit(f"{dst} = {src};")

    def _gen_const(self, instr: IRInstruction):
        if instr.result and instr.operands:
            dest = self._c_var(str(instr.result))
            val = self._c_value(instr.operands[0])
            self._emit(f"{dest} = {val};")

    def _gen_copy(self, instr: IRInstruction):
        if instr.result and instr.operands:
            dest = self._c_var(str(instr.result))
            src = self._c_value(instr.operands[0])
            self._emit(f"{dest} = {src};")

    # ── Array operations ──

    def _gen_array_new(self, instr: IRInstruction):
        if instr.result:
            dest = self._c_var(str(instr.result))
            size = self._c_value(instr.operands[0]) if instr.operands else "4"
            self._emit(f"{dest} = (int64_t)grey_array_new({size});")

    def _gen_array_push(self, instr: IRInstruction):
        if len(instr.operands) >= 2:
            arr = self._c_value(instr.operands[0])
            val = self._c_value(instr.operands[1])
            self._emit(f"grey_array_push((GreyArray*){arr}, {val});")

    def _gen_array_len(self, instr: IRInstruction):
        if instr.result and instr.operands:
            dest = self._c_var(str(instr.result))
            arr = self._c_value(instr.operands[0])
            self._emit(f"{dest} = ((GreyArray*){arr})->length;")

    def _gen_get_index(self, instr: IRInstruction):
        if instr.result and len(instr.operands) >= 2:
            dest = self._c_var(str(instr.result))
            arr = self._c_value(instr.operands[0])
            idx = self._c_value(instr.operands[1])
            self._emit(f"{dest} = grey_array_get((GreyArray*){arr}, {idx});")

    def _gen_set_index(self, instr: IRInstruction):
        if len(instr.operands) >= 3:
            arr = self._c_value(instr.operands[0])
            idx = self._c_value(instr.operands[1])
            val = self._c_value(instr.operands[2])
            self._emit(f"grey_array_set((GreyArray*){arr}, {idx}, {val});")

    # ── Struct operations ──

    def _gen_get_field(self, instr: IRInstruction):
        if instr.result and len(instr.operands) >= 2:
            dest = self._c_var(str(instr.result))
            obj = self._c_value(instr.operands[0])
            field = instr.operands[1].value if isinstance(instr.operands[1], IRConst) else str(instr.operands[1])
            field_c = self._intern_string(field)
            self._emit(f"{dest} = grey_struct_get((GreyStruct*){obj}, {field_c});")

    def _gen_set_field(self, instr: IRInstruction):
        if len(instr.operands) >= 3:
            obj = self._c_value(instr.operands[0])
            field = instr.operands[1].value if isinstance(instr.operands[1], IRConst) else str(instr.operands[1])
            val = self._c_value(instr.operands[2])
            field_c = self._intern_string(field)
            self._emit(f"grey_struct_set((GreyStruct*){obj}, {field_c}, {val});")

    # ── Control flow ──

    def _gen_jump(self, instr: IRInstruction):
        if instr.operands:
            target = instr.operands[0].value
            self._emit(f"goto {self._c_label(target)};")

    def _gen_branch(self, instr: IRInstruction):
        if len(instr.operands) >= 3:
            cond = self._c_value(instr.operands[0])
            true_label = self._c_label(instr.operands[1].value)
            false_label = self._c_label(instr.operands[2].value)
            self._emit(f"if ({cond}) goto {true_label}; else goto {false_label};")

    def _gen_return(self, instr: IRInstruction):
        if instr.operands:
            val = self._c_value(instr.operands[0])
            self._emit(f"return {val};")
        else:
            if self._current_fn_name == "__main__":
                self._emit("return 0;")
            else:
                self._emit("return;")

    def _gen_call(self, instr: IRInstruction):
        if not instr.operands:
            return
        callee = instr.operands[0]
        args = instr.operands[1:]
        args_str = ", ".join(self._c_value(a) for a in args)

        if isinstance(callee, IRGlobal):
            fn_name = self._c_fn_name(callee.name)
        else:
            fn_name = self._c_value(callee)

        if instr.result:
            dest = self._c_var(str(instr.result))
            self._emit(f"{dest} = {fn_name}({args_str});")
        else:
            self._emit(f"{fn_name}({args_str});")

    def _gen_call_builtin(self, instr: IRInstruction):
        if not instr.operands:
            return
        name = instr.operands[0].value
        args = instr.operands[1:]

        if name == "print":
            self._gen_c_print(args, newline=False)
        elif name == "println":
            self._gen_c_print(args, newline=True)
        elif name == "input":
            self._gen_c_input(args, instr.result)
        elif name == "assert":
            self._gen_c_assert(args)
        elif name == "exit":
            if args:
                self._emit(f"exit({self._c_value(args[0])});")
            else:
                self._emit("exit(0);")
        elif name == "abs":
            if instr.result and args:
                self._emit(f"{self._c_var(str(instr.result))} = grey_abs({self._c_value(args[0])});")
        elif name == "min":
            if instr.result and len(args) >= 2:
                self._emit(f"{self._c_var(str(instr.result))} = grey_min({self._c_value(args[0])}, {self._c_value(args[1])});")
        elif name == "max":
            if instr.result and len(args) >= 2:
                self._emit(f"{self._c_var(str(instr.result))} = grey_max({self._c_value(args[0])}, {self._c_value(args[1])});")
        elif name == "__range__":
            if instr.result and len(args) >= 3:
                start = self._c_value(args[0])
                end = self._c_value(args[1])
                incl = self._c_value(args[2])
                dest = self._c_var(str(instr.result))
                self._emit(f"{dest} = (int64_t)grey_range({start}, {end}, {incl});")
        elif name in ("len",):
            if instr.result and args:
                dest = self._c_var(str(instr.result))
                arr = self._c_value(args[0])
                self._emit(f"{dest} = ((GreyArray*){arr})->length;")
        elif name in ("to_int", "to_float", "to_string"):
            if instr.result and args:
                self._emit(f"{self._c_var(str(instr.result))} = {self._c_value(args[0])};")
        elif name == "clock":
            if instr.result:
                self._emit(f"{self._c_var(str(instr.result))} = (int64_t)clock();")
        elif name == "panic":
            msg = self._c_value(args[0]) if args else '"panic"'
            self._emit(f'fprintf(stderr, "panic: %s\\n", (const char*){msg}); exit(1);')
        else:
            self._emit(f"/* TODO: builtin '{name}' not implemented */")
            if instr.result:
                self._emit(f"{self._c_var(str(instr.result))} = 0;")

    # ── Built-in print / input / assert ──

    def _gen_c_print(self, args: list[IRValue], newline: bool):
        nl = '\\n' if newline else ''
        if not args:
            if newline:
                self._emit('printf("\\n");')
            return

        arg = args[0]
        if isinstance(arg, IRConst):
            if isinstance(arg.value, str):
                lbl = self._intern_string(arg.value)
                self._emit(f'printf("%s{nl}", {lbl});')
            elif isinstance(arg.value, bool):
                val_str = "true" if arg.value else "false"
                self._emit(f'printf("{val_str}{nl}");')
            elif isinstance(arg.value, float):
                self._emit(f'printf("%g{nl}", {arg.value!r});')
            elif isinstance(arg.value, int):
                self._emit(f'printf("%lld{nl}", (long long){arg.value});')
            elif arg.value is None:
                self._emit(f'printf("nil{nl}");')
            else:
                self._emit(f'printf("%lld{nl}", (long long)0);')
        else:
            val = self._c_var(str(arg))
            self._emit(f'printf("%lld{nl}", (long long){val});')
        self._emit("fflush(stdout);")

    def _gen_c_input(self, args: list[IRValue], result: Optional[IRValue]):
        # Print prompt
        if args:
            self._gen_c_print(args, newline=False)
        if result:
            dest = self._c_var(str(result))
            self._emit(f'scanf("%lld", &{dest});')

    def _gen_c_assert(self, args: list[IRValue]):
        if not args:
            return
        cond = self._c_value(args[0])
        msg = '"Assertion failed!"'
        if len(args) > 1 and isinstance(args[1], IRConst) and isinstance(args[1].value, str):
            escaped = args[1].value.replace('"', '\\"')
            msg = f'"{escaped}"'
        self._emit(f'if (!({cond})) {{ fprintf(stderr, "%s\\n", {msg}); exit(1); }}')

    # ──────────────────────────────────────────────────────────
    #  Name mangling & value conversion
    # ──────────────────────────────────────────────────────────

    def _c_fn_name(self, name: str) -> str:
        """Mangle a Grey function name to a valid C identifier."""
        return "grey_fn_" + name.replace(".", "__")

    def _c_var(self, name: str) -> str:
        """Convert an IR value name to a valid C identifier."""
        return "v_" + name.replace("%", "").replace("@", "g_").replace(".", "_").replace(" ", "_")

    def _c_label(self, label: str) -> str:
        """Convert an IR block label to a valid C label."""
        return "lbl_" + label.replace(".", "_").replace("-", "_")

    def _c_value(self, value: IRValue) -> str:
        """Return the C expression for an IR value."""
        if isinstance(value, IRConst):
            return self._c_literal(value)
        return self._c_var(str(value))

    def _c_literal(self, c: IRConst) -> str:
        if c.value is None:
            return "0"
        if isinstance(c.value, bool):
            return "1" if c.value else "0"
        if isinstance(c.value, int):
            return f"{c.value}LL"
        if isinstance(c.value, float):
            return repr(c.value)
        if isinstance(c.value, str):
            return self._intern_string(c.value)
        return "0"

    def _is_param(self, key: str, fn: IRFunction) -> bool:
        for p in fn.params:
            if str(p) == key:
                return True
        return False
