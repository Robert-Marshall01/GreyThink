"""
Standard Library for the Grey runtime.

Implements all built-in functions:
  - I/O:    print, println, input
  - Math:   abs, min, max, pow, sqrt, floor, ceil, round
  - String: len, substr, concat, split, trim, upper, lower,
            starts_with, ends_with, contains, replace, to_string, char_at
  - Array:  push, pop, len, sort, reverse, map, filter, range
  - Type:   typeof, to_int, to_float, to_string, is_nil
  - System: assert, panic, exit, clock
"""

from __future__ import annotations
import time
import math
from typing import Callable, Any

from .memory import (
    GreyValue, ValueType,
    NIL, TRUE, FALSE,
    make_int, make_float, make_string, make_bool, make_array,
)


class StandardLibrary:
    """
    Registry of built-in (native) functions for the Grey VM.

    Each built-in is a Python callable:
        (args: list[GreyValue]) -> GreyValue
    """

    def __init__(self):
        self.functions: dict[str, Callable] = {}
        self._register_all()

    def _register_all(self):
        """Register every built-in function."""
        # I/O
        self.register("print",    self._print)
        self.register("println",  self._println)
        self.register("input",    self._input)

        # Math
        self.register("abs",      self._abs)
        self.register("min",      self._min)
        self.register("max",      self._max)
        self.register("pow",      self._pow)
        self.register("sqrt",     self._sqrt)
        self.register("floor",    self._floor)
        self.register("ceil",     self._ceil)
        self.register("round",    self._round)

        # String / Array length
        self.register("len",      self._len)

        # String ops
        self.register("substr",       self._substr)
        self.register("concat",       self._concat)
        self.register("split",        self._split)
        self.register("trim",         self._trim)
        self.register("upper",        self._upper)
        self.register("lower",        self._lower)
        self.register("starts_with",  self._starts_with)
        self.register("ends_with",    self._ends_with)
        self.register("contains",     self._contains)
        self.register("replace",      self._replace)
        self.register("char_at",      self._char_at)

        # Array ops
        self.register("push",    self._push)
        self.register("pop",     self._pop)
        self.register("sort",    self._sort)
        self.register("reverse", self._reverse)
        self.register("range",   self._range)

        # Type ops
        self.register("typeof",     self._typeof)
        self.register("to_int",     self._to_int)
        self.register("to_float",   self._to_float)
        self.register("to_string",  self._to_string)
        self.register("is_nil",     self._is_nil)

        # System
        self.register("assert",  self._assert)
        self.register("panic",   self._panic)
        self.register("exit",    self._exit)
        self.register("clock",   self._clock)

    def register(self, name: str, fn: Callable):
        """Register a native function."""
        self.functions[name] = fn

    def get(self, name: str):
        """Get a native function by name, or None."""
        return self.functions.get(name)

    def has(self, name: str) -> bool:
        return name in self.functions

    def call(self, name: str, args: list[GreyValue]) -> GreyValue:
        """Call a native function by name."""
        fn = self.functions.get(name)
        if fn is None:
            raise RuntimeError(f"Unknown built-in function: {name}")
        return fn(args)

    # ── I/O Functions ────────────────────────────────────────

    def _print(self, args: list[GreyValue]) -> GreyValue:
        parts = [self._display(a) for a in args]
        print(" ".join(parts), end="")
        return NIL

    def _println(self, args: list[GreyValue]) -> GreyValue:
        parts = [self._display(a) for a in args]
        print(" ".join(parts))
        return NIL

    def _input(self, args: list[GreyValue]) -> GreyValue:
        prompt = ""
        if args:
            prompt = self._display(args[0])
        result = input(prompt)
        return make_string(result)

    # ── Math Functions ───────────────────────────────────────

    def _abs(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("abs", args, 1)
        v = args[0]
        if v.type == ValueType.INT:
            return make_int(abs(v.data))
        if v.type == ValueType.FLOAT:
            return make_float(abs(v.data))
        raise RuntimeError(f"abs() expects a number, got {v.type_name()}")

    def _min(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("min", args, 2)
        a, b = args[0], args[1]
        av = self._to_number(a, "min")
        bv = self._to_number(b, "min")
        if av <= bv:
            return a
        return b

    def _max(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("max", args, 2)
        a, b = args[0], args[1]
        av = self._to_number(a, "max")
        bv = self._to_number(b, "max")
        if av >= bv:
            return a
        return b

    def _pow(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("pow", args, 2)
        base = self._to_number(args[0], "pow")
        exp = self._to_number(args[1], "pow")
        result = base ** exp
        if isinstance(result, float):
            return make_float(result)
        return make_int(result)

    def _sqrt(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("sqrt", args, 1)
        v = self._to_number(args[0], "sqrt")
        return make_float(math.sqrt(v))

    def _floor(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("floor", args, 1)
        v = self._to_number(args[0], "floor")
        return make_int(math.floor(v))

    def _ceil(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("ceil", args, 1)
        v = self._to_number(args[0], "ceil")
        return make_int(math.ceil(v))

    def _round(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("round", args, 1)
        v = self._to_number(args[0], "round")
        return make_int(round(v))

    # ── String / Array Functions ─────────────────────────────

    def _len(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("len", args, 1)
        v = args[0]
        if v.type == ValueType.STRING:
            return make_int(len(v.data))
        if v.type == ValueType.ARRAY:
            return make_int(len(v.data))
        raise RuntimeError(f"len() expects string or array, got {v.type_name()}")

    def _substr(self, args: list[GreyValue]) -> GreyValue:
        if len(args) < 2:
            raise RuntimeError("substr() requires at least 2 arguments")
        s = self._expect_string(args[0], "substr")
        start = self._expect_int(args[1], "substr")
        end = len(s) if len(args) < 3 else self._expect_int(args[2], "substr")
        return make_string(s[start:end])

    def _concat(self, args: list[GreyValue]) -> GreyValue:
        parts = [self._display(a) for a in args]
        return make_string("".join(parts))

    def _split(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("split", args, 2)
        s = self._expect_string(args[0], "split")
        sep = self._expect_string(args[1], "split")
        parts = s.split(sep)
        return make_array([make_string(p) for p in parts])

    def _trim(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("trim", args, 1)
        s = self._expect_string(args[0], "trim")
        return make_string(s.strip())

    def _upper(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("upper", args, 1)
        return make_string(self._expect_string(args[0], "upper").upper())

    def _lower(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("lower", args, 1)
        return make_string(self._expect_string(args[0], "lower").lower())

    def _starts_with(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("starts_with", args, 2)
        s = self._expect_string(args[0], "starts_with")
        prefix = self._expect_string(args[1], "starts_with")
        return make_bool(s.startswith(prefix))

    def _ends_with(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("ends_with", args, 2)
        s = self._expect_string(args[0], "ends_with")
        suffix = self._expect_string(args[1], "ends_with")
        return make_bool(s.endswith(suffix))

    def _contains(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("contains", args, 2)
        s = self._expect_string(args[0], "contains")
        sub = self._expect_string(args[1], "contains")
        return make_bool(sub in s)

    def _replace(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("replace", args, 3)
        s = self._expect_string(args[0], "replace")
        old = self._expect_string(args[1], "replace")
        new = self._expect_string(args[2], "replace")
        return make_string(s.replace(old, new))

    def _char_at(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("char_at", args, 2)
        s = self._expect_string(args[0], "char_at")
        idx = self._expect_int(args[1], "char_at")
        if 0 <= idx < len(s):
            return make_string(s[idx])
        raise RuntimeError(f"char_at: index {idx} out of range for string of length {len(s)}")

    # ── Array Functions ──────────────────────────────────────

    def _push(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("push", args, 2)
        arr = args[0]
        if arr.type != ValueType.ARRAY:
            raise RuntimeError(f"push() expects array, got {arr.type_name()}")
        arr.data.append(args[1])
        return NIL

    def _pop(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("pop", args, 1)
        arr = args[0]
        if arr.type != ValueType.ARRAY:
            raise RuntimeError(f"pop() expects array, got {arr.type_name()}")
        if not arr.data:
            raise RuntimeError("pop() on empty array")
        return arr.data.pop()

    def _sort(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("sort", args, 1)
        arr = args[0]
        if arr.type != ValueType.ARRAY:
            raise RuntimeError(f"sort() expects array, got {arr.type_name()}")
        arr.data.sort(key=lambda v: v.data if v.data is not None else 0)
        return arr

    def _reverse(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("reverse", args, 1)
        arr = args[0]
        if arr.type != ValueType.ARRAY:
            raise RuntimeError(f"reverse() expects array, got {arr.type_name()}")
        arr.data.reverse()
        return arr

    def _range(self, args: list[GreyValue]) -> GreyValue:
        if len(args) < 1:
            raise RuntimeError("range() requires at least 1 argument")
        if len(args) == 1:
            end = self._expect_int(args[0], "range")
            return make_array([make_int(i) for i in range(end)])
        if len(args) == 2:
            start = self._expect_int(args[0], "range")
            end = self._expect_int(args[1], "range")
            return make_array([make_int(i) for i in range(start, end)])
        start = self._expect_int(args[0], "range")
        end = self._expect_int(args[1], "range")
        step = self._expect_int(args[2], "range")
        return make_array([make_int(i) for i in range(start, end, step)])

    # ── Type Functions ───────────────────────────────────────

    def _typeof(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("typeof", args, 1)
        return make_string(args[0].type_name())

    def _to_int(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("to_int", args, 1)
        v = args[0]
        if v.type == ValueType.INT:
            return v
        if v.type == ValueType.FLOAT:
            return make_int(int(v.data))
        if v.type == ValueType.STRING:
            try:
                return make_int(int(v.data))
            except ValueError:
                raise RuntimeError(f"to_int: cannot convert '{v.data}' to int")
        if v.type == ValueType.BOOL:
            return make_int(1 if v.data else 0)
        raise RuntimeError(f"to_int: cannot convert {v.type_name()} to int")

    def _to_float(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("to_float", args, 1)
        v = args[0]
        if v.type == ValueType.FLOAT:
            return v
        if v.type == ValueType.INT:
            return make_float(float(v.data))
        if v.type == ValueType.STRING:
            try:
                return make_float(float(v.data))
            except ValueError:
                raise RuntimeError(f"to_float: cannot convert '{v.data}' to float")
        raise RuntimeError(f"to_float: cannot convert {v.type_name()} to float")

    def _to_string(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("to_string", args, 1)
        return make_string(self._display(args[0]))

    def _is_nil(self, args: list[GreyValue]) -> GreyValue:
        self._check_args("is_nil", args, 1)
        return make_bool(args[0].type == ValueType.NIL)

    # ── System Functions ─────────────────────────────────────

    def _assert(self, args: list[GreyValue]) -> GreyValue:
        if not args:
            raise RuntimeError("assert() requires at least 1 argument")
        cond = args[0]
        if not cond.is_truthy():
            msg = "Assertion failed"
            if len(args) > 1 and args[1].type == ValueType.STRING:
                msg = f"Assertion failed: {args[1].data}"
            raise RuntimeError(msg)
        return NIL

    def _panic(self, args: list[GreyValue]) -> GreyValue:
        msg = "panic!"
        if args and args[0].type == ValueType.STRING:
            msg = args[0].data
        raise RuntimeError(f"PANIC: {msg}")

    def _exit(self, args: list[GreyValue]) -> GreyValue:
        code = 0
        if args and args[0].type == ValueType.INT:
            code = args[0].data
        raise SystemExit(code)

    def _clock(self, args: list[GreyValue]) -> GreyValue:
        return make_float(time.time())

    # ── Helpers ──────────────────────────────────────────────

    @staticmethod
    def _display(value: GreyValue) -> str:
        """Convert a GreyValue to its display string."""
        if value.type == ValueType.NIL:
            return "nil"
        if value.type == ValueType.BOOL:
            return "true" if value.data else "false"
        if value.type == ValueType.STRING:
            return value.data  # no quotes in print output
        if value.type == ValueType.ARRAY:
            items = ", ".join(StandardLibrary._display(v) for v in value.data)
            return f"[{items}]"
        if value.type == ValueType.STRUCT:
            fields = ", ".join(
                f"{k}: {StandardLibrary._display(v)}" for k, v in value.data.items()
            )
            return f"{{{fields}}}"
        return str(value.data)

    @staticmethod
    def _check_args(name: str, args: list, expected: int):
        if len(args) < expected:
            raise RuntimeError(
                f"{name}() expects {expected} argument(s), got {len(args)}"
            )

    @staticmethod
    def _expect_int(value: GreyValue, fn_name: str) -> int:
        if value.type != ValueType.INT:
            raise RuntimeError(f"{fn_name}() expects int, got {value.type_name()}")
        return value.data

    @staticmethod
    def _expect_string(value: GreyValue, fn_name: str) -> str:
        if value.type != ValueType.STRING:
            raise RuntimeError(f"{fn_name}() expects string, got {value.type_name()}")
        return value.data

    @staticmethod
    def _to_number(value: GreyValue, fn_name: str):
        if value.type == ValueType.INT:
            return value.data
        if value.type == ValueType.FLOAT:
            return value.data
        raise RuntimeError(f"{fn_name}() expects number, got {value.type_name()}")
