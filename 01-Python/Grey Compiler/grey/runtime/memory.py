"""
Memory Manager for the Grey runtime.

Provides:
  - Heap allocation for Grey objects
  - Object representation (tagged values)
  - Stack management for call frames
  - Reference tracking for GC
"""

from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Optional


class ValueType(Enum):
    """Runtime value type tags."""
    NIL = auto()
    BOOL = auto()
    INT = auto()
    FLOAT = auto()
    STRING = auto()
    ARRAY = auto()
    STRUCT = auto()
    FUNCTION = auto()
    CLOSURE = auto()
    NATIVE_FN = auto()


@dataclass
class GreyValue:
    """
    A tagged runtime value.
    All Grey values at runtime are represented as GreyValue.
    """
    type: ValueType
    data: Any = None
    marked: bool = False     # GC mark bit
    ref_count: int = 0       # Reference count (for RC-based GC)

    def __repr__(self):
        if self.type == ValueType.NIL:
            return "nil"
        if self.type == ValueType.BOOL:
            return "true" if self.data else "false"
        if self.type == ValueType.STRING:
            return f'"{self.data}"'
        if self.type == ValueType.ARRAY:
            items = ", ".join(str(v) for v in self.data)
            return f"[{items}]"
        if self.type == ValueType.STRUCT:
            fields = ", ".join(f"{k}: {v}" for k, v in self.data.items())
            return f"{{{fields}}}"
        if self.type == ValueType.FUNCTION:
            return f"<fn {self.data}>"
        if self.type == ValueType.CLOSURE:
            return f"<closure>"
        if self.type == ValueType.NATIVE_FN:
            return f"<native fn>"
        return str(self.data)

    def to_python(self) -> Any:
        """Convert to a Python native value."""
        if self.type == ValueType.NIL:
            return None
        if self.type == ValueType.ARRAY:
            return [v.to_python() for v in self.data]
        if self.type == ValueType.STRUCT:
            return {k: v.to_python() for k, v in self.data.items()}
        return self.data

    def is_truthy(self) -> bool:
        """Grey truthiness rules."""
        if self.type == ValueType.NIL:
            return False
        if self.type == ValueType.BOOL:
            return self.data
        if self.type == ValueType.INT:
            return self.data != 0
        if self.type == ValueType.FLOAT:
            return self.data != 0.0
        if self.type == ValueType.STRING:
            return len(self.data) > 0
        if self.type == ValueType.ARRAY:
            return len(self.data) > 0
        return True

    def type_name(self) -> str:
        """Human-readable type name."""
        TYPE_NAMES = {
            ValueType.NIL: "nil",
            ValueType.BOOL: "bool",
            ValueType.INT: "int",
            ValueType.FLOAT: "float",
            ValueType.STRING: "string",
            ValueType.ARRAY: "array",
            ValueType.STRUCT: "struct",
            ValueType.FUNCTION: "function",
            ValueType.CLOSURE: "closure",
            ValueType.NATIVE_FN: "native_function",
        }
        return TYPE_NAMES.get(self.type, "unknown")


# Singleton constants
NIL = GreyValue(ValueType.NIL)
TRUE = GreyValue(ValueType.BOOL, True)
FALSE = GreyValue(ValueType.BOOL, False)


def make_int(value: int) -> GreyValue:
    return GreyValue(ValueType.INT, value)

def make_float(value: float) -> GreyValue:
    return GreyValue(ValueType.FLOAT, value)

def make_string(value: str) -> GreyValue:
    return GreyValue(ValueType.STRING, value)

def make_bool(value: bool) -> GreyValue:
    return TRUE if value else FALSE

def make_array(elements: list[GreyValue] = None) -> GreyValue:
    return GreyValue(ValueType.ARRAY, elements or [])

def make_struct(fields: dict[str, GreyValue] = None) -> GreyValue:
    return GreyValue(ValueType.STRUCT, fields or {})

def make_function(name: str, chunk_index: int) -> GreyValue:
    return GreyValue(ValueType.FUNCTION, {"name": name, "chunk": chunk_index})

def make_native(fn) -> GreyValue:
    return GreyValue(ValueType.NATIVE_FN, fn)


@dataclass
class CallFrame:
    """
    A call frame on the VM stack.
    Represents a single function invocation.
    """
    chunk_index: int          # which chunk is executing
    ip: int = 0               # instruction pointer
    base_register: int = 0    # base of this frame's registers
    return_register: int = 0  # where to put the return value
    function_name: str = ""


class MemoryManager:
    """
    Manages memory for the Grey runtime.

    Provides:
      - Object allocation and tracking
      - Register file management
      - Call stack management
      - Memory statistics
    """

    def __init__(self, stack_size: int = 1024, register_count: int = 256):
        self.stack_size = stack_size
        self.register_count = register_count

        # Register file (per frame)
        self.registers: list[GreyValue] = [NIL] * register_count

        # Global variables
        self.globals: dict[str, GreyValue] = {}

        # Call stack
        self.call_stack: list[CallFrame] = []
        self.max_call_depth = 256

        # Heap: all allocated objects (tracked for GC)
        self.heap: list[GreyValue] = []
        self.total_allocated: int = 0
        self.gc_threshold: int = 1024  # trigger GC after this many allocations

    def alloc(self, value: GreyValue) -> GreyValue:
        """Allocate a heap object and track it."""
        self.heap.append(value)
        self.total_allocated += 1
        value.ref_count = 1
        return value

    def alloc_array(self, size: int = 0) -> GreyValue:
        """Allocate a new array."""
        arr = make_array([NIL] * size)
        return self.alloc(arr)

    def alloc_struct(self, fields: dict[str, GreyValue] = None) -> GreyValue:
        """Allocate a new struct."""
        s = make_struct(fields)
        return self.alloc(s)

    def alloc_string(self, value: str) -> GreyValue:
        """Allocate a string (could be interned in future)."""
        return self.alloc(make_string(value))

    # ── Register Access ──────────────────────────────────────

    def get_register(self, index: int) -> GreyValue:
        """Read a register."""
        if 0 <= index < len(self.registers):
            return self.registers[index]
        return NIL

    def set_register(self, index: int, value: GreyValue):
        """Write to a register."""
        if 0 <= index < len(self.registers):
            self.registers[index] = value

    # ── Global Access ────────────────────────────────────────

    def get_global(self, name: str) -> GreyValue:
        return self.globals.get(name, NIL)

    def set_global(self, name: str, value: GreyValue):
        self.globals[name] = value

    # ── Call Stack ───────────────────────────────────────────

    def push_frame(self, frame: CallFrame):
        """Push a new call frame."""
        if len(self.call_stack) >= self.max_call_depth:
            raise RuntimeError("Stack overflow: maximum call depth exceeded")
        self.call_stack.append(frame)

    def pop_frame(self) -> Optional[CallFrame]:
        """Pop the current call frame."""
        if self.call_stack:
            return self.call_stack.pop()
        return None

    @property
    def current_frame(self) -> Optional[CallFrame]:
        if self.call_stack:
            return self.call_stack[-1]
        return None

    @property
    def call_depth(self) -> int:
        return len(self.call_stack)

    # ── Statistics ───────────────────────────────────────────

    def stats(self) -> dict:
        return {
            "heap_objects": len(self.heap),
            "total_allocated": self.total_allocated,
            "call_depth": self.call_depth,
            "globals": len(self.globals),
        }

    def should_gc(self) -> bool:
        return len(self.heap) >= self.gc_threshold
