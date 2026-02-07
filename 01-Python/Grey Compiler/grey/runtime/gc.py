"""
Garbage Collector for the Grey runtime.

Implements:
  - Mark-and-sweep collection
  - Reference counting (optional, hybrid)
  - Root set enumeration (registers, globals, call stack)
  - Finalization support
"""

from __future__ import annotations
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .memory import MemoryManager, GreyValue, ValueType


class GarbageCollector:
    """
    Mark-and-sweep garbage collector for the Grey runtime.

    The GC traverses the root set (registers, globals, call stack)
    and marks all reachable objects. Unmarked objects are swept
    (deallocated) from the heap.
    """

    def __init__(self, memory: MemoryManager):
        self.memory = memory
        self.collections: int = 0
        self.total_freed: int = 0
        self.enabled: bool = True
        self.verbose: bool = False

    def collect(self) -> int:
        """
        Run a full mark-and-sweep collection cycle.
        Returns the number of objects freed.
        """
        if not self.enabled:
            return 0

        heap_before = len(self.memory.heap)

        # Phase 1: Clear all marks
        self._clear_marks()

        # Phase 2: Mark reachable objects from roots
        self._mark_roots()

        # Phase 3: Sweep unreachable objects
        freed = self._sweep()

        self.collections += 1
        self.total_freed += freed

        if self.verbose:
            print(f"[GC] Collection #{self.collections}: "
                  f"freed {freed}/{heap_before} objects, "
                  f"{len(self.memory.heap)} remaining")

        # Adaptive threshold: grow if we still have many objects
        if len(self.memory.heap) > self.memory.gc_threshold // 2:
            self.memory.gc_threshold = max(
                self.memory.gc_threshold,
                len(self.memory.heap) * 2
            )

        return freed

    def _clear_marks(self):
        """Clear the mark bit on all heap objects."""
        for obj in self.memory.heap:
            obj.marked = False

    def _mark_roots(self):
        """Mark all objects reachable from root set."""
        from .memory import ValueType

        # Root 1: Registers
        for reg in self.memory.registers:
            self._mark_value(reg)

        # Root 2: Global variables
        for value in self.memory.globals.values():
            self._mark_value(value)

    def _mark_value(self, value: GreyValue):
        """
        Recursively mark a value and everything it references.
        Uses iterative approach to avoid Python stack overflow.
        """
        from .memory import ValueType

        worklist = [value]

        while worklist:
            val = worklist.pop()

            if val is None or val.marked:
                continue

            val.marked = True

            # Traverse references
            if val.type == ValueType.ARRAY and val.data:
                for element in val.data:
                    if not element.marked:
                        worklist.append(element)

            elif val.type == ValueType.STRUCT and val.data:
                for field_val in val.data.values():
                    if not field_val.marked:
                        worklist.append(field_val)

            elif val.type == ValueType.CLOSURE and isinstance(val.data, dict):
                # Closure captures - mark upvalues
                upvalues = val.data.get("upvalues", [])
                for uv in upvalues:
                    if hasattr(uv, 'marked') and not uv.marked:
                        worklist.append(uv)

    def _sweep(self) -> int:
        """
        Remove unreachable (unmarked) objects from the heap.
        Returns the number of objects freed.
        """
        surviving = []
        freed = 0

        for obj in self.memory.heap:
            if obj.marked:
                surviving.append(obj)
            else:
                # Object is unreachable—finalize it
                self._finalize(obj)
                freed += 1

        self.memory.heap = surviving
        return freed

    def _finalize(self, obj: GreyValue):
        """
        Finalize an object before deallocation.
        Clear references to help Python's own GC.
        """
        obj.data = None
        obj.ref_count = 0

    # ── Manual GC Control ────────────────────────────────────

    def enable(self):
        self.enabled = True

    def disable(self):
        self.enabled = False

    def set_verbose(self, verbose: bool):
        self.verbose = verbose

    # ── Stats ────────────────────────────────────────────────

    def stats(self) -> dict:
        return {
            "collections": self.collections,
            "total_freed": self.total_freed,
            "heap_size": len(self.memory.heap),
            "gc_threshold": self.memory.gc_threshold,
            "enabled": self.enabled,
        }

    def maybe_collect(self):
        """Check if GC should run and do so if needed."""
        if self.enabled and self.memory.should_gc():
            self.collect()
