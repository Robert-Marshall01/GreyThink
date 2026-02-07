"""
Register Allocator for the Grey compiler.

Implements a linear scan register allocation algorithm.
Maps virtual registers (IR temporaries) to physical VM registers.
When registers are exhausted, spills to stack slots.

This is a simplified register allocator suitable for a VM with
a large register file (256 registers).
"""

from dataclasses import dataclass, field
from typing import Optional
from ..middle.ir import IRFunction, BasicBlock, IRInstruction, IRValue, IRTemp


@dataclass
class LiveInterval:
    """Represents the live range of a value."""
    value: str
    start: int = 0        # first use position
    end: int = 0          # last use position
    register: int = -1    # assigned register (-1 = unassigned)
    spilled: bool = False
    spill_slot: int = -1

    def overlaps(self, other: 'LiveInterval') -> bool:
        return self.start < other.end and other.start < self.end


@dataclass
class RegisterPool:
    """Manages available registers."""
    total_registers: int = 256
    used: set[int] = field(default_factory=set)
    next_reg: int = 0

    def allocate(self) -> Optional[int]:
        """Allocate a register. Returns None if all are in use."""
        if self.next_reg < self.total_registers:
            reg = self.next_reg
            self.next_reg += 1
            self.used.add(reg)
            return reg
        # Try to find a free register
        for i in range(self.total_registers):
            if i not in self.used:
                self.used.add(i)
                return i
        return None

    def free(self, reg: int):
        """Free a register."""
        self.used.discard(reg)


class RegisterAllocator:
    """
    Linear scan register allocator.

    The algorithm:
      1. Compute live intervals for all values
      2. Sort intervals by start position
      3. Iterate through intervals:
         - Expire old intervals (free their registers)
         - Allocate a register for the current interval
         - If no register available, spill the longest-lived interval

    Usage:
        allocator = RegisterAllocator(num_registers=256)
        allocation = allocator.allocate(ir_function)
        # allocation maps value names to register numbers
    """

    def __init__(self, num_registers: int = 256):
        self.num_registers = num_registers
        self.pool = RegisterPool(total_registers=num_registers)
        self.intervals: list[LiveInterval] = []
        self.active: list[LiveInterval] = []
        self.allocation: dict[str, int] = {}
        self.spill_count: int = 0

    def allocate(self, fn: IRFunction) -> dict[str, int]:
        """
        Perform register allocation for a function.
        Returns a mapping from value names to register numbers.
        """
        self.pool = RegisterPool(total_registers=self.num_registers)
        self.active = []
        self.allocation = {}
        self.spill_count = 0

        # Step 1: Compute live intervals
        self.intervals = self._compute_live_intervals(fn)

        # Step 2: Sort by start position
        self.intervals.sort(key=lambda iv: iv.start)

        # Step 3: Linear scan
        for interval in self.intervals:
            # Expire old intervals
            self._expire_old_intervals(interval)

            # Try to allocate
            reg = self.pool.allocate()
            if reg is not None:
                interval.register = reg
                self.allocation[interval.value] = reg
                self.active.append(interval)
                self.active.sort(key=lambda iv: iv.end)
            else:
                # Spill
                self._spill_at(interval)

        return self.allocation

    def _compute_live_intervals(self, fn: IRFunction) -> list[LiveInterval]:
        """Compute live intervals for all values in the function."""
        intervals: dict[str, LiveInterval] = {}
        position = 0

        # First: assign positions to all params
        for param in fn.params:
            key = str(param)
            intervals[key] = LiveInterval(value=key, start=0, end=0)

        # Linear scan through all instructions
        for block in fn.blocks:
            for instr in block.instructions:
                position += 1

                # Record definitions
                if instr.result:
                    key = str(instr.result)
                    if key not in intervals:
                        intervals[key] = LiveInterval(value=key, start=position, end=position)
                    else:
                        intervals[key].end = position

                # Record uses
                for op in instr.operands:
                    key = str(op)
                    if key in intervals:
                        intervals[key].end = max(intervals[key].end, position)
                    else:
                        intervals[key] = LiveInterval(value=key, start=0, end=position)

        return list(intervals.values())

    def _expire_old_intervals(self, current: LiveInterval):
        """Remove intervals that have ended before the current one starts."""
        still_active = []
        for interval in self.active:
            if interval.end <= current.start:
                self.pool.free(interval.register)
            else:
                still_active.append(interval)
        self.active = still_active

    def _spill_at(self, interval: LiveInterval):
        """Spill a value to memory when no registers are available."""
        if self.active and self.active[-1].end > interval.end:
            # Spill the active interval with the longest remaining life
            spilled = self.active.pop()
            interval.register = spilled.register
            spilled.register = -1
            spilled.spilled = True
            spilled.spill_slot = self.spill_count
            self.spill_count += 1
            self.allocation[spilled.value] = -(spilled.spill_slot + 1)  # negative = spilled
            self.allocation[interval.value] = interval.register
            self.active.append(interval)
            self.active.sort(key=lambda iv: iv.end)
        else:
            # Spill the current interval
            interval.spilled = True
            interval.spill_slot = self.spill_count
            self.spill_count += 1
            self.allocation[interval.value] = -(interval.spill_slot + 1)

    def get_stats(self) -> str:
        """Return allocation statistics."""
        total = len(self.intervals)
        spilled = sum(1 for iv in self.intervals if iv.spilled)
        max_reg = max((iv.register for iv in self.intervals if not iv.spilled), default=0)
        return (
            f"Register Allocation: {total} values, "
            f"{total - spilled} in registers, {spilled} spilled, "
            f"max register = {max_reg}"
        )
