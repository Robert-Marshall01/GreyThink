"""
IR Optimization Pipeline for the Grey compiler.

Implements multiple optimization passes:
  - Constant folding (evaluate constant expressions at compile time)
  - Dead code elimination (remove unreachable or unused code)
  - Common subexpression elimination (CSE)
  - Constant propagation
  - Strength reduction
  - Block merging (merge simple linear chains of blocks)
  - Peephole optimizations

Optimizations can be local (within a block), global (within a function),
or interprocedural (across functions).
"""

from typing import Optional
from .ir import (
    IRModule, IRFunction, BasicBlock, IRInstruction,
    IRValue, IRConst, IRTemp, Opcode, IRType,
)


class OptimizationPass:
    """Base class for optimization passes."""

    name: str = "unnamed"

    def run_on_module(self, module: IRModule) -> bool:
        """Run this pass on the entire module. Returns True if changes were made."""
        changed = False
        for fn in module.functions:
            changed |= self.run_on_function(fn)
        return changed

    def run_on_function(self, fn: IRFunction) -> bool:
        """Run on a single function."""
        changed = False
        for block in fn.blocks:
            changed |= self.run_on_block(block)
        return changed

    def run_on_block(self, block: BasicBlock) -> bool:
        """Run on a single basic block."""
        return False


class ConstantFolding(OptimizationPass):
    """
    Evaluate constant expressions at compile time.

    Example: %t1 = add 3, 4  →  %t1 = const 7
    """

    name = "constant-folding"

    def run_on_block(self, block: BasicBlock) -> bool:
        changed = False
        new_instructions = []

        for instr in block.instructions:
            folded = self._try_fold(instr)
            if folded is not None:
                new_instructions.append(folded)
                changed = True
            else:
                new_instructions.append(instr)

        block.instructions = new_instructions
        return changed

    def _try_fold(self, instr: IRInstruction) -> Optional[IRInstruction]:
        """Try to fold a constant expression."""
        if len(instr.operands) != 2:
            return None

        left, right = instr.operands[0], instr.operands[1]

        if not (isinstance(left, IRConst) and isinstance(right, IRConst)):
            return None

        if left.value is None or right.value is None:
            return None

        try:
            result = None

            if instr.opcode == Opcode.ADD:
                result = left.value + right.value
            elif instr.opcode == Opcode.SUB:
                result = left.value - right.value
            elif instr.opcode == Opcode.MUL:
                result = left.value * right.value
            elif instr.opcode == Opcode.DIV:
                if right.value == 0:
                    return None
                result = left.value / right.value
            elif instr.opcode == Opcode.MOD:
                if right.value == 0:
                    return None
                result = left.value % right.value
            elif instr.opcode == Opcode.POW:
                result = left.value ** right.value
            elif instr.opcode == Opcode.EQ:
                result = left.value == right.value
            elif instr.opcode == Opcode.NEQ:
                result = left.value != right.value
            elif instr.opcode == Opcode.LT:
                result = left.value < right.value
            elif instr.opcode == Opcode.GT:
                result = left.value > right.value
            elif instr.opcode == Opcode.LTE:
                result = left.value <= right.value
            elif instr.opcode == Opcode.GTE:
                result = left.value >= right.value
            elif instr.opcode == Opcode.AND:
                result = left.value and right.value
            elif instr.opcode == Opcode.OR:
                result = left.value or right.value
            elif instr.opcode == Opcode.BIT_AND:
                if isinstance(left.value, int) and isinstance(right.value, int):
                    result = left.value & right.value
            elif instr.opcode == Opcode.BIT_OR:
                if isinstance(left.value, int) and isinstance(right.value, int):
                    result = left.value | right.value
            elif instr.opcode == Opcode.BIT_XOR:
                if isinstance(left.value, int) and isinstance(right.value, int):
                    result = left.value ^ right.value
            elif instr.opcode == Opcode.SHL:
                if isinstance(left.value, int) and isinstance(right.value, int):
                    result = left.value << right.value
            elif instr.opcode == Opcode.SHR:
                if isinstance(left.value, int) and isinstance(right.value, int):
                    result = left.value >> right.value

            if result is not None:
                if isinstance(result, bool):
                    ir_type = IRType.BOOL
                elif isinstance(result, int):
                    ir_type = IRType.I64
                elif isinstance(result, float):
                    ir_type = IRType.F64
                else:
                    ir_type = instr.ir_type

                const = IRConst(
                    name=f"folded_{result}",
                    ir_type=ir_type,
                    value=result
                )
                return IRInstruction(
                    opcode=Opcode.COPY,
                    result=instr.result,
                    operands=[const],
                    ir_type=ir_type,
                )

        except (ArithmeticError, OverflowError, TypeError):
            pass

        return None


class DeadCodeElimination(OptimizationPass):
    """
    Remove dead (unreachable or unused) instructions.

    An instruction is dead if:
      - Its result is never used
      - It has no side effects
      - The block is unreachable
    """

    name = "dead-code-elimination"

    def run_on_function(self, fn: IRFunction) -> bool:
        changed = False

        # Remove unreachable blocks
        reachable = self._find_reachable_blocks(fn)
        original_count = len(fn.blocks)
        fn.blocks = [b for b in fn.blocks if b.label in reachable]
        if len(fn.blocks) < original_count:
            changed = True

        # Remove unused instructions within blocks
        used_values = self._find_used_values(fn)
        for block in fn.blocks:
            new_instrs = []
            for instr in block.instructions:
                if instr.is_terminator or instr.has_side_effects:
                    new_instrs.append(instr)
                elif instr.result and str(instr.result) in used_values:
                    new_instrs.append(instr)
                elif instr.result is None:
                    new_instrs.append(instr)
                else:
                    changed = True
            block.instructions = new_instrs

        return changed

    def _find_reachable_blocks(self, fn: IRFunction) -> set[str]:
        """Find all reachable block labels starting from the entry."""
        if not fn.blocks:
            return set()

        reachable: set[str] = set()
        worklist = [fn.blocks[0].label]
        block_map = {b.label: b for b in fn.blocks}

        while worklist:
            label = worklist.pop()
            if label in reachable:
                continue
            reachable.add(label)

            block = block_map.get(label)
            if block is None:
                continue

            # Find successors from terminator
            if block.terminator:
                term = block.terminator
                if term.opcode == Opcode.JUMP:
                    for op in term.operands:
                        if isinstance(op, IRConst) and op.value in block_map:
                            worklist.append(op.value)
                elif term.opcode == Opcode.BRANCH:
                    for op in term.operands[1:]:  # skip condition
                        if isinstance(op, IRConst) and op.value in block_map:
                            worklist.append(op.value)

        return reachable

    def _find_used_values(self, fn: IRFunction) -> set[str]:
        """Find all values that are used as operands."""
        used = set()
        for block in fn.blocks:
            for instr in block.instructions:
                for op in instr.operands:
                    used.add(str(op))
        return used


class ConstantPropagation(OptimizationPass):
    """
    Replace uses of known-constant variables with the constant value.

    If %t1 = copy 42, then replace all uses of %t1 with 42.
    """

    name = "constant-propagation"

    def run_on_function(self, fn: IRFunction) -> bool:
        changed = False
        constants: dict[str, IRConst] = {}

        # Find constant definitions
        for block in fn.blocks:
            for instr in block.instructions:
                if instr.opcode == Opcode.COPY and instr.result:
                    if len(instr.operands) == 1 and isinstance(instr.operands[0], IRConst):
                        constants[str(instr.result)] = instr.operands[0]
                elif instr.opcode == Opcode.CONST and instr.result:
                    if len(instr.operands) == 1 and isinstance(instr.operands[0], IRConst):
                        constants[str(instr.result)] = instr.operands[0]

        # Replace uses
        for block in fn.blocks:
            for instr in block.instructions:
                for i, op in enumerate(instr.operands):
                    key = str(op)
                    if key in constants:
                        instr.operands[i] = constants[key]
                        changed = True

        return changed


class StrengthReduction(OptimizationPass):
    """
    Replace expensive operations with cheaper equivalents.

    Examples:
      - x * 2  →  x + x (or x << 1)
      - x * 1  →  x
      - x * 0  →  0
      - x + 0  →  x
      - x ** 2 →  x * x
      - x / 1  →  x
    """

    name = "strength-reduction"

    def run_on_block(self, block: BasicBlock) -> bool:
        changed = False
        new_instructions = []

        for instr in block.instructions:
            reduced = self._try_reduce(instr)
            if reduced is not None:
                new_instructions.append(reduced)
                changed = True
            else:
                new_instructions.append(instr)

        block.instructions = new_instructions
        return changed

    def _try_reduce(self, instr: IRInstruction) -> Optional[IRInstruction]:
        if len(instr.operands) != 2 or instr.result is None:
            return None

        left, right = instr.operands

        # Multiply by 0
        if instr.opcode == Opcode.MUL:
            if isinstance(right, IRConst) and right.value == 0:
                return IRInstruction(
                    opcode=Opcode.COPY, result=instr.result,
                    operands=[IRConst(name="0", ir_type=instr.ir_type, value=0)],
                    ir_type=instr.ir_type,
                )
            if isinstance(left, IRConst) and left.value == 0:
                return IRInstruction(
                    opcode=Opcode.COPY, result=instr.result,
                    operands=[IRConst(name="0", ir_type=instr.ir_type, value=0)],
                    ir_type=instr.ir_type,
                )

        # Multiply by 1
        if instr.opcode == Opcode.MUL:
            if isinstance(right, IRConst) and right.value == 1:
                return IRInstruction(
                    opcode=Opcode.COPY, result=instr.result,
                    operands=[left], ir_type=instr.ir_type,
                )
            if isinstance(left, IRConst) and left.value == 1:
                return IRInstruction(
                    opcode=Opcode.COPY, result=instr.result,
                    operands=[right], ir_type=instr.ir_type,
                )

        # Multiply by power of 2 → shift left
        if instr.opcode == Opcode.MUL and isinstance(right, IRConst):
            if isinstance(right.value, int) and right.value > 0:
                if (right.value & (right.value - 1)) == 0:
                    import math
                    shift = int(math.log2(right.value))
                    return IRInstruction(
                        opcode=Opcode.SHL, result=instr.result,
                        operands=[left, IRConst(name=str(shift), ir_type=IRType.I64, value=shift)],
                        ir_type=instr.ir_type,
                    )

        # Add 0
        if instr.opcode == Opcode.ADD:
            if isinstance(right, IRConst) and right.value == 0:
                return IRInstruction(
                    opcode=Opcode.COPY, result=instr.result,
                    operands=[left], ir_type=instr.ir_type,
                )
            if isinstance(left, IRConst) and left.value == 0:
                return IRInstruction(
                    opcode=Opcode.COPY, result=instr.result,
                    operands=[right], ir_type=instr.ir_type,
                )

        # Subtract 0
        if instr.opcode == Opcode.SUB:
            if isinstance(right, IRConst) and right.value == 0:
                return IRInstruction(
                    opcode=Opcode.COPY, result=instr.result,
                    operands=[left], ir_type=instr.ir_type,
                )

        # Divide by 1
        if instr.opcode == Opcode.DIV:
            if isinstance(right, IRConst) and right.value == 1:
                return IRInstruction(
                    opcode=Opcode.COPY, result=instr.result,
                    operands=[left], ir_type=instr.ir_type,
                )

        # Power of 2 → multiply by self
        if instr.opcode == Opcode.POW:
            if isinstance(right, IRConst) and right.value == 2:
                return IRInstruction(
                    opcode=Opcode.MUL, result=instr.result,
                    operands=[left, left], ir_type=instr.ir_type,
                )

        return None


class CommonSubexpressionElimination(OptimizationPass):
    """
    Identify and eliminate redundant computations within a basic block.

    If two instructions compute the same expression, reuse the first result.
    """

    name = "common-subexpression-elimination"

    def run_on_block(self, block: BasicBlock) -> bool:
        changed = False
        seen: dict[str, IRValue] = {}
        new_instructions = []

        for instr in block.instructions:
            if instr.is_pure and instr.result:
                key = self._expr_key(instr)
                if key in seen:
                    # Replace with a copy
                    new_instructions.append(IRInstruction(
                        opcode=Opcode.COPY, result=instr.result,
                        operands=[seen[key]], ir_type=instr.ir_type
                    ))
                    changed = True
                else:
                    seen[key] = instr.result
                    new_instructions.append(instr)
            else:
                new_instructions.append(instr)

        block.instructions = new_instructions
        return changed

    def _expr_key(self, instr: IRInstruction) -> str:
        ops = ",".join(str(op) for op in instr.operands)
        return f"{instr.opcode.name}({ops})"


class BlockMerging(OptimizationPass):
    """
    Merge linear chains of basic blocks.

    If block A jumps unconditionally to block B, and B has only A as predecessor,
    merge B into A.
    """

    name = "block-merging"

    def run_on_function(self, fn: IRFunction) -> bool:
        if len(fn.blocks) <= 1:
            return False

        changed = False
        # Build predecessor map
        pred_count: dict[str, int] = {b.label: 0 for b in fn.blocks}
        succ_map: dict[str, list[str]] = {}

        for block in fn.blocks:
            succs = []
            if block.terminator:
                term = block.terminator
                if term.opcode == Opcode.JUMP:
                    for op in term.operands:
                        if isinstance(op, IRConst):
                            target = op.value
                            if target in pred_count:
                                pred_count[target] += 1
                                succs.append(target)
                elif term.opcode == Opcode.BRANCH:
                    for op in term.operands[1:]:
                        if isinstance(op, IRConst):
                            target = op.value
                            if target in pred_count:
                                pred_count[target] += 1
                                succs.append(target)
            succ_map[block.label] = succs

        # Merge blocks
        block_map = {b.label: b for b in fn.blocks}
        merged = set()

        for block in fn.blocks:
            if block.label in merged:
                continue

            while True:
                term = block.terminator
                if not term or term.opcode != Opcode.JUMP:
                    break
                if len(term.operands) != 1:
                    break

                target_label = term.operands[0].value
                if target_label not in block_map or target_label in merged:
                    break
                if pred_count.get(target_label, 0) != 1:
                    break

                target_block = block_map[target_label]
                # Remove the jump and append target's instructions
                block.instructions.pop()  # remove jump
                block.instructions.extend(target_block.instructions)
                merged.add(target_label)
                changed = True

        fn.blocks = [b for b in fn.blocks if b.label not in merged]
        return changed


class Optimizer:
    """
    The optimization pipeline manager.

    Runs multiple optimization passes in sequence, repeating until
    no more changes are made (fixed-point iteration).

    Usage:
        optimizer = Optimizer(level=2)
        optimizer.optimize(module)
    """

    def __init__(self, level: int = 2):
        """
        Create optimizer with given optimization level.
          0 = no optimization
          1 = basic optimizations (constant folding, dead code)
          2 = standard optimizations (all passes)
          3 = aggressive optimizations (more iterations)
        """
        self.level = level
        self.passes: list[OptimizationPass] = self._create_passes()
        self.stats: dict[str, int] = {}

    def _create_passes(self) -> list[OptimizationPass]:
        if self.level == 0:
            return []

        passes = []

        if self.level >= 1:
            passes.extend([
                ConstantFolding(),
                ConstantPropagation(),
                DeadCodeElimination(),
            ])

        if self.level >= 2:
            passes.extend([
                StrengthReduction(),
                CommonSubexpressionElimination(),
                BlockMerging(),
            ])

        return passes

    def optimize(self, module: IRModule) -> IRModule:
        """Run the optimization pipeline on the IR module."""
        if self.level == 0:
            return module

        max_iterations = 3 if self.level < 3 else 10
        iteration = 0

        while iteration < max_iterations:
            any_changed = False
            for opt_pass in self.passes:
                changed = opt_pass.run_on_module(module)
                if changed:
                    self.stats[opt_pass.name] = self.stats.get(opt_pass.name, 0) + 1
                    any_changed = True

            iteration += 1
            if not any_changed:
                break

        return module

    def get_stats(self) -> str:
        """Return optimization statistics as a string."""
        if not self.stats:
            return "No optimizations applied."
        lines = ["Optimization Statistics:"]
        for name, count in sorted(self.stats.items()):
            lines.append(f"  {name}: {count} pass(es)")
        return "\n".join(lines)
