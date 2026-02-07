"""
IR Generator for the Grey compiler.

Transforms the (normalized, type-checked) AST into IR (three-address code).
This is the bridge between the frontend and the middle-end optimization pipeline.
"""

from typing import Optional
from ..frontend.ast_nodes import *
from ..frontend.symbols import TypeInfo, INT_TYPE, FLOAT_TYPE, STRING_TYPE, BOOL_TYPE, VOID_TYPE
from .ir import (
    IRModule, IRFunction, BasicBlock, IRInstruction,
    IRValue, IRConst, IRTemp, IRParam, IRGlobal, IRGlobalVar,
    IRType, Opcode,
)


def _grey_type_to_ir(type_info: Optional[TypeInfo]) -> IRType:
    """Convert a Grey TypeInfo to an IR type."""
    if type_info is None:
        return IRType.ANY

    name = type_info.name
    if name == "int":
        return IRType.I64
    if name == "float":
        return IRType.F64
    if name == "string":
        return IRType.STR
    if name == "bool":
        return IRType.BOOL
    if name == "void":
        return IRType.VOID
    if type_info.is_array:
        return IRType.ARRAY
    if type_info.is_struct:
        return IRType.STRUCT
    if type_info.is_function:
        return IRType.FUNC
    return IRType.ANY


class IRGenerator(ASTVisitor):
    """
    Generates IR from a type-checked, normalized AST.

    Usage:
        gen = IRGenerator()
        module = gen.generate(program, module_name="main")
    """

    def __init__(self):
        self.module: IRModule = IRModule()
        self.current_function: Optional[IRFunction] = None
        self.current_block: Optional[BasicBlock] = None
        self.temp_counter: int = 0
        self.block_counter: int = 0
        self.locals: dict[str, IRValue] = {}
        self.loop_stack: list[tuple[BasicBlock, BasicBlock]] = []  # (continue_target, break_target)

    def generate(self, program: Program, module_name: str = "main") -> IRModule:
        """Generate IR for the entire program."""
        self.module = IRModule(name=module_name)

        # Generate a __main__ function for top-level statements
        main_fn = IRFunction(name="__main__", return_type=IRType.VOID)
        self.module.add_function(main_fn)

        self.current_function = main_fn
        entry = main_fn.add_block("entry")
        self.current_block = entry

        # Process all declarations
        for decl in program.declarations:
            decl.accept(self)

        # Process top-level statements
        for stmt in program.statements:
            stmt.accept(self)

        # Add return to main if not terminated
        if self.current_block and not self.current_block.is_terminated:
            self._emit(IRInstruction(opcode=Opcode.RETURN))

        return self.module

    # ── Helpers ──────────────────────────────────────────────

    def _new_temp(self, ir_type: IRType = IRType.ANY) -> IRTemp:
        self.temp_counter += 1
        return IRTemp(name=f"t{self.temp_counter}", ir_type=ir_type, index=self.temp_counter)

    def _new_block(self, prefix: str = "bb") -> BasicBlock:
        self.block_counter += 1
        label = f"{prefix}_{self.block_counter}"
        block = self.current_function.add_block(label)
        return block

    def _emit(self, instr: IRInstruction):
        """Emit an instruction into the current basic block."""
        if self.current_block:
            self.current_block.append(instr)

    def _switch_block(self, block: BasicBlock):
        """Switch emission to a new basic block."""
        self.current_block = block

    def _emit_const(self, value, ir_type: IRType) -> IRConst:
        """Create and emit a constant value."""
        const = IRConst(name=f"c_{value}", ir_type=ir_type, value=value)
        return const

    # ══════════════════════════════════════════════════════════
    #  Declarations
    # ══════════════════════════════════════════════════════════

    def visit_VarDecl(self, node: VarDecl):
        ir_type = _grey_type_to_ir(node.resolved_type)

        if node.initializer:
            value = self._gen_expr(node.initializer)
        else:
            value = self._emit_const(0, ir_type)

        # Allocate local variable
        alloca = self._new_temp(ir_type)
        self._emit(IRInstruction(
            opcode=Opcode.ALLOCA,
            result=alloca,
            ir_type=ir_type,
        ))

        # Store initial value
        self._emit(IRInstruction(
            opcode=Opcode.STORE,
            operands=[alloca, value],
            ir_type=ir_type,
        ))

        self.locals[node.name] = alloca
        if self.current_function:
            self.current_function.locals[node.name] = alloca

    def visit_FnDecl(self, node: FnDecl):
        # Save context
        prev_fn = self.current_function
        prev_block = self.current_block
        prev_locals = self.locals.copy()

        # Create IR function
        ret_type = _grey_type_to_ir(
            node.resolved_type.return_type if node.resolved_type and node.resolved_type.is_function
            else (node.resolved_type if node.resolved_type else None)
        )

        params = []
        for i, param in enumerate(node.params):
            p_type = _grey_type_to_ir(
                param.type_annotation.resolved_type if param.type_annotation and hasattr(param.type_annotation, 'resolved_type')
                else None
            )
            ir_param = IRParam(name=param.name, ir_type=p_type, param_index=i)
            params.append(ir_param)

        ir_fn = IRFunction(
            name=node.name,
            params=params,
            return_type=ret_type,
        )
        self.module.add_function(ir_fn)

        # Set up function context
        self.current_function = ir_fn
        self.locals = {}

        entry = ir_fn.add_block("entry")
        self.current_block = entry

        # Store params as locals
        for param in params:
            alloca = self._new_temp(param.ir_type)
            self._emit(IRInstruction(
                opcode=Opcode.ALLOCA, result=alloca, ir_type=param.ir_type
            ))
            self._emit(IRInstruction(
                opcode=Opcode.STORE, operands=[alloca, param], ir_type=param.ir_type
            ))
            self.locals[param.name] = alloca
            ir_fn.locals[param.name] = alloca

        # Generate body
        if node.body:
            for stmt in node.body.statements:
                stmt.accept(self)

        # Add implicit return
        if self.current_block and not self.current_block.is_terminated:
            self._emit(IRInstruction(opcode=Opcode.RETURN))

        # Restore context
        self.current_function = prev_fn
        self.current_block = prev_block
        self.locals = prev_locals

    def visit_StructDecl(self, node: StructDecl):
        pass  # Struct layouts handled by type info

    def visit_EnumDecl(self, node: EnumDecl):
        pass

    def visit_ImplDecl(self, node: ImplDecl):
        for method in node.methods:
            # Rename method to Type.method
            method.name = f"{node.target}.{method.name}"
            method.accept(self)

    def visit_TraitDecl(self, node: TraitDecl):
        pass

    def visit_ImportDecl(self, node: ImportDecl):
        pass  # Handled by module system

    def visit_TypeAlias(self, node: TypeAlias):
        pass

    # ══════════════════════════════════════════════════════════
    #  Statements
    # ══════════════════════════════════════════════════════════

    def visit_Block(self, node: Block):
        for stmt in node.statements:
            stmt.accept(self)

    def visit_ExpressionStmt(self, node: ExpressionStmt):
        if node.expression:
            self._gen_expr(node.expression)

    def visit_ReturnStmt(self, node: ReturnStmt):
        if node.value:
            value = self._gen_expr(node.value)
            self._emit(IRInstruction(
                opcode=Opcode.RETURN, operands=[value]
            ))
        else:
            self._emit(IRInstruction(opcode=Opcode.RETURN))

    def visit_IfStmt(self, node: IfStmt):
        cond = self._gen_expr(node.condition)

        then_block = self._new_block("then")
        else_block = self._new_block("else") if node.else_body else None
        merge_block = self._new_block("if_merge")

        # Branch
        target_else = else_block if else_block else merge_block
        self._emit(IRInstruction(
            opcode=Opcode.BRANCH,
            operands=[cond,
                      IRConst(name=then_block.label, ir_type=IRType.ANY, value=then_block.label),
                      IRConst(name=target_else.label, ir_type=IRType.ANY, value=target_else.label)],
        ))

        # Then block
        self._switch_block(then_block)
        if node.then_body:
            node.then_body.accept(self)
        if not self.current_block.is_terminated:
            self._emit(IRInstruction(
                opcode=Opcode.JUMP,
                operands=[IRConst(name=merge_block.label, ir_type=IRType.ANY, value=merge_block.label)],
            ))

        # Else block
        if else_block and node.else_body:
            self._switch_block(else_block)
            node.else_body.accept(self)
            if not self.current_block.is_terminated:
                self._emit(IRInstruction(
                    opcode=Opcode.JUMP,
                    operands=[IRConst(name=merge_block.label, ir_type=IRType.ANY, value=merge_block.label)],
                ))

        # Continue in merge block
        self._switch_block(merge_block)

    def visit_WhileStmt(self, node: WhileStmt):
        cond_block = self._new_block("while_cond")
        body_block = self._new_block("while_body")
        exit_block = self._new_block("while_exit")

        # Jump to condition
        self._emit(IRInstruction(
            opcode=Opcode.JUMP,
            operands=[IRConst(name=cond_block.label, ir_type=IRType.ANY, value=cond_block.label)],
        ))

        # Condition block
        self._switch_block(cond_block)
        cond = self._gen_expr(node.condition)
        self._emit(IRInstruction(
            opcode=Opcode.BRANCH,
            operands=[cond,
                      IRConst(name=body_block.label, ir_type=IRType.ANY, value=body_block.label),
                      IRConst(name=exit_block.label, ir_type=IRType.ANY, value=exit_block.label)],
        ))

        # Body block
        self._switch_block(body_block)
        self.loop_stack.append((cond_block, exit_block))
        if node.body:
            node.body.accept(self)
        self.loop_stack.pop()

        if not self.current_block.is_terminated:
            self._emit(IRInstruction(
                opcode=Opcode.JUMP,
                operands=[IRConst(name=cond_block.label, ir_type=IRType.ANY, value=cond_block.label)],
            ))

        # Continue after loop
        self._switch_block(exit_block)

    def visit_ForStmt(self, node: ForStmt):
        # For loops should be desugared by normalizer, but handle just in case
        if node.body:
            node.body.accept(self)

    def visit_BreakStmt(self, node: BreakStmt):
        if self.loop_stack:
            _, exit_block = self.loop_stack[-1]
            self._emit(IRInstruction(
                opcode=Opcode.JUMP,
                operands=[IRConst(name=exit_block.label, ir_type=IRType.ANY, value=exit_block.label)],
            ))

    def visit_ContinueStmt(self, node: ContinueStmt):
        if self.loop_stack:
            cont_block, _ = self.loop_stack[-1]
            self._emit(IRInstruction(
                opcode=Opcode.JUMP,
                operands=[IRConst(name=cont_block.label, ir_type=IRType.ANY, value=cont_block.label)],
            ))

    def visit_MatchStmt(self, node: MatchStmt):
        subject = self._gen_expr(node.subject)
        merge_block = self._new_block("match_merge")

        for i, arm in enumerate(node.arms):
            arm_block = self._new_block(f"match_arm_{i}")
            next_block = self._new_block(f"match_next_{i}")

            # Compare subject with pattern
            pattern_val = self._gen_expr(arm.pattern)
            cmp_result = self._new_temp(IRType.BOOL)
            self._emit(IRInstruction(
                opcode=Opcode.EQ, result=cmp_result,
                operands=[subject, pattern_val], ir_type=IRType.BOOL
            ))
            self._emit(IRInstruction(
                opcode=Opcode.BRANCH,
                operands=[cmp_result,
                          IRConst(name=arm_block.label, ir_type=IRType.ANY, value=arm_block.label),
                          IRConst(name=next_block.label, ir_type=IRType.ANY, value=next_block.label)],
            ))

            # Arm body
            self._switch_block(arm_block)
            if arm.body:
                arm.body.accept(self)
            if not self.current_block.is_terminated:
                self._emit(IRInstruction(
                    opcode=Opcode.JUMP,
                    operands=[IRConst(name=merge_block.label, ir_type=IRType.ANY, value=merge_block.label)],
                ))

            self._switch_block(next_block)

        # Fall through to merge
        if not self.current_block.is_terminated:
            self._emit(IRInstruction(
                opcode=Opcode.JUMP,
                operands=[IRConst(name=merge_block.label, ir_type=IRType.ANY, value=merge_block.label)],
            ))
        self._switch_block(merge_block)

    # ══════════════════════════════════════════════════════════
    #  Expression Generation
    # ══════════════════════════════════════════════════════════

    def _gen_expr(self, expr: Expression) -> IRValue:
        """Generate IR for an expression and return the result value."""
        if isinstance(expr, IntegerLiteral):
            return IRConst(name=f"{expr.value}", ir_type=IRType.I64, value=expr.value)

        if isinstance(expr, FloatLiteral):
            return IRConst(name=f"{expr.value}", ir_type=IRType.F64, value=expr.value)

        if isinstance(expr, StringLiteral):
            self.module.intern_string(expr.value)
            return IRConst(name=f"str_{id(expr)}", ir_type=IRType.STR, value=expr.value)

        if isinstance(expr, BoolLiteral):
            return IRConst(name=str(expr.value).lower(), ir_type=IRType.BOOL, value=expr.value)

        if isinstance(expr, NilLiteral):
            return IRConst(name="nil", ir_type=IRType.PTR, value=None)

        if isinstance(expr, Identifier):
            return self._gen_identifier(expr)

        if isinstance(expr, BinaryExpr):
            return self._gen_binary(expr)

        if isinstance(expr, UnaryExpr):
            return self._gen_unary(expr)

        if isinstance(expr, CallExpr):
            return self._gen_call(expr)

        if isinstance(expr, IndexExpr):
            return self._gen_index(expr)

        if isinstance(expr, MemberExpr):
            return self._gen_member(expr)

        if isinstance(expr, AssignExpr):
            return self._gen_assign(expr)

        if isinstance(expr, ArrayLiteral):
            return self._gen_array_literal(expr)

        if isinstance(expr, RangeExpr):
            return self._gen_range(expr)

        if isinstance(expr, StructLiteral):
            return self._gen_struct_literal(expr)

        if isinstance(expr, LambdaExpr):
            return self._gen_lambda(expr)

        # Fallback
        return IRConst(name="undef", ir_type=IRType.ANY, value=None)

    def _gen_identifier(self, node: Identifier) -> IRValue:
        """Generate IR for a variable reference."""
        if node.name in self.locals:
            alloca = self.locals[node.name]
            result = self._new_temp(alloca.ir_type)
            self._emit(IRInstruction(
                opcode=Opcode.LOAD, result=result,
                operands=[alloca], ir_type=alloca.ir_type
            ))
            return result
        # Function reference
        return IRGlobal(name=node.name, ir_type=IRType.FUNC)

    OP_MAP = {
        '+': Opcode.ADD, '-': Opcode.SUB, '*': Opcode.MUL,
        '/': Opcode.DIV, '%': Opcode.MOD, '**': Opcode.POW,
        '&': Opcode.BIT_AND, '|': Opcode.BIT_OR, '^': Opcode.BIT_XOR,
        '<<': Opcode.SHL, '>>': Opcode.SHR,
        '==': Opcode.EQ, '!=': Opcode.NEQ,
        '<': Opcode.LT, '>': Opcode.GT,
        '<=': Opcode.LTE, '>=': Opcode.GTE,
        'and': Opcode.AND, 'or': Opcode.OR,
    }

    def _gen_binary(self, node: BinaryExpr) -> IRValue:
        """Generate IR for a binary expression."""
        left = self._gen_expr(node.left)
        right = self._gen_expr(node.right)

        opcode = self.OP_MAP.get(node.operator)
        if opcode is None:
            return IRConst(name="error", ir_type=IRType.ANY, value=None)

        # Determine result type
        if opcode in (Opcode.EQ, Opcode.NEQ, Opcode.LT, Opcode.GT,
                      Opcode.LTE, Opcode.GTE, Opcode.AND, Opcode.OR):
            result_type = IRType.BOOL
        elif left.ir_type == IRType.F64 or right.ir_type == IRType.F64:
            result_type = IRType.F64
        elif left.ir_type == IRType.STR and node.operator == '+':
            result_type = IRType.STR
        else:
            result_type = left.ir_type

        result = self._new_temp(result_type)
        self._emit(IRInstruction(
            opcode=opcode, result=result,
            operands=[left, right], ir_type=result_type
        ))
        return result

    def _gen_unary(self, node: UnaryExpr) -> IRValue:
        """Generate IR for a unary expression."""
        operand = self._gen_expr(node.operand)

        if node.operator == '-':
            result = self._new_temp(operand.ir_type)
            self._emit(IRInstruction(
                opcode=Opcode.NEG, result=result,
                operands=[operand], ir_type=operand.ir_type
            ))
            return result

        if node.operator in ('not', '!'):
            result = self._new_temp(IRType.BOOL)
            self._emit(IRInstruction(
                opcode=Opcode.NOT, result=result,
                operands=[operand], ir_type=IRType.BOOL
            ))
            return result

        if node.operator == '~':
            result = self._new_temp(IRType.I64)
            self._emit(IRInstruction(
                opcode=Opcode.BIT_NOT, result=result,
                operands=[operand], ir_type=IRType.I64
            ))
            return result

        return operand

    def _gen_call(self, node: CallExpr) -> IRValue:
        """Generate IR for a function call."""
        args = [self._gen_expr(arg) for arg in node.arguments]

        # Determine if it's a builtin
        is_builtin = False
        fn_name = ""
        if isinstance(node.callee, Identifier):
            fn_name = node.callee.name
            if node.callee.symbol and node.callee.symbol.kind.name == "BUILTIN":
                is_builtin = True

        if is_builtin:
            result = self._new_temp(IRType.ANY)
            self._emit(IRInstruction(
                opcode=Opcode.CALL_BUILTIN, result=result,
                operands=[IRConst(name=fn_name, ir_type=IRType.FUNC, value=fn_name)] + args,
                ir_type=IRType.ANY,
            ))
            return result
        else:
            callee = self._gen_expr(node.callee)
            result_type = _grey_type_to_ir(
                node.resolved_type
            ) if node.resolved_type else IRType.ANY

            result = self._new_temp(result_type)
            self._emit(IRInstruction(
                opcode=Opcode.CALL, result=result,
                operands=[callee] + args,
                ir_type=result_type,
            ))
            return result

    def _gen_index(self, node: IndexExpr) -> IRValue:
        """Generate IR for indexing."""
        obj = self._gen_expr(node.object)
        idx = self._gen_expr(node.index)
        result = self._new_temp(IRType.ANY)
        self._emit(IRInstruction(
            opcode=Opcode.GET_INDEX, result=result,
            operands=[obj, idx], ir_type=IRType.ANY
        ))
        return result

    def _gen_member(self, node: MemberExpr) -> IRValue:
        """Generate IR for member access."""
        obj = self._gen_expr(node.object)
        field_name = IRConst(name=node.member, ir_type=IRType.STR, value=node.member)
        result = self._new_temp(IRType.ANY)
        self._emit(IRInstruction(
            opcode=Opcode.GET_FIELD, result=result,
            operands=[obj, field_name], ir_type=IRType.ANY
        ))
        return result

    def _gen_assign(self, node: AssignExpr) -> IRValue:
        """Generate IR for assignment."""
        value = self._gen_expr(node.value)

        if isinstance(node.target, Identifier):
            if node.target.name in self.locals:
                alloca = self.locals[node.target.name]
                self._emit(IRInstruction(
                    opcode=Opcode.STORE,
                    operands=[alloca, value],
                    ir_type=value.ir_type,
                ))
                return value

        if isinstance(node.target, IndexExpr):
            obj = self._gen_expr(node.target.object)
            idx = self._gen_expr(node.target.index)
            self._emit(IRInstruction(
                opcode=Opcode.SET_INDEX,
                operands=[obj, idx, value],
                ir_type=value.ir_type,
            ))
            return value

        if isinstance(node.target, MemberExpr):
            obj = self._gen_expr(node.target.object)
            field_name = IRConst(name=node.target.member, ir_type=IRType.STR,
                                 value=node.target.member)
            self._emit(IRInstruction(
                opcode=Opcode.SET_FIELD,
                operands=[obj, field_name, value],
                ir_type=value.ir_type,
            ))
            return value

        return value

    def _gen_array_literal(self, node: ArrayLiteral) -> IRValue:
        """Generate IR for an array literal."""
        elements = [self._gen_expr(e) for e in node.elements]
        result = self._new_temp(IRType.ARRAY)

        # Create empty array (elements will be pushed)
        size = IRConst(name="0", ir_type=IRType.I64, value=0)
        self._emit(IRInstruction(
            opcode=Opcode.ARRAY_NEW, result=result,
            operands=[size], ir_type=IRType.ARRAY
        ))

        # Push elements
        for elem in elements:
            self._emit(IRInstruction(
                opcode=Opcode.ARRAY_PUSH,
                operands=[result, elem], ir_type=IRType.VOID
            ))

        return result

    def _gen_range(self, node: RangeExpr) -> IRValue:
        """Generate IR for a range expression."""
        start = self._gen_expr(node.start) if node.start else IRConst(name="0", ir_type=IRType.I64, value=0)
        end = self._gen_expr(node.end) if node.end else IRConst(name="0", ir_type=IRType.I64, value=0)

        result = self._new_temp(IRType.ARRAY)
        self._emit(IRInstruction(
            opcode=Opcode.CALL_BUILTIN, result=result,
            operands=[IRConst(name="__range__", ir_type=IRType.FUNC, value="__range__"),
                      start, end,
                      IRConst(name=str(node.inclusive), ir_type=IRType.BOOL, value=node.inclusive)],
            ir_type=IRType.ARRAY,
        ))
        return result

    def _gen_struct_literal(self, node: StructLiteral) -> IRValue:
        """Generate IR for struct instantiation."""
        result = self._new_temp(IRType.STRUCT)

        # Allocate struct
        self._emit(IRInstruction(
            opcode=Opcode.ALLOCA, result=result,
            ir_type=IRType.STRUCT,
            metadata={"struct_name": node.name}
        ))

        # Set fields
        for field_init in node.fields:
            value = self._gen_expr(field_init.value)
            field_name = IRConst(name=field_init.name, ir_type=IRType.STR, value=field_init.name)
            self._emit(IRInstruction(
                opcode=Opcode.SET_FIELD,
                operands=[result, field_name, value],
                ir_type=value.ir_type,
            ))

        return result

    def _gen_lambda(self, node: LambdaExpr) -> IRValue:
        """Generate IR for a lambda expression."""
        # Create a hidden function for the lambda
        lambda_name = f"__lambda_{self.temp_counter}"
        self.temp_counter += 1

        # Save context
        prev_fn = self.current_function
        prev_block = self.current_block
        prev_locals = self.locals.copy()

        params = []
        for i, p in enumerate(node.params):
            params.append(IRParam(name=p.name, ir_type=IRType.ANY, param_index=i))

        lambda_fn = IRFunction(name=lambda_name, params=params, return_type=IRType.ANY)
        self.module.add_function(lambda_fn)

        self.current_function = lambda_fn
        self.locals = {}
        entry = lambda_fn.add_block("entry")
        self.current_block = entry

        for param in params:
            alloca = self._new_temp(param.ir_type)
            self._emit(IRInstruction(opcode=Opcode.ALLOCA, result=alloca, ir_type=param.ir_type))
            self._emit(IRInstruction(opcode=Opcode.STORE, operands=[alloca, param], ir_type=param.ir_type))
            self.locals[param.name] = alloca

        if node.body:
            if isinstance(node.body, Expression):
                result = self._gen_expr(node.body)
                self._emit(IRInstruction(opcode=Opcode.RETURN, operands=[result]))
            else:
                node.body.accept(self)
                if not self.current_block.is_terminated:
                    self._emit(IRInstruction(opcode=Opcode.RETURN))

        # Restore context
        self.current_function = prev_fn
        self.current_block = prev_block
        self.locals = prev_locals

        return IRGlobal(name=lambda_name, ir_type=IRType.FUNC)
