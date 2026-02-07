"""
AST Normalizer for the Grey language.

Performs AST-to-AST transformations:
  - Desugaring (turning syntactic sugar into core constructs)
  - Normalizing control flow
  - Simplifying expressions
  - Flattening nested structures
  - Constant folding at the AST level

This runs after semantic analysis and before IR generation.
"""

from typing import Optional
from .ast_nodes import *


class ASTNormalizer(ASTVisitor):
    """
    Walks and transforms the AST into a normalized form.

    Transformations:
      1. Desugar compound assignment (x += 1 → x = x + 1)
      2. Desugar for-in loops into while loops
      3. Fold constant expressions
      4. Normalize if-elif chains into nested if-else
      5. Flatten nested blocks
    """

    def normalize(self, program: Program) -> Program:
        """Normalize the entire program AST."""
        program.declarations = [self._normalize_decl(d) for d in program.declarations]
        program.statements = [self._normalize_stmt(s) for s in program.statements]
        return program

    # ── Declaration Normalization ────────────────────────────

    def _normalize_decl(self, decl: Declaration) -> Declaration:
        if isinstance(decl, VarDecl):
            if decl.initializer:
                decl.initializer = self._normalize_expr(decl.initializer)
            return decl

        if isinstance(decl, FnDecl):
            if decl.body:
                decl.body = self._normalize_block(decl.body)
            return decl

        if isinstance(decl, ImplDecl):
            decl.methods = [self._normalize_decl(m) for m in decl.methods]
            return decl

        return decl

    # ── Statement Normalization ──────────────────────────────

    def _normalize_stmt(self, stmt: Statement) -> Statement:
        if isinstance(stmt, ExpressionStmt):
            if stmt.expression:
                stmt.expression = self._normalize_expr(stmt.expression)
            return stmt

        if isinstance(stmt, Block):
            return self._normalize_block(stmt)

        if isinstance(stmt, ReturnStmt):
            if stmt.value:
                stmt.value = self._normalize_expr(stmt.value)
            return stmt

        if isinstance(stmt, IfStmt):
            return self._normalize_if(stmt)

        if isinstance(stmt, WhileStmt):
            stmt.condition = self._normalize_expr(stmt.condition)
            if stmt.body:
                stmt.body = self._normalize_block(stmt.body)
            return stmt

        if isinstance(stmt, ForStmt):
            return self._desugar_for(stmt)

        if isinstance(stmt, VarDecl):
            return self._normalize_decl(stmt)

        if isinstance(stmt, MatchStmt):
            stmt.subject = self._normalize_expr(stmt.subject)
            for arm in stmt.arms:
                if arm.pattern:
                    arm.pattern = self._normalize_expr(arm.pattern)
                if arm.body:
                    if isinstance(arm.body, Statement):
                        arm.body = self._normalize_stmt(arm.body)
                    elif isinstance(arm.body, Expression):
                        arm.body = self._normalize_expr(arm.body)
            return stmt

        return stmt

    def _normalize_block(self, block: Block) -> Block:
        """Normalize all statements in a block and flatten nested blocks."""
        normalized = []
        for stmt in block.statements:
            result = self._normalize_stmt(stmt)
            # Flatten single-statement blocks
            if isinstance(result, Block) and len(result.statements) == 1:
                normalized.append(result.statements[0])
            else:
                normalized.append(result)
        block.statements = normalized
        return block

    # ── Desugaring ───────────────────────────────────────────

    def _desugar_for(self, node: ForStmt) -> Statement:
        """
        Desugar for-in loop into a while loop.

        for i in 0..10 { body }
        →
        let __iter = 0;
        while __iter < 10 {
            let i = __iter;
            body
            __iter = __iter + 1;
        }
        """
        loc = node.location

        if isinstance(node.iterable, RangeExpr):
            range_expr = node.iterable
            iter_name = f"__iter_{node.variable}"

            # let __iter = start;
            iter_decl = VarDecl(
                location=loc,
                name=iter_name,
                mutable=True,
                initializer=range_expr.start or IntegerLiteral(value=0, location=loc),
            )

            # __iter < end
            condition = BinaryExpr(
                location=loc,
                left=Identifier(name=iter_name, location=loc),
                operator="<" if not range_expr.inclusive else "<=",
                right=range_expr.end,
            )

            # let variable = __iter;
            var_decl = VarDecl(
                location=loc,
                name=node.variable,
                initializer=Identifier(name=iter_name, location=loc),
            )

            # __iter = __iter + 1;
            increment = ExpressionStmt(
                location=loc,
                expression=AssignExpr(
                    location=loc,
                    target=Identifier(name=iter_name, location=loc),
                    operator="=",
                    value=BinaryExpr(
                        location=loc,
                        left=Identifier(name=iter_name, location=loc),
                        operator="+",
                        right=IntegerLiteral(value=1, location=loc),
                    ),
                ),
            )

            # Construct while body
            body_stmts = [var_decl]
            if node.body:
                body_stmts.extend(node.body.statements)
            body_stmts.append(increment)

            while_body = Block(location=loc, statements=body_stmts)
            while_stmt = WhileStmt(
                location=loc, condition=condition, body=while_body
            )

            # Wrap in a block to contain the iterator variable
            return Block(location=loc, statements=[iter_decl, while_stmt])

        # For non-range iterables, keep as-is
        # (would need iterator protocol for arrays, etc.)
        if node.body:
            node.body = self._normalize_block(node.body)
        return node

    def _normalize_if(self, node: IfStmt) -> IfStmt:
        """
        Normalize elif chains into nested if-else.

        if a { ... } elif b { ... } else { ... }
        →
        if a { ... } else { if b { ... } else { ... } }
        """
        node.condition = self._normalize_expr(node.condition)
        if node.then_body:
            node.then_body = self._normalize_block(node.then_body)

        if node.elif_clauses:
            # Convert elif chain into nested else { if ... }
            rest = node.elif_clauses
            node.elif_clauses = []

            # Build nested if from the last elif backward
            tail_else = node.else_body
            for elif_clause in reversed(rest):
                inner_if = IfStmt(
                    location=elif_clause.location,
                    condition=self._normalize_expr(elif_clause.condition),
                    then_body=self._normalize_block(elif_clause.body) if elif_clause.body else Block(statements=[]),
                    else_body=tail_else,
                )
                tail_else = Block(
                    location=elif_clause.location,
                    statements=[inner_if]
                )

            node.else_body = tail_else

        if node.else_body:
            node.else_body = self._normalize_block(node.else_body)

        return node

    # ── Expression Normalization ─────────────────────────────

    def _normalize_expr(self, expr: Expression) -> Expression:
        if expr is None:
            return expr

        # Desugar compound assignment: x += 1 → x = x + 1
        if isinstance(expr, AssignExpr) and expr.operator != "=":
            op_map = {
                "+=": "+", "-=": "-", "*=": "*",
                "/=": "/", "%=": "%",
            }
            op = op_map.get(expr.operator, "+")
            return AssignExpr(
                location=expr.location,
                target=expr.target,
                operator="=",
                value=BinaryExpr(
                    location=expr.location,
                    left=expr.target,
                    operator=op,
                    right=self._normalize_expr(expr.value),
                ),
            )

        # Constant folding
        if isinstance(expr, BinaryExpr):
            expr.left = self._normalize_expr(expr.left)
            expr.right = self._normalize_expr(expr.right)
            folded = self._try_fold_binary(expr)
            if folded is not None:
                return folded
            return expr

        if isinstance(expr, UnaryExpr):
            expr.operand = self._normalize_expr(expr.operand)
            folded = self._try_fold_unary(expr)
            if folded is not None:
                return folded
            return expr

        if isinstance(expr, CallExpr):
            if expr.callee:
                expr.callee = self._normalize_expr(expr.callee)
            expr.arguments = [self._normalize_expr(a) for a in expr.arguments]
            return expr

        if isinstance(expr, IndexExpr):
            if expr.object:
                expr.object = self._normalize_expr(expr.object)
            if expr.index:
                expr.index = self._normalize_expr(expr.index)
            return expr

        if isinstance(expr, MemberExpr):
            if expr.object:
                expr.object = self._normalize_expr(expr.object)
            return expr

        if isinstance(expr, ArrayLiteral):
            expr.elements = [self._normalize_expr(e) for e in expr.elements]
            return expr

        return expr

    # ── Constant Folding ─────────────────────────────────────

    def _try_fold_binary(self, expr: BinaryExpr) -> Optional[Expression]:
        """Attempt to fold a binary expression with constant operands."""
        left = expr.left
        right = expr.right

        # Both must be literals
        if not (isinstance(left, (IntegerLiteral, FloatLiteral, BoolLiteral, StringLiteral)) and
                isinstance(right, (IntegerLiteral, FloatLiteral, BoolLiteral, StringLiteral))):
            return None

        op = expr.operator
        loc = expr.location

        try:
            # Numeric operations
            if isinstance(left, (IntegerLiteral, FloatLiteral)) and \
               isinstance(right, (IntegerLiteral, FloatLiteral)):
                lv = left.value
                rv = right.value

                if op == '+': result = lv + rv
                elif op == '-': result = lv - rv
                elif op == '*': result = lv * rv
                elif op == '/':
                    if rv == 0:
                        return None  # Don't fold division by zero
                    result = lv / rv
                elif op == '%':
                    if rv == 0:
                        return None
                    result = lv % rv
                elif op == '**': result = lv ** rv
                elif op == '==': return BoolLiteral(location=loc, value=lv == rv)
                elif op == '!=': return BoolLiteral(location=loc, value=lv != rv)
                elif op == '<': return BoolLiteral(location=loc, value=lv < rv)
                elif op == '>': return BoolLiteral(location=loc, value=lv > rv)
                elif op == '<=': return BoolLiteral(location=loc, value=lv <= rv)
                elif op == '>=': return BoolLiteral(location=loc, value=lv >= rv)
                else:
                    return None

                if isinstance(result, float) and result == int(result) and \
                   isinstance(left, IntegerLiteral) and isinstance(right, IntegerLiteral):
                    return IntegerLiteral(location=loc, value=int(result))
                elif isinstance(result, float):
                    return FloatLiteral(location=loc, value=result)
                else:
                    return IntegerLiteral(location=loc, value=int(result))

            # String concatenation
            if isinstance(left, StringLiteral) and isinstance(right, StringLiteral):
                if op == '+':
                    return StringLiteral(location=loc, value=left.value + right.value)

            # Boolean operations
            if isinstance(left, BoolLiteral) and isinstance(right, BoolLiteral):
                if op == 'and':
                    return BoolLiteral(location=loc, value=left.value and right.value)
                elif op == 'or':
                    return BoolLiteral(location=loc, value=left.value or right.value)

        except (ArithmeticError, OverflowError):
            return None

        return None

    def _try_fold_unary(self, expr: UnaryExpr) -> Optional[Expression]:
        """Attempt to fold a unary expression with a constant operand."""
        operand = expr.operand
        loc = expr.location

        if isinstance(operand, IntegerLiteral) and expr.operator == '-':
            return IntegerLiteral(location=loc, value=-operand.value)

        if isinstance(operand, FloatLiteral) and expr.operator == '-':
            return FloatLiteral(location=loc, value=-operand.value)

        if isinstance(operand, BoolLiteral) and expr.operator in ('not', '!'):
            return BoolLiteral(location=loc, value=not operand.value)

        if isinstance(operand, IntegerLiteral) and expr.operator == '~':
            return IntegerLiteral(location=loc, value=~operand.value)

        return None
