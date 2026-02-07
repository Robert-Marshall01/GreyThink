"""
Tests for the Grey Parser.
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from grey.frontend.lexer import Lexer
from grey.frontend.parser import Parser
from grey.frontend.ast_nodes import (
    Program, FunctionDeclaration, VariableDeclaration,
    IfStatement, WhileStatement, ForStatement,
    ReturnStatement, BinaryExpression, CallExpression,
    Identifier, NumberLiteral, StringLiteral,
    StructDeclaration, EnumDeclaration, ImplBlock,
    ExpressionStatement, BlockStatement,
)


def parse(source: str) -> Program:
    lexer = Lexer(source)
    tokens = lexer.scan_tokens()
    parser = Parser(tokens)
    return parser.parse()


def test_variable_declaration():
    """Test parsing variable declarations."""
    prog = parse("let x = 42;")
    assert len(prog.declarations) == 1
    decl = prog.declarations[0]
    assert isinstance(decl, VariableDeclaration)
    assert decl.name == "x"
    print("  PASS: test_variable_declaration")


def test_function_declaration():
    """Test parsing function declarations."""
    prog = parse("fn add(a: int, b: int) -> int { return a + b; }")
    assert len(prog.declarations) == 1
    decl = prog.declarations[0]
    assert isinstance(decl, FunctionDeclaration)
    assert decl.name == "add"
    assert len(decl.params) == 2
    print("  PASS: test_function_declaration")


def test_if_statement():
    """Test parsing if/elif/else."""
    prog = parse("""
        if x > 0 {
            let a = 1;
        } elif x == 0 {
            let b = 0;
        } else {
            let c = -1;
        }
    """)
    assert len(prog.statements) >= 1 or len(prog.declarations) >= 1
    print("  PASS: test_if_statement")


def test_while_loop():
    """Test parsing while loops."""
    prog = parse("while x > 0 { x = x - 1; }")
    assert len(prog.statements) >= 1
    stmt = prog.statements[0]
    assert isinstance(stmt, WhileStatement)
    print("  PASS: test_while_loop")


def test_for_loop():
    """Test parsing for..in loops."""
    prog = parse("for i in 0..10 { println(i); }")
    assert len(prog.statements) >= 1
    stmt = prog.statements[0]
    assert isinstance(stmt, ForStatement)
    print("  PASS: test_for_loop")


def test_struct_declaration():
    """Test parsing struct declarations."""
    prog = parse("""
        struct Point {
            x: float,
            y: float,
        }
    """)
    assert len(prog.declarations) == 1
    decl = prog.declarations[0]
    assert isinstance(decl, StructDeclaration)
    assert decl.name == "Point"
    print("  PASS: test_struct_declaration")


def test_expressions():
    """Test parsing various expressions."""
    cases = [
        "1 + 2 * 3;",
        "a and b or c;",
        "arr[0];",
        "obj.field;",
        'fn_call(1, "hello", true);',
        "[1, 2, 3];",
        "x = 42;",
    ]
    for source in cases:
        prog = parse(source)
        total = len(prog.declarations) + len(prog.statements)
        assert total >= 1, f"Failed to parse: {source}"
    print("  PASS: test_expressions")


def test_nested_functions():
    """Test parsing nested function calls."""
    prog = parse("println(add(1, mul(2, 3)));")
    total = len(prog.declarations) + len(prog.statements)
    assert total == 1
    print("  PASS: test_nested_functions")


def test_match_statement():
    """Test parsing match statements."""
    prog = parse("""
        match x {
            1 => println("one"),
            2 => println("two"),
            _ => println("other"),
        }
    """)
    total = len(prog.declarations) + len(prog.statements)
    assert total >= 1
    print("  PASS: test_match_statement")


def test_error_recovery():
    """Test parser error recovery."""
    prog = parse("let x = ; let y = 42;")
    # Should parse y despite error in x
    total = len(prog.declarations) + len(prog.statements)
    assert total >= 1
    print("  PASS: test_error_recovery")


def run_all():
    print("Running Parser Tests...")
    test_variable_declaration()
    test_function_declaration()
    test_if_statement()
    test_while_loop()
    test_for_loop()
    test_struct_declaration()
    test_expressions()
    test_nested_functions()
    test_match_statement()
    test_error_recovery()
    print("All Parser tests passed!\n")


if __name__ == "__main__":
    run_all()
