"""
Tests for the Grey Lexer.
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from grey.frontend.lexer import Lexer
from grey.frontend.tokens import TokenType


def test_basic_tokens():
    """Test lexing of basic tokens."""
    source = "let x = 42;"
    lexer = Lexer(source)
    tokens = lexer.scan_tokens()

    types = [t.type for t in tokens]
    assert TokenType.LET in types, f"Expected LET token, got {types}"
    assert TokenType.IDENTIFIER in types, f"Expected IDENTIFIER token"
    assert TokenType.ASSIGN in types, f"Expected ASSIGN token, got {types}"
    assert TokenType.INTEGER in types, f"Expected INTEGER token"
    assert TokenType.SEMICOLON in types, f"Expected SEMICOLON token"
    print("  PASS: test_basic_tokens")


def test_keywords():
    """Test all keyword recognition."""
    keywords = [
        "let", "mut", "const", "fn", "return", "if", "else", "elif",
        "while", "for", "in", "break", "continue", "true", "false",
        "nil", "struct", "enum", "impl", "trait", "match", "import",
        "from", "as", "type", "and", "or", "not",
    ]
    for kw in keywords:
        lexer = Lexer(kw)
        tokens = lexer.scan_tokens()
        assert tokens[0].type != TokenType.IDENTIFIER, \
            f"Keyword '{kw}' was lexed as IDENTIFIER instead of keyword"
    print("  PASS: test_keywords")


def test_numbers():
    """Test number literal lexing."""
    cases = [
        ("42", TokenType.INTEGER),
        ("3.14", TokenType.FLOAT),
        ("0xFF", TokenType.INTEGER),
        ("0b1010", TokenType.INTEGER),
        ("0o77", TokenType.INTEGER),
        ("1_000_000", TokenType.INTEGER),
    ]
    for source, expected_type in cases:
        lexer = Lexer(source)
        tokens = lexer.scan_tokens()
        assert tokens[0].type == expected_type, \
            f"'{source}' was lexed as {tokens[0].type}, expected {expected_type}"
    print("  PASS: test_numbers")


def test_strings():
    """Test string literal lexing."""
    source = '"hello world"'
    lexer = Lexer(source)
    tokens = lexer.scan_tokens()
    assert tokens[0].type == TokenType.STRING
    assert tokens[0].value == "hello world"

    # Escape sequences
    source2 = r'"hello\nworld"'
    lexer2 = Lexer(source2)
    tokens2 = lexer2.scan_tokens()
    assert tokens2[0].type == TokenType.STRING
    print("  PASS: test_strings")


def test_operators():
    """Test operator lexing."""
    source = "+-*/% == != <= >= && || >> << -> =>"
    lexer = Lexer(source)
    tokens = lexer.scan_tokens()
    # Should not have any errors
    assert not lexer.errors, f"Unexpected lexer errors: {lexer.errors}"
    print("  PASS: test_operators")


def test_comments():
    """Test comment handling."""
    source = """
    // single line comment
    let x = 1;
    /* multi
       line
       comment */
    let y = 2;
    """
    lexer = Lexer(source)
    tokens = lexer.scan_tokens()
    # Comments should be skipped
    idents = [t for t in tokens if t.type == TokenType.IDENTIFIER]
    assert len(idents) == 2, f"Expected 2 identifiers, got {len(idents)}"
    print("  PASS: test_comments")


def test_complex_expression():
    """Test lexing a complex expression."""
    source = "fn fibonacci(n: int) -> int { if n <= 1 { return n; } }"
    lexer = Lexer(source)
    tokens = lexer.scan_tokens()
    assert not lexer.errors, f"Unexpected errors: {lexer.errors}"
    assert len(tokens) > 10, f"Expected many tokens, got {len(tokens)}"
    print("  PASS: test_complex_expression")


def run_all():
    print("Running Lexer Tests...")
    test_basic_tokens()
    test_keywords()
    test_numbers()
    test_strings()
    test_operators()
    test_comments()
    test_complex_expression()
    print("All Lexer tests passed!\n")


if __name__ == "__main__":
    run_all()
