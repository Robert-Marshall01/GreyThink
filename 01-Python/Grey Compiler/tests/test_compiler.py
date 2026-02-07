"""
End-to-end tests for the Grey compiler pipeline.

Tests the full pipeline: Source → Lex → Parse → Semantic → Normalize → IR → Optimize → Codegen
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from grey.frontend.lexer import Lexer
from grey.frontend.parser import Parser
from grey.frontend.semantic import SemanticAnalyzer
from grey.frontend.normalizer import ASTNormalizer
from grey.middle.ir_generator import IRGenerator
from grey.middle.optimizer import Optimizer
from grey.backend.codegen import CodeGenerator
from grey.backend.emitter import BytecodeEmitter


def compile_source(source: str, optimize_level: int = 1):
    """Run the full compilation pipeline on source code."""
    # Frontend
    lexer = Lexer(source, "<test>")
    tokens = lexer.scan_tokens()
    assert not lexer.errors, f"Lex errors: {lexer.errors}"

    parser = Parser(tokens)
    program = parser.parse()
    assert not parser.errors, f"Parse errors: {parser.errors}"

    analyzer = SemanticAnalyzer()
    analyzer.analyze(program)
    # Allow warnings but no hard errors preventing compilation

    normalizer = ASTNormalizer()
    program = normalizer.normalize(program)

    # Middle end
    ir_gen = IRGenerator()
    ir_module = ir_gen.generate(program)

    optimizer = Optimizer(level=optimize_level)
    ir_module = optimizer.optimize(ir_module)

    # Backend
    codegen = CodeGenerator()
    compiled = codegen.generate(ir_module)

    return compiled


def test_compile_hello():
    """Test compiling a hello world program."""
    source = 'println("Hello, World!");'
    compiled = compile_source(source)
    assert len(compiled.chunks) > 0, "Expected at least one chunk"
    print("  PASS: test_compile_hello")


def test_compile_arithmetic():
    """Test compiling arithmetic expressions."""
    source = "let x = 2 + 3 * 4;"
    compiled = compile_source(source)
    assert len(compiled.chunks) > 0
    print("  PASS: test_compile_arithmetic")


def test_compile_function():
    """Test compiling a function."""
    source = """
    fn add(a: int, b: int) -> int {
        return a + b;
    }
    let result = add(3, 4);
    """
    compiled = compile_source(source)
    assert len(compiled.chunks) >= 1
    print("  PASS: test_compile_function")


def test_compile_if_else():
    """Test compiling if/else."""
    source = """
    let x = 10;
    if x > 5 {
        println("big");
    } else {
        println("small");
    }
    """
    compiled = compile_source(source)
    assert len(compiled.chunks) > 0
    print("  PASS: test_compile_if_else")


def test_compile_while_loop():
    """Test compiling while loop."""
    source = """
    let x = 0;
    while x < 10 {
        x = x + 1;
    }
    """
    compiled = compile_source(source)
    assert len(compiled.chunks) > 0
    print("  PASS: test_compile_while_loop")


def test_compile_struct():
    """Test compiling struct usage."""
    source = """
    struct Point {
        x: int,
        y: int,
    }
    let p = Point { x: 10, y: 20 };
    """
    compiled = compile_source(source)
    assert len(compiled.chunks) > 0
    print("  PASS: test_compile_struct")


def test_compile_array():
    """Test compiling array usage."""
    source = """
    let arr = [1, 2, 3, 4, 5];
    let first = arr[0];
    """
    compiled = compile_source(source)
    assert len(compiled.chunks) > 0
    print("  PASS: test_compile_array")


def test_optimizer_levels():
    """Test that different optimization levels work."""
    source = """
    let x = 2 + 3;
    let y = x * 0;
    let z = y + 1;
    """
    for level in range(4):
        compiled = compile_source(source, optimize_level=level)
        assert compiled is not None, f"Optimization level {level} failed"
    print("  PASS: test_optimizer_levels")


def test_disassembly():
    """Test disassembly output."""
    source = 'let x = 42; println(x);'
    compiled = compile_source(source)
    emitter = BytecodeEmitter(compiled)
    disasm = emitter.disassemble()
    assert "Grey Compiler" in disasm
    assert len(disasm) > 100
    print("  PASS: test_disassembly")


def test_binary_roundtrip():
    """Test binary emit and load roundtrip."""
    import tempfile
    source = "let x = 42;"
    compiled = compile_source(source)
    emitter = BytecodeEmitter(compiled)

    with tempfile.NamedTemporaryFile(suffix=".greyc", delete=False) as tmp:
        tmp_path = tmp.name

    try:
        emitter.emit_binary(tmp_path)
        loaded = BytecodeEmitter.load_binary(tmp_path)
        assert len(loaded.chunks) == len(compiled.chunks)
        print("  PASS: test_binary_roundtrip")
    finally:
        os.unlink(tmp_path)


def test_compile_complex_program():
    """Test compiling a complex program."""
    source = """
    fn fibonacci(n: int) -> int {
        if n <= 1 {
            return n;
        }
        return fibonacci(n - 1) + fibonacci(n - 2);
    }

    let i = 0;
    while i < 10 {
        let fib = fibonacci(i);
        println(fib);
        i = i + 1;
    }
    """
    compiled = compile_source(source)
    assert len(compiled.chunks) >= 1, "Expected at least 1 chunk for fibonacci"
    print("  PASS: test_compile_complex_program")


def run_all():
    print("Running Compiler Pipeline Tests...")
    test_compile_hello()
    test_compile_arithmetic()
    test_compile_function()
    test_compile_if_else()
    test_compile_while_loop()
    test_compile_struct()
    test_compile_array()
    test_optimizer_levels()
    test_disassembly()
    test_binary_roundtrip()
    test_compile_complex_program()
    print("All Compiler Pipeline tests passed!\n")


if __name__ == "__main__":
    run_all()
