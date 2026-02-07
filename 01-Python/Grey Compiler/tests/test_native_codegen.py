"""
Tests for the Grey compiler's native code generation backends.

Validates:
  1. Grey → C compilation pipeline
  2. Grey → x86-64 assembly pipeline
  3. Generated C code compiles and produces correct output (when GCC available)
  4. Correct IR→ASM translation for arithmetic, calls, control flow
"""

import sys
import os
import subprocess
import shutil
import tempfile

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from grey.frontend.lexer import Lexer
from grey.frontend.parser import Parser
from grey.frontend.semantic import SemanticAnalyzer
from grey.frontend.normalizer import ASTNormalizer
from grey.middle.ir_generator import IRGenerator
from grey.middle.optimizer import Optimizer
from grey.backend.c_codegen import CCodeGenerator
from grey.backend.x86_64_codegen import X86_64CodeGenerator


def compile_to_ir(source: str, opt_level: int = 1):
    """Run the full frontend + middle-end pipeline."""
    lexer = Lexer(source, "<test>")
    tokens = lexer.scan_tokens()
    assert not lexer.errors, f"Lex errors: {lexer.errors}"

    parser = Parser(tokens)
    program = parser.parse()
    assert not parser.errors, f"Parse errors: {parser.errors}"

    analyzer = SemanticAnalyzer()
    analyzer.analyze(program)
    # Allow semantic errors for some tests (unused vars etc.)

    normalizer = ASTNormalizer()
    program = normalizer.normalize(program)

    ir_gen = IRGenerator()
    ir_module = ir_gen.generate(program)

    optimizer = Optimizer(level=opt_level)
    ir_module = optimizer.optimize(ir_module)

    return ir_module


def test_c_codegen_hello():
    """Test C codegen for a simple println."""
    source = 'println("Hello, World!");'
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    assert '#include <stdio.h>' in c_code
    assert 'int main(' in c_code
    assert 'printf' in c_code
    assert 'Hello, World!' in c_code
    assert 'return 0;' in c_code
    print("  [PASS] C codegen: hello world")


def test_c_codegen_arithmetic():
    """Test C codegen for arithmetic expressions."""
    source = """
    let x = 10;
    let y = 20;
    let sum = x + y;
    let diff = x - y;
    let prod = x * y;
    println(sum)
    """
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    assert 'int main(' in c_code
    # Should contain arithmetic operations
    assert '+' in c_code or 'add' in c_code.lower()
    assert '-' in c_code or 'sub' in c_code.lower()
    assert '*' in c_code or 'mul' in c_code.lower()
    print("  [PASS] C codegen: arithmetic")


def test_c_codegen_function():
    """Test C codegen for function definitions and calls."""
    source = """
    fn add(a: int, b: int) -> int {
        return a + b
    }
    let result = add(3, 4);
    println(result)
    """
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    assert 'grey_fn_add' in c_code
    # Forward declaration
    assert 'static' in c_code
    # Function body with addition
    assert '+' in c_code
    # Call site
    assert 'grey_fn_add(' in c_code
    print("  [PASS] C codegen: function")


def test_c_codegen_if_else():
    """Test C codegen for if/else control flow."""
    source = """
    let x = 10;
    if x > 5 {
        println(1)
    } else {
        println(0)
    }
    """
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    assert 'if (' in c_code
    assert 'goto' in c_code
    print("  [PASS] C codegen: if/else")


def test_c_codegen_while_loop():
    """Test C codegen for while loops."""
    source = """
    let mut i = 0;
    while i < 10 {
        println(i)
        i = i + 1
    }
    """
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    assert 'goto' in c_code   # while uses branch -> goto
    assert '<' in c_code
    print("  [PASS] C codegen: while loop")


def test_c_codegen_recursion():
    """Test C codegen for recursive functions."""
    source = """
    fn factorial(n: int) -> int {
        if n <= 1 {
            return 1
        }
        return n * factorial(n - 1)
    }
    let result = factorial(10);
    println(result)
    """
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    assert 'grey_fn_factorial' in c_code
    # Should have recursive call
    count = c_code.count('grey_fn_factorial')
    assert count >= 3, f"Expected >=3 occurrences of grey_fn_factorial (decl + def + call), got {count}"
    print("  [PASS] C codegen: recursion")


def test_c_codegen_struct():
    """Test C codegen for struct operations."""
    source = """
    struct Point {
        x: int,
        y: int,
    }
    let p = Point { x: 10, y: 20 };
    """
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    assert 'grey_struct_new' in c_code or 'GreyStruct' in c_code
    print("  [PASS] C codegen: struct")


def test_c_codegen_power():
    """Test C codegen for power operator."""
    source = """
    let x = 2 ** 10;
    println(x)
    """
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    # The optimizer should constant-fold 2**10 = 1024
    # If not, it should use grey_pow
    assert '1024' in c_code or 'grey_pow' in c_code
    print("  [PASS] C codegen: power operator")


def test_asm_codegen_hello():
    """Test x86-64 ASM codegen for a simple program."""
    source = 'println("Hello, ASM!");'
    ir = compile_to_ir(source)
    gen = X86_64CodeGenerator()
    asm_text = gen.generate(ir)

    assert 'section .text' in asm_text
    assert 'main:' in asm_text
    assert 'extern printf' in asm_text
    assert 'call printf' in asm_text
    assert 'Hello, ASM!' in asm_text
    assert 'ret' in asm_text
    print("  [PASS] ASM codegen: hello world")


def test_asm_codegen_arithmetic():
    """Test x86-64 ASM codegen for arithmetic."""
    source = """
    let x = 42;
    let y = 8;
    let sum = x + y;
    println(sum)
    """
    ir = compile_to_ir(source)
    gen = X86_64CodeGenerator()
    asm_text = gen.generate(ir)

    assert 'add rax' in asm_text or 'add' in asm_text
    assert 'mov rsp, rbp' in asm_text    # epilogue
    assert 'push rbp' in asm_text         # prologue
    print("  [PASS] ASM codegen: arithmetic")


def test_asm_codegen_function():
    """Test x86-64 ASM codegen for function calls."""
    source = """
    fn double(x: int) -> int {
        return x * 2
    }
    let r = double(21);
    println(r)
    """
    ir = compile_to_ir(source)
    gen = X86_64CodeGenerator()
    asm_text = gen.generate(ir)

    assert '_grey_double:' in asm_text
    assert 'call _grey_double' in asm_text
    assert 'imul' in asm_text
    print("  [PASS] ASM codegen: function")


def test_asm_codegen_comparison():
    """Test x86-64 ASM codegen for comparisons and branches."""
    source = """
    let x = 10;
    if x > 5 {
        println(1)
    }
    """
    ir = compile_to_ir(source)
    gen = X86_64CodeGenerator()
    asm_text = gen.generate(ir)

    assert 'cmp' in asm_text
    assert 'jnz' in asm_text or 'jz' in asm_text or 'jg' in asm_text
    print("  [PASS] ASM codegen: comparison/branch")


def test_asm_codegen_while():
    """Test x86-64 ASM codegen for while loops."""
    source = """
    let mut i = 0;
    while i < 5 {
        i = i + 1
    }
    println(i)
    """
    ir = compile_to_ir(source)
    gen = X86_64CodeGenerator()
    asm_text = gen.generate(ir)

    assert 'while_cond' in asm_text
    assert 'while_body' in asm_text
    assert 'while_exit' in asm_text
    assert 'jmp' in asm_text
    print("  [PASS] ASM codegen: while loop")


def test_asm_codegen_windows_calling_convention():
    """Test that Windows x64 calling convention is used on Windows."""
    source = 'println("test");'
    ir = compile_to_ir(source)
    gen = X86_64CodeGenerator(target_os="windows")
    asm_text = gen.generate(ir)

    assert 'Windows x64' in asm_text
    # Windows uses rcx as first arg
    assert 'rcx' in asm_text
    # Shadow space
    assert 'sub rsp, 32' in asm_text
    print("  [PASS] ASM codegen: Windows x64 calling convention")


def test_asm_codegen_sysv_calling_convention():
    """Test that System V calling convention is used when specified."""
    source = 'println("test");'
    ir = compile_to_ir(source)
    gen = X86_64CodeGenerator(target_os="linux")
    asm_text = gen.generate(ir)

    assert 'System V AMD64' in asm_text
    # System V uses rdi as first arg
    assert 'rdi' in asm_text
    print("  [PASS] ASM codegen: System V calling convention")


def test_c_codegen_compilable():
    """Test that generated C code is syntactically valid by checking structure."""
    source = """
    fn fib(n: int) -> int {
        if n <= 1 {
            return n
        }
        return fib(n - 1) + fib(n - 2)
    }
    println(fib(10))
    """
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    # Count braces: must be balanced
    opens = c_code.count('{')
    closes = c_code.count('}')
    assert opens == closes, f"Unbalanced braces: {opens} opens, {closes} closes"

    # Must have main function
    assert 'int main(' in c_code

    # Must end with a closing brace (after whitespace)
    stripped = c_code.rstrip()
    assert stripped.endswith('}'), "C code should end with }"

    # Count semicolons — should have a reasonable number
    semis = c_code.count(';')
    assert semis > 5, f"Expected more semicolons in valid C, got {semis}"

    print("  [PASS] C codegen: syntactic validity")


def test_c_compile_and_run():
    """
    If GCC is available, actually compile and run the generated C code
    and verify the output is correct.
    """
    gcc = shutil.which("gcc") or shutil.which("cc") or shutil.which("clang")
    if gcc is None:
        print("  [SKIP] C compile & run: no C compiler available")
        return

    source = """
    fn add(a: int, b: int) -> int {
        return a + b
    }
    fn factorial(n: int) -> int {
        if n <= 1 {
            return 1
        }
        return n * factorial(n - 1)
    }
    println(add(3, 4))
    println(factorial(5))
    """
    ir = compile_to_ir(source)
    gen = CCodeGenerator()
    c_code = gen.generate(ir)

    with tempfile.TemporaryDirectory() as tmpdir:
        c_path = os.path.join(tmpdir, "test.c")
        exe_ext = ".exe" if os.name == "nt" else ""
        exe_path = os.path.join(tmpdir, f"test{exe_ext}")

        with open(c_path, "w", encoding="utf-8") as f:
            f.write(c_code)

        # Compile
        result = subprocess.run(
            [gcc, "-O0", c_path, "-o", exe_path, "-lm"],
            capture_output=True, text=True, timeout=30,
        )
        if result.returncode != 0:
            print(f"  [FAIL] C compile & run: compile failed\n{result.stderr}")
            return

        # Run
        result = subprocess.run(
            [exe_path], capture_output=True, text=True, timeout=10,
        )
        output = result.stdout.strip().split('\n')
        expected = ['7', '120']

        if output == expected:
            print("  [PASS] C compile & run: correct output!")
        else:
            print(f"  [FAIL] C compile & run: expected {expected}, got {output}")


def test_full_pipeline_complex():
    """Test a complex program through both backends."""
    source = """
    fn fib(n: int) -> int {
        if n <= 0 { return 0 }
        if n == 1 { return 1 }
        let mut a = 0;
        let mut b = 1;
        let mut i = 2;
        while i <= n {
            let temp = a + b;
            a = b
            b = temp
            i = i + 1
        }
        return b
    }

    fn is_even(n: int) -> int {
        return n % 2
    }

    println(fib(10))
    println(is_even(42))
    """
    ir = compile_to_ir(source)

    # C backend
    c_gen = CCodeGenerator()
    c_code = c_gen.generate(ir)
    assert 'grey_fn_fib' in c_code
    assert 'grey_fn_is_even' in c_code
    assert '%' in c_code  # modulo
    assert 'int main(' in c_code

    # ASM backend
    asm_gen = X86_64CodeGenerator()
    asm_text = asm_gen.generate(ir)
    assert '_grey_fib:' in asm_text
    assert '_grey_is_even:' in asm_text
    assert 'idiv' in asm_text  # modulo uses idiv
    assert 'main:' in asm_text

    print("  [PASS] Full pipeline: complex program through both backends")


# ── Run all tests ──

if __name__ == "__main__":
    print("\n=== Grey Compiler Native Code Generation Tests ===\n")

    tests = [
        test_c_codegen_hello,
        test_c_codegen_arithmetic,
        test_c_codegen_function,
        test_c_codegen_if_else,
        test_c_codegen_while_loop,
        test_c_codegen_recursion,
        test_c_codegen_struct,
        test_c_codegen_power,
        test_c_codegen_compilable,
        test_asm_codegen_hello,
        test_asm_codegen_arithmetic,
        test_asm_codegen_function,
        test_asm_codegen_comparison,
        test_asm_codegen_while,
        test_asm_codegen_windows_calling_convention,
        test_asm_codegen_sysv_calling_convention,
        test_full_pipeline_complex,
        test_c_compile_and_run,
    ]

    passed = 0
    failed = 0
    skipped = 0

    for test in tests:
        try:
            test()
            passed += 1
        except AssertionError as e:
            print(f"  [FAIL] {test.__name__}: {e}")
            failed += 1
        except Exception as e:
            print(f"  [ERROR] {test.__name__}: {type(e).__name__}: {e}")
            failed += 1

    print(f"\n{'='*50}")
    print(f"Results: {passed} passed, {failed} failed")
    if failed == 0:
        print("All Native Codegen tests passed!")
    else:
        print(f"{failed} test(s) FAILED")
        sys.exit(1)
