"""
Grey Compiler — Main Entry Point.

Usage:
    python -m grey <command> [options] [file]

Commands:
    run <file.grey>       Compile and execute a Grey source file
    compile <file.grey>   Compile to bytecode (.greyc)
    build <file.grey>     Compile to native executable via C
    emit-asm <file.grey>  Emit x86-64 assembly (.asm)
    emit-c <file.grey>    Emit C source code (.c)
    disasm <file.greyc>   Disassemble a compiled bytecode file
    repl                  Start the interactive REPL
    fmt <file.grey>       Format a Grey source file
    lint <file.grey>      Lint a Grey source file
    version               Show version information

Options:
    -O<level>             Optimization level (0-3, default 1)
    -o <output>           Output file path
    --trace               Enable VM trace output
    --ast                 Print AST
    --ir                  Print IR
    --dis                 Print disassembly
    --json                Emit JSON instead of binary
"""

import sys
import os
import argparse

# Ensure the project root is on the path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from grey import __version__
from grey.frontend.lexer import Lexer
from grey.frontend.parser import Parser
from grey.frontend.semantic import SemanticAnalyzer
from grey.frontend.normalizer import ASTNormalizer
from grey.frontend.ast_nodes import ASTPrinter
from grey.middle.ir_generator import IRGenerator
from grey.middle.optimizer import Optimizer
from grey.backend.codegen import CodeGenerator
from grey.backend.emitter import BytecodeEmitter
from grey.runtime.vm import GreyVM, VMError


def compile_to_ir(source: str, filename: str = "<input>", opt_level: int = 1,
                  show_ast: bool = False, show_ir: bool = False):
    """
    Run the compilation pipeline up to optimized IR.
    Returns the IRModule or None on error.
    """
    # ── Lexing ──
    lexer = Lexer(source, filename)
    tokens = lexer.scan_tokens()
    if lexer.errors:
        for err in lexer.errors:
            print(f"  Lex error: {err}", file=sys.stderr)
        return None

    # ── Parsing ──
    parser = Parser(tokens)
    program = parser.parse()
    if parser.errors:
        for err in parser.errors:
            print(f"  Parse error: {err}", file=sys.stderr)
        return None

    if show_ast:
        printer = ASTPrinter()
        print("\n─── AST ─────────────────────────────────")
        print(printer.print(program))
        print("─────────────────────────────────────────\n")

    # ── Semantic Analysis ──
    analyzer = SemanticAnalyzer()
    analyzer.analyze(program)
    if analyzer.errors:
        for err in analyzer.errors:
            print(f"  Semantic error: {err}", file=sys.stderr)
        return None

    # ── Normalization ──
    normalizer = ASTNormalizer()
    program = normalizer.normalize(program)

    # ── IR Generation ──
    ir_gen = IRGenerator()
    ir_module = ir_gen.generate(program)

    if show_ir:
        print("\n─── IR ──────────────────────────────────")
        print(ir_module)
        print("─────────────────────────────────────────\n")

    # ── Optimization ──
    optimizer = Optimizer(level=opt_level)
    ir_module = optimizer.optimize(ir_module)

    return ir_module


def compile_source(source: str, filename: str = "<input>", opt_level: int = 1,
                   show_ast: bool = False, show_ir: bool = False):
    """
    Run the full compilation pipeline (to VM bytecode).
    Returns the CompiledProgram or None on error.
    """
    ir_module = compile_to_ir(source, filename, opt_level, show_ast, show_ir)
    if ir_module is None:
        return None

    # ── Code Generation (VM bytecode) ──
    codegen = CodeGenerator()
    compiled = codegen.generate(ir_module)

    return compiled


def cmd_run(args):
    """Compile and execute a Grey source file."""
    with open(args.file, "r") as f:
        source = f.read()

    compiled = compile_source(
        source, args.file,
        opt_level=args.opt_level,
        show_ast=args.ast,
        show_ir=args.ir,
    )
    if compiled is None:
        sys.exit(1)

    if args.dis:
        emitter = BytecodeEmitter(compiled)
        print(emitter.disassemble())
        print()

    # Execute
    vm = GreyVM(trace=args.trace)
    vm.load(compiled)
    try:
        result = vm.run()
    except VMError as e:
        print(f"\nRuntime error: {e}", file=sys.stderr)
        print(vm.stack_trace(), file=sys.stderr)
        sys.exit(1)
    except SystemExit as e:
        sys.exit(e.code)


def cmd_compile(args):
    """Compile a Grey source file to bytecode."""
    with open(args.file, "r") as f:
        source = f.read()

    compiled = compile_source(
        source, args.file,
        opt_level=args.opt_level,
        show_ast=args.ast,
        show_ir=args.ir,
    )
    if compiled is None:
        sys.exit(1)

    emitter = BytecodeEmitter(compiled)

    # Determine output file
    output = args.output
    if not output:
        base = os.path.splitext(args.file)[0]
        output = base + ".greyc"

    if args.json:
        json_path = output.replace(".greyc", ".json")
        emitter.emit_json(json_path)
        print(f"Compiled to {json_path}")
    else:
        emitter.emit_binary(output)
        print(f"Compiled to {output}")

    if args.dis:
        emitter.emit_disassembly(output.replace(".greyc", ".greyd"))
        print(f"Disassembly written to {output.replace('.greyc', '.greyd')}")


def cmd_disasm(args):
    """Disassemble a compiled bytecode file."""
    compiled = BytecodeEmitter.load_binary(args.file)
    emitter = BytecodeEmitter(compiled)
    print(emitter.disassemble())


def cmd_repl(args):
    """Start the interactive REPL."""
    from grey.tools.repl import GreyREPL
    repl = GreyREPL()
    repl.start()


def cmd_fmt(args):
    """Format a Grey source file."""
    from grey.tools.formatter import GreyFormatter
    formatter = GreyFormatter()

    if args.check:
        with open(args.file, "r") as f:
            original = f.read()
        formatted = formatter.format(original)
        if original != formatted:
            print(f"{args.file}: needs formatting")
            sys.exit(1)
        else:
            print(f"{args.file}: OK")
    else:
        formatted = formatter.format_file(args.file)
        if args.output:
            with open(args.output, "w") as f:
                f.write(formatted)
        elif args.inplace:
            formatter.format_file_in_place(args.file)
            print(f"Formatted {args.file}")
        else:
            print(formatted)


def cmd_lint(args):
    """Lint a Grey source file."""
    from grey.tools.linter import GreyLinter
    linter = GreyLinter()
    issues = linter.lint_file(args.file)

    if not issues:
        print(f"{args.file}: no issues found")
    else:
        for issue in issues:
            print(f"  {issue}")
        print(f"\n{len(issues)} issue(s) found")
        sys.exit(1)


def cmd_version(args):
    """Show version information."""
    print(f"Grey Compiler v{__version__}")
    print("A modern compiled language with a register-based VM")


def cmd_emit_asm(args):
    """Emit x86-64 assembly for a Grey source file."""
    from grey.backend.x86_64_codegen import X86_64CodeGenerator

    with open(args.file, "r") as f:
        source = f.read()

    ir_module = compile_to_ir(
        source, args.file,
        opt_level=args.opt_level,
        show_ast=args.ast,
        show_ir=args.ir,
    )
    if ir_module is None:
        sys.exit(1)

    gen = X86_64CodeGenerator()
    asm_text = gen.generate(ir_module)

    output = args.output
    if not output:
        output = os.path.splitext(args.file)[0] + ".asm"

    gen.write(output, asm_text)
    print(f"Assembly written to {output}")
    print(f"  Assemble: nasm -f {'win64' if gen.windows else 'elf64'} {output} -o {os.path.splitext(output)[0]}.o")
    print(f"  Link:     gcc {os.path.splitext(output)[0]}.o -o {os.path.splitext(output)[0]}{'.exe' if gen.windows else ''} -lm")


def cmd_emit_c(args):
    """Emit C source code for a Grey source file."""
    from grey.backend.c_codegen import CCodeGenerator

    with open(args.file, "r") as f:
        source = f.read()

    ir_module = compile_to_ir(
        source, args.file,
        opt_level=args.opt_level,
        show_ast=args.ast,
        show_ir=args.ir,
    )
    if ir_module is None:
        sys.exit(1)

    gen = CCodeGenerator()
    c_code = gen.generate(ir_module)

    output = args.output
    if not output:
        output = os.path.splitext(args.file)[0] + ".c"

    gen.write(output, c_code)
    print(f"C source written to {output}")
    print(f"  Compile: gcc -O2 {output} -o {os.path.splitext(output)[0]}{'.exe' if os.name == 'nt' else ''} -lm")


def cmd_build(args):
    """Compile a Grey source file to a native executable via C."""
    import subprocess
    import shutil
    from grey.backend.c_codegen import CCodeGenerator

    with open(args.file, "r") as f:
        source = f.read()

    ir_module = compile_to_ir(
        source, args.file,
        opt_level=args.opt_level,
        show_ast=args.ast,
        show_ir=args.ir,
    )
    if ir_module is None:
        sys.exit(1)

    gen = CCodeGenerator()
    c_code = gen.generate(ir_module)

    base = os.path.splitext(args.file)[0]
    c_file = base + ".c"
    gen.write(c_file, c_code)
    print(f"Generated C source: {c_file}")

    # Determine output executable path
    exe_ext = ".exe" if os.name == "nt" else ""
    output_exe = args.output if args.output else base + exe_ext

    # Find a C compiler
    cc = None
    for candidate in ["gcc", "cc", "clang", "cl"]:
        if shutil.which(candidate):
            cc = candidate
            break

    if cc is None:
        print(f"C source is at {c_file}")
        print("No C compiler found. Install GCC, Clang, or MSVC and run:")
        print(f"  gcc -O2 {c_file} -o {output_exe} -lm")
        sys.exit(1)

    # Build
    opt_flag = {0: "-O0", 1: "-O1", 2: "-O2", 3: "-O3"}.get(args.opt_level, "-O1")
    if cc == "cl":
        cmd = [cc, "/Fe:" + output_exe, opt_flag.replace("-O", "/O"), c_file]
    else:
        cmd = [cc, opt_flag, c_file, "-o", output_exe, "-lm"]

    print(f"Compiling: {' '.join(cmd)}")
    result = subprocess.run(cmd, capture_output=True, text=True)

    if result.returncode != 0:
        print("Compilation failed:", file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        sys.exit(1)

    print(f"Native executable: {output_exe}")

    # Clean up C file unless user wants to keep it
    if not args.keep_c:
        os.remove(c_file)
        print(f"(Removed intermediate {c_file})")

    # Run if requested
    if args.run:
        print(f"\n── Running {output_exe} ──")
        subprocess.run([output_exe])


def main():
    parser = argparse.ArgumentParser(
        prog="grey",
        description="Grey Compiler — a modern programming language",
    )
    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # run
    p_run = subparsers.add_parser("run", help="Compile and execute a Grey file")
    p_run.add_argument("file", help="Grey source file (.grey)")
    p_run.add_argument("-O", "--opt-level", type=int, default=1,
                       choices=[0, 1, 2, 3], help="Optimization level")
    p_run.add_argument("--trace", action="store_true", help="Trace execution")
    p_run.add_argument("--ast", action="store_true", help="Print AST")
    p_run.add_argument("--ir", action="store_true", help="Print IR")
    p_run.add_argument("--dis", action="store_true", help="Print disassembly")
    p_run.set_defaults(func=cmd_run)

    # compile
    p_compile = subparsers.add_parser("compile", help="Compile to bytecode")
    p_compile.add_argument("file", help="Grey source file (.grey)")
    p_compile.add_argument("-o", "--output", help="Output file")
    p_compile.add_argument("-O", "--opt-level", type=int, default=1,
                           choices=[0, 1, 2, 3], help="Optimization level")
    p_compile.add_argument("--json", action="store_true", help="Emit JSON")
    p_compile.add_argument("--ast", action="store_true", help="Print AST")
    p_compile.add_argument("--ir", action="store_true", help="Print IR")
    p_compile.add_argument("--dis", action="store_true", help="Print disassembly")
    p_compile.set_defaults(func=cmd_compile)

    # disasm
    p_disasm = subparsers.add_parser("disasm", help="Disassemble bytecode")
    p_disasm.add_argument("file", help="Compiled bytecode file (.greyc)")
    p_disasm.set_defaults(func=cmd_disasm)

    # repl
    p_repl = subparsers.add_parser("repl", help="Interactive REPL")
    p_repl.set_defaults(func=cmd_repl)

    # fmt
    p_fmt = subparsers.add_parser("fmt", help="Format Grey source code")
    p_fmt.add_argument("file", help="Grey source file")
    p_fmt.add_argument("-o", "--output", help="Output file")
    p_fmt.add_argument("-i", "--inplace", action="store_true",
                       help="Format file in place")
    p_fmt.add_argument("--check", action="store_true",
                       help="Check formatting without modifying")
    p_fmt.set_defaults(func=cmd_fmt)

    # lint
    p_lint = subparsers.add_parser("lint", help="Lint Grey source code")
    p_lint.add_argument("file", help="Grey source file")
    p_lint.set_defaults(func=cmd_lint)

    # version
    p_ver = subparsers.add_parser("version", help="Show version")
    p_ver.set_defaults(func=cmd_version)

    # emit-asm
    p_asm = subparsers.add_parser("emit-asm", help="Emit x86-64 assembly")
    p_asm.add_argument("file", help="Grey source file (.grey)")
    p_asm.add_argument("-o", "--output", help="Output file")
    p_asm.add_argument("-O", "--opt-level", type=int, default=1,
                       choices=[0, 1, 2, 3], help="Optimization level")
    p_asm.add_argument("--ast", action="store_true", help="Print AST")
    p_asm.add_argument("--ir", action="store_true", help="Print IR")
    p_asm.set_defaults(func=cmd_emit_asm)

    # emit-c
    p_c = subparsers.add_parser("emit-c", help="Emit C source code")
    p_c.add_argument("file", help="Grey source file (.grey)")
    p_c.add_argument("-o", "--output", help="Output file")
    p_c.add_argument("-O", "--opt-level", type=int, default=1,
                     choices=[0, 1, 2, 3], help="Optimization level")
    p_c.add_argument("--ast", action="store_true", help="Print AST")
    p_c.add_argument("--ir", action="store_true", help="Print IR")
    p_c.set_defaults(func=cmd_emit_c)

    # build (native)
    p_build = subparsers.add_parser("build", help="Compile to native executable")
    p_build.add_argument("file", help="Grey source file (.grey)")
    p_build.add_argument("-o", "--output", help="Output executable")
    p_build.add_argument("-O", "--opt-level", type=int, default=1,
                         choices=[0, 1, 2, 3], help="Optimization level")
    p_build.add_argument("--ast", action="store_true", help="Print AST")
    p_build.add_argument("--ir", action="store_true", help="Print IR")
    p_build.add_argument("--keep-c", action="store_true",
                         help="Keep generated C file")
    p_build.add_argument("--run", action="store_true",
                         help="Run the executable after building")
    p_build.set_defaults(func=cmd_build)

    args = parser.parse_args()

    if args.command is None:
        parser.print_help()
        sys.exit(0)

    args.func(args)


if __name__ == "__main__":
    main()
