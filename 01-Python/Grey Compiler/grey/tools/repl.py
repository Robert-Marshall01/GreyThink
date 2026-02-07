"""
Grey Interactive REPL (Read-Eval-Print Loop).

Provides an interactive shell for writing and executing Grey code,
with features like:
  - Multi-line input support
  - Command history
  - Special REPL commands (:help, :quit, :clear, :ast, :ir, :bytecode)
  - Expression evaluation with automatic printing
  - Error recovery
"""

from __future__ import annotations
import sys
import traceback

from ..frontend.lexer import Lexer
from ..frontend.parser import Parser
from ..frontend.semantic import SemanticAnalyzer
from ..frontend.normalizer import ASTNormalizer
from ..frontend.ast_nodes import ASTPrinter, ExpressionStatement
from ..middle.ir_generator import IRGenerator
from ..middle.optimizer import Optimizer
from ..backend.codegen import CodeGenerator
from ..backend.emitter import BytecodeEmitter
from ..runtime.vm import GreyVM, VMError


BANNER = r"""
   ____
  / ___|_ __ ___ _   _
 | |  _| '__/ _ \ | | |
 | |_| | | |  __/ |_| |
  \____|_|  \___|\__, |
                 |___/
 Grey Language REPL v1.0
 Type :help for available commands.
"""

HELP_TEXT = """
REPL Commands:
  :help        Show this help message
  :quit, :q    Exit the REPL
  :clear       Clear the screen
  :ast <code>  Show the AST for an expression
  :ir <code>   Show the IR for code
  :dis <code>  Show the disassembly for code
  :reset       Reset the REPL state
  :env         Show current variables

Tips:
  - Expressions are automatically printed
  - Use \\ at end of line for multi-line input
  - Semicolons are optional for single expressions
"""


class GreyREPL:
    """Interactive REPL for the Grey language."""

    def __init__(self, *, show_banner: bool = True):
        self.show_banner = show_banner
        self.running = False
        self.history: list[str] = []
        self.vm = GreyVM()
        self.line_no = 0

    def start(self):
        """Start the REPL main loop."""
        if self.show_banner:
            print(BANNER)

        self.running = True

        while self.running:
            try:
                source = self._read_input()
                if source is None:
                    continue

                self.history.append(source)

                # Check for REPL commands
                if source.startswith(":"):
                    self._handle_command(source)
                    continue

                # Compile and execute
                self._eval(source)

            except KeyboardInterrupt:
                print("\n(Use :quit to exit)")

            except EOFError:
                print("\nBye!")
                self.running = False

    def _read_input(self) -> str | None:
        """Read input, supporting multi-line with backslash continuation."""
        try:
            line = input("grey> ")
        except EOFError:
            raise

        self.line_no += 1

        if not line.strip():
            return None

        # Multi-line support
        lines = [line]
        while line.rstrip().endswith("\\"):
            lines[-1] = lines[-1].rstrip()[:-1]  # remove backslash
            try:
                line = input("  ... ")
                self.line_no += 1
                lines.append(line)
            except EOFError:
                break

        return "\n".join(lines)

    def _handle_command(self, cmd: str):
        """Process a REPL command."""
        parts = cmd.split(None, 1)
        command = parts[0].lower()
        arg = parts[1] if len(parts) > 1 else ""

        if command in (":quit", ":q"):
            print("Bye!")
            self.running = False

        elif command == ":help":
            print(HELP_TEXT)

        elif command == ":clear":
            print("\033[2J\033[H", end="")  # ANSI clear

        elif command == ":reset":
            self.vm = GreyVM()
            print("REPL state reset.")

        elif command == ":ast":
            if arg:
                self._show_ast(arg)
            else:
                print("Usage: :ast <code>")

        elif command == ":ir":
            if arg:
                self._show_ir(arg)
            else:
                print("Usage: :ir <code>")

        elif command == ":dis":
            if arg:
                self._show_disassembly(arg)
            else:
                print("Usage: :dis <code>")

        elif command == ":env":
            self._show_env()

        elif command == ":history":
            for i, entry in enumerate(self.history):
                print(f"  [{i + 1}] {entry}")

        else:
            print(f"Unknown command: {command}. Type :help for help.")

    def _eval(self, source: str):
        """Compile and evaluate Grey source code."""
        try:
            # Lex
            lexer = Lexer(source, "<repl>")
            tokens = lexer.scan_tokens()
            if lexer.errors:
                for err in lexer.errors:
                    print(f"Lex error: {err}")
                return

            # Parse
            parser = Parser(tokens)
            program = parser.parse()
            if parser.errors:
                for err in parser.errors:
                    print(f"Parse error: {err}")
                return

            # Semantic analysis
            analyzer = SemanticAnalyzer()
            analyzer.analyze(program)
            if analyzer.errors:
                for err in analyzer.errors:
                    print(f"Semantic error: {err}")
                return

            # Normalize
            normalizer = ASTNormalizer()
            program = normalizer.normalize(program)

            # Generate IR
            ir_gen = IRGenerator()
            ir_module = ir_gen.generate(program)

            # Optimize
            optimizer = Optimizer(level=1)
            ir_module = optimizer.optimize(ir_module)

            # Generate bytecode
            codegen = CodeGenerator()
            compiled = codegen.generate(ir_module)

            # Execute
            vm = GreyVM()
            vm.load(compiled)
            result = vm.run()

            # Auto-print result for expressions
            from ..runtime.memory import ValueType
            if result is not None and result.type != ValueType.NIL:
                print(f"=> {result}")

        except VMError as e:
            print(f"Runtime error: {e}")
        except Exception as e:
            print(f"Error: {e}")
            if "--debug" in sys.argv:
                traceback.print_exc()

    def _show_ast(self, source: str):
        """Show the AST for source code."""
        try:
            lexer = Lexer(source, "<repl>")
            tokens = lexer.scan_tokens()
            parser = Parser(tokens)
            program = parser.parse()
            printer = ASTPrinter()
            print(printer.print(program))
        except Exception as e:
            print(f"Error: {e}")

    def _show_ir(self, source: str):
        """Show the IR for source code."""
        try:
            lexer = Lexer(source, "<repl>")
            tokens = lexer.scan_tokens()
            parser = Parser(tokens)
            program = parser.parse()
            analyzer = SemanticAnalyzer()
            analyzer.analyze(program)
            normalizer = ASTNormalizer()
            program = normalizer.normalize(program)
            ir_gen = IRGenerator()
            ir_module = ir_gen.generate(program)
            print(ir_module)
        except Exception as e:
            print(f"Error: {e}")

    def _show_disassembly(self, source: str):
        """Show disassembly for source code."""
        try:
            lexer = Lexer(source, "<repl>")
            tokens = lexer.scan_tokens()
            parser = Parser(tokens)
            program = parser.parse()
            analyzer = SemanticAnalyzer()
            analyzer.analyze(program)
            normalizer = ASTNormalizer()
            program = normalizer.normalize(program)
            ir_gen = IRGenerator()
            ir_module = ir_gen.generate(program)
            optimizer = Optimizer(level=1)
            ir_module = optimizer.optimize(ir_module)
            codegen = CodeGenerator()
            compiled = codegen.generate(ir_module)
            emitter = BytecodeEmitter(compiled)
            print(emitter.disassemble())
        except Exception as e:
            print(f"Error: {e}")

    def _show_env(self):
        """Show current REPL environment variables."""
        if not self.vm.memory.globals:
            print("(no variables defined)")
        else:
            for name, val in self.vm.memory.globals.items():
                print(f"  {name} = {val}")
