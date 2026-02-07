"""
Grey Compiler - A complete compiler implementation.

Pipeline: Source → Lexer → Parser → AST → Semantic Analysis → IR → Optimization → Codegen → Executable

Components:
  - frontend: Lexer, Parser, Semantic Analyzer, AST Normalizer
  - middle:   IR Generation, Optimization Pipeline
  - backend:  Code Generation, Register Allocation, Emission
  - runtime:  Virtual Machine, Memory Management, GC, Standard Library
  - tools:    REPL, Formatter, Linter
"""

__version__ = "1.0.0"
__author__ = "Grey Language Team"
