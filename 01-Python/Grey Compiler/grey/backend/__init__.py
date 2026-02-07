"""Backend package - Code Generation, Register Allocation, Emission, Native Backends."""

from .instruction import *
from .codegen import CodeGenerator
from .register_alloc import RegisterAllocator
from .emitter import BytecodeEmitter
from .x86_64_codegen import X86_64CodeGenerator
from .c_codegen import CCodeGenerator

__all__ = [
    'CodeGenerator', 'RegisterAllocator', 'BytecodeEmitter',
    'X86_64CodeGenerator', 'CCodeGenerator',
]
