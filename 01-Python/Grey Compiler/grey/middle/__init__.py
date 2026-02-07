"""Middle end package - IR Generation and Optimization."""

from .ir import *
from .ir_generator import IRGenerator
from .optimizer import Optimizer

__all__ = ['IRGenerator', 'Optimizer']
