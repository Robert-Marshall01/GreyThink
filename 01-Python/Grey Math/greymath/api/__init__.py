"""
Grey Math — LLM & Agent Integration API.

Provides:
- JSON-RPC server for programmatic access
- Safe expression DSL with sandboxed evaluation
- Structured error reporting
- Streaming results via WebSocket
- Tool definitions for function-calling LLMs
"""

from greymath.api.rpc import RPCServer
from greymath.api.tools import ToolRegistry, get_math_tools
from greymath.api.sandbox import SandboxedEvaluator

__all__ = ["RPCServer", "ToolRegistry", "get_math_tools", "SandboxedEvaluator"]
