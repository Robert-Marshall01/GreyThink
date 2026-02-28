"""
Grey Math — Example: LLM Tool Integration.

Demonstrates how to use Grey Math as a tool backend for LLMs,
including tool schema generation and RPC server usage.
"""

import json
import asyncio

from greymath.api.tools import get_math_tools
from greymath.api.sandbox import SandboxedEvaluator
from greymath.api.rpc import RPCServer


def demo_tool_schemas():
    """Show how to generate tool schemas for LLM function calling."""
    print("── Tool Schemas for LLMs ──\n")

    registry = get_math_tools()
    tools = registry.list_tools()
    print(f"Registered {len(tools)} tools:\n")

    for tool in tools:
        print(f"  {tool.name:<35} [{tool.category}]")
        print(f"    {tool.description[:80]}")

    # Export for OpenAI
    openai_tools = registry.to_openai_tools()
    print(f"\nOpenAI function schema (first tool):")
    print(json.dumps(openai_tools[0], indent=2)[:300])

    # Export for Anthropic
    anthropic_tools = registry.to_anthropic_tools()
    print(f"\nAnthropic tool schema (first tool):")
    print(json.dumps(anthropic_tools[0], indent=2)[:300])


def demo_sandbox():
    """Demonstrate safe expression evaluation."""
    print("\n── Sandbox Evaluation ──\n")

    sandbox = SandboxedEvaluator()

    # Safe expressions
    safe_exprs = [
        "2 + 3 * 5",
        "sqrt(2) * pi",
        "sin(pi / 4)",
        "[x**2 for x in range(10)]",
        "sum([1/factorial(n) for n in range(10)])",  # Approx e
    ]

    for expr in safe_exprs:
        result = sandbox.evaluate(expr)
        status = "✓" if result.success else "✗"
        value = result.value if result.success else result.error
        print(f"  {status} {expr:<45} → {value}")

    # Dangerous expressions (should be blocked)
    print("\n  Blocked expressions:")
    dangerous = [
        "__import__('os').system('ls')",
        "open('/etc/passwd').read()",
        "exec('print(1)')",
        "eval('1+1')",
    ]

    for expr in dangerous:
        result = sandbox.evaluate(expr)
        print(f"  ✗ {expr:<45} → {result.error}")


async def demo_rpc():
    """Demonstrate JSON-RPC server usage."""
    print("\n── JSON-RPC Server ──\n")

    server = RPCServer()

    # List methods
    request = json.dumps({
        "jsonrpc": "2.0",
        "method": "rpc.listMethods",
        "id": 1,
    })
    response = json.loads(await server.handle(request))
    methods = response["result"]
    print(f"Available RPC methods ({len(methods)}):")
    for m in methods[:5]:
        print(f"  {m['name']:<25} {m['description'][:50]}")
    print(f"  ... and {len(methods) - 5} more")

    # Create session and evaluate
    req_create = json.dumps({
        "jsonrpc": "2.0",
        "method": "session.create",
        "id": 2,
    })
    resp = json.loads(await server.handle(req_create))
    session_id = resp["result"]["session_id"]
    print(f"\nSession created: {session_id}")

    # Evaluate expressions
    expressions = [
        "2 ** 10",
        "3.14159 * 2",
    ]

    for expr in expressions:
        req = json.dumps({
            "jsonrpc": "2.0",
            "method": "eval",
            "params": {"expression": expr, "session_id": session_id},
            "id": 3,
        })
        resp = json.loads(await server.handle(req))
        print(f"  eval('{expr}') → {resp.get('result', resp.get('error'))}")

    # Batch request
    batch = json.dumps([
        {"jsonrpc": "2.0", "method": "eval",
         "params": {"expression": "7 * 6", "session_id": session_id}, "id": 10},
        {"jsonrpc": "2.0", "method": "eval",
         "params": {"expression": "2 ** 8", "session_id": session_id}, "id": 11},
    ])
    resp = json.loads(await server.handle(batch))
    print(f"\n  Batch results: {[r.get('result') for r in resp]}")


def main():
    print("═══ Grey Math: LLM Integration Example ═══\n")

    demo_tool_schemas()
    demo_sandbox()
    asyncio.run(demo_rpc())

    print("\n✓ LLM integration demo complete.")


if __name__ == "__main__":
    main()
