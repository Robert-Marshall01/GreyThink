"""Tests for Grey Math plugin system, API, and runtime."""

import pytest
import json
import asyncio
import numpy as np

from greymath.plugins.base import (
    Plugin, PluginType, PluginMetadata, PluginCapability,
    PluginManager, PluginHost,
)
from greymath.plugins.loader import PluginLoader
from greymath.api.sandbox import SandboxedEvaluator, SecurityViolation
from greymath.api.rpc import RPCServer
from greymath.api.tools import get_math_tools, ToolRegistry
from greymath.runtime.cache import ExprCache, ResultCache
from greymath.runtime.tracer import ExecutionTracer, TraceEventType
from greymath.runtime.compiler import ExprCompiler
from greymath.runtime.parallel import ParallelExecutor
from greymath.core.expr import Expr, ExprKind


# ── Test Plugin ────────────────────────────────────────────────────────────────

class DummyPlugin(Plugin):
    @property
    def metadata(self) -> PluginMetadata:
        return PluginMetadata(
            name="test_plugin",
            version="1.0.0",
            author="Test",
            description="A test plugin",
            plugin_type=PluginType.DOMAIN,
            capabilities=[PluginCapability.NUMERIC_SOLVE],
        )

    def setup(self, host: PluginHost) -> None:
        host.register_function("test_func", lambda x: x * 2)


class DependentPlugin(Plugin):
    @property
    def metadata(self) -> PluginMetadata:
        return PluginMetadata(
            name="dependent_plugin",
            version="1.0.0",
            dependencies=["test_plugin"],
        )

    def setup(self, host: PluginHost) -> None:
        host.register_function("dep_func", lambda x: x + 1)


# ── Plugin Tests ───────────────────────────────────────────────────────────────

class TestPluginManager:
    def test_register_plugin(self):
        mgr = PluginManager()
        plugin = DummyPlugin()
        mgr.register(plugin)
        assert len(mgr.list_plugins()) == 1

    def test_load_plugin(self):
        mgr = PluginManager()
        plugin = DummyPlugin()
        mgr.register(plugin)
        mgr.load("test_plugin")
        assert "test_plugin" in mgr.list_loaded()
        assert "test_func" in mgr.host.functions

    def test_unload_plugin(self):
        mgr = PluginManager()
        plugin = DummyPlugin()
        mgr.register(plugin)
        mgr.load("test_plugin")
        mgr.unload("test_plugin")
        assert "test_plugin" not in mgr.list_loaded()

    def test_dependency_order(self):
        mgr = PluginManager()
        base = DummyPlugin()
        dep = DependentPlugin()
        mgr.register(base)
        mgr.register(dep)
        mgr.load_all()
        loaded = mgr.list_loaded()
        assert loaded.index("test_plugin") < loaded.index("dependent_plugin")

    def test_missing_dependency(self):
        mgr = PluginManager()
        dep = DependentPlugin()
        with pytest.raises(ValueError, match="requires"):
            mgr.register(dep)

    def test_find_by_capability(self):
        mgr = PluginManager()
        plugin = DummyPlugin()
        mgr.register(plugin)
        mgr.load("test_plugin")
        found = mgr.find_by_capability(PluginCapability.NUMERIC_SOLVE)
        assert len(found) == 1

    def test_duplicate_registration(self):
        mgr = PluginManager()
        mgr.register(DummyPlugin())
        with pytest.raises(ValueError, match="already registered"):
            mgr.register(DummyPlugin())


# ── Sandbox Tests ──────────────────────────────────────────────────────────────

class TestSandbox:
    def test_basic_arithmetic(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("2 + 3 * 4")
        assert result.success
        assert result.value == 14

    def test_math_functions(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("sqrt(4)")
        assert result.success
        assert result.value == 2.0

    def test_constants(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("pi")
        assert result.success
        assert abs(result.value - 3.14159265) < 1e-5

    def test_list_comprehension(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("[x**2 for x in range(5)]")
        assert result.success
        assert result.value == [0, 1, 4, 9, 16]

    def test_block_import(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("__import__('os')")
        assert not result.success
        assert "Security violation" in result.error or "Forbidden" in result.error

    def test_block_open(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("open('/etc/passwd')")
        assert not result.success

    def test_block_exec(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("exec('print(1)')")
        assert not result.success

    def test_block_large_exponent(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("2 ** 100000")
        assert not result.success

    def test_variable_assignment(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("x = 42")
        assert result.success
        assert sandbox.get_variable("x") == 42

    def test_comparison(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("3 < 5")
        assert result.success
        assert result.value is True

    def test_empty_expression(self):
        sandbox = SandboxedEvaluator()
        result = sandbox.evaluate("")
        assert not result.success


# ── RPC Tests ──────────────────────────────────────────────────────────────────

class TestRPCServer:
    @pytest.fixture
    def server(self):
        return RPCServer()

    def test_list_methods(self, server):
        result = asyncio.get_event_loop().run_until_complete(
            server.handle(json.dumps({
                "jsonrpc": "2.0",
                "method": "rpc.listMethods",
                "id": 1,
            }))
        )
        resp = json.loads(result)
        assert "result" in resp
        assert isinstance(resp["result"], list)
        assert len(resp["result"]) > 0

    def test_create_session(self, server):
        result = asyncio.get_event_loop().run_until_complete(
            server.handle(json.dumps({
                "jsonrpc": "2.0",
                "method": "session.create",
                "id": 2,
            }))
        )
        resp = json.loads(result)
        assert "result" in resp
        assert "session_id" in resp["result"]

    def test_eval(self, server):
        # Create session first
        create_resp = json.loads(asyncio.get_event_loop().run_until_complete(
            server.handle(json.dumps({
                "jsonrpc": "2.0",
                "method": "session.create",
                "id": 1,
            }))
        ))
        sid = create_resp["result"]["session_id"]

        # Evaluate
        result = asyncio.get_event_loop().run_until_complete(
            server.handle(json.dumps({
                "jsonrpc": "2.0",
                "method": "eval",
                "params": {"expression": "2 + 3", "session_id": sid},
                "id": 2,
            }))
        )
        resp = json.loads(result)
        assert "result" in resp

    def test_method_not_found(self, server):
        result = asyncio.get_event_loop().run_until_complete(
            server.handle(json.dumps({
                "jsonrpc": "2.0",
                "method": "nonexistent",
                "id": 3,
            }))
        )
        resp = json.loads(result)
        assert "error" in resp
        assert resp["error"]["code"] == -32601

    def test_parse_error(self, server):
        result = asyncio.get_event_loop().run_until_complete(
            server.handle("not json{{{")
        )
        resp = json.loads(result)
        assert "error" in resp
        assert resp["error"]["code"] == -32700


# ── Tool Registry Tests ───────────────────────────────────────────────────────

class TestToolRegistry:
    def test_get_math_tools(self):
        registry = get_math_tools()
        tools = registry.list_tools()
        assert len(tools) > 0

    def test_openai_schema(self):
        registry = get_math_tools()
        schemas = registry.to_openai_tools()
        assert len(schemas) > 0
        assert schemas[0]["type"] == "function"

    def test_anthropic_schema(self):
        registry = get_math_tools()
        schemas = registry.to_anthropic_tools()
        assert len(schemas) > 0
        assert "input_schema" in schemas[0]

    def test_filter_by_category(self):
        registry = get_math_tools()
        linalg_tools = registry.list_tools(category="linalg")
        assert all(t.category == "linalg" for t in linalg_tools)


# ── Cache Tests ────────────────────────────────────────────────────────────────

class TestExprCache:
    def test_put_and_get(self):
        cache = ExprCache(max_entries=100)
        expr = Expr.var("x")
        cache.put(expr, 42)
        assert cache.get(expr) == 42

    def test_cache_miss(self):
        cache = ExprCache(max_entries=100)
        expr = Expr.var("x")
        assert cache.get(expr) is None

    def test_lru_eviction(self):
        cache = ExprCache(max_entries=2)
        a = Expr.var("a")
        b = Expr.var("b")
        c = Expr.var("c")
        cache.put(a, 1)
        cache.put(b, 2)
        cache.put(c, 3)  # Should evict 'a'
        assert cache.get(a) is None
        assert cache.get(b) == 2
        assert cache.get(c) == 3

    def test_cache_stats(self):
        cache = ExprCache(max_entries=100)
        expr = Expr.var("x")
        cache.put(expr, 1)
        cache.get(expr)  # Hit
        cache.get(Expr.var("y"))  # Miss
        assert cache.stats.hits == 1
        assert cache.stats.misses == 1


class TestResultCache:
    def test_basic_operations(self):
        cache = ResultCache(max_entries=100)
        cache.put("key1", [1, 2, 3])
        assert cache.get("key1") == [1, 2, 3]

    def test_delete(self):
        cache = ResultCache(max_entries=100)
        cache.put("key1", "value")
        assert cache.delete("key1")
        assert cache.get("key1") is None


# ── Tracer Tests ───────────────────────────────────────────────────────────────

class TestExecutionTracer:
    def test_record_event(self):
        tracer = ExecutionTracer()
        eid = tracer.record(TraceEventType.EVAL_START, component="test", operation="add")
        assert eid > 0
        assert len(tracer.events) == 1

    def test_disabled_tracer(self):
        tracer = ExecutionTracer(enabled=False)
        eid = tracer.record(TraceEventType.EVAL_START)
        assert eid == -1
        assert len(tracer.events) == 0

    def test_span(self):
        tracer = ExecutionTracer()
        sid = tracer.begin_span("test", "multiply")
        tracer.end_span(sid, outputs={"result": 42})
        assert len(tracer.events) == 2

    def test_profile(self):
        tracer = ExecutionTracer()
        tracer.record(TraceEventType.EVAL_START, component="linalg", duration_ms=10.0)
        tracer.record(TraceEventType.CACHE_HIT, component="cache")
        tracer.record(TraceEventType.CACHE_MISS, component="cache")
        tracer.record(TraceEventType.EVAL_END, component="linalg", duration_ms=5.0)
        profile = tracer.get_profile()
        assert profile.n_events == 4
        assert profile.cache_hit_rate == 0.5


# ── Compiler Tests ─────────────────────────────────────────────────────────────

class TestExprCompiler:
    def test_compile_literal(self):
        compiler = ExprCompiler()
        expr = Expr.lit(42)
        compiled = compiler.compile(expr)
        assert compiled.function() == 42

    def test_compile_variable(self):
        compiler = ExprCompiler()
        expr = Expr.var("x")
        compiled = compiler.compile(expr, ["x"])
        assert compiled.function(5) == 5

    def test_compile_arithmetic(self):
        compiler = ExprCompiler()
        x = Expr.var("x")
        expr = x + Expr.lit(1)
        compiled = compiler.compile(expr, ["x"])
        assert compiled.function(4) == 5

    def test_compile_power(self):
        compiler = ExprCompiler()
        x = Expr.var("x")
        expr = x ** Expr.lit(2)
        compiled = compiler.compile(expr, ["x"])
        assert compiled.function(3) == 9


# ── Parallel Executor Tests ────────────────────────────────────────────────────

class TestParallelExecutor:
    def test_parallel_map(self):
        executor = ParallelExecutor(max_workers=2, use_processes=False)
        results = executor.map(lambda x: x ** 2, [1, 2, 3, 4, 5])
        assert all(r.success for r in results)
        assert [r.value for r in results] == [1, 4, 9, 16, 25]

    def test_parallel_matrix_ops(self):
        executor = ParallelExecutor(max_workers=2, use_processes=False)
        matrices = [np.random.randn(3, 3) for _ in range(4)]
        results = executor.parallel_matrix_ops(matrices, np.linalg.det)
        assert all(r.success for r in results)
        assert len(results) == 4
