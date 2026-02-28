"""
Grey Math — Execution Tracer & Profiler.

Records and profiles computation execution for debugging,
optimization, and provenance tracking.
"""

from __future__ import annotations

import time
from collections import defaultdict
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Optional


class TraceEventType(Enum):
    """Types of events in an execution trace."""
    EVAL_START = auto()
    EVAL_END = auto()
    SIMPLIFY_START = auto()
    SIMPLIFY_END = auto()
    REWRITE_APPLY = auto()
    NUMERIC_COMPUTE = auto()
    CACHE_HIT = auto()
    CACHE_MISS = auto()
    SOLVER_STEP = auto()
    CONVERGENCE = auto()
    WARNING = auto()
    ERROR = auto()
    ALLOCATION = auto()
    PLUGIN_CALL = auto()


@dataclass
class TraceEvent:
    """A single event in an execution trace."""
    event_type: TraceEventType
    timestamp: float
    duration_ms: float = 0.0
    component: str = ""  # Which subsystem generated this event
    operation: str = ""
    inputs: dict[str, Any] = field(default_factory=dict)
    outputs: dict[str, Any] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)
    parent_id: Optional[int] = None
    event_id: int = 0


@dataclass
class ProfileSummary:
    """Profiling summary of an execution."""
    total_time_ms: float
    n_events: int
    by_component: dict[str, float] = field(default_factory=dict)  # component -> total ms
    by_operation: dict[str, float] = field(default_factory=dict)  # operation -> total ms
    cache_hit_rate: float = 0.0
    n_rewrites: int = 0
    n_solver_steps: int = 0
    peak_memory_mb: float = 0.0
    hotspots: list[tuple[str, float]] = field(default_factory=list)  # Top time consumers


class ExecutionTracer:
    """
    Records execution traces for analysis, debugging, and profiling.
    Supports nested spans for hierarchical tracing.
    """

    def __init__(self, enabled: bool = True, max_events: int = 100_000) -> None:
        self._enabled = enabled
        self._max_events = max_events
        self._events: list[TraceEvent] = []
        self._event_counter = 0
        self._span_stack: list[int] = []  # Stack of parent event IDs
        self._start_time: float = 0.0

    @property
    def enabled(self) -> bool:
        return self._enabled

    def enable(self) -> None:
        self._enabled = True

    def disable(self) -> None:
        self._enabled = False

    def reset(self) -> None:
        """Clear all recorded events."""
        self._events.clear()
        self._event_counter = 0
        self._span_stack.clear()

    def record(
        self,
        event_type: TraceEventType,
        component: str = "",
        operation: str = "",
        inputs: dict[str, Any] | None = None,
        outputs: dict[str, Any] | None = None,
        metadata: dict[str, Any] | None = None,
        duration_ms: float = 0.0,
    ) -> int:
        """Record a single trace event. Returns the event ID."""
        if not self._enabled:
            return -1

        if len(self._events) >= self._max_events:
            return -1

        self._event_counter += 1
        event = TraceEvent(
            event_type=event_type,
            timestamp=time.perf_counter(),
            duration_ms=duration_ms,
            component=component,
            operation=operation,
            inputs=inputs or {},
            outputs=outputs or {},
            metadata=metadata or {},
            parent_id=self._span_stack[-1] if self._span_stack else None,
            event_id=self._event_counter,
        )
        self._events.append(event)
        return event.event_id

    def begin_span(
        self,
        component: str,
        operation: str,
        inputs: dict[str, Any] | None = None,
    ) -> int:
        """Begin a timed span. Returns span ID for end_span."""
        eid = self.record(
            TraceEventType.EVAL_START,
            component=component,
            operation=operation,
            inputs=inputs,
        )
        if eid >= 0:
            self._span_stack.append(eid)
        return eid

    def end_span(self, span_id: int, outputs: dict[str, Any] | None = None) -> None:
        """End a timed span and record duration."""
        if not self._enabled or span_id < 0:
            return

        if self._span_stack and self._span_stack[-1] == span_id:
            self._span_stack.pop()

        # Find the start event and compute duration
        start_event = None
        for e in reversed(self._events):
            if e.event_id == span_id:
                start_event = e
                break

        if start_event is not None:
            duration = (time.perf_counter() - start_event.timestamp) * 1000
            self.record(
                TraceEventType.EVAL_END,
                component=start_event.component,
                operation=start_event.operation,
                outputs=outputs,
                duration_ms=duration,
                metadata={"span_id": span_id},
            )

    def trace_function(self, component: str) -> Callable:
        """Decorator to trace a function call."""
        def decorator(func: Callable) -> Callable:
            def wrapper(*args: Any, **kwargs: Any) -> Any:
                span_id = self.begin_span(
                    component=component,
                    operation=func.__name__,
                    inputs={"args_count": len(args), "kwargs": list(kwargs.keys())},
                )
                try:
                    result = func(*args, **kwargs)
                    self.end_span(span_id, outputs={"success": True})
                    return result
                except Exception as e:
                    self.end_span(span_id, outputs={"error": str(e)})
                    raise
            return wrapper
        return decorator

    @property
    def events(self) -> list[TraceEvent]:
        return list(self._events)

    def get_profile(self) -> ProfileSummary:
        """Generate a profiling summary from recorded events."""
        if not self._events:
            return ProfileSummary(total_time_ms=0, n_events=0)

        by_component: dict[str, float] = defaultdict(float)
        by_operation: dict[str, float] = defaultdict(float)
        cache_hits = 0
        cache_total = 0
        n_rewrites = 0
        n_solver_steps = 0

        for event in self._events:
            if event.duration_ms > 0:
                by_component[event.component] += event.duration_ms
                by_operation[event.operation] += event.duration_ms

            if event.event_type == TraceEventType.CACHE_HIT:
                cache_hits += 1
                cache_total += 1
            elif event.event_type == TraceEventType.CACHE_MISS:
                cache_total += 1
            elif event.event_type == TraceEventType.REWRITE_APPLY:
                n_rewrites += 1
            elif event.event_type == TraceEventType.SOLVER_STEP:
                n_solver_steps += 1

        # Calculate total time from first to last event
        first = self._events[0].timestamp
        last = self._events[-1].timestamp
        total_ms = (last - first) * 1000

        # Top hotspots
        hotspots = sorted(by_operation.items(), key=lambda x: x[1], reverse=True)[:10]

        return ProfileSummary(
            total_time_ms=total_ms,
            n_events=len(self._events),
            by_component=dict(by_component),
            by_operation=dict(by_operation),
            cache_hit_rate=cache_hits / cache_total if cache_total > 0 else 0.0,
            n_rewrites=n_rewrites,
            n_solver_steps=n_solver_steps,
            hotspots=hotspots,
        )

    def to_json(self) -> list[dict[str, Any]]:
        """Export trace as JSON-serializable list."""
        return [
            {
                "event_id": e.event_id,
                "type": e.event_type.name,
                "timestamp": e.timestamp,
                "duration_ms": e.duration_ms,
                "component": e.component,
                "operation": e.operation,
                "parent_id": e.parent_id,
            }
            for e in self._events
        ]

    def print_profile(self) -> str:
        """Generate a human-readable profile report."""
        profile = self.get_profile()
        lines = [
            "═══ Grey Math Execution Profile ═══",
            f"Total time: {profile.total_time_ms:.2f} ms",
            f"Events: {profile.n_events}",
            f"Cache hit rate: {profile.cache_hit_rate:.1%}",
            f"Rewrites: {profile.n_rewrites}",
            f"Solver steps: {profile.n_solver_steps}",
            "",
            "── By Component ──",
        ]

        for comp, ms in sorted(profile.by_component.items(), key=lambda x: -x[1]):
            pct = ms / profile.total_time_ms * 100 if profile.total_time_ms > 0 else 0
            lines.append(f"  {comp:<30} {ms:>8.2f} ms ({pct:.1f}%)")

        lines.append("")
        lines.append("── Hotspots ──")
        for op, ms in profile.hotspots[:5]:
            lines.append(f"  {op:<30} {ms:>8.2f} ms")

        return "\n".join(lines)
