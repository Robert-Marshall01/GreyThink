"""
Grey Math — Parallel Execution Engine.

Provides parallel and concurrent computation support:
- Thread pool for I/O-bound tasks
- Process pool for CPU-bound computation
- Task graph execution with dependency resolution
- Batch evaluation
"""

from __future__ import annotations

import concurrent.futures
import multiprocessing
from dataclasses import dataclass, field
from typing import Any, Callable, Optional

import numpy as np


@dataclass
class TaskResult:
    """Result of a parallel task."""
    task_id: str
    success: bool
    value: Any = None
    error: str | None = None
    duration_ms: float = 0.0


@dataclass
class TaskNode:
    """A node in a computation task graph."""
    task_id: str
    function: Callable
    args: tuple = ()
    kwargs: dict[str, Any] = field(default_factory=dict)
    dependencies: list[str] = field(default_factory=list)  # Task IDs this depends on
    result: Optional[TaskResult] = None


class ParallelExecutor:
    """
    Execute computations in parallel using threads or processes.
    """

    def __init__(
        self,
        max_workers: int | None = None,
        use_processes: bool = False,
    ) -> None:
        self.max_workers = max_workers or min(32, (multiprocessing.cpu_count() or 1) + 4)
        self.use_processes = use_processes

    def map(
        self,
        func: Callable,
        items: list[Any],
        chunk_size: int | None = None,
    ) -> list[TaskResult]:
        """
        Apply a function to each item in parallel.
        """
        import time

        results: list[TaskResult] = []
        executor_cls = (
            concurrent.futures.ProcessPoolExecutor
            if self.use_processes
            else concurrent.futures.ThreadPoolExecutor
        )

        with executor_cls(max_workers=self.max_workers) as executor:
            future_to_idx: dict[concurrent.futures.Future, int] = {}
            start_times: dict[int, float] = {}

            for i, item in enumerate(items):
                start_times[i] = time.perf_counter()
                future = executor.submit(func, item)
                future_to_idx[future] = i

            # Collect results in order
            result_map: dict[int, TaskResult] = {}
            for future in concurrent.futures.as_completed(future_to_idx):
                idx = future_to_idx[future]
                duration = (time.perf_counter() - start_times[idx]) * 1000

                try:
                    value = future.result()
                    result_map[idx] = TaskResult(
                        task_id=f"task_{idx}",
                        success=True,
                        value=value,
                        duration_ms=duration,
                    )
                except Exception as e:
                    result_map[idx] = TaskResult(
                        task_id=f"task_{idx}",
                        success=False,
                        error=str(e),
                        duration_ms=duration,
                    )

            results = [result_map[i] for i in range(len(items))]

        return results

    def execute_graph(self, tasks: list[TaskNode]) -> dict[str, TaskResult]:
        """
        Execute a task graph respecting dependencies.
        Tasks without dependencies run in parallel.
        """
        import time

        task_map = {t.task_id: t for t in tasks}
        results: dict[str, TaskResult] = {}
        completed: set[str] = set()

        executor_cls = (
            concurrent.futures.ProcessPoolExecutor
            if self.use_processes
            else concurrent.futures.ThreadPoolExecutor
        )

        with executor_cls(max_workers=self.max_workers) as executor:
            while len(completed) < len(tasks):
                # Find tasks ready to run
                ready = [
                    t for t in tasks
                    if t.task_id not in completed
                    and all(d in completed for d in t.dependencies)
                ]

                if not ready:
                    # Deadlock — circular dependencies
                    remaining = [t.task_id for t in tasks if t.task_id not in completed]
                    for tid in remaining:
                        results[tid] = TaskResult(
                            task_id=tid,
                            success=False,
                            error="Circular dependency detected",
                        )
                        completed.add(tid)
                    break

                # Submit ready tasks
                futures: dict[concurrent.futures.Future, TaskNode] = {}
                start_times_local: dict[str, float] = {}

                for task in ready:
                    # Inject dependency results into kwargs
                    dep_results = {
                        dep_id: results[dep_id].value
                        for dep_id in task.dependencies
                        if dep_id in results and results[dep_id].success
                    }
                    kwargs = {**task.kwargs, "_dep_results": dep_results}

                    start_times_local[task.task_id] = time.perf_counter()
                    future = executor.submit(task.function, *task.args, **kwargs)
                    futures[future] = task

                # Wait for this batch
                for future in concurrent.futures.as_completed(futures):
                    task = futures[future]
                    duration = (time.perf_counter() - start_times_local[task.task_id]) * 1000

                    try:
                        value = future.result()
                        results[task.task_id] = TaskResult(
                            task_id=task.task_id,
                            success=True,
                            value=value,
                            duration_ms=duration,
                        )
                    except Exception as e:
                        results[task.task_id] = TaskResult(
                            task_id=task.task_id,
                            success=False,
                            error=str(e),
                            duration_ms=duration,
                        )

                    completed.add(task.task_id)

        return results

    def batch_eval(
        self,
        expressions: list[str],
        session_factory: Callable,
    ) -> list[TaskResult]:
        """
        Evaluate multiple expressions in parallel using separate sessions.
        """
        def eval_expr(expr: str) -> Any:
            session = session_factory()
            return session.evaluate(expr)

        return self.map(eval_expr, expressions)

    def parallel_matrix_ops(
        self,
        matrices: list[np.ndarray],
        operation: Callable[[np.ndarray], Any],
    ) -> list[TaskResult]:
        """Apply a matrix operation to multiple matrices in parallel."""
        return self.map(operation, matrices)

    def parallel_parameter_sweep(
        self,
        func: Callable,
        param_grid: dict[str, list[Any]],
    ) -> list[TaskResult]:
        """
        Run a function over a parameter grid in parallel.
        """
        import itertools

        keys = list(param_grid.keys())
        values = list(param_grid.values())
        combinations = list(itertools.product(*values))

        def run_with_params(combo: tuple) -> Any:
            kwargs = dict(zip(keys, combo))
            return func(**kwargs)

        return self.map(run_with_params, combinations)
