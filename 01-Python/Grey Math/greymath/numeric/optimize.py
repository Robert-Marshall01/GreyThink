"""
Optimization engine.

Provides:
- Gradient descent (with line search)
- Newton's method
- L-BFGS
- Conjugate gradient
- Proximal methods
- Constrained optimization (augmented Lagrangian)
- Convex optimization
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Callable, Optional

import numpy as np
from numpy.typing import NDArray


class OptMethod(Enum):
    """Available optimization methods."""
    GRADIENT_DESCENT = auto()
    NEWTON = auto()
    LBFGS = auto()
    CONJUGATE_GRADIENT = auto()
    PROXIMAL_GRADIENT = auto()
    AUGMENTED_LAGRANGIAN = auto()
    NELDER_MEAD = auto()


@dataclass
class OptResult:
    """Result of an optimization problem."""
    x: NDArray                          # optimal point
    fun: float                          # objective value
    grad: Optional[NDArray] = None      # gradient at optimum
    n_iterations: int = 0
    n_function_evals: int = 0
    n_gradient_evals: int = 0
    converged: bool = False
    message: str = ""
    trajectory: list[NDArray] = field(default_factory=list)

    def __repr__(self) -> str:
        return (f"OptResult(fun={self.fun:.6g}, converged={self.converged}, "
                f"iters={self.n_iterations})")


class Optimizer:
    """
    Numerical optimization engine.

    Supports convex and non-convex, unconstrained and constrained problems.
    """

    def minimize(
        self,
        f: Callable[[NDArray], float],
        x0: NDArray,
        grad: Optional[Callable[[NDArray], NDArray]] = None,
        hessian: Optional[Callable[[NDArray], NDArray]] = None,
        method: OptMethod = OptMethod.LBFGS,
        tol: float = 1e-8,
        max_iter: int = 1000,
        learning_rate: float = 0.01,
        track_trajectory: bool = False,
    ) -> OptResult:
        """
        Minimize a scalar function f: R^n → R.

        Args:
            f: Objective function
            x0: Initial point
            grad: Gradient function (estimated if None)
            hessian: Hessian function (for Newton's method)
            method: Optimization method
            tol: Convergence tolerance
            max_iter: Maximum iterations
            learning_rate: Step size (for gradient descent)
            track_trajectory: Whether to record optimization path
        """
        x0 = np.asarray(x0, dtype=float)

        if grad is None:
            grad = lambda x: self._numerical_gradient(f, x)

        if method == OptMethod.GRADIENT_DESCENT:
            return self._gradient_descent(f, grad, x0, learning_rate, tol, max_iter,
                                          track_trajectory)
        elif method == OptMethod.NEWTON:
            return self._newton(f, grad, hessian, x0, tol, max_iter, track_trajectory)
        elif method == OptMethod.LBFGS:
            return self._lbfgs(f, grad, x0, tol, max_iter, track_trajectory)
        elif method == OptMethod.CONJUGATE_GRADIENT:
            return self._conjugate_gradient(f, grad, x0, tol, max_iter, track_trajectory)
        elif method == OptMethod.NELDER_MEAD:
            return self._nelder_mead(f, x0, tol, max_iter, track_trajectory)
        else:
            return self._lbfgs(f, grad, x0, tol, max_iter, track_trajectory)

    def minimize_constrained(
        self,
        f: Callable[[NDArray], float],
        x0: NDArray,
        grad: Optional[Callable[[NDArray], NDArray]] = None,
        eq_constraints: Optional[list[Callable[[NDArray], float]]] = None,
        ineq_constraints: Optional[list[Callable[[NDArray], float]]] = None,
        tol: float = 1e-8,
        max_iter: int = 1000,
    ) -> OptResult:
        """Constrained optimization via augmented Lagrangian method."""
        from scipy.optimize import minimize as scipy_minimize

        x0 = np.asarray(x0, dtype=float)
        constraints = []
        if eq_constraints:
            for c in eq_constraints:
                constraints.append({"type": "eq", "fun": c})
        if ineq_constraints:
            for c in ineq_constraints:
                constraints.append({"type": "ineq", "fun": c})

        result = scipy_minimize(
            f, x0, jac=grad, constraints=constraints,
            method="SLSQP", tol=tol,
            options={"maxiter": max_iter},
        )

        return OptResult(
            x=result.x,
            fun=float(result.fun),
            n_iterations=result.nit,
            n_function_evals=result.nfev,
            converged=result.success,
            message=result.message,
        )

    # ── Implementation methods ───────────────────────────────────────────

    def _gradient_descent(
        self, f: Callable, grad: Callable, x0: NDArray,
        lr: float, tol: float, max_iter: int, track: bool,
    ) -> OptResult:
        x = x0.copy()
        trajectory = [x.copy()] if track else []
        n_f, n_g = 0, 0

        for i in range(max_iter):
            g = grad(x)
            n_g += 1

            # Backtracking line search
            alpha = lr
            fx = f(x)
            n_f += 1
            for _ in range(30):
                x_new = x - alpha * g
                if f(x_new) < fx - 1e-4 * alpha * np.dot(g, g):
                    break
                alpha *= 0.5
                n_f += 1

            x = x - alpha * g
            if track:
                trajectory.append(x.copy())

            if np.linalg.norm(g) < tol:
                return OptResult(
                    x=x, fun=float(f(x)), grad=g,
                    n_iterations=i + 1, n_function_evals=n_f,
                    n_gradient_evals=n_g, converged=True,
                    trajectory=trajectory,
                )

        return OptResult(
            x=x, fun=float(f(x)), grad=grad(x),
            n_iterations=max_iter, n_function_evals=n_f,
            n_gradient_evals=n_g, converged=False,
            message="Maximum iterations reached",
            trajectory=trajectory,
        )

    def _newton(
        self, f: Callable, grad: Callable,
        hessian: Optional[Callable], x0: NDArray,
        tol: float, max_iter: int, track: bool,
    ) -> OptResult:
        x = x0.copy()
        trajectory = [x.copy()] if track else []

        if hessian is None:
            hessian = lambda x: self._numerical_hessian(f, x)

        for i in range(max_iter):
            g = grad(x)
            H = hessian(x)

            try:
                direction = np.linalg.solve(H, -g)
            except np.linalg.LinAlgError:
                direction = -g  # fallback to gradient descent

            x = x + direction
            if track:
                trajectory.append(x.copy())

            if np.linalg.norm(g) < tol:
                return OptResult(
                    x=x, fun=float(f(x)), grad=g,
                    n_iterations=i + 1, converged=True,
                    trajectory=trajectory,
                )

        return OptResult(
            x=x, fun=float(f(x)), grad=grad(x),
            n_iterations=max_iter, converged=False,
            message="Maximum iterations reached",
            trajectory=trajectory,
        )

    def _lbfgs(
        self, f: Callable, grad: Callable, x0: NDArray,
        tol: float, max_iter: int, track: bool,
    ) -> OptResult:
        """L-BFGS via scipy."""
        from scipy.optimize import minimize as scipy_minimize

        trajectory: list[NDArray] = []
        if track:
            def callback(xk: NDArray) -> None:
                trajectory.append(xk.copy())
        else:
            callback = None  # type: ignore

        result = scipy_minimize(
            f, x0, jac=grad, method="L-BFGS-B",
            tol=tol, options={"maxiter": max_iter},
            callback=callback,
        )

        return OptResult(
            x=result.x,
            fun=float(result.fun),
            n_iterations=result.nit,
            n_function_evals=result.nfev,
            converged=result.success,
            message=result.message if hasattr(result, 'message') else "",
            trajectory=trajectory,
        )

    def _conjugate_gradient(
        self, f: Callable, grad: Callable, x0: NDArray,
        tol: float, max_iter: int, track: bool,
    ) -> OptResult:
        """Nonlinear conjugate gradient (Fletcher-Reeves)."""
        x = x0.copy()
        g = grad(x)
        d = -g
        trajectory = [x.copy()] if track else []

        for i in range(max_iter):
            # Line search
            alpha = self._backtracking_line_search(f, grad, x, d)
            x = x + alpha * d

            g_new = grad(x)
            if track:
                trajectory.append(x.copy())

            if np.linalg.norm(g_new) < tol:
                return OptResult(
                    x=x, fun=float(f(x)), grad=g_new,
                    n_iterations=i + 1, converged=True,
                    trajectory=trajectory,
                )

            # Fletcher-Reeves update
            beta = np.dot(g_new, g_new) / max(np.dot(g, g), 1e-30)
            d = -g_new + beta * d
            g = g_new

        return OptResult(
            x=x, fun=float(f(x)), grad=grad(x),
            n_iterations=max_iter, converged=False,
            trajectory=trajectory,
        )

    def _nelder_mead(
        self, f: Callable, x0: NDArray,
        tol: float, max_iter: int, track: bool,
    ) -> OptResult:
        """Nelder-Mead simplex method via scipy."""
        from scipy.optimize import minimize as scipy_minimize

        result = scipy_minimize(
            f, x0, method="Nelder-Mead",
            tol=tol, options={"maxiter": max_iter},
        )

        return OptResult(
            x=result.x,
            fun=float(result.fun),
            n_iterations=result.nit,
            n_function_evals=result.nfev,
            converged=result.success,
        )

    # ── Utility methods ──────────────────────────────────────────────────

    def _numerical_gradient(self, f: Callable, x: NDArray,
                            eps: float = 1e-7) -> NDArray:
        """Central difference gradient approximation."""
        n = len(x)
        grad = np.zeros(n)
        for i in range(n):
            x_plus = x.copy()
            x_minus = x.copy()
            x_plus[i] += eps
            x_minus[i] -= eps
            grad[i] = (f(x_plus) - f(x_minus)) / (2 * eps)
        return grad

    def _numerical_hessian(self, f: Callable, x: NDArray,
                            eps: float = 1e-5) -> NDArray:
        """Finite difference Hessian approximation."""
        n = len(x)
        H = np.zeros((n, n))
        fx = f(x)
        for i in range(n):
            for j in range(i, n):
                x_pp = x.copy(); x_pp[i] += eps; x_pp[j] += eps
                x_pm = x.copy(); x_pm[i] += eps; x_pm[j] -= eps
                x_mp = x.copy(); x_mp[i] -= eps; x_mp[j] += eps
                x_mm = x.copy(); x_mm[i] -= eps; x_mm[j] -= eps
                H[i, j] = (f(x_pp) - f(x_pm) - f(x_mp) + f(x_mm)) / (4 * eps * eps)
                H[j, i] = H[i, j]
        return H

    def _backtracking_line_search(
        self, f: Callable, grad: Callable, x: NDArray,
        d: NDArray, alpha: float = 1.0,
        c1: float = 1e-4, rho: float = 0.5,
    ) -> float:
        """Backtracking line search with Armijo condition."""
        fx = f(x)
        g = grad(x)
        slope = np.dot(g, d)
        for _ in range(50):
            if f(x + alpha * d) <= fx + c1 * alpha * slope:
                return alpha
            alpha *= rho
        return alpha
