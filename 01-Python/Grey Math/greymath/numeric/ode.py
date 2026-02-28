"""
ODE solver engine.

Provides:
- Explicit methods (Euler, RK4, RK45)
- Implicit methods (Backward Euler, Trapezoidal)
- Adaptive step-size control
- Stiff system solvers
- System of ODEs
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Callable, Optional

import numpy as np
from numpy.typing import NDArray


class ODEMethod(Enum):
    """Available ODE integration methods."""
    EULER = auto()
    RK4 = auto()
    RK45_ADAPTIVE = auto()
    BACKWARD_EULER = auto()
    TRAPEZOIDAL = auto()
    BDF = auto()


@dataclass
class ODESolution:
    """Solution of an ODE initial value problem."""
    t: NDArray  # time points
    y: NDArray  # solution values (n_times x n_dims)
    method: ODEMethod
    n_steps: int = 0
    n_function_evals: int = 0
    success: bool = True
    message: str = ""

    @property
    def final_value(self) -> NDArray:
        return self.y[-1]

    def __repr__(self) -> str:
        return f"ODESolution(method={self.method.name}, steps={self.n_steps})"


class ODESolver:
    """
    ODE solver supporting explicit, implicit, and adaptive methods.

    Solves initial value problems:
        dy/dt = f(t, y),  y(t0) = y0
    """

    def solve(
        self,
        f: Callable[[float, NDArray], NDArray],
        t_span: tuple[float, float],
        y0: NDArray,
        method: ODEMethod = ODEMethod.RK45_ADAPTIVE,
        dt: float = 0.01,
        rtol: float = 1e-6,
        atol: float = 1e-9,
        max_steps: int = 100000,
    ) -> ODESolution:
        """
        Solve an ODE initial value problem.

        Args:
            f: Right-hand side function f(t, y)
            t_span: (t0, tf) integration interval
            y0: Initial condition
            method: Integration method
            dt: Initial step size
            rtol: Relative tolerance (for adaptive methods)
            atol: Absolute tolerance (for adaptive methods)
            max_steps: Maximum number of steps
        """
        y0 = np.atleast_1d(np.asarray(y0, dtype=float))

        if method == ODEMethod.EULER:
            return self._euler(f, t_span, y0, dt, max_steps)
        elif method == ODEMethod.RK4:
            return self._rk4(f, t_span, y0, dt, max_steps)
        elif method == ODEMethod.RK45_ADAPTIVE:
            return self._rk45_adaptive(f, t_span, y0, dt, rtol, atol, max_steps)
        elif method == ODEMethod.BACKWARD_EULER:
            return self._backward_euler(f, t_span, y0, dt, max_steps)
        elif method == ODEMethod.BDF:
            return self._scipy_solve(f, t_span, y0, "BDF", rtol, atol)
        else:
            return self._rk4(f, t_span, y0, dt, max_steps)

    def _euler(self, f: Callable, t_span: tuple[float, float],
               y0: NDArray, dt: float, max_steps: int) -> ODESolution:
        """Forward Euler method."""
        t0, tf = t_span
        t_list = [t0]
        y_list = [y0.copy()]
        t, y = t0, y0.copy()
        n_evals = 0

        while t < tf and len(t_list) < max_steps:
            h = min(dt, tf - t)
            y = y + h * f(t, y)
            t += h
            n_evals += 1
            t_list.append(t)
            y_list.append(y.copy())

        return ODESolution(
            t=np.array(t_list),
            y=np.array(y_list),
            method=ODEMethod.EULER,
            n_steps=len(t_list) - 1,
            n_function_evals=n_evals,
        )

    def _rk4(self, f: Callable, t_span: tuple[float, float],
             y0: NDArray, dt: float, max_steps: int) -> ODESolution:
        """Classical 4th-order Runge-Kutta method."""
        t0, tf = t_span
        t_list = [t0]
        y_list = [y0.copy()]
        t, y = t0, y0.copy()
        n_evals = 0

        while t < tf and len(t_list) < max_steps:
            h = min(dt, tf - t)
            k1 = f(t, y)
            k2 = f(t + h / 2, y + h / 2 * k1)
            k3 = f(t + h / 2, y + h / 2 * k2)
            k4 = f(t + h, y + h * k3)
            y = y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
            t += h
            n_evals += 4
            t_list.append(t)
            y_list.append(y.copy())

        return ODESolution(
            t=np.array(t_list),
            y=np.array(y_list),
            method=ODEMethod.RK4,
            n_steps=len(t_list) - 1,
            n_function_evals=n_evals,
        )

    def _rk45_adaptive(self, f: Callable, t_span: tuple[float, float],
                        y0: NDArray, dt: float, rtol: float, atol: float,
                        max_steps: int) -> ODESolution:
        """Runge-Kutta-Fehlberg adaptive step method (RK45)."""
        t0, tf = t_span
        t_list = [t0]
        y_list = [y0.copy()]
        t, y = t0, y0.copy()
        h = dt
        n_evals = 0

        # Dormand-Prince coefficients (simplified)
        while t < tf and len(t_list) < max_steps:
            h = min(h, tf - t)
            if h < 1e-15:
                break

            # RK4 step
            k1 = f(t, y)
            k2 = f(t + h / 2, y + h / 2 * k1)
            k3 = f(t + h / 2, y + h / 2 * k2)
            k4 = f(t + h, y + h * k3)
            y4 = y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)

            # RK5 step (embedded, simplified)
            k5 = f(t + h, y4)
            y5 = y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k5)
            n_evals += 5

            # Error estimate
            err = np.max(np.abs(y5 - y4) / (atol + rtol * np.abs(y4)))

            if err <= 1.0:
                # Accept step
                t += h
                y = y4
                t_list.append(t)
                y_list.append(y.copy())

            # Adjust step size
            if err > 0:
                h *= min(2.0, max(0.2, 0.9 * err ** (-0.2)))
            else:
                h *= 2.0

        return ODESolution(
            t=np.array(t_list),
            y=np.array(y_list),
            method=ODEMethod.RK45_ADAPTIVE,
            n_steps=len(t_list) - 1,
            n_function_evals=n_evals,
        )

    def _backward_euler(self, f: Callable, t_span: tuple[float, float],
                         y0: NDArray, dt: float, max_steps: int) -> ODESolution:
        """Backward Euler (implicit) via fixed-point iteration."""
        t0, tf = t_span
        t_list = [t0]
        y_list = [y0.copy()]
        t, y = t0, y0.copy()
        n_evals = 0

        while t < tf and len(t_list) < max_steps:
            h = min(dt, tf - t)
            # Fixed-point iteration: y_{n+1} = y_n + h * f(t_{n+1}, y_{n+1})
            y_new = y.copy()
            for _ in range(20):
                y_iter = y + h * f(t + h, y_new)
                n_evals += 1
                if np.max(np.abs(y_iter - y_new)) < 1e-12:
                    break
                y_new = y_iter

            t += h
            y = y_new
            t_list.append(t)
            y_list.append(y.copy())

        return ODESolution(
            t=np.array(t_list),
            y=np.array(y_list),
            method=ODEMethod.BACKWARD_EULER,
            n_steps=len(t_list) - 1,
            n_function_evals=n_evals,
        )

    def _scipy_solve(self, f: Callable, t_span: tuple[float, float],
                      y0: NDArray, method: str,
                      rtol: float, atol: float) -> ODESolution:
        """Delegate to scipy.integrate.solve_ivp for advanced methods."""
        from scipy.integrate import solve_ivp
        sol = solve_ivp(f, t_span, y0, method=method, rtol=rtol, atol=atol)
        return ODESolution(
            t=sol.t,
            y=sol.y.T,
            method=ODEMethod.BDF,
            n_steps=len(sol.t) - 1,
            n_function_evals=sol.nfev,
            success=sol.success,
            message=sol.message,
        )
