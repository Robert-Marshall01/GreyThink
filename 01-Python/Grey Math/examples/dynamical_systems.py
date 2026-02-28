"""
Grey Math — Example: Dynamical Systems & Lorenz Attractor.

Demonstrates ODE solving, dynamical systems analysis,
and Lyapunov exponent computation.
"""

import numpy as np

from greymath.numeric.ode import ODESolver, ODEMethod
from greymath.domains.dynamical_systems import DynamicalSystem, lorenz_system


def main():
    print("═══ Grey Math: Dynamical Systems Example ═══\n")

    # ── Lorenz System ──────────────────────────────────────────────────────
    print("── Lorenz System ──")
    lorenz = lorenz_system()
    print(f"System: {lorenz.name}")
    print(f"Dimension: {lorenz.dimension}")
    print(f"Parameters: {lorenz.params}")

    # Compute orbit
    y0 = np.array([1.0, 1.0, 1.0])
    t_span = (0.0, 50.0)
    dt = 0.01

    print(f"\nIntegrating from t={t_span[0]} to t={t_span[1]}, dt={dt}")
    orbit = lorenz.orbit(y0, t_span, dt)
    print(f"Orbit length: {len(orbit)} points")

    # Statistics
    orbit_arr = np.array(orbit)
    print(f"x range: [{orbit_arr[:,0].min():.2f}, {orbit_arr[:,0].max():.2f}]")
    print(f"y range: [{orbit_arr[:,1].min():.2f}, {orbit_arr[:,1].max():.2f}]")
    print(f"z range: [{orbit_arr[:,2].min():.2f}, {orbit_arr[:,2].max():.2f}]")

    # ── Fixed Points ───────────────────────────────────────────────────────
    print("\n── Fixed Point Analysis ──")

    # Lorenz has 3 fixed points for standard parameters
    origins = [
        np.array([0.0, 0.0, 0.0]),
        np.array([8.0, 8.0, 27.0]),
        np.array([-8.0, -8.0, 27.0]),
    ]

    for x0 in origins:
        fp = lorenz.find_fixed_point(x0)
        if fp is not None:
            classification = lorenz.classify_fixed_point(fp)
            print(f"  Fixed point near {x0}: {np.round(fp, 4)}")
            print(f"    Type: {classification.fp_type.name}")
            print(f"    Stable: {classification.stable}")

    # ── Lyapunov Exponents ─────────────────────────────────────────────────
    print("\n── Lyapunov Exponents ──")
    lyap = lorenz.lyapunov_exponents(y0, t_total=20.0, dt=0.01)
    print(f"Lyapunov exponents: {np.round(lyap, 4)}")
    print(f"Largest: {lyap[0]:.4f}")
    if lyap[0] > 0:
        print("→ System is CHAOTIC (positive largest Lyapunov exponent)")
    else:
        print("→ System is non-chaotic")

    # ── Simple ODE: Harmonic Oscillator ────────────────────────────────────
    print("\n── Harmonic Oscillator (ODE Solver) ──")
    solver = ODESolver()

    def harmonic(t: float, y: np.ndarray) -> np.ndarray:
        """dy/dt = [y[1], -y[0]]  (simple harmonic oscillator)"""
        return np.array([y[1], -y[0]])

    y0_osc = np.array([1.0, 0.0])  # x=1, v=0 at t=0
    sol = solver.solve(harmonic, y0_osc, (0.0, 2 * np.pi), dt=0.01, method=ODEMethod.RK4)

    print(f"Method: {sol.method}")
    print(f"Time steps: {len(sol.t)}")
    print(f"Final state: x={sol.y[-1, 0]:.6f}, v={sol.y[-1, 1]:.6f}")
    print(f"Expected:    x=1.000000, v=0.000000 (periodic)")
    print(f"Error: {np.linalg.norm(sol.y[-1] - y0_osc):.2e}")

    # Compare methods
    print("\n── Method Comparison ──")
    for method in [ODEMethod.EULER, ODEMethod.RK4, ODEMethod.RK45]:
        sol_m = solver.solve(harmonic, y0_osc, (0.0, 2 * np.pi), dt=0.01, method=method)
        error = np.linalg.norm(sol_m.y[-1] - y0_osc)
        print(f"  {method.name:<10} final error: {error:.2e}  steps: {len(sol_m.t)}")

    print("\n✓ Dynamical systems analysis complete.")


if __name__ == "__main__":
    main()
