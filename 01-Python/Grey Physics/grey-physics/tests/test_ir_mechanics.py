"""Tests for Core IR mechanics module."""
import pytest

from grey_physics.core.ir.types import Expr
from grey_physics.core.ir.mechanics import Lagrangian, Hamiltonian, ActionFunctional


class TestLagrangian:
    def test_free_particle(self):
        L = Lagrangian.free_particle(mass=2.0)
        assert L.name == "free_particle"
        assert "m" in L.parameters

    def test_harmonic_oscillator(self):
        L = Lagrangian.harmonic_oscillator(mass=1.0, omega=2.0)
        assert "omega" in L.parameters

    def test_klein_gordon(self):
        L = Lagrangian.klein_gordon(mass=0.5)
        assert L.name == "klein_gordon"


class TestHamiltonian:
    def test_from_lagrangian(self):
        L = Lagrangian.free_particle(mass=1.0)
        H = Hamiltonian.from_lagrangian(L)
        assert H is not None
        assert H.name.startswith("H_")


class TestActionFunctional:
    def test_euler_lagrange(self):
        L = Lagrangian.harmonic_oscillator(mass=1.0, omega=1.0)
        S = ActionFunctional(lagrangian=L, name="S_sho")
        el = S.euler_lagrange_lhs()
        assert el is not None
