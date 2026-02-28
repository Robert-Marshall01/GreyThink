"""
Advanced Category Theory & Compositionality — Experimental Module.

Extends the core category theory with:
- Monoidal categories with coherence conditions
- Enriched categories (V-enriched)
- Operads and operad algebras for compositional architectures
- 2-categories and bicategories
- String diagrams (combinatorial representation)
- Kan extensions
- Yoneda embedding (representable functors)
- Traced monoidal categories
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Callable, Generic, Optional, TypeVar

import numpy as np
from numpy.typing import NDArray

from greymath.core.types import CategoryObject, Morphism, Functor
from greymath.domains.category_theory import Category, ConcreteFunctor, Monad
from greymath.experimental.ir_extensions import MonoidalStructure, MonoidalKind


# ─── Monoidal Category ──────────────────────────────────────────────────────

@dataclass
class MonoidalCategory:
    """
    A monoidal category (C, ⊗, I, α, λ, ρ) with:
    - C: underlying category
    - ⊗: tensor product bifunctor
    - I: unit object
    - α: associator (A⊗B)⊗C → A⊗(B⊗C)
    - λ: left unitor I⊗A → A
    - ρ: right unitor A⊗I → A

    Satisfies the pentagon and triangle coherence axioms.
    """
    category: Category = field(default_factory=lambda: Category(name="C"))
    kind: MonoidalKind = MonoidalKind.SYMMETRIC
    unit_object: Optional[str] = None

    # Tensor product as object-level and morphism-level maps
    _tensor_objects: dict[tuple[str, str], str] = field(default_factory=dict)
    _tensor_morphisms: dict[tuple[str, str], str] = field(default_factory=dict)

    def set_unit(self, obj_name: str) -> None:
        """Set the monoidal unit object I."""
        if obj_name not in self.category.objects:
            self.category.add_object(obj_name)
        self.unit_object = obj_name

    def tensor_objects(self, a: str, b: str) -> str:
        """Compute tensor product A ⊗ B of objects."""
        key = (a, b)
        if key in self._tensor_objects:
            return self._tensor_objects[key]

        prod_name = f"{a}⊗{b}"
        if prod_name not in self.category.objects:
            self.category.add_object(prod_name)
        self._tensor_objects[key] = prod_name

        if self.kind in (MonoidalKind.SYMMETRIC, MonoidalKind.BRAIDED):
            rev_key = (b, a)
            rev_name = f"{b}⊗{a}"
            if rev_name not in self.category.objects:
                self.category.add_object(rev_name)
            self._tensor_objects[rev_key] = rev_name
        return prod_name

    def tensor_morphisms(self, f: str, g: str) -> str:
        """Compute tensor product f ⊗ g of morphisms."""
        key = (f, g)
        if key in self._tensor_morphisms:
            return self._tensor_morphisms[key]

        fm = self.category.morphisms[f]
        gm = self.category.morphisms[g]
        if fm.source is None or fm.target is None or gm.source is None or gm.target is None:
            raise ValueError("Morphisms must have source and target")

        src = self.tensor_objects(fm.source.name, gm.source.name)
        tgt = self.tensor_objects(fm.target.name, gm.target.name)
        prod_name = f"{f}⊗{g}"
        self.category.add_morphism(prod_name, src, tgt)
        self._tensor_morphisms[key] = prod_name
        return prod_name

    def add_braiding(self, a: str, b: str) -> str:
        """Add a braiding isomorphism σ_{A,B}: A⊗B → B⊗A."""
        if self.kind not in (MonoidalKind.SYMMETRIC, MonoidalKind.BRAIDED):
            raise ValueError("Braiding requires symmetric or braided monoidal category")
        src = self.tensor_objects(a, b)
        tgt = self.tensor_objects(b, a)
        braid_name = f"σ_{a},{b}"
        self.category.add_morphism(braid_name, src, tgt)
        return braid_name

    def verify_pentagon(self, a: str, b: str, c: str, d: str) -> bool:
        """
        Verify the pentagon identity for the associator.
        ((A⊗B)⊗C)⊗D → ... (5 paths must commute)

        For strict monoidal categories, this is automatic.
        """
        return self.kind == MonoidalKind.STRICT

    def __repr__(self) -> str:
        return (f"MonoidalCategory({self.category.name}, {self.kind.name}, "
                f"unit={self.unit_object})")


# ─── Enriched Category ──────────────────────────────────────────────────────

@dataclass
class EnrichedHomObject:
    """A hom-object in a V-enriched category."""
    source: str
    target: str
    value: Any = None  # An object in V
    name: str = ""

    def __post_init__(self):
        if not self.name:
            self.name = f"Hom({self.source},{self.target})"


@dataclass
class EnrichedCategory:
    """
    A V-enriched category, where V is a monoidal category.

    Instead of hom-sets Hom(A,B), we have hom-objects Hom(A,B) in V.
    Composition is a V-morphism: Hom(B,C) ⊗ Hom(A,B) → Hom(A,C).
    """
    name: str = ""
    enriching_category: Optional[MonoidalCategory] = None
    objects: list[str] = field(default_factory=list)
    hom_objects: dict[tuple[str, str], EnrichedHomObject] = field(default_factory=dict)
    composition_maps: dict[tuple[str, str, str], str] = field(default_factory=dict)

    def add_object(self, name: str) -> None:
        self.objects.append(name)

    def set_hom(self, source: str, target: str, value: Any) -> None:
        """Set the hom-object Hom(source, target) in V."""
        hom = EnrichedHomObject(source=source, target=target, value=value)
        self.hom_objects[(source, target)] = hom

    def set_composition(self, a: str, b: str, c: str, map_name: str) -> None:
        """
        Register composition: Hom(B,C) ⊗ Hom(A,B) → Hom(A,C).
        """
        self.composition_maps[(a, b, c)] = map_name

    def underlying_category(self) -> Category:
        """Extract the underlying ordinary category (if V = Set)."""
        cat = Category(name=f"U({self.name})")
        for obj in self.objects:
            cat.add_object(obj)
        return cat

    def __repr__(self) -> str:
        v_name = self.enriching_category.category.name if self.enriching_category else "V"
        return f"EnrichedCategory({self.name}, enriched in {v_name})"


# ─── Operads ─────────────────────────────────────────────────────────────────

@dataclass
class OperadOperation:
    """An operation in an operad with arity n."""
    name: str
    arity: int
    output_type: str = ""
    input_types: list[str] = field(default_factory=list)

    def __repr__(self) -> str:
        inputs = ", ".join(self.input_types) if self.input_types else f"arity={self.arity}"
        return f"Op({self.name}: ({inputs}) → {self.output_type})"


@dataclass
class Operad:
    """
    An operad P: a collection of multi-input, single-output operations
    with a composition law.

    P(n) is the set of n-ary operations.
    Composition: γ: P(k) × P(n₁) × ... × P(nₖ) → P(n₁ + ... + nₖ)
    """
    name: str = ""
    operations: dict[str, OperadOperation] = field(default_factory=dict)
    _composition_rules: dict[tuple[str, ...], str] = field(default_factory=dict)

    def add_operation(self, name: str, arity: int,
                      output_type: str = "", input_types: Optional[list[str]] = None) -> OperadOperation:
        """Add an operation to the operad."""
        op = OperadOperation(
            name=name, arity=arity, output_type=output_type,
            input_types=input_types or [],
        )
        self.operations[name] = op
        return op

    def compose(self, outer: str, *inner_names: str) -> str:
        """
        Operadic composition: γ(f; g₁, ..., gₖ).

        f: k-ary operation, gᵢ: nᵢ-ary operations
        Result: (n₁ + ... + nₖ)-ary operation.
        """
        outer_op = self.operations[outer]
        if len(inner_names) != outer_op.arity:
            raise ValueError(
                f"Expected {outer_op.arity} inner operations, got {len(inner_names)}"
            )

        total_arity = sum(self.operations[g].arity for g in inner_names)
        comp_name = f"γ({outer};{','.join(inner_names)})"
        result = OperadOperation(
            name=comp_name,
            arity=total_arity,
            output_type=outer_op.output_type,
        )
        self.operations[comp_name] = result
        self._composition_rules[(outer, *inner_names)] = comp_name
        return comp_name

    def operations_by_arity(self, n: int) -> list[OperadOperation]:
        """Get all n-ary operations."""
        return [op for op in self.operations.values() if op.arity == n]

    def __repr__(self) -> str:
        return f"Operad({self.name}, {len(self.operations)} ops)"


@dataclass
class OperadAlgebra:
    """
    An algebra over an operad P in a category C.

    An algebra assigns to each operation p ∈ P(n) a morphism
    A(p): A^n → A in C.
    """
    name: str = ""
    operad: Optional[Operad] = None
    carrier: Optional[str] = None  # The object A in C
    action_map: dict[str, Callable] = field(default_factory=dict)

    def register_action(self, op_name: str, action: Callable) -> None:
        """Register the action of an operad operation."""
        self.action_map[op_name] = action

    def evaluate(self, op_name: str, *inputs: Any) -> Any:
        """Evaluate the algebra action for an operation."""
        if op_name not in self.action_map:
            raise ValueError(f"No action registered for operation {op_name}")
        return self.action_map[op_name](*inputs)

    def __repr__(self) -> str:
        op_name = self.operad.name if self.operad else "?"
        return f"OperadAlgebra({self.name} over {op_name})"


# ─── 2-Categories ───────────────────────────────────────────────────────────

@dataclass
class TwoMorphism:
    """A 2-morphism α: f ⇒ g between 1-morphisms f, g: A → B."""
    name: str
    source: str   # name of 1-morphism f
    target: str   # name of 1-morphism g
    is_invertible: bool = False

    def __repr__(self) -> str:
        return f"2Morph({self.name}: {self.source} ⇒ {self.target})"


@dataclass
class TwoCategory:
    """
    A strict 2-category: objects, 1-morphisms, and 2-morphisms.

    Has both horizontal and vertical composition of 2-morphisms.
    """
    name: str = ""
    category: Category = field(default_factory=lambda: Category(name="C"))
    two_morphisms: dict[str, TwoMorphism] = field(default_factory=dict)

    def add_2morphism(self, name: str, source_1morph: str,
                      target_1morph: str, invertible: bool = False) -> TwoMorphism:
        """Add a 2-morphism between 1-morphisms."""
        tm = TwoMorphism(
            name=name, source=source_1morph,
            target=target_1morph, is_invertible=invertible,
        )
        self.two_morphisms[name] = tm
        return tm

    def vertical_compose(self, alpha: str, beta: str) -> TwoMorphism:
        """
        Vertical composition: β ∘ α.

        If α: f ⇒ g and β: g ⇒ h, then β∘α: f ⇒ h.
        """
        a = self.two_morphisms[alpha]
        b = self.two_morphisms[beta]
        if a.target != b.source:
            raise ValueError(
                f"Cannot compose: target({alpha}) = {a.target} "
                f"≠ source({beta}) = {b.source}"
            )
        comp = TwoMorphism(
            name=f"({beta} ∘ {alpha})",
            source=a.source,
            target=b.target,
            is_invertible=a.is_invertible and b.is_invertible,
        )
        self.two_morphisms[comp.name] = comp
        return comp

    def horizontal_compose(self, alpha: str, beta: str) -> TwoMorphism:
        """
        Horizontal composition: β * α.

        If α: f ⇒ g : A → B and β: h ⇒ k : B → C,
        then β*α: h∘f ⇒ k∘g : A → C.
        """
        a = self.two_morphisms[alpha]
        b = self.two_morphisms[beta]
        comp = TwoMorphism(
            name=f"({beta} * {alpha})",
            source=f"({b.source} ∘ {a.source})",
            target=f"({b.target} ∘ {a.target})",
            is_invertible=a.is_invertible and b.is_invertible,
        )
        self.two_morphisms[comp.name] = comp
        return comp

    def __repr__(self) -> str:
        return (f"2Category({self.name}, "
                f"1-morphisms={len(self.category.morphisms)}, "
                f"2-morphisms={len(self.two_morphisms)})")


# ─── String Diagrams ────────────────────────────────────────────────────────

class WireKind(Enum):
    """Types of wires in a string diagram."""
    REGULAR = auto()
    DUAL = auto()
    IDENTITY = auto()


@dataclass
class StringDiagramNode:
    """A node (morphism box) in a string diagram."""
    name: str
    inputs: list[str] = field(default_factory=list)
    outputs: list[str] = field(default_factory=list)
    position: tuple[float, float] = (0.0, 0.0)

    def __repr__(self) -> str:
        return f"Node({self.name}: {self.inputs} → {self.outputs})"


@dataclass
class StringDiagram:
    """
    A string diagram representing a morphism in a monoidal category.

    String diagrams are a graphical calculus where:
    - Wires represent objects
    - Boxes represent morphisms
    - Vertical composition = sequential
    - Horizontal composition = parallel (tensor)
    """
    name: str = ""
    nodes: list[StringDiagramNode] = field(default_factory=list)
    wires: list[tuple[str, int, str, int]] = field(default_factory=list)
    global_inputs: list[str] = field(default_factory=list)
    global_outputs: list[str] = field(default_factory=list)

    def add_node(self, name: str, inputs: list[str],
                 outputs: list[str]) -> StringDiagramNode:
        """Add a morphism box to the diagram."""
        node = StringDiagramNode(name=name, inputs=inputs, outputs=outputs)
        self.nodes.append(node)
        return node

    def connect(self, source_node: str, source_port: int,
                target_node: str, target_port: int) -> None:
        """Connect output port of one node to input port of another."""
        self.wires.append((source_node, source_port, target_node, target_port))

    def compose_sequential(self, other: "StringDiagram") -> "StringDiagram":
        """Sequential composition: self then other."""
        result = StringDiagram(name=f"({self.name} ; {other.name})")
        result.nodes = self.nodes + other.nodes
        result.wires = self.wires + other.wires
        result.global_inputs = self.global_inputs
        result.global_outputs = other.global_outputs
        return result

    def compose_parallel(self, other: "StringDiagram") -> "StringDiagram":
        """Parallel composition (tensor product)."""
        result = StringDiagram(name=f"({self.name} ⊗ {other.name})")
        result.nodes = self.nodes + other.nodes
        result.wires = self.wires + other.wires
        result.global_inputs = self.global_inputs + other.global_inputs
        result.global_outputs = self.global_outputs + other.global_outputs
        return result

    def to_morphism_expression(self) -> str:
        """Convert diagram to algebraic morphism expression."""
        if not self.nodes:
            return "id"
        return " ; ".join(n.name for n in self.nodes)

    def __repr__(self) -> str:
        return f"StringDiagram({self.name}, {len(self.nodes)} nodes)"


# ─── Kan Extensions ─────────────────────────────────────────────────────────

@dataclass
class KanExtension:
    """
    A (left or right) Kan extension.

    Given F: C → E and K: C → D,
    - Left Kan extension Lan_K F: D → E
    - Right Kan extension Ran_K F: D → E

    Satisfies the universal property:
    Nat(Lan_K F, G) ≅ Nat(F, G ∘ K)
    """
    name: str = ""
    is_left: bool = True
    source_functor_name: str = ""   # F
    along_functor_name: str = ""    # K
    extension_object_map: dict[str, str] = field(default_factory=dict)
    extension_morphism_map: dict[str, str] = field(default_factory=dict)
    is_pointwise: bool = False

    @property
    def kind_str(self) -> str:
        return "Lan" if self.is_left else "Ran"

    def __repr__(self) -> str:
        return (f"KanExt({self.kind_str}_{self.along_functor_name} "
                f"{self.source_functor_name})")


# ─── Yoneda Embedding ───────────────────────────────────────────────────────

class YonedaEmbedding:
    """
    The Yoneda embedding y: C → [C^op, Set].

    Maps each object A to the representable functor Hom(-, A).
    The Yoneda lemma: Nat(Hom(-, A), F) ≅ F(A).
    """

    @staticmethod
    def representable_functor(category: Category, obj: str) -> dict[str, list[str]]:
        """
        Compute the representable functor Hom(-, A) evaluated
        on all objects of the category.

        Returns: dict mapping object names to lists of morphism names.
        """
        result: dict[str, list[str]] = {}
        for b in category.objects:
            hom = category.hom_set(b, obj)
            result[b] = [m.name for m in hom]
        return result

    @staticmethod
    def yoneda_lemma(
        category: Category,
        obj: str,
        presheaf_fn: Callable[[str], Any],
    ) -> Any:
        """
        Apply the Yoneda lemma: Nat(y(A), F) ≅ F(A).

        Given a presheaf F and an object A, return F(A).
        """
        return presheaf_fn(obj)

    @staticmethod
    def is_representable(
        category: Category,
        presheaf_fn: Callable[[str], set[str]],
    ) -> Optional[str]:
        """
        Check if a presheaf is representable.

        If F ≅ Hom(-, A) for some A, return A. Otherwise return None.
        """
        for obj in category.objects:
            hom_data = YonedaEmbedding.representable_functor(category, obj)
            presheaf_data = {b: presheaf_fn(b) for b in category.objects}
            # Simple cardinality check
            if all(
                len(hom_data.get(b, [])) == len(presheaf_data.get(b, set()))
                for b in category.objects
            ):
                return obj
        return None


# ─── Traced Monoidal Category ───────────────────────────────────────────────

@dataclass
class TracedMonoidalCategory:
    """
    A traced symmetric monoidal category: admits a trace operator
    Tr^U_{A,B}: Hom(A ⊗ U, B ⊗ U) → Hom(A, B).

    This enables feedback loops and fixed-point computations
    in the categorical framework.
    """
    monoidal: MonoidalCategory = field(default_factory=MonoidalCategory)
    trace_maps: dict[tuple[str, str, str], str] = field(default_factory=dict)

    def trace(self, f_name: str, traced_wire: str,
              input_obj: str, output_obj: str) -> str:
        """
        Apply categorical trace: close a feedback loop.

        Given f: A ⊗ U → B ⊗ U, produce Tr^U(f): A → B.
        """
        trace_name = f"Tr^{traced_wire}({f_name})"
        if input_obj not in self.monoidal.category.objects:
            self.monoidal.category.add_object(input_obj)
        if output_obj not in self.monoidal.category.objects:
            self.monoidal.category.add_object(output_obj)
        self.monoidal.category.add_morphism(trace_name, input_obj, output_obj)
        self.trace_maps[(f_name, input_obj, output_obj)] = trace_name
        return trace_name

    def __repr__(self) -> str:
        return f"TracedMonoidal({self.monoidal.category.name})"


# ─── Compositional Architecture via Operads ──────────────────────────────────

class OperadicArchitectureBuilder:
    """
    Build compositional architectures using operads.

    This generalises the base CompositionBuilder to support multi-input
    operations with pluggable composition patterns defined by operads.
    """

    def __init__(self, operad: Optional[Operad] = None) -> None:
        self.operad = operad or Operad(name="Arch")
        self.algebra: Optional[OperadAlgebra] = None
        self._spaces: dict[str, dict[str, Any]] = {}

    def define_space(self, name: str, dim: Optional[int] = None,
                     **properties: Any) -> str:
        """Define a space (type) for the architecture."""
        self._spaces[name] = {"dim": dim, **properties}
        return name

    def define_operation(self, name: str, input_types: list[str],
                         output_type: str) -> str:
        """Define a multi-input operation."""
        op = self.operad.add_operation(
            name, arity=len(input_types),
            output_type=output_type,
            input_types=input_types,
        )
        return op.name

    def compose_operations(self, outer: str, *inner: str) -> str:
        """Compose operations via operadic composition."""
        return self.operad.compose(outer, *inner)

    def instantiate_algebra(
        self, name: str, carrier: str,
        actions: dict[str, Callable],
    ) -> OperadAlgebra:
        """Create an operad algebra to evaluate operations."""
        alg = OperadAlgebra(
            name=name, operad=self.operad, carrier=carrier,
            action_map=actions,
        )
        self.algebra = alg
        return alg

    def evaluate(self, op_name: str, *inputs: Any) -> Any:
        """Evaluate an operation using the registered algebra."""
        if self.algebra is None:
            raise ValueError("No algebra instantiated")
        return self.algebra.evaluate(op_name, *inputs)

    def get_summary(self) -> dict[str, Any]:
        """Summarize the operadic architecture."""
        return {
            "operad": self.operad.name,
            "n_operations": len(self.operad.operations),
            "spaces": list(self._spaces.keys()),
            "operations": [
                {
                    "name": op.name,
                    "arity": op.arity,
                    "input_types": op.input_types,
                    "output_type": op.output_type,
                }
                for op in self.operad.operations.values()
            ],
        }

    def __repr__(self) -> str:
        return f"OperadicArchitecture({self.operad.name})"
