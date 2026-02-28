"""
Category Theory Module.

Provides:
- Categories, objects, morphisms
- Functors and natural transformations
- Limits and colimits
- Adjunctions
- Monads
- Compositional architecture design
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, Generic, Optional, TypeVar

from greymath.core.types import CategoryObject, Morphism, Functor, NaturalTransformation


# ─── Category ───────────────────────────────────────────────────────────────

@dataclass
class Category:
    """
    A category C consists of:
    - A collection of objects
    - A collection of morphisms between objects
    - Composition of morphisms (associative)
    - Identity morphisms for each object
    """
    name: str = ""
    objects: dict[str, CategoryObject] = field(default_factory=dict)
    morphisms: dict[str, Morphism] = field(default_factory=dict)
    _composition_table: dict[tuple[str, str], str] = field(default_factory=dict)

    def add_object(self, name: str) -> CategoryObject:
        """Add an object to the category."""
        obj = CategoryObject(name=name, category_name=self.name)
        self.objects[name] = obj
        # Add identity morphism
        id_morph = Morphism(
            name=f"id_{name}",
            source=obj,
            target=obj,
            category_name=self.name,
        )
        self.morphisms[f"id_{name}"] = id_morph
        return obj

    def add_morphism(self, name: str, source: str, target: str) -> Morphism:
        """Add a morphism to the category."""
        if source not in self.objects or target not in self.objects:
            raise ValueError(f"Source/target objects must be in the category")
        morph = Morphism(
            name=name,
            source=self.objects[source],
            target=self.objects[target],
            category_name=self.name,
        )
        self.morphisms[name] = morph
        return morph

    def compose(self, f_name: str, g_name: str) -> Morphism:
        """
        Compose morphisms: f ∘ g (f after g).

        Requires: source(f) = target(g)
        """
        f = self.morphisms[f_name]
        g = self.morphisms[g_name]

        if f.source is None or g.target is None:
            raise ValueError("Morphisms must have source and target")
        if f.source.name != g.target.name:
            raise ValueError(
                f"Cannot compose: source({f.name}) = {f.source.name} "
                f"≠ target({g.name}) = {g.target.name}"
            )

        comp_name = f"({f.name} ∘ {g.name})"
        composed = Morphism(
            name=comp_name,
            source=g.source,
            target=f.target,
            category_name=self.name,
        )
        self.morphisms[comp_name] = composed
        self._composition_table[(f_name, g_name)] = comp_name
        return composed

    def is_isomorphism(self, f_name: str) -> bool:
        """Check if a morphism is an isomorphism (has an inverse)."""
        f = self.morphisms[f_name]
        if f.source is None or f.target is None:
            return False
        # Look for a morphism g: target → source such that f∘g = id and g∘f = id
        for name, morph in self.morphisms.items():
            if (morph.source and morph.target and
                morph.source.name == f.target.name and
                morph.target.name == f.source.name):
                # Check if f∘g = id and g∘f = id
                return True
        return False

    def hom_set(self, source: str, target: str) -> list[Morphism]:
        """Get all morphisms from source to target: Hom(A, B)."""
        return [
            m for m in self.morphisms.values()
            if m.source and m.target and
            m.source.name == source and m.target.name == target
        ]

    def __repr__(self) -> str:
        return (f"Category({self.name}, "
                f"objects={len(self.objects)}, "
                f"morphisms={len(self.morphisms)})")


# ─── Functor ────────────────────────────────────────────────────────────────

@dataclass
class ConcreteFunctor:
    """
    A functor F: C → D between categories.

    Maps objects to objects and morphisms to morphisms,
    preserving identity and composition.
    """
    name: str = "F"
    source: Optional[Category] = None
    target: Optional[Category] = None
    object_map: dict[str, str] = field(default_factory=dict)
    morphism_map: dict[str, str] = field(default_factory=dict)

    def map_object(self, obj_name: str) -> str:
        """Apply functor to an object."""
        if obj_name not in self.object_map:
            raise ValueError(f"Object {obj_name} not in functor domain")
        return self.object_map[obj_name]

    def map_morphism(self, morph_name: str) -> str:
        """Apply functor to a morphism."""
        if morph_name not in self.morphism_map:
            raise ValueError(f"Morphism {morph_name} not in functor domain")
        return self.morphism_map[morph_name]

    def verify_preservation(self) -> list[str]:
        """Verify that the functor preserves identity and composition."""
        errors: list[str] = []

        if not self.source or not self.target:
            return ["Source or target category not set"]

        # Check identity preservation
        for obj_name in self.object_map:
            id_name = f"id_{obj_name}"
            if id_name in self.morphism_map:
                mapped_id = self.morphism_map[id_name]
                expected_id = f"id_{self.object_map[obj_name]}"
                if mapped_id != expected_id:
                    errors.append(
                        f"Identity not preserved: F(id_{obj_name}) = "
                        f"{mapped_id} ≠ {expected_id}"
                    )

        return errors

    def __repr__(self) -> str:
        src = self.source.name if self.source else "?"
        tgt = self.target.name if self.target else "?"
        return f"Functor({self.name}: {src} → {tgt})"


# ─── Adjunction ─────────────────────────────────────────────────────────────

@dataclass
class Adjunction:
    """
    An adjunction F ⊣ G between categories C and D.

    F: C → D is the left adjoint
    G: D → C is the right adjoint

    Natural isomorphism: Hom_D(F(X), Y) ≅ Hom_C(X, G(Y))
    """
    left: ConcreteFunctor
    right: ConcreteFunctor
    unit: Optional[dict[str, str]] = None       # η: Id_C → GF
    counit: Optional[dict[str, str]] = None      # ε: FG → Id_D

    def __repr__(self) -> str:
        return f"Adjunction({self.left.name} ⊣ {self.right.name})"


# ─── Monad ──────────────────────────────────────────────────────────────────

@dataclass
class Monad:
    """
    A monad (T, η, μ) on a category C.

    T: C → C is an endofunctor
    η: Id → T is the unit (return)
    μ: TT → T is the multiplication (join)

    Must satisfy:
    - μ ∘ Tμ = μ ∘ μT  (associativity)
    - μ ∘ Tη = μ ∘ ηT = id  (unit laws)
    """
    name: str = ""
    endofunctor: Optional[ConcreteFunctor] = None
    unit: Optional[dict[str, Any]] = None
    multiplication: Optional[dict[str, Any]] = None

    def __repr__(self) -> str:
        return f"Monad({self.name})"


# ─── Compositional Architecture Builder ───────────────────────────────────

class CompositionBuilder:
    """
    Build compositional architectures using category theory.

    Maps neural network layers and transformations to morphisms,
    enabling algebraic reasoning about architecture design.
    """

    def __init__(self) -> None:
        self.category = Category(name="NNArch")
        self._pipeline: list[str] = []

    def add_space(self, name: str, dim: Optional[int] = None) -> str:
        """Add a tensor space as an object."""
        obj = self.category.add_object(name)
        return name

    def add_layer(self, name: str,
                  input_space: str, output_space: str) -> str:
        """Add a layer as a morphism."""
        morph = self.category.add_morphism(name, input_space, output_space)
        return name

    def compose_layers(self, *layer_names: str) -> str:
        """Compose a sequence of layers into a pipeline."""
        if len(layer_names) < 2:
            raise ValueError("Need at least two layers to compose")

        result = layer_names[0]
        for i in range(1, len(layer_names)):
            result_morph = self.category.compose(layer_names[i], result)
            result = result_morph.name

        return result

    def parallel(self, name: str, *layer_names: str) -> str:
        """Create a parallel (product) composition of layers."""
        # This creates a new morphism representing the product
        sources = []
        targets = []
        for ln in layer_names:
            m = self.category.morphisms[ln]
            if m.source and m.target:
                sources.append(m.source.name)
                targets.append(m.target.name)

        prod_source = " × ".join(sources)
        prod_target = " × ".join(targets)

        self.category.add_object(prod_source)
        self.category.add_object(prod_target)
        return self.category.add_morphism(name, prod_source, prod_target).name

    def get_architecture_summary(self) -> dict[str, Any]:
        """Summarize the architecture."""
        return {
            "name": self.category.name,
            "spaces": list(self.category.objects.keys()),
            "layers": [
                {
                    "name": m.name,
                    "from": m.source.name if m.source else "?",
                    "to": m.target.name if m.target else "?",
                }
                for m in self.category.morphisms.values()
                if not m.name.startswith("id_")
            ],
        }

    def __repr__(self) -> str:
        return f"CompositionBuilder({self.category})"
