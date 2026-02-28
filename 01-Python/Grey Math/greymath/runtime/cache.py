"""
Grey Math — Caching & Memoization.

Multi-level cache for expression evaluation results, intermediate
computations, and DAG subgraph results.
"""

from __future__ import annotations

import hashlib
import json
import pickle
import time
from collections import OrderedDict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional

from greymath.core.expr import ExprNode


@dataclass
class CacheEntry:
    """A cached computation result."""
    key: str
    value: Any
    created_at: float = field(default_factory=time.time)
    access_count: int = 0
    last_accessed: float = field(default_factory=time.time)
    size_bytes: int = 0
    metadata: dict[str, Any] = field(default_factory=dict)

    def touch(self) -> None:
        self.access_count += 1
        self.last_accessed = time.time()


@dataclass
class CacheStats:
    """Statistics about cache performance."""
    hits: int = 0
    misses: int = 0
    evictions: int = 0
    total_entries: int = 0
    total_size_bytes: int = 0

    @property
    def hit_rate(self) -> float:
        total = self.hits + self.misses
        return self.hits / total if total > 0 else 0.0


class ExprCache:
    """
    LRU cache for expression DAG nodes.
    Uses structural hashing for cache keys.
    """

    def __init__(self, max_entries: int = 10_000) -> None:
        self._cache: OrderedDict[str, CacheEntry] = OrderedDict()
        self._max_entries = max_entries
        self._stats = CacheStats()

    @property
    def stats(self) -> CacheStats:
        self._stats.total_entries = len(self._cache)
        return self._stats

    def _make_key(self, expr: ExprNode) -> str:
        """Generate a cache key from an expression's structure."""
        return hashlib.sha256(
            expr.to_string().encode("utf-8")
        ).hexdigest()[:32]

    def get(self, expr: ExprNode) -> Any | None:
        """Retrieve a cached result for the given expression."""
        key = self._make_key(expr)
        entry = self._cache.get(key)
        if entry is not None:
            entry.touch()
            self._cache.move_to_end(key)
            self._stats.hits += 1
            return entry.value
        self._stats.misses += 1
        return None

    def put(self, expr: ExprNode, value: Any, metadata: dict | None = None) -> None:
        """Store a result in the cache."""
        key = self._make_key(expr)

        if key in self._cache:
            self._cache.move_to_end(key)
            self._cache[key].value = value
            self._cache[key].touch()
            return

        # Evict if at capacity
        while len(self._cache) >= self._max_entries:
            self._cache.popitem(last=False)
            self._stats.evictions += 1

        self._cache[key] = CacheEntry(
            key=key,
            value=value,
            metadata=metadata or {},
        )

    def invalidate(self, expr: ExprNode) -> bool:
        """Remove a specific entry from the cache."""
        key = self._make_key(expr)
        if key in self._cache:
            del self._cache[key]
            return True
        return False

    def clear(self) -> None:
        """Clear all cache entries."""
        self._cache.clear()
        self._stats = CacheStats()

    def __len__(self) -> int:
        return len(self._cache)


class ResultCache:
    """
    General-purpose computation result cache with optional
    disk persistence and TTL support.
    """

    def __init__(
        self,
        max_entries: int = 50_000,
        max_size_bytes: int = 500_000_000,  # 500 MB
        default_ttl: float | None = None,  # seconds, None = no expiry
        persist_dir: str | Path | None = None,
    ) -> None:
        self._cache: OrderedDict[str, CacheEntry] = OrderedDict()
        self._max_entries = max_entries
        self._max_size_bytes = max_size_bytes
        self._default_ttl = default_ttl
        self._persist_dir = Path(persist_dir) if persist_dir else None
        self._total_size = 0
        self._stats = CacheStats()

        if self._persist_dir:
            self._persist_dir.mkdir(parents=True, exist_ok=True)
            self._load_from_disk()

    @property
    def stats(self) -> CacheStats:
        self._stats.total_entries = len(self._cache)
        self._stats.total_size_bytes = self._total_size
        return self._stats

    def _estimate_size(self, value: Any) -> int:
        """Estimate memory size of a value."""
        try:
            return len(pickle.dumps(value, protocol=pickle.HIGHEST_PROTOCOL))
        except Exception:
            return 1000  # Default estimate

    def get(self, key: str) -> Any | None:
        """Retrieve a cached value."""
        entry = self._cache.get(key)
        if entry is None:
            self._stats.misses += 1
            return None

        # Check TTL
        if self._default_ttl is not None:
            age = time.time() - entry.created_at
            if age > self._default_ttl:
                self._evict(key)
                self._stats.misses += 1
                return None

        entry.touch()
        self._cache.move_to_end(key)
        self._stats.hits += 1
        return entry.value

    def put(
        self,
        key: str,
        value: Any,
        ttl: float | None = None,
        metadata: dict | None = None,
    ) -> None:
        """Store a value in the cache."""
        size = self._estimate_size(value)

        # Evict if necessary
        while (len(self._cache) >= self._max_entries or
               self._total_size + size > self._max_size_bytes):
            if not self._cache:
                break
            self._evict_oldest()

        if key in self._cache:
            old_entry = self._cache[key]
            self._total_size -= old_entry.size_bytes

        entry = CacheEntry(
            key=key,
            value=value,
            size_bytes=size,
            metadata=metadata or {},
        )
        self._cache[key] = entry
        self._cache.move_to_end(key)
        self._total_size += size

    def delete(self, key: str) -> bool:
        """Remove a specific entry."""
        if key in self._cache:
            self._evict(key)
            return True
        return False

    def clear(self) -> None:
        """Clear all entries."""
        self._cache.clear()
        self._total_size = 0
        self._stats = CacheStats()

    def _evict(self, key: str) -> None:
        entry = self._cache.pop(key, None)
        if entry:
            self._total_size -= entry.size_bytes
            self._stats.evictions += 1

    def _evict_oldest(self) -> None:
        if self._cache:
            key, _ = self._cache.popitem(last=False)
            self._evict(key) if key in self._cache else None
            self._stats.evictions += 1

    def persist(self) -> None:
        """Save cache to disk."""
        if self._persist_dir is None:
            return
        cache_file = self._persist_dir / "result_cache.pkl"
        data = {
            key: {"value": entry.value, "metadata": entry.metadata, "created_at": entry.created_at}
            for key, entry in self._cache.items()
        }
        with open(cache_file, "wb") as f:
            pickle.dump(data, f, protocol=pickle.HIGHEST_PROTOCOL)

    def _load_from_disk(self) -> None:
        """Load cache from disk."""
        if self._persist_dir is None:
            return
        cache_file = self._persist_dir / "result_cache.pkl"
        if not cache_file.exists():
            return
        try:
            with open(cache_file, "rb") as f:
                data = pickle.load(f)
            for key, info in data.items():
                self.put(key, info["value"], metadata=info.get("metadata"))
        except Exception:
            pass  # Silently handle corrupt cache files

    def __len__(self) -> int:
        return len(self._cache)
