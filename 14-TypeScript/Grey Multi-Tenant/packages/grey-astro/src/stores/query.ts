/**
 * Grey Astro - Query Store
 *
 * Client-side generic query store for Astro islands.
 * Uses nanostores for framework-agnostic reactive state.
 * Calls server endpoints via fetch.
 *
 * Exposes: data, loading, error, executeQuery()
 */

import { atom, computed } from 'nanostores';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface QueryInput {
  endpoint: string;
  params?: Record<string, unknown>;
  [key: string]: unknown;
}

export interface QueryStoreState<T = unknown> {
  data: T | null;
  loading: boolean;
  error: GreyError | null;
}

export interface QueryStoreOptions {
  queryEndpoint?: string;
}

// ============================================================
// Error Normalization
// ============================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    const error = err as Error & { code?: string; status?: number };
    return {
      message: error.message,
      code: error.code,
      status: error.status,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err, raw: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

// ============================================================
// Store Factory
// ============================================================

export function createQueryStore<T = unknown>(options: QueryStoreOptions = {}) {
  const { queryEndpoint = '/api/query' } = options;

  // Atoms
  const $data = atom<T | null>(null);
  const $loading = atom<boolean>(false);
  const $error = atom<GreyError | null>(null);

  // Computed state
  const $state = computed(
    [$data, $loading, $error],
    (data: T | null, loading: boolean, error: GreyError | null) => ({
      data,
      loading,
      error,
    })
  );

  // Actions
  async function executeQuery(input: QueryInput): Promise<void> {
    $loading.set(true);
    $error.set(null);

    try {
      const response = await fetch(queryEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(input),
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        $error.set(result.error ?? { message: 'Query failed', status: response.status });
        $data.set(null);
        return;
      }

      $data.set(result.data);
    } catch (err) {
      $error.set(normalizeError(err));
      $data.set(null);
    } finally {
      $loading.set(false);
    }
  }

  return {
    // Stores
    $data,
    $loading,
    $error,
    $state,
    // Actions
    executeQuery,
  };
}

// Default instance
export const queryStore = createQueryStore();
