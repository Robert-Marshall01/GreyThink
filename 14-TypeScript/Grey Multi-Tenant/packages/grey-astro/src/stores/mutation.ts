/**
 * Grey Astro - Mutation Store
 *
 * Client-side generic mutation store for Astro islands.
 * Uses nanostores for framework-agnostic reactive state.
 * Calls server endpoints via fetch.
 *
 * Exposes: data, loading, error, executeMutation(), reset()
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

export interface MutationInput<TVariables = unknown> {
  endpoint: string;
  method?: 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  variables?: TVariables;
  [key: string]: unknown;
}

export interface MutationStoreState<TData = unknown> {
  data: TData | null;
  loading: boolean;
  error: GreyError | null;
}

export interface MutationStoreOptions {
  mutationEndpoint?: string;
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

export function createMutationStore<TData = unknown, TVariables = unknown>(
  options: MutationStoreOptions = {}
) {
  const { mutationEndpoint = '/api/mutation' } = options;

  // Atoms
  const $data = atom<TData | null>(null);
  const $loading = atom<boolean>(false);
  const $error = atom<GreyError | null>(null);

  // Computed state
  const $state = computed(
    [$data, $loading, $error],
    (data: TData | null, loading: boolean, error: GreyError | null) => ({
      data,
      loading,
      error,
    })
  );

  // Actions
  async function executeMutation(input: MutationInput<TVariables>): Promise<TData | null> {
    $loading.set(true);
    $error.set(null);

    try {
      const response = await fetch(mutationEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(input),
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        $error.set(result.error ?? { message: 'Mutation failed', status: response.status });
        $data.set(null);
        return null;
      }

      $data.set(result.data);
      return result.data;
    } catch (err) {
      $error.set(normalizeError(err));
      $data.set(null);
      return null;
    } finally {
      $loading.set(false);
    }
  }

  function reset(): void {
    $data.set(null);
    $loading.set(false);
    $error.set(null);
  }

  return {
    // Stores
    $data,
    $loading,
    $error,
    $state,
    // Actions
    executeMutation,
    reset,
  };
}

// Default instance
export const mutationStore = createMutationStore();
