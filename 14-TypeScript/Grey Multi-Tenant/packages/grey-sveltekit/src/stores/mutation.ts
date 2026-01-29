/**
 * Grey SvelteKit - Mutation Store
 *
 * Svelte store for generic mutation state and actions.
 * Uses writable() for SSR-safe state.
 *
 * No browser APIs. SSR-safe.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
import type { MutationOptions } from '@grey/adapters';

/**
 * Normalized error shape
 */
export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

/**
 * Mutation store return type
 */
export interface MutationStoreReturn<TData, TVariables> {
  data: Readable<TData | null>;
  loading: Readable<boolean>;
  error: Readable<GreyError | null>;
  executeMutation: (
    variables: TVariables,
    options: MutationOptions<TData, TVariables>
  ) => Promise<TData | null>;
  reset: () => void;
}

/**
 * Create a mutation store
 *
 * @returns Mutation store with data, loading, error, and actions
 */
export function createMutationStore<TData, TVariables>(): MutationStoreReturn<TData, TVariables> {
  const dataStore: Writable<TData | null> = writable(null);
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  /**
   * Execute a mutation
   */
  async function executeMutation(
    variables: TVariables,
    options: MutationOptions<TData, TVariables>
  ): Promise<TData | null> {
    loadingStore.set(true);
    errorStore.set(null);

    try {
      // Execute the mutation function directly
      const result = await options.mutationFn(variables);

      dataStore.set(result);
      loadingStore.set(false);

      // Call onSuccess callback if provided
      options.onSuccess?.(result);

      return result;
    } catch (err) {
      const normalizedError: GreyError = {
        message: err instanceof Error ? err.message : 'Mutation failed',
        raw: err,
      };

      errorStore.set(normalizedError);
      loadingStore.set(false);

      // Call onError callback if provided
      options.onError?.(err instanceof Error ? err : new Error(normalizedError.message));

      return null;
    }
  }

  /**
   * Reset mutation state
   */
  function reset(): void {
    dataStore.set(null);
    loadingStore.set(false);
    errorStore.set(null);
  }

  return {
    data: { subscribe: dataStore.subscribe },
    loading: { subscribe: loadingStore.subscribe },
    error: { subscribe: errorStore.subscribe },
    executeMutation,
    reset,
  };
}
