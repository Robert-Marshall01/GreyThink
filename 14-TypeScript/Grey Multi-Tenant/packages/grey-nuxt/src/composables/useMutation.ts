/**
 * Grey Nuxt - useMutation Composable
 *
 * Provides generic mutation state and actions for Nuxt components.
 * Uses useState() for SSR-safe state. Wraps server routes from /server/mutation.ts.
 *
 * No browser APIs. SSR-safe.
 */

import { useState } from '#app';
import type { Ref } from 'vue';
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
 * Mutation composable return type
 */
export interface UseMutationReturn<TData, TVariables> {
  data: Ref<TData | null>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  executeMutation: (
    variables: TVariables,
    options: MutationOptions<TData, TVariables>
  ) => Promise<TData | null>;
  reset: () => void;
}

/**
 * useMutation Composable
 *
 * Provides generic mutation state and actions via server routes.
 * Uses useState() for SSR-safe hydration.
 *
 * @param key - Unique key for the mutation state (for useState hydration)
 */
export function useMutation<TData, TVariables>(key: string = 'default'): UseMutationReturn<TData, TVariables> {
  const data = useState<TData | null>(`grey-mutation-data-${key}`, () => null);
  const loading = useState<boolean>(`grey-mutation-loading-${key}`, () => false);
  const error = useState<GreyError | null>(`grey-mutation-error-${key}`, () => null);

  /**
   * Execute a mutation
   */
  async function executeMutation(
    variables: TVariables,
    options: MutationOptions<TData, TVariables>
  ): Promise<TData | null> {
    loading.value = true;
    error.value = null;

    try {
      // Execute the mutation function directly
      const result = await options.mutationFn(variables);

      data.value = result;
      loading.value = false;

      // Call onSuccess callback if provided
      options.onSuccess?.(result);

      return result;
    } catch (err) {
      const normalizedError: GreyError = {
        message: err instanceof Error ? err.message : 'Mutation failed',
        raw: err,
      };

      error.value = normalizedError;
      loading.value = false;

      // Call onError callback if provided
      options.onError?.(err instanceof Error ? err : new Error(normalizedError.message));

      return null;
    }
  }

  /**
   * Reset mutation state
   */
  function reset(): void {
    data.value = null;
    loading.value = false;
    error.value = null;
  }

  return {
    data,
    loading,
    error,
    executeMutation,
    reset,
  };
}
