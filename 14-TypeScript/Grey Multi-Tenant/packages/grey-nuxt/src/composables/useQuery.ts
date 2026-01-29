/**
 * Grey Nuxt - useQuery Composable
 *
 * Provides generic query state and actions for Nuxt components.
 * Uses useState() for SSR-safe state. Wraps server routes from /server/query.ts.
 *
 * No browser APIs. SSR-safe.
 */

import { useState } from '#app';
import type { Ref } from 'vue';
import type { QueryOptions } from '@grey/adapters';

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
 * Query composable return type
 */
export interface UseQueryReturn<T> {
  data: Ref<T | null>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  executeQuery: (options: QueryOptions<T>) => Promise<T | null>;
}

/**
 * useQuery Composable
 *
 * Provides generic query state and actions via server routes.
 * Uses useState() for SSR-safe hydration.
 *
 * @param key - Unique key for the query state (for useState hydration)
 */
export function useQuery<T>(key: string = 'default'): UseQueryReturn<T> {
  const data = useState<T | null>(`grey-query-data-${key}`, () => null);
  const loading = useState<boolean>(`grey-query-loading-${key}`, () => false);
  const error = useState<GreyError | null>(`grey-query-error-${key}`, () => null);

  /**
   * Execute a query
   */
  async function executeQuery(options: QueryOptions<T>): Promise<T | null> {
    loading.value = true;
    error.value = null;

    try {
      // Execute the query function directly (for client-side)
      // In server context, use the server route
      const result = await options.queryFn();

      data.value = result;
      loading.value = false;
      return result;
    } catch (err) {
      error.value = {
        message: err instanceof Error ? err.message : 'Query failed',
        raw: err,
      };
      loading.value = false;
      return null;
    }
  }

  return {
    data,
    loading,
    error,
    executeQuery,
  };
}
