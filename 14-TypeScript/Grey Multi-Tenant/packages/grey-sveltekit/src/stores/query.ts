/**
 * Grey SvelteKit - Query Store
 *
 * Svelte store for generic query state and actions.
 * Uses writable() for SSR-safe state.
 *
 * No browser APIs. SSR-safe.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
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
 * Query store return type
 */
export interface QueryStoreReturn<T> {
  data: Readable<T | null>;
  loading: Readable<boolean>;
  error: Readable<GreyError | null>;
  executeQuery: (options: QueryOptions<T>) => Promise<T | null>;
}

/**
 * Create a query store
 *
 * @returns Query store with data, loading, error, and actions
 */
export function createQueryStore<T>(): QueryStoreReturn<T> {
  const dataStore: Writable<T | null> = writable(null);
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  /**
   * Execute a query
   */
  async function executeQuery(options: QueryOptions<T>): Promise<T | null> {
    loadingStore.set(true);
    errorStore.set(null);

    try {
      // Execute the query function directly
      const result = await options.queryFn();

      dataStore.set(result);
      loadingStore.set(false);
      return result;
    } catch (err) {
      errorStore.set({
        message: err instanceof Error ? err.message : 'Query failed',
        raw: err,
      });
      loadingStore.set(false);
      return null;
    }
  }

  return {
    data: { subscribe: dataStore.subscribe },
    loading: { subscribe: loadingStore.subscribe },
    error: { subscribe: errorStore.subscribe },
    executeQuery,
  };
}
