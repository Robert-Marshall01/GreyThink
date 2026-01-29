/**
 * Grey Svelte - Query Store
 *
 * Svelte store for generic data fetching.
 * Wraps QueryController from @grey/adapters.
 * Exposes: data, loading, error, executeQuery.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
import { QueryController, type QueryOptions } from '@grey/adapters';

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
 * Query store options
 */
export interface QueryStoreOptions<T> {
  /** Async function that fetches data */
  queryFn: () => Promise<T>;
  /** Whether to execute on creation (default: false) */
  enabled?: boolean;
  /** Called on success */
  onSuccess?: (data: T) => void;
  /** Called on error */
  onError?: (error: Error) => void;
}

/**
 * Query store return type
 */
export interface QueryStoreReturn<T> {
  /** Query result data */
  data: Readable<T | null>;
  /** Loading state */
  loading: Readable<boolean>;
  /** Error state */
  error: Readable<GreyError | null>;
  /** Execute the query */
  executeQuery: () => Promise<T | null>;
}

/**
 * Normalize error into standard shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return { message: err.message, raw: err };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

/**
 * Create a query store
 *
 * @param options - Query options
 * @returns Query store with data, loading, error, and executeQuery action
 */
export function createQueryStore<T>(options: QueryStoreOptions<T>): QueryStoreReturn<T> {
  const controller = new QueryController<T>({
    queryFn: options.queryFn,
    enabled: options.enabled,
    onSuccess: options.onSuccess,
    onError: options.onError,
  });

  // Create writable stores
  const dataStore: Writable<T | null> = writable(null);
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  // Subscribe to controller state changes
  controller.subscribe((state) => {
    dataStore.set(state.data);
    loadingStore.set(state.isLoading);
    if (state.error) {
      errorStore.set(normalizeError(state.error));
    } else {
      errorStore.set(null);
    }
  });

  /**
   * Execute the query
   */
  async function executeQuery(): Promise<T | null> {
    loadingStore.set(true);
    errorStore.set(null);
    try {
      const result = await controller.execute();
      loadingStore.set(false);
      return result;
    } catch (err) {
      errorStore.set(normalizeError(err));
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

export type { QueryOptions };
