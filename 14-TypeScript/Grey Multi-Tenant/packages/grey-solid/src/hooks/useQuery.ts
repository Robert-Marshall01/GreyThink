/**
 * Grey Solid - useQuery Hook
 *
 * Solid hook for generic data fetching.
 * Wraps QueryController from @grey/adapters.
 * Exposes: data, loading, error, executeQuery.
 */

import { createSignal, onMount, onCleanup, type Accessor } from 'solid-js';
import { QueryController, type QueryOptions, type QueryState } from '@grey/adapters';

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
 * Query hook options
 */
export interface UseQueryOptions<T> {
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
 * useQuery hook return type
 */
export interface UseQueryReturn<T> {
  /** Query result data */
  data: Accessor<T | null>;
  /** Loading state */
  loading: Accessor<boolean>;
  /** Error state */
  error: Accessor<GreyError | null>;
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
 * useQuery hook
 *
 * @param options - Query options
 * @returns Query hook with data, loading, error, and executeQuery action
 */
export function useQuery<T>(options: UseQueryOptions<T>): UseQueryReturn<T> {
  const controller = new QueryController<T>({
    queryFn: options.queryFn,
    enabled: options.enabled,
    onSuccess: options.onSuccess,
    onError: options.onError,
  });

  // Create signals
  const [data, setData] = createSignal<T | null>(null);
  const [loading, setLoading] = createSignal(false);
  const [error, setError] = createSignal<GreyError | null>(null);

  // Subscribe to controller state changes
  let unsubscribe: (() => void) | undefined;

  onMount(() => {
    unsubscribe = controller.subscribe((state: QueryState<T>) => {
      setData(() => state.data);
      setLoading(state.isLoading);
      if (state.error) {
        setError(normalizeError(state.error));
      } else {
        setError(null);
      }
    });
  });

  onCleanup(() => {
    unsubscribe?.();
  });

  /**
   * Execute the query
   */
  async function executeQuery(): Promise<T | null> {
    setLoading(true);
    setError(null);
    try {
      const result = await controller.execute();
      setLoading(false);
      return result;
    } catch (err) {
      setError(normalizeError(err));
      setLoading(false);
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

export type { QueryOptions, QueryState };
