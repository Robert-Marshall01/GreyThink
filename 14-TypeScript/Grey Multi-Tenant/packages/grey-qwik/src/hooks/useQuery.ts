/**
 * Grey Qwik - useQuery Hook
 *
 * Qwik hook for generic data fetching.
 * Wraps QueryController from @grey/adapters.
 * Exposes: data, loading, error, executeQuery.
 */

import { useSignal, useVisibleTask$, $ } from '@builder.io/qwik';
import type { Signal, QRL } from '@builder.io/qwik';
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
  data: Signal<T | null>;
  /** Loading state */
  loading: Signal<boolean>;
  /** Error state */
  error: Signal<GreyError | null>;
  /** Execute the query */
  executeQuery: QRL<() => Promise<T | null>>;
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
  const data = useSignal<T | null>(null);
  const loading = useSignal(false);
  const error = useSignal<GreyError | null>(null);

  // Subscribe to controller state changes (client-side only)
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    const unsubscribe = controller.subscribe((state: QueryState<T>) => {
      data.value = state.data;
      loading.value = state.isLoading;
      if (state.error) {
        error.value = normalizeError(state.error);
      } else {
        error.value = null;
      }
    });

    cleanup(() => {
      unsubscribe();
    });
  });

  /**
   * Execute the query
   */
  const executeQuery = $(async (): Promise<T | null> => {
    loading.value = true;
    error.value = null;
    try {
      const result = await controller.execute();
      loading.value = false;
      return result;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
      return null;
    }
  });

  return {
    data,
    loading,
    error,
    executeQuery,
  };
}

export type { QueryOptions, QueryState };
