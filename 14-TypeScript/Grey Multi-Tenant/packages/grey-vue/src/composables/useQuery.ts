/**
 * Grey Vue - useQuery Composable
 *
 * Generic data fetching composable with loading and error states.
 * Wraps the QueryController from @grey/adapters.
 */

import { ref, onMounted, onUnmounted, type Ref } from 'vue';
import {
  QueryController,
  type QueryState as CoreQueryState,
  type QueryOptions as CoreQueryOptions,
} from '@grey/adapters';

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
 * Query composable options
 */
export interface UseQueryOptions<T> {
  queryFn: () => Promise<T>;
  enabled?: boolean;
  onSuccess?: (data: T) => void;
  onError?: (error: Error) => void;
}

/**
 * Query composable return type
 */
export interface UseQueryReturn<T> {
  data: Ref<T | null>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  executeQuery: () => Promise<T | null>;
}

/**
 * Normalize any error into the standard error shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return {
    message: 'An unknown error occurred',
    raw: err,
  };
}

/**
 * useQuery Composable
 *
 * Fetch data with loading states and error handling.
 *
 * @param options - Query options including queryFn
 */
export function useQuery<T>(options: UseQueryOptions<T>): UseQueryReturn<T> {
  const { queryFn, enabled = true, onSuccess, onError } = options;

  const controllerRef = ref<QueryController<T> | null>(null);
  const data = ref<T | null>(null) as Ref<T | null>;
  const loading = ref(enabled);
  const error = ref<GreyError | null>(null);

  let unsubscribe: (() => void) | undefined;

  onMounted(() => {
    const coreOptions: CoreQueryOptions<T> = {
      queryFn,
      enabled,
      onSuccess,
      onError,
    };

    const controller = new QueryController(coreOptions);
    controllerRef.value = controller;

    unsubscribe = controller.subscribe((state: CoreQueryState<T>) => {
      data.value = state.data;
      loading.value = state.isLoading;
      if (state.error) {
        error.value = normalizeError(state.error);
      } else {
        error.value = null;
      }
    });

    // Execute initial query if enabled
    if (enabled) {
      controller.execute();
    }
  });

  onUnmounted(() => {
    unsubscribe?.();
  });

  /**
   * Execute the query manually
   */
  async function executeQuery(): Promise<T | null> {
    const controller = controllerRef.value;
    if (!controller) {
      // Create a new controller for manual execution
      const newController = new QueryController({
        queryFn,
        enabled: true,
        onSuccess,
        onError,
      });
      controllerRef.value = newController;
    }

    loading.value = true;
    error.value = null;

    try {
      const result = await controllerRef.value!.execute();
      loading.value = false;
      return result;
    } catch (err) {
      error.value = normalizeError(err);
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
