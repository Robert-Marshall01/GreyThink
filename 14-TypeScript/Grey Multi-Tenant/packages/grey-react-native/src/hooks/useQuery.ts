/**
 * Grey React Native - useQuery Hook
 *
 * Generic data fetching hook with loading and error states.
 * Wraps the QueryController from @grey/adapters.
 * Mobile-safe: no browser/DOM APIs.
 */

import { useState, useEffect, useCallback, useRef } from 'react';
import {
  QueryController,
  type QueryState as CoreQueryState,
  type QueryOptions as CoreQueryOptions,
} from '@grey/adapters';

// ============================================================
// Types
// ============================================================

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
  queryFn: () => Promise<T>;
  enabled?: boolean;
  onSuccess?: (data: T) => void;
  onError?: (error: Error) => void;
}

/**
 * Query hook return type
 */
export interface UseQueryReturn<T> {
  data: T | null;
  loading: boolean;
  error: GreyError | null;
  executeQuery: () => Promise<T | null>;
}

// ============================================================
// Error Normalization
// ============================================================

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

// ============================================================
// Hook Implementation
// ============================================================

/**
 * useQuery Hook
 *
 * Fetch data with loading states and error handling.
 *
 * @param options - Query options including queryFn
 */
export function useQuery<T>(options: UseQueryOptions<T>): UseQueryReturn<T> {
  const { queryFn, enabled = true, onSuccess, onError } = options;

  const controllerRef = useRef<QueryController<T> | null>(null);
  const [data, setData] = useState<T | null>(null);
  const [loading, setLoading] = useState(enabled);
  const [error, setError] = useState<GreyError | null>(null);

  // Track if component is mounted
  const mountedRef = useRef(true);
  useEffect(() => {
    mountedRef.current = true;
    return () => {
      mountedRef.current = false;
    };
  }, []);

  // Initialize controller
  useEffect(() => {
    const coreOptions: CoreQueryOptions<T> = {
      queryFn,
      enabled,
      onSuccess,
      onError,
    };

    const controller = new QueryController(coreOptions);
    controllerRef.current = controller;

    const unsubscribe = controller.subscribe((state: CoreQueryState<T>) => {
      if (mountedRef.current) {
        setData(state.data);
        setLoading(state.isLoading);
        if (state.error) {
          setError(normalizeError(state.error));
        } else {
          setError(null);
        }
      }
    });

    // Execute initial query if enabled
    if (enabled) {
      controller.execute();
    }

    return () => {
      unsubscribe();
    };
  }, [enabled]);

  /**
   * Execute the query manually
   */
  const executeQuery = useCallback(async (): Promise<T | null> => {
    const controller = controllerRef.current;
    if (!controller) {
      // Create a new controller for manual execution
      const newController = new QueryController({
        queryFn,
        enabled: true,
        onSuccess,
        onError,
      });
      controllerRef.current = newController;
    }

    setLoading(true);
    setError(null);

    try {
      const result = await controllerRef.current!.execute();
      if (mountedRef.current) {
        setLoading(false);
      }
      return result;
    } catch (err) {
      if (mountedRef.current) {
        setError(normalizeError(err));
        setLoading(false);
      }
      return null;
    }
  }, [queryFn, onSuccess, onError]);

  return {
    data,
    loading,
    error,
    executeQuery,
  };
}
