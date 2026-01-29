/**
 * useQuery - Preact hook for generic queries
 *
 * Wraps @grey/adapters QueryController using Preact hooks.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useEffect, useCallback } from 'preact/hooks';
import { QueryController, type QueryOptions, type QueryState } from '@grey/adapters';
import type { GreyError } from './useAuth';

// =============================================================================
// Normalize Errors
// =============================================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      code: (err as { code?: string }).code,
      status: (err as { status?: number }).status,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err, raw: err };
  }
  return { message: 'Unknown error', raw: err };
}

// =============================================================================
// Hook Result
// =============================================================================

export interface UseQueryResult<T> {
  data: T | null;
  loading: boolean;
  error: GreyError | null;
  executeQuery: () => Promise<T | null>;
}

// =============================================================================
// useQuery Hook
// =============================================================================

export function useQuery<T>(options: QueryOptions<T>): UseQueryResult<T> {
  const [data, setData] = useState<T | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);
  const [controller, setController] = useState<QueryController<T> | null>(null);

  // Initialize controller on mount
  useEffect(() => {
    const queryController = new QueryController(options);
    setController(queryController);

    const unsubscribe = queryController.subscribe((state: QueryState<T>) => {
      setData(state.data);
      setLoading(state.isLoading);
      setError(state.error ? normalizeError(state.error) : null);
    });

    // Auto-execute if enabled is not explicitly false
    if (options.enabled !== false) {
      queryController.execute();
    }

    return () => {
      unsubscribe();
    };
  }, []);

  const executeQuery = useCallback(async (): Promise<T | null> => {
    if (!controller) {
      throw new Error('Query controller not initialized');
    }

    setLoading(true);
    setError(null);

    try {
      const result = await controller.execute();
      return result;
    } catch (err) {
      setError(normalizeError(err));
      return null;
    } finally {
      setLoading(false);
    }
  }, [controller]);

  return {
    data,
    loading,
    error,
    executeQuery,
  };
}
