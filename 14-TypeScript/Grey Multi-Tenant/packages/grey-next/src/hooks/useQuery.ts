'use client';

/**
 * useQuery - Next.js Client Hook for generic queries
 *
 * Wraps server actions from /server/query.ts.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useCallback } from 'react';
import { executeQuery as executeQueryAction } from '../server/query.js';
import type { GreyError } from '../server/auth.js';
import type { QueryOptions } from '@grey/adapters';

// =============================================================================
// Types
// =============================================================================

export interface UseQueryReturn<T> {
  data: T | null;
  loading: boolean;
  error: GreyError | null;
  executeQuery: (options: QueryOptions<T>) => Promise<T | null>;
}

// =============================================================================
// Hook
// =============================================================================

export function useQuery<T>(): UseQueryReturn<T> {
  const [data, setData] = useState<T | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<GreyError | null>(null);

  const executeQuery = useCallback(async (options: QueryOptions<T>): Promise<T | null> => {
    setLoading(true);
    setError(null);

    try {
      const result = await executeQueryAction<T>(options);

      if (result.error) {
        setError(result.error);
        return null;
      }

      setData(result.data);
      return result.data;
    } catch (err) {
      setError({
        message: err instanceof Error ? err.message : 'Query failed',
        raw: err,
      });
      return null;
    } finally {
      setLoading(false);
    }
  }, []);

  return {
    data,
    loading,
    error,
    executeQuery,
  };
}
