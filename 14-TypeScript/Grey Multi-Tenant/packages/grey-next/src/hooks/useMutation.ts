'use client';

/**
 * useMutation - Next.js Client Hook for generic mutations
 *
 * Wraps server actions from /server/mutation.ts.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useCallback } from 'react';
import { executeMutation as executeMutationAction } from '../server/mutation.js';
import type { GreyError } from '../server/auth.js';
import type { MutationOptions } from '@grey/adapters';

// =============================================================================
// Types
// =============================================================================

export interface UseMutationReturn<TData, TVariables> {
  data: TData | null;
  loading: boolean;
  error: GreyError | null;
  executeMutation: (variables: TVariables, options: MutationOptions<TData, TVariables>) => Promise<TData | null>;
  reset: () => void;
}

// =============================================================================
// Hook
// =============================================================================

export function useMutation<TData, TVariables>(): UseMutationReturn<TData, TVariables> {
  const [data, setData] = useState<TData | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<GreyError | null>(null);

  const executeMutation = useCallback(async (
    variables: TVariables,
    options: MutationOptions<TData, TVariables>
  ): Promise<TData | null> => {
    setLoading(true);
    setError(null);

    try {
      const result = await executeMutationAction<TData, TVariables>(variables, options);

      if (result.error) {
        setError(result.error);
        return null;
      }

      setData(result.data);
      return result.data;
    } catch (err) {
      setError({
        message: err instanceof Error ? err.message : 'Mutation failed',
        raw: err,
      });
      return null;
    } finally {
      setLoading(false);
    }
  }, []);

  const reset = useCallback(() => {
    setData(null);
    setLoading(false);
    setError(null);
  }, []);

  return {
    data,
    loading,
    error,
    executeMutation,
    reset,
  };
}
