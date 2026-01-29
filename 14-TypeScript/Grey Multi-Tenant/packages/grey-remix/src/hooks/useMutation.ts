/**
 * Grey Remix - useMutation Hook
 *
 * Client-side generic mutation hook for Remix.
 * Uses React state and calls server actions via fetch.
 *
 * Exposes: data, loading, error, executeMutation()
 */

import { useState, useCallback } from 'react';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface MutationInput<TVariables = unknown> {
  endpoint: string;
  method?: 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  variables?: TVariables;
  [key: string]: unknown;
}

export interface UseMutationResult<TData = unknown, TVariables = unknown> {
  data: TData | null;
  loading: boolean;
  error: GreyError | null;
  executeMutation: (input: MutationInput<TVariables>) => Promise<TData | null>;
  reset: () => void;
}

export interface UseMutationOptions {
  mutationEndpoint?: string;
}

// ============================================================
// Error Normalization
// ============================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    const error = err as Error & { code?: string; status?: number };
    return {
      message: error.message,
      code: error.code,
      status: error.status,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err, raw: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

// ============================================================
// Hook
// ============================================================

export function useMutation<TData = unknown, TVariables = unknown>(
  options: UseMutationOptions = {}
): UseMutationResult<TData, TVariables> {
  const { mutationEndpoint = '/api/mutation' } = options;

  const [data, setData] = useState<TData | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const executeMutation = useCallback(
    async (input: MutationInput<TVariables>): Promise<TData | null> => {
      setLoading(true);
      setError(null);

      try {
        const response = await fetch(mutationEndpoint, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(input),
        });

        const result = await response.json();

        if (!response.ok || result.error) {
          setError(result.error ?? { message: 'Mutation failed', status: response.status });
          setData(null);
          return null;
        }

        setData(result.data);
        return result.data;
      } catch (err) {
        setError(normalizeError(err));
        setData(null);
        return null;
      } finally {
        setLoading(false);
      }
    },
    [mutationEndpoint]
  );

  const reset = useCallback((): void => {
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
