/**
 * Grey Remix - useQuery Hook
 *
 * Client-side generic query hook for Remix.
 * Uses React state and calls server loaders via fetch.
 *
 * Exposes: data, loading, error, executeQuery()
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

export interface QueryInput {
  endpoint: string;
  params?: Record<string, unknown>;
  [key: string]: unknown;
}

export interface UseQueryResult<T = unknown> {
  data: T | null;
  loading: boolean;
  error: GreyError | null;
  executeQuery: (input: QueryInput) => Promise<void>;
}

export interface UseQueryOptions {
  queryEndpoint?: string;
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

export function useQuery<T = unknown>(options: UseQueryOptions = {}): UseQueryResult<T> {
  const { queryEndpoint = '/api/query' } = options;

  const [data, setData] = useState<T | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const executeQuery = useCallback(
    async (input: QueryInput): Promise<void> => {
      setLoading(true);
      setError(null);

      try {
        const response = await fetch(queryEndpoint, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(input),
        });

        const result = await response.json();

        if (!response.ok || result.error) {
          setError(result.error ?? { message: 'Query failed', status: response.status });
          setData(null);
          return;
        }

        setData(result.data);
      } catch (err) {
        setError(normalizeError(err));
        setData(null);
      } finally {
        setLoading(false);
      }
    },
    [queryEndpoint]
  );

  return {
    data,
    loading,
    error,
    executeQuery,
  };
}
