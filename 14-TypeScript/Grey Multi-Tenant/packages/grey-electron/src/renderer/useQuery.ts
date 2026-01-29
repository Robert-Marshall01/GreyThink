/**
 * Grey Electron - Renderer useQuery Hook
 *
 * Client-side generic query hook for Electron renderer.
 * Calls preload bridge functions (never IPC directly).
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
}

export interface UseQueryResult<T = unknown> {
  data: T | null;
  loading: boolean;
  error: GreyError | null;
  executeQuery: (token: string | undefined, input: QueryInput) => Promise<void>;
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

export function useQuery<T = unknown>(): UseQueryResult<T> {
  const [data, setData] = useState<T | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const executeQuery = useCallback(
    async (token: string | undefined, input: QueryInput): Promise<void> => {
      setLoading(true);
      setError(null);

      try {
        const result = await window.grey.query.executeQuery<T>(token, input);

        if (result.error) {
          setError(result.error);
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
    []
  );

  return {
    data,
    loading,
    error,
    executeQuery,
  };
}
