/**
 * Grey Electron - Renderer useMutation Hook
 *
 * Client-side generic mutation hook for Electron renderer.
 * Calls preload bridge functions (never IPC directly).
 *
 * Exposes: data, loading, error, executeMutation(), reset()
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
}

export interface UseMutationResult<TData = unknown, TVariables = unknown> {
  data: TData | null;
  loading: boolean;
  error: GreyError | null;
  executeMutation: (token: string | undefined, input: MutationInput<TVariables>) => Promise<TData | null>;
  reset: () => void;
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

export function useMutation<TData = unknown, TVariables = unknown>(): UseMutationResult<TData, TVariables> {
  const [data, setData] = useState<TData | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const executeMutation = useCallback(
    async (token: string | undefined, input: MutationInput<TVariables>): Promise<TData | null> => {
      setLoading(true);
      setError(null);

      try {
        const result = await window.grey.mutation.executeMutation<TData, TVariables>(token, input);

        if (result.error) {
          setError(result.error);
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
    []
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
