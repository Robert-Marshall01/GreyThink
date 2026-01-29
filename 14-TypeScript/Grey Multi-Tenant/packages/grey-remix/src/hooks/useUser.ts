/**
 * Grey Remix - useUser Hook
 *
 * Client-side user hook for Remix.
 * Uses React state and calls server loaders via fetch.
 *
 * Exposes: data, loading, error, fetchUser()
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

export interface UseUserResult {
  data: unknown | null;
  loading: boolean;
  error: GreyError | null;
  fetchUser: () => Promise<void>;
}

export interface UseUserOptions {
  userEndpoint?: string;
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

export function useUser(options: UseUserOptions = {}): UseUserResult {
  const { userEndpoint = '/api/user' } = options;

  const [data, setData] = useState<unknown | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const fetchUser = useCallback(async (): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const response = await fetch(userEndpoint, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json' },
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        setError(result.error ?? { message: 'Failed to fetch user', status: response.status });
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
  }, [userEndpoint]);

  return {
    data,
    loading,
    error,
    fetchUser,
  };
}
