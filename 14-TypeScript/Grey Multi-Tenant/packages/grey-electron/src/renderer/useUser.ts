/**
 * Grey Electron - Renderer useUser Hook
 *
 * Client-side user hook for Electron renderer.
 * Calls preload bridge functions (never IPC directly).
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
  fetchUser: (token: string) => Promise<void>;
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

export function useUser(): UseUserResult {
  const [data, setData] = useState<unknown | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const fetchUser = useCallback(async (token: string): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const result = await window.grey.user.fetchUser(token);

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
  }, []);

  return {
    data,
    loading,
    error,
    fetchUser,
  };
}
