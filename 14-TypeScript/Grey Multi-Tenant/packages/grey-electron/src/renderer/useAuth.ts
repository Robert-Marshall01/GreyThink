/**
 * Grey Electron - Renderer useAuth Hook
 *
 * Client-side authentication hook for Electron renderer.
 * Calls preload bridge functions (never IPC directly).
 *
 * Exposes: data, loading, error, login(), logout(), refresh()
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

export interface AuthData {
  user: unknown | null;
  session: {
    accessToken: string;
    refreshToken?: string;
    expiresIn?: number;
  } | null;
}

export interface UseAuthResult {
  data: AuthData | null;
  loading: boolean;
  error: GreyError | null;
  login: (email: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
  refresh: (refreshToken: string) => Promise<void>;
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

export function useAuth(): UseAuthResult {
  const [data, setData] = useState<AuthData | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const login = useCallback(async (email: string, password: string): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const result = await window.grey.auth.login({ email, password });

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

  const logout = useCallback(async (): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const token = data?.session?.accessToken;
      const result = await window.grey.auth.logout(token);

      if (result.error) {
        setError(result.error);
        return;
      }

      setData(null);
    } catch (err) {
      setError(normalizeError(err));
    } finally {
      setLoading(false);
    }
  }, [data]);

  const refresh = useCallback(async (refreshToken: string): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const result = await window.grey.auth.refresh({ refreshToken });

      if (result.error) {
        setError(result.error);
        return;
      }

      setData(result.data);
    } catch (err) {
      setError(normalizeError(err));
    } finally {
      setLoading(false);
    }
  }, []);

  return {
    data,
    loading,
    error,
    login,
    logout,
    refresh,
  };
}
