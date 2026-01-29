/**
 * Grey Remix - useAuth Hook
 *
 * Client-side authentication hook for Remix.
 * Uses React state and calls server actions via fetch.
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
  refresh: () => Promise<void>;
}

export interface UseAuthOptions {
  loginEndpoint?: string;
  logoutEndpoint?: string;
  refreshEndpoint?: string;
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

export function useAuth(options: UseAuthOptions = {}): UseAuthResult {
  const {
    loginEndpoint = '/api/auth/login',
    logoutEndpoint = '/api/auth/logout',
    refreshEndpoint = '/api/auth/refresh',
  } = options;

  const [data, setData] = useState<AuthData | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const login = useCallback(
    async (email: string, password: string): Promise<void> => {
      setLoading(true);
      setError(null);

      try {
        const response = await fetch(loginEndpoint, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ email, password }),
        });

        const result = await response.json();

        if (!response.ok || result.error) {
          setError(result.error ?? { message: 'Login failed', status: response.status });
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
    [loginEndpoint]
  );

  const logout = useCallback(async (): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const response = await fetch(logoutEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        setError(result.error ?? { message: 'Logout failed', status: response.status });
        return;
      }

      setData(null);
    } catch (err) {
      setError(normalizeError(err));
    } finally {
      setLoading(false);
    }
  }, [logoutEndpoint]);

  const refresh = useCallback(async (): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const response = await fetch(refreshEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        setError(result.error ?? { message: 'Refresh failed', status: response.status });
        return;
      }

      setData(result.data);
    } catch (err) {
      setError(normalizeError(err));
    } finally {
      setLoading(false);
    }
  }, [refreshEndpoint]);

  return {
    data,
    loading,
    error,
    login,
    logout,
    refresh,
  };
}
