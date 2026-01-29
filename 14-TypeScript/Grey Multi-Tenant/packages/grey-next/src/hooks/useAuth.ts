'use client';

/**
 * useAuth - Next.js Client Hook for authentication
 *
 * Wraps server actions from /server/auth.ts.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useCallback } from 'react';
import { login as loginAction, logout as logoutAction, refresh as refreshAction } from '../server/auth.js';
import type { GreyError, AuthResult } from '../server/auth.js';
import type { User, AuthSession } from '@grey/core-client';

// =============================================================================
// Types
// =============================================================================

export interface AuthData {
  user: User | null;
  session: AuthSession | null;
  isAuthenticated: boolean;
}

export interface UseAuthReturn {
  data: AuthData | null;
  loading: boolean;
  error: GreyError | null;
  login: (baseUrl: string, email: string, password: string) => Promise<boolean>;
  logout: (baseUrl: string, token?: string) => Promise<void>;
  refresh: (baseUrl: string, refreshToken?: string) => Promise<boolean>;
}

// =============================================================================
// Hook
// =============================================================================

export function useAuth(): UseAuthReturn {
  const [data, setData] = useState<AuthData | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<GreyError | null>(null);

  const login = useCallback(async (baseUrl: string, email: string, password: string): Promise<boolean> => {
    setLoading(true);
    setError(null);

    try {
      const result = await loginAction(baseUrl, email, password);

      if (result.error) {
        setError(result.error);
        return false;
      }

      setData({
        user: result.user,
        session: result.session,
        isAuthenticated: !!result.session,
      });

      return true;
    } catch (err) {
      setError({
        message: err instanceof Error ? err.message : 'Login failed',
        raw: err,
      });
      return false;
    } finally {
      setLoading(false);
    }
  }, []);

  const logout = useCallback(async (baseUrl: string, token?: string): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const result = await logoutAction(baseUrl, token);

      if (result.error) {
        setError(result.error);
      }

      setData(null);
    } catch (err) {
      setError({
        message: err instanceof Error ? err.message : 'Logout failed',
        raw: err,
      });
    } finally {
      setLoading(false);
    }
  }, []);

  const refresh = useCallback(async (baseUrl: string, refreshToken?: string): Promise<boolean> => {
    setLoading(true);
    setError(null);

    try {
      const result = await refreshAction(baseUrl, refreshToken);

      if (result.error) {
        setError(result.error);
        return false;
      }

      setData((prev: AuthData | null) => ({
        user: prev?.user ?? null,
        session: result.session,
        isAuthenticated: !!result.session,
      }));

      return true;
    } catch (err) {
      setError({
        message: err instanceof Error ? err.message : 'Refresh failed',
        raw: err,
      });
      return false;
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
