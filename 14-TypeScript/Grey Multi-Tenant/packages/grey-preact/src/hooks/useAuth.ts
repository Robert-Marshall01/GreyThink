/**
 * useAuth - Preact hook for authentication
 *
 * Wraps @grey/adapters AuthController using Preact hooks.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useEffect, useCallback } from 'preact/hooks';
import {
  AuthController,
  type AuthState,
  BrowserTokenStorage,
  MemoryTokenStorage,
} from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';

// =============================================================================
// Error Shape
// =============================================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

// =============================================================================
// SSR Safety
// =============================================================================

function isBrowser(): boolean {
  return typeof window !== 'undefined' && typeof document !== 'undefined';
}

function createPlatformStorage(): BrowserTokenStorage | MemoryTokenStorage {
  return isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage();
}

// =============================================================================
// Normalize Errors
// =============================================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      code: (err as { code?: string }).code,
      status: (err as { status?: number }).status,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err, raw: err };
  }
  return { message: 'Unknown error', raw: err };
}

// =============================================================================
// Hook Configuration
// =============================================================================

export interface UseAuthConfig {
  baseUrl: string;
  onAuthChange?: (user: User | null) => void;
  onLogout?: () => void;
}

// =============================================================================
// Hook Result
// =============================================================================

export interface UseAuthResult {
  data: User | null;
  loading: boolean;
  error: GreyError | null;
  isAuthenticated: boolean;
  login: (email: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
  refresh: () => Promise<void>;
  getClient: () => GreyClient;
}

// =============================================================================
// useAuth Hook
// =============================================================================

export function useAuth(config: UseAuthConfig): UseAuthResult {
  const [data, setData] = useState<User | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);
  const [isAuthenticated, setIsAuthenticated] = useState<boolean>(false);
  const [controller, setController] = useState<AuthController | null>(null);

  // Initialize controller on mount
  useEffect(() => {
    const storage = createPlatformStorage();

    const authController = new AuthController({
      apiBaseUrl: config.baseUrl,
      storage,
      onAuthChange: (state: AuthState) => {
        setData(state.user);
        setIsAuthenticated(state.isAuthenticated);
        setLoading(state.isLoading);
        setError(state.error ? normalizeError(state.error) : null);
        config.onAuthChange?.(state.user);
      },
      onLogout: config.onLogout,
    });

    setController(authController);

    const unsubscribe = authController.subscribe((state: AuthState) => {
      setData(state.user);
      setIsAuthenticated(state.isAuthenticated);
      setLoading(state.isLoading);
      setError(state.error ? normalizeError(state.error) : null);
    });

    return () => {
      unsubscribe();
    };
  }, [config.baseUrl]);

  const login = useCallback(async (email: string, password: string): Promise<void> => {
    if (!controller) {
      throw new Error('Auth controller not initialized');
    }

    setLoading(true);
    setError(null);

    try {
      await controller.login(email, password);
    } catch (err) {
      setError(normalizeError(err));
      throw err;
    } finally {
      setLoading(false);
    }
  }, [controller]);

  const logout = useCallback(async (): Promise<void> => {
    if (!controller) {
      throw new Error('Auth controller not initialized');
    }

    setLoading(true);
    setError(null);

    try {
      controller.logout();
    } catch (err) {
      setError(normalizeError(err));
      throw err;
    } finally {
      setLoading(false);
    }
  }, [controller]);

  const refresh = useCallback(async (): Promise<void> => {
    if (!controller) {
      throw new Error('Auth controller not initialized');
    }

    setLoading(true);
    setError(null);

    try {
      await controller.restoreSession();
    } catch (err) {
      setError(normalizeError(err));
      throw err;
    } finally {
      setLoading(false);
    }
  }, [controller]);

  const getClient = useCallback((): GreyClient => {
    if (!controller) {
      throw new Error('Auth controller not initialized');
    }
    return controller.getClient();
  }, [controller]);

  return {
    data,
    loading,
    error,
    isAuthenticated,
    login,
    logout,
    refresh,
    getClient,
  };
}
