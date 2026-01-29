/**
 * Grey Expo - useAuth Hook
 *
 * Provides authentication state and actions for Expo components.
 * Wraps the AuthController from @grey/adapters.
 * Expo-safe: no DOM, no Node APIs.
 */

import { useState, useEffect, useCallback, useRef } from 'react';
import {
  AuthController,
  MemoryTokenStorage,
  type AuthState as CoreAuthState,
  type AuthConfig,
} from '@grey/adapters';

// ============================================================
// Types
// ============================================================

/**
 * Normalized error shape
 */
export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

/**
 * Auth hook return type
 */
export interface UseAuthReturn {
  data: CoreAuthState;
  loading: boolean;
  error: GreyError | null;
  login: (email: string, password: string) => Promise<boolean>;
  logout: () => Promise<void>;
  refresh: () => Promise<boolean>;
}

// ============================================================
// Error Normalization
// ============================================================

/**
 * Normalize any error into the standard error shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return {
    message: 'An unknown error occurred',
    raw: err,
  };
}

// ============================================================
// Hook Implementation
// ============================================================

/**
 * useAuth Hook
 *
 * Provides authentication state and actions.
 * Must be used within a GreyProvider or with a config.
 * Uses MemoryTokenStorage by default (Expo-safe).
 *
 * @param config - Optional auth configuration for standalone usage
 */
export function useAuth(config?: AuthConfig): UseAuthReturn {
  const controllerRef = useRef<AuthController | null>(null);
  const [data, setData] = useState<CoreAuthState>({
    user: null,
    isAuthenticated: false,
    isLoading: false,
    error: null,
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<GreyError | null>(null);

  // Track if component is mounted
  const mountedRef = useRef(true);
  useEffect(() => {
    mountedRef.current = true;
    return () => {
      mountedRef.current = false;
    };
  }, []);

  // Initialize controller
  useEffect(() => {
    if (!config) return;

    // Use MemoryTokenStorage for Expo-safe token storage
    const authConfig: AuthConfig = {
      ...config,
      tokenStorage: config.tokenStorage ?? new MemoryTokenStorage(),
    };

    const controller = new AuthController(authConfig);
    controllerRef.current = controller;

    const unsubscribe = controller.subscribe((state: CoreAuthState) => {
      if (mountedRef.current) {
        setData(state);
        setLoading(state.isLoading);
        if (state.error) {
          setError({ message: state.error });
        }
      }
    });

    // Restore session on mount
    controller.restoreSession().catch(() => {
      // Session restore failed, user not authenticated
    });

    return () => {
      unsubscribe();
    };
  }, [config?.apiBaseUrl]);

  /**
   * Login with email and password
   */
  const login = useCallback(async (email: string, password: string): Promise<boolean> => {
    const controller = controllerRef.current;
    if (!controller) {
      setError({ message: 'Auth controller not initialized' });
      return false;
    }

    setLoading(true);
    setError(null);

    try {
      const result = await controller.login(email, password);
      if (mountedRef.current) {
        setLoading(false);
      }
      return result;
    } catch (err) {
      if (mountedRef.current) {
        setError(normalizeError(err));
        setLoading(false);
      }
      return false;
    }
  }, []);

  /**
   * Logout and clear session
   */
  const logout = useCallback(async (): Promise<void> => {
    const controller = controllerRef.current;
    if (!controller) {
      return;
    }

    setLoading(true);
    setError(null);

    try {
      controller.logout();
      if (mountedRef.current) {
        setLoading(false);
      }
    } catch (err) {
      if (mountedRef.current) {
        setError(normalizeError(err));
        setLoading(false);
      }
    }
  }, []);

  /**
   * Refresh the session
   */
  const refresh = useCallback(async (): Promise<boolean> => {
    const controller = controllerRef.current;
    if (!controller) {
      setError({ message: 'Auth controller not initialized' });
      return false;
    }

    setLoading(true);
    setError(null);

    try {
      const result = await controller.restoreSession();
      if (mountedRef.current) {
        setLoading(false);
      }
      return result;
    } catch (err) {
      if (mountedRef.current) {
        setError(normalizeError(err));
        setLoading(false);
      }
      return false;
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
