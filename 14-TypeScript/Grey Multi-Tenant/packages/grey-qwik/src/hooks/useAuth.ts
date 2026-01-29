/**
 * Grey Qwik - useAuth Hook
 *
 * Qwik hook for authentication state and actions.
 * Wraps AuthController from @grey/adapters.
 * Exposes: data, loading, error, login, logout, refresh.
 */

import { useSignal, useVisibleTask$, $ } from '@builder.io/qwik';
import type { Signal, QRL } from '@builder.io/qwik';
import {
  AuthController,
  BrowserTokenStorage,
  MemoryTokenStorage,
  type AuthState,
  type AuthConfig,
} from '@grey/adapters';
import type { GreyClient } from '@grey/core-client';

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
 * useAuth hook return type
 */
export interface UseAuthReturn {
  /** Auth state data (user, isAuthenticated, etc.) */
  data: Signal<AuthState>;
  /** Loading state */
  loading: Signal<boolean>;
  /** Error state */
  error: Signal<GreyError | null>;
  /** Login with email and password */
  login: QRL<(email: string, password: string) => Promise<boolean>>;
  /** Logout and clear session */
  logout: QRL<() => Promise<void>>;
  /** Refresh/restore session */
  refresh: QRL<() => Promise<boolean>>;
  /** Get the API client */
  getClient: () => GreyClient;
}

/**
 * SSR-safe browser check
 */
function isBrowser(): boolean {
  return typeof window !== 'undefined' && typeof document !== 'undefined';
}

/**
 * Normalize error into standard shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return { message: err.message, raw: err };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

/**
 * useAuth hook
 *
 * @param config - Auth configuration (apiBaseUrl required)
 * @returns Auth hook with data, loading, error, and actions
 */
export function useAuth(config: AuthConfig): UseAuthReturn {
  // Use browser storage in browser, memory storage during SSR
  const storage = isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage();

  const controller = new AuthController({
    ...config,
    storage: config.storage ?? storage,
  });

  // Create signals
  const data = useSignal<AuthState>(controller.getState());
  const loading = useSignal(false);
  const error = useSignal<GreyError | null>(null);

  // Subscribe to controller state changes (client-side only)
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    const unsubscribe = controller.subscribe((state: AuthState) => {
      data.value = state;
      loading.value = state.isLoading;
      if (state.error) {
        error.value = { message: state.error };
      }
    });

    cleanup(() => {
      unsubscribe();
    });
  });

  /**
   * Login with email and password
   */
  const login = $(async (email: string, password: string): Promise<boolean> => {
    loading.value = true;
    error.value = null;
    try {
      const result = await controller.login(email, password);
      loading.value = false;
      return result;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
      return false;
    }
  });

  /**
   * Logout and clear session
   */
  const logout = $(async (): Promise<void> => {
    loading.value = true;
    error.value = null;
    try {
      controller.logout();
      loading.value = false;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
    }
  });

  /**
   * Refresh/restore session
   */
  const refresh = $(async (): Promise<boolean> => {
    if (!isBrowser()) {
      return false;
    }
    loading.value = true;
    error.value = null;
    try {
      const result = await controller.restoreSession();
      loading.value = false;
      return result;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
      return false;
    }
  });

  /**
   * Get the API client
   */
  function getClient(): GreyClient {
    return controller.getClient();
  }

  return {
    data,
    loading,
    error,
    login,
    logout,
    refresh,
    getClient,
  };
}

export type { AuthState, AuthConfig };