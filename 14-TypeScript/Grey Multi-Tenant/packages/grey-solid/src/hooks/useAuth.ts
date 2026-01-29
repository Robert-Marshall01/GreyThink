/**
 * Grey Solid - useAuth Hook
 *
 * Solid hook for authentication state and actions.
 * Wraps AuthController from @grey/adapters.
 * Exposes: data, loading, error, login, logout, refresh.
 */

import { createSignal, onMount, onCleanup, type Accessor } from 'solid-js';
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
  data: Accessor<AuthState>;
  /** Loading state */
  loading: Accessor<boolean>;
  /** Error state */
  error: Accessor<GreyError | null>;
  /** Login with email and password */
  login: (email: string, password: string) => Promise<boolean>;
  /** Logout and clear session */
  logout: () => Promise<void>;
  /** Refresh/restore session */
  refresh: () => Promise<boolean>;
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
  const [data, setData] = createSignal<AuthState>(controller.getState());
  const [loading, setLoading] = createSignal(false);
  const [error, setError] = createSignal<GreyError | null>(null);

  // Subscribe to controller state changes
  let unsubscribe: (() => void) | undefined;

  onMount(() => {
    unsubscribe = controller.subscribe((state: AuthState) => {
      setData(state);
      setLoading(state.isLoading);
      if (state.error) {
        setError({ message: state.error });
      }
    });
  });

  onCleanup(() => {
    unsubscribe?.();
  });

  /**
   * Login with email and password
   */
  async function login(email: string, password: string): Promise<boolean> {
    setLoading(true);
    setError(null);
    try {
      const result = await controller.login(email, password);
      setLoading(false);
      return result;
    } catch (err) {
      setError(normalizeError(err));
      setLoading(false);
      return false;
    }
  }

  /**
   * Logout and clear session
   */
  async function logout(): Promise<void> {
    setLoading(true);
    setError(null);
    try {
      controller.logout();
      setLoading(false);
    } catch (err) {
      setError(normalizeError(err));
      setLoading(false);
    }
  }

  /**
   * Refresh/restore session
   */
  async function refresh(): Promise<boolean> {
    if (!isBrowser()) {
      return false;
    }
    setLoading(true);
    setError(null);
    try {
      const result = await controller.restoreSession();
      setLoading(false);
      return result;
    } catch (err) {
      setError(normalizeError(err));
      setLoading(false);
      return false;
    }
  }

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