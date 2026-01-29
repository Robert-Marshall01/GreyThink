/**
 * Grey Svelte - Auth Store
 *
 * Svelte store for authentication state and actions.
 * Wraps AuthController from @grey/adapters.
 * Exposes: data, loading, error, login, logout, refresh.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
import {
  AuthController,
  BrowserTokenStorage,
  MemoryTokenStorage,
  type AuthState,
  type AuthConfig,
} from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';

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
 * Auth store return type
 */
export interface AuthStoreReturn {
  /** Auth state data (user, isAuthenticated, etc.) */
  data: Readable<AuthState>;
  /** Loading state */
  loading: Readable<boolean>;
  /** Error state */
  error: Readable<GreyError | null>;
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
 * Create an auth store
 *
 * @param config - Auth configuration (apiBaseUrl required)
 * @returns Auth store with data, loading, error, and actions
 */
export function createAuthStore(config: AuthConfig): AuthStoreReturn {
  // Use browser storage in browser, memory storage during SSR
  const storage = isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage();

  const controller = new AuthController({
    ...config,
    storage: config.storage ?? storage,
  });

  // Create writable stores
  const dataStore: Writable<AuthState> = writable(controller.getState());
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  // Subscribe to controller state changes
  controller.subscribe((state) => {
    dataStore.set(state);
    loadingStore.set(state.isLoading);
    if (state.error) {
      errorStore.set({ message: state.error });
    }
  });

  /**
   * Login with email and password
   */
  async function login(email: string, password: string): Promise<boolean> {
    loadingStore.set(true);
    errorStore.set(null);
    try {
      const result = await controller.login(email, password);
      loadingStore.set(false);
      return result;
    } catch (err) {
      errorStore.set(normalizeError(err));
      loadingStore.set(false);
      return false;
    }
  }

  /**
   * Logout and clear session
   */
  async function logout(): Promise<void> {
    loadingStore.set(true);
    errorStore.set(null);
    try {
      controller.logout();
      loadingStore.set(false);
    } catch (err) {
      errorStore.set(normalizeError(err));
      loadingStore.set(false);
    }
  }

  /**
   * Refresh/restore session
   */
  async function refresh(): Promise<boolean> {
    if (!isBrowser()) {
      return false;
    }
    loadingStore.set(true);
    errorStore.set(null);
    try {
      const result = await controller.restoreSession();
      loadingStore.set(false);
      return result;
    } catch (err) {
      errorStore.set(normalizeError(err));
      loadingStore.set(false);
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
    data: { subscribe: dataStore.subscribe },
    loading: { subscribe: loadingStore.subscribe },
    error: { subscribe: errorStore.subscribe },
    login,
    logout,
    refresh,
    getClient,
  };
}

export type { AuthState, AuthConfig };
