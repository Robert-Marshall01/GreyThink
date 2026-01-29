/**
 * Grey SvelteKit - Auth Store
 *
 * Svelte store for authentication state and actions.
 * Uses writable() for SSR-safe state. Wraps server routes from /server/auth.ts.
 *
 * No browser APIs. SSR-safe.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
import type { User, AuthSession } from '@grey/core-client';

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
 * Auth data state
 */
export interface AuthData {
  user: User | null;
  session: AuthSession | null;
  isAuthenticated: boolean;
}

/**
 * Auth store return type
 */
export interface AuthStoreReturn {
  data: Readable<AuthData | null>;
  loading: Readable<boolean>;
  error: Readable<GreyError | null>;
  login: (email: string, password: string) => Promise<boolean>;
  logout: () => Promise<void>;
  refresh: () => Promise<boolean>;
}

/**
 * Create an auth store
 *
 * @param baseUrl - Base URL for the API
 * @returns Auth store with data, loading, error, and actions
 */
export function createAuthStore(baseUrl: string): AuthStoreReturn {
  const dataStore: Writable<AuthData | null> = writable(null);
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  /**
   * Login with email and password
   */
  async function login(email: string, password: string): Promise<boolean> {
    loadingStore.set(true);
    errorStore.set(null);

    try {
      const response = await fetch('/api/grey/auth/login', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ email, password }),
      });

      const result = await response.json();

      if (result.error) {
        errorStore.set(result.error);
        loadingStore.set(false);
        return false;
      }

      dataStore.set({
        user: result.user,
        session: result.session,
        isAuthenticated: !!result.user,
      });

      loadingStore.set(false);
      return true;
    } catch (err) {
      errorStore.set({
        message: err instanceof Error ? err.message : 'Login failed',
        raw: err,
      });
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
      await fetch('/api/grey/auth/logout', {
        method: 'POST',
      });

      dataStore.set({
        user: null,
        session: null,
        isAuthenticated: false,
      });
    } catch (err) {
      errorStore.set({
        message: err instanceof Error ? err.message : 'Logout failed',
        raw: err,
      });
    } finally {
      loadingStore.set(false);
    }
  }

  /**
   * Refresh the session
   */
  async function refresh(): Promise<boolean> {
    loadingStore.set(true);
    errorStore.set(null);

    try {
      const response = await fetch('/api/grey/auth/refresh', {
        method: 'POST',
      });

      const result = await response.json();

      if (result.error) {
        errorStore.set(result.error);
        loadingStore.set(false);
        return false;
      }

      dataStore.update((prev: AuthData | null) => ({
        user: prev?.user ?? null,
        session: result.session,
        isAuthenticated: !!result.session,
      }));

      loadingStore.set(false);
      return true;
    } catch (err) {
      errorStore.set({
        message: err instanceof Error ? err.message : 'Refresh failed',
        raw: err,
      });
      loadingStore.set(false);
      return false;
    }
  }

  return {
    data: { subscribe: dataStore.subscribe },
    loading: { subscribe: loadingStore.subscribe },
    error: { subscribe: errorStore.subscribe },
    login,
    logout,
    refresh,
  };
}
