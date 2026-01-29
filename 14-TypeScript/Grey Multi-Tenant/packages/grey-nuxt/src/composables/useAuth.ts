/**
 * Grey Nuxt - useAuth Composable
 *
 * Provides authentication state and actions for Nuxt components.
 * Uses useState() for SSR-safe state. Wraps server routes from /server/auth.ts.
 *
 * No browser APIs. SSR-safe.
 */

import { useState } from '#app';
import type { Ref } from 'vue';
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
 * Auth composable return type
 */
export interface UseAuthReturn {
  data: Ref<AuthData | null>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  login: (email: string, password: string) => Promise<boolean>;
  logout: () => Promise<void>;
  refresh: () => Promise<boolean>;
}

/**
 * useAuth Composable
 *
 * Provides authentication state and actions via server routes.
 * Uses useState() for SSR-safe hydration.
 */
export function useAuth(): UseAuthReturn {
  const data = useState<AuthData | null>('grey-auth-data', () => null);
  const loading = useState<boolean>('grey-auth-loading', () => false);
  const error = useState<GreyError | null>('grey-auth-error', () => null);

  /**
   * Login with email and password
   */
  async function login(email: string, password: string): Promise<boolean> {
    loading.value = true;
    error.value = null;

    try {
      const result = await $fetch<{
        session: AuthSession | null;
        user: User | null;
        error: GreyError | null;
      }>('/api/grey/auth/login', {
        method: 'POST',
        body: { email, password },
      });

      if (result.error) {
        error.value = result.error;
        loading.value = false;
        return false;
      }

      data.value = {
        user: result.user,
        session: result.session,
        isAuthenticated: !!result.user,
      };

      loading.value = false;
      return true;
    } catch (err) {
      error.value = {
        message: err instanceof Error ? err.message : 'Login failed',
        raw: err,
      };
      loading.value = false;
      return false;
    }
  }

  /**
   * Logout and clear session
   */
  async function logout(): Promise<void> {
    loading.value = true;
    error.value = null;

    try {
      await $fetch('/api/grey/auth/logout', {
        method: 'POST',
      });

      data.value = {
        user: null,
        session: null,
        isAuthenticated: false,
      };
    } catch (err) {
      error.value = {
        message: err instanceof Error ? err.message : 'Logout failed',
        raw: err,
      };
    } finally {
      loading.value = false;
    }
  }

  /**
   * Refresh the session
   */
  async function refresh(): Promise<boolean> {
    loading.value = true;
    error.value = null;

    try {
      const result = await $fetch<{
        session: AuthSession | null;
        error: GreyError | null;
      }>('/api/grey/auth/refresh', {
        method: 'POST',
      });

      if (result.error) {
        error.value = result.error;
        loading.value = false;
        return false;
      }

      data.value = {
        user: data.value?.user ?? null,
        session: result.session,
        isAuthenticated: !!result.session,
      };

      loading.value = false;
      return true;
    } catch (err) {
      error.value = {
        message: err instanceof Error ? err.message : 'Refresh failed',
        raw: err,
      };
      loading.value = false;
      return false;
    }
  }

  return {
    data,
    loading,
    error,
    login,
    logout,
    refresh,
  };
}
