/**
 * Grey Nuxt - useUser Composable
 *
 * Provides user state and actions for Nuxt components.
 * Uses useState() for SSR-safe state. Wraps server routes from /server/user.ts.
 *
 * No browser APIs. SSR-safe.
 */

import { useState } from '#app';
import type { Ref } from 'vue';
import type { User } from '@grey/core-client';

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
 * User data state
 */
export interface UserData {
  user: User | null;
}

/**
 * User composable return type
 */
export interface UseUserReturn {
  data: Ref<UserData | null>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  fetchUser: () => Promise<User | null>;
}

/**
 * useUser Composable
 *
 * Provides user state and actions via server routes.
 * Uses useState() for SSR-safe hydration.
 */
export function useUser(): UseUserReturn {
  const data = useState<UserData | null>('grey-user-data', () => null);
  const loading = useState<boolean>('grey-user-loading', () => false);
  const error = useState<GreyError | null>('grey-user-error', () => null);

  /**
   * Fetch current user
   */
  async function fetchUser(): Promise<User | null> {
    loading.value = true;
    error.value = null;

    try {
      const result = await $fetch<{
        user: User | null;
        error: GreyError | null;
      }>('/api/grey/user/me');

      if (result.error) {
        error.value = result.error;
        loading.value = false;
        return null;
      }

      data.value = {
        user: result.user,
      };

      loading.value = false;
      return result.user;
    } catch (err) {
      error.value = {
        message: err instanceof Error ? err.message : 'Failed to fetch user',
        raw: err,
      };
      loading.value = false;
      return null;
    }
  }

  return {
    data,
    loading,
    error,
    fetchUser,
  };
}
