/**
 * Grey SvelteKit - User Store
 *
 * Svelte store for user state and actions.
 * Uses writable() for SSR-safe state. Wraps server routes from /server/user.ts.
 *
 * No browser APIs. SSR-safe.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
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
 * User store return type
 */
export interface UserStoreReturn {
  data: Readable<UserData | null>;
  loading: Readable<boolean>;
  error: Readable<GreyError | null>;
  fetchUser: () => Promise<User | null>;
}

/**
 * Create a user store
 *
 * @returns User store with data, loading, error, and actions
 */
export function createUserStore(): UserStoreReturn {
  const dataStore: Writable<UserData | null> = writable(null);
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  /**
   * Fetch current user
   */
  async function fetchUser(): Promise<User | null> {
    loadingStore.set(true);
    errorStore.set(null);

    try {
      const response = await fetch('/api/grey/user/me');
      const result = await response.json();

      if (result.error) {
        errorStore.set(result.error);
        loadingStore.set(false);
        return null;
      }

      dataStore.set({
        user: result.user,
      });

      loadingStore.set(false);
      return result.user;
    } catch (err) {
      errorStore.set({
        message: err instanceof Error ? err.message : 'Failed to fetch user',
        raw: err,
      });
      loadingStore.set(false);
      return null;
    }
  }

  return {
    data: { subscribe: dataStore.subscribe },
    loading: { subscribe: loadingStore.subscribe },
    error: { subscribe: errorStore.subscribe },
    fetchUser,
  };
}
