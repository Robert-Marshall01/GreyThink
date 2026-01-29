/**
 * Grey Svelte - User Store
 *
 * Svelte store for user state and actions.
 * Wraps UserController from @grey/adapters.
 * Exposes: data, loading, error, fetchUser.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
import { UserController, type UserState } from '@grey/adapters';
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
 * User store return type
 */
export interface UserStoreReturn {
  /** User data */
  data: Readable<User | null>;
  /** Loading state */
  loading: Readable<boolean>;
  /** Error state */
  error: Readable<GreyError | null>;
  /** Fetch current user */
  fetchUser: () => Promise<User | null>;
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
 * Create a user store
 *
 * @param client - Grey API client
 * @returns User store with data, loading, error, and actions
 */
export function createUserStore(client: GreyClient): UserStoreReturn {
  const controller = new UserController(client);

  // Create writable stores
  const dataStore: Writable<User | null> = writable(null);
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  // Subscribe to controller state changes
  controller.subscribe((state: UserState) => {
    dataStore.set(state.user);
    loadingStore.set(state.isLoading);
    if (state.error) {
      errorStore.set({ message: state.error });
    }
  });

  /**
   * Fetch current user
   */
  async function fetchUser(): Promise<User | null> {
    loadingStore.set(true);
    errorStore.set(null);
    try {
      const user = await controller.fetchCurrentUser();
      loadingStore.set(false);
      return user;
    } catch (err) {
      errorStore.set(normalizeError(err));
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

export type { UserState };
