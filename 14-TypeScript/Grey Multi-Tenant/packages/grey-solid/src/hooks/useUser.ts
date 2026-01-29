/**
 * Grey Solid - useUser Hook
 *
 * Solid hook for user state and actions.
 * Wraps UserController from @grey/adapters.
 * Exposes: data, loading, error, fetchUser.
 */

import { createSignal, onMount, onCleanup, type Accessor } from 'solid-js';
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
 * useUser hook return type
 */
export interface UseUserReturn {
  /** User data */
  data: Accessor<User | null>;
  /** Loading state */
  loading: Accessor<boolean>;
  /** Error state */
  error: Accessor<GreyError | null>;
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
 * useUser hook
 *
 * @param client - Grey API client
 * @returns User hook with data, loading, error, and actions
 */
export function useUser(client: GreyClient): UseUserReturn {
  const controller = new UserController(client);

  // Create signals
  const [data, setData] = createSignal<User | null>(null);
  const [loading, setLoading] = createSignal(false);
  const [error, setError] = createSignal<GreyError | null>(null);

  // Subscribe to controller state changes
  let unsubscribe: (() => void) | undefined;

  onMount(() => {
    unsubscribe = controller.subscribe((state: UserState) => {
      setData(state.user);
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
   * Fetch current user
   */
  async function fetchUser(): Promise<User | null> {
    setLoading(true);
    setError(null);
    try {
      const user = await controller.fetchCurrentUser();
      setLoading(false);
      return user;
    } catch (err) {
      setError(normalizeError(err));
      setLoading(false);
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

export type { UserState };
