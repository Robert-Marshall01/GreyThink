/**
 * Grey Qwik - useUser Hook
 *
 * Qwik hook for user state and actions.
 * Wraps UserController from @grey/adapters.
 * Exposes: data, loading, error, fetchUser.
 */

import { useSignal, useVisibleTask$, $ } from '@builder.io/qwik';
import type { Signal, QRL } from '@builder.io/qwik';
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
  data: Signal<User | null>;
  /** Loading state */
  loading: Signal<boolean>;
  /** Error state */
  error: Signal<GreyError | null>;
  /** Fetch current user */
  fetchUser: QRL<() => Promise<User | null>>;
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
  const data = useSignal<User | null>(null);
  const loading = useSignal(false);
  const error = useSignal<GreyError | null>(null);

  // Subscribe to controller state changes (client-side only)
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    const unsubscribe = controller.subscribe((state: UserState) => {
      data.value = state.user;
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
   * Fetch current user
   */
  const fetchUser = $(async (): Promise<User | null> => {
    loading.value = true;
    error.value = null;
    try {
      const user = await controller.fetchCurrentUser();
      loading.value = false;
      return user;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
      return null;
    }
  });

  return {
    data,
    loading,
    error,
    fetchUser,
  };
}

export type { UserState };
