/**
 * Grey Vue - useUser Composable
 *
 * Provides current user state and actions for Vue components.
 * Wraps the UserController from @grey/adapters.
 */

import { ref, onMounted, onUnmounted, type Ref } from 'vue';
import {
  UserController,
  type UserState as CoreUserState,
  type GreyClient,
  type User,
} from '@grey/adapters';

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
 * User composable return type
 */
export interface UseUserReturn {
  data: Ref<User | null>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  fetchUser: () => Promise<User | null>;
}

/**
 * Normalize any error into the standard error shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return {
    message: 'An unknown error occurred',
    raw: err,
  };
}

/**
 * useUser Composable
 *
 * Provides current user state and actions.
 *
 * @param client - GreyClient instance
 */
export function useUser(client?: GreyClient): UseUserReturn {
  const controllerRef = ref<UserController | null>(null);
  const data = ref<User | null>(null);
  const loading = ref(false);
  const error = ref<GreyError | null>(null);

  let unsubscribe: (() => void) | undefined;

  onMounted(() => {
    if (!client) return;

    const controller = new UserController(client);
    controllerRef.value = controller;

    unsubscribe = controller.subscribe((state: CoreUserState) => {
      data.value = state.user;
      loading.value = state.isLoading;
      if (state.error) {
        error.value = { message: state.error };
      }
    });
  });

  onUnmounted(() => {
    unsubscribe?.();
  });

  /**
   * Fetch the current user
   */
  async function fetchUser(): Promise<User | null> {
    const controller = controllerRef.value;
    if (!controller) {
      error.value = { message: 'User controller not initialized' };
      return null;
    }

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
  }

  return {
    data,
    loading,
    error,
    fetchUser,
  };
}
