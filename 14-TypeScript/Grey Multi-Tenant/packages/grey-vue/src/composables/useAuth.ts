/**
 * Grey Vue - useAuth Composable
 *
 * Provides authentication state and actions for Vue components.
 * Wraps the AuthController from @grey/adapters.
 */

import { ref, onMounted, onUnmounted, type Ref } from 'vue';
import {
  AuthController,
  type AuthState as CoreAuthState,
  type AuthConfig,
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
 * Auth composable return type
 */
export interface UseAuthReturn {
  data: Ref<CoreAuthState>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  login: (email: string, password: string) => Promise<boolean>;
  logout: () => Promise<void>;
  refresh: () => Promise<boolean>;
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
 * useAuth Composable
 *
 * Provides authentication state and actions.
 *
 * @param config - Auth configuration
 */
export function useAuth(config?: AuthConfig): UseAuthReturn {
  const controllerRef = ref<AuthController | null>(null);
  const data = ref<CoreAuthState>({
    user: null,
    isAuthenticated: false,
    isLoading: false,
    error: null,
  });
  const loading = ref(false);
  const error = ref<GreyError | null>(null);

  let unsubscribe: (() => void) | undefined;

  onMounted(() => {
    if (!config) return;

    const controller = new AuthController(config);
    controllerRef.value = controller;

    unsubscribe = controller.subscribe((state) => {
      data.value = state;
      loading.value = state.isLoading;
      if (state.error) {
        error.value = { message: state.error };
      }
    });

    // Restore session on mount
    controller.restoreSession().catch(() => {
      // Session restore failed
    });
  });

  onUnmounted(() => {
    unsubscribe?.();
  });

  /**
   * Login with email and password
   */
  async function login(email: string, password: string): Promise<boolean> {
    const controller = controllerRef.value;
    if (!controller) {
      error.value = { message: 'Auth controller not initialized' };
      return false;
    }

    loading.value = true;
    error.value = null;

    try {
      const result = await controller.login(email, password);
      loading.value = false;
      return result;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
      return false;
    }
  }

  /**
   * Logout and clear session
   */
  async function logout(): Promise<void> {
    const controller = controllerRef.value;
    if (!controller) return;

    loading.value = true;
    error.value = null;

    try {
      controller.logout();
      loading.value = false;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
    }
  }

  /**
   * Refresh the session
   */
  async function refresh(): Promise<boolean> {
    const controller = controllerRef.value;
    if (!controller) {
      error.value = { message: 'Auth controller not initialized' };
      return false;
    }

    loading.value = true;
    error.value = null;

    try {
      const result = await controller.restoreSession();
      loading.value = false;
      return result;
    } catch (err) {
      error.value = normalizeError(err);
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
