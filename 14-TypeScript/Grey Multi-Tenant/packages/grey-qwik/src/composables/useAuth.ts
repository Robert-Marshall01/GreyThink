/**
 * Grey Qwik - useAuth Composable
 *
 * Provides authentication state and actions using Qwik's reactive primitives.
 * Must be used within a GreyProvider.
 *
 * @example
 * ```tsx
 * import { component$ } from '@builder.io/qwik';
 * import { useAuth } from '@grey/qwik';
 *
 * export default component$(() => {
 *   const auth = useAuth();
 *
 *   return (
 *     <div>
 *       {auth.isAuthenticated.value ? (
 *         <button onClick$={auth.logout$}>Logout</button>
 *       ) : (
 *         <button onClick$={() => auth.login$('user@example.com', 'password')}>
 *           Login
 *         </button>
 *       )}
 *     </div>
 *   );
 * });
 * ```
 */

import { useSignal, useTask$, useVisibleTask$, $ } from '@builder.io/qwik';
import type { AuthState as CoreAuthState } from '@grey/adapters';
import { useGreyContext, isBrowser } from '../context/index.js';
import type { AuthComposable } from '../types/index.js';

/**
 * useAuth Composable
 *
 * Wraps the AuthController from @grey/adapters and exposes
 * Qwik signals for reactive state access.
 *
 * @returns Auth composable with state signals and actions
 *
 * @example
 * ```tsx
 * export default component$(() => {
 *   const auth = useAuth();
 *
 *   // Auto-restore session on mount using useVisibleTask$
 *   useVisibleTask$(async () => {
 *     await auth.restoreSession$();
 *   });
 *
 *   return (
 *     <div>
 *       {auth.isLoading.value && <p>Loading...</p>}
 *       {auth.error.value && <p class="error">{auth.error.value}</p>}
 *       {auth.isAuthenticated.value && (
 *         <p>Welcome, {auth.user.value?.email}</p>
 *       )}
 *     </div>
 *   );
 * });
 * ```
 */
export function useAuth(): AuthComposable {
  const { authController } = useGreyContext();

  // Create signals with initial state from controller
  const state = useSignal<CoreAuthState>(authController.getState());
  const isAuthenticated = useSignal(state.value.isAuthenticated);
  const isLoading = useSignal(state.value.isLoading);
  const error = useSignal<string | null>(state.value.error);
  const user = useSignal(state.value.user);

  // Subscribe to controller state changes (client-side only)
  // useVisibleTask$ ensures this only runs in the browser
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    const unsubscribe = authController.subscribe((newState: CoreAuthState) => {
      state.value = newState;
      isAuthenticated.value = newState.isAuthenticated;
      isLoading.value = newState.isLoading;
      error.value = newState.error;
      user.value = newState.user;
    });

    cleanup(() => {
      unsubscribe();
    });
  });

  /**
   * Login with email and password
   */
  const login$ = $(async (email: string, password: string): Promise<boolean> => {
    return authController.login(email, password);
  });

  /**
   * Logout and clear session
   */
  const logout$ = $(() => {
    authController.logout();
  });

  /**
   * Restore session from storage
   */
  const restoreSession$ = $(async (): Promise<boolean> => {
    if (!isBrowser()) {
      return false;
    }
    return authController.restoreSession();
  });

  /**
   * Clear current error
   */
  const clearError$ = $(() => {
    authController.clearError();
  });

  return {
    state,
    isAuthenticated,
    isLoading,
    error,
    user,
    login$,
    logout$,
    restoreSession$,
    clearError$,
  };
}
