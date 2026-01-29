/**
 * Grey Solid - Auth Signal
 *
 * Provides authentication state and actions as SolidJS signals.
 * Must be used within a GreyProvider.
 *
 * @example
 * ```tsx
 * import { createAuthSignal } from '@grey/solid';
 *
 * function LoginButton() {
 *   const auth = createAuthSignal();
 *
 *   return (
 *     <Show when={auth.isAuthenticated()} fallback={
 *       <button onClick={() => auth.login('user@example.com', 'password')}>
 *         Login
 *       </button>
 *     }>
 *       <span>Welcome, {auth.user()?.email}</span>
 *       <button onClick={() => auth.logout()}>Logout</button>
 *     </Show>
 *   );
 * }
 * ```
 */

import { createSignal, onCleanup, onMount } from 'solid-js';
import type { AuthState as CoreAuthState } from '@grey/adapters';
import { useGreyContext, isBrowser } from '../context/index.js';
import type { AuthSignal } from '../types/index.js';

/**
 * Create reactive auth signal
 *
 * Wraps the AuthController from @grey/adapters and exposes
 * SolidJS signals for reactive state access.
 *
 * @returns Auth signal with state accessors and actions
 *
 * @example
 * ```tsx
 * function AuthStatus() {
 *   const auth = createAuthSignal();
 *
 *   onMount(() => {
 *     // Restore session on mount
 *     auth.restoreSession();
 *   });
 *
 *   return (
 *     <div>
 *       <Show when={auth.isLoading()}>
 *         <span>Loading...</span>
 *       </Show>
 *       <Show when={auth.error()}>
 *         <span class="error">{auth.error()}</span>
 *       </Show>
 *       <Show when={auth.isAuthenticated()}>
 *         <span>Logged in as {auth.user()?.email}</span>
 *       </Show>
 *     </div>
 *   );
 * }
 * ```
 */
export function createAuthSignal(): AuthSignal {
  const { authController } = useGreyContext();

  // Create signal with initial state from controller
  const [state, setState] = createSignal<CoreAuthState>(authController.getState());

  // Subscribe to controller state changes
  const unsubscribe = authController.subscribe((newState: CoreAuthState) => {
    setState(newState);
  });

  // Cleanup subscription on unmount
  onCleanup(() => {
    unsubscribe();
  });

  // Derived accessors
  const isAuthenticated = () => state().isAuthenticated;
  const accessToken = () => {
    // Access token is stored in storage, not in state
    // Use a derived value based on authentication status
    return state().isAuthenticated ? 'authenticated' : null;
  };
  const isLoading = () => state().isLoading;
  const error = () => state().error;
  const user = () => state().user;

  /**
   * Login with email and password
   */
  async function login(email: string, password: string): Promise<boolean> {
    return authController.login(email, password);
  }

  /**
   * Logout and clear session
   */
  function logout(): void {
    authController.logout();
  }

  /**
   * Restore session from storage
   */
  async function restoreSession(): Promise<boolean> {
    if (!isBrowser()) {
      return false;
    }
    return authController.restoreSession();
  }

  /**
   * Clear current error
   */
  function clearError(): void {
    authController.clearError();
  }

  return {
    state,
    isAuthenticated,
    accessToken,
    isLoading,
    error,
    user,
    login,
    logout,
    restoreSession,
    clearError,
  };
}

/**
 * Hook to access auth state from context
 *
 * This is a convenience wrapper that creates the auth signal.
 * For most cases, use createAuthSignal() directly.
 */
export function useAuth(): AuthSignal {
  return createAuthSignal();
}
