/**
 * Grey Solid - User Signal
 *
 * Provides current user state as SolidJS signals.
 * Must be used within a GreyProvider.
 *
 * @example
 * ```tsx
 * import { createUserSignal } from '@grey/solid';
 *
 * function UserProfile() {
 *   const { user, isLoading, refresh } = createUserSignal();
 *
 *   return (
 *     <Show when={!isLoading()} fallback={<Spinner />}>
 *       <Show when={user()}>
 *         <div>
 *           <h1>{user()?.name}</h1>
 *           <p>{user()?.email}</p>
 *           <button onClick={refresh}>Refresh</button>
 *         </div>
 *       </Show>
 *     </Show>
 *   );
 * }
 * ```
 */

import { createSignal, onCleanup } from 'solid-js';
import type { UserState as CoreUserState } from '@grey/adapters';
import type { User } from '@grey/core-client';
import { useGreyContext } from '../context/index.js';
import type { UserSignal } from '../types/index.js';

/**
 * Create reactive user signal
 *
 * Wraps the UserController from @grey/adapters and exposes
 * SolidJS signals for reactive state access.
 *
 * @returns User signal with state accessors and actions
 *
 * @example
 * ```tsx
 * function UserAvatar() {
 *   const { user, isLoading } = createUserSignal();
 *
 *   return (
 *     <Show when={!isLoading() && user()}>
 *       <img src={user()?.avatar_url} alt={user()?.name} />
 *     </Show>
 *   );
 * }
 * ```
 */
export function createUserSignal(): UserSignal {
  const { userController } = useGreyContext();

  // Create signal with initial state from controller
  const [state, setState] = createSignal<CoreUserState>(userController.getState());

  // Subscribe to controller state changes
  const unsubscribe = userController.subscribe((newState: CoreUserState) => {
    setState(newState);
  });

  // Cleanup subscription on unmount
  onCleanup(() => {
    unsubscribe();
  });

  // Derived accessors
  const user = () => state().user;
  const isLoading = () => state().isLoading;
  const error = () => state().error;

  /**
   * Refresh user data from API
   */
  async function refresh(): Promise<User | null> {
    return userController.fetchCurrentUser();
  }

  /**
   * Clear user state
   */
  function clear(): void {
    userController.clearUser();
  }

  /**
   * Clear current error
   */
  function clearError(): void {
    userController.clearError();
  }

  return {
    state,
    user,
    isLoading,
    error,
    refresh,
    clear,
    clearError,
  };
}

/**
 * Hook to access user state from context
 *
 * This is a convenience wrapper that creates the user signal.
 * For most cases, use createUserSignal() directly.
 */
export function useUser(): UserSignal {
  return createUserSignal();
}
