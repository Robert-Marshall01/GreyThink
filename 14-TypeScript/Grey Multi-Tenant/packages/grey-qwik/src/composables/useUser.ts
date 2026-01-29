/**
 * Grey Qwik - useUser Composable
 *
 * Provides current user state using Qwik's reactive primitives.
 * Must be used within a GreyProvider.
 *
 * @example
 * ```tsx
 * import { component$ } from '@builder.io/qwik';
 * import { useUser } from '@grey/qwik';
 *
 * export default component$(() => {
 *   const { user, isLoading, refresh$ } = useUser();
 *
 *   return (
 *     <div>
 *       {isLoading.value ? (
 *         <p>Loading...</p>
 *       ) : user.value ? (
 *         <div>
 *           <h1>{user.value.name}</h1>
 *           <p>{user.value.email}</p>
 *           <button onClick$={refresh$}>Refresh</button>
 *         </div>
 *       ) : null}
 *     </div>
 *   );
 * });
 * ```
 */

import { useSignal, useVisibleTask$, $ } from '@builder.io/qwik';
import type { UserState as CoreUserState } from '@grey/adapters';
import type { User } from '@grey/core-client';
import { useGreyContext } from '../context/index.js';
import type { UserComposable } from '../types/index.js';

/**
 * useUser Composable
 *
 * Wraps the UserController from @grey/adapters and exposes
 * Qwik signals for reactive state access.
 *
 * @returns User composable with state signals and actions
 *
 * @example
 * ```tsx
 * export default component$(() => {
 *   const { user, isLoading } = useUser();
 *
 *   return (
 *     <div>
 *       {isLoading.value ? (
 *         <Spinner />
 *       ) : user.value ? (
 *         <Avatar src={user.value.avatar_url} alt={user.value.name} />
 *       ) : null}
 *     </div>
 *   );
 * });
 * ```
 */
export function useUser(): UserComposable {
  const { userController } = useGreyContext();

  // Create signals with initial state from controller
  const state = useSignal<CoreUserState>(userController.getState());
  const user = useSignal(state.value.user);
  const isLoading = useSignal(state.value.isLoading);
  const error = useSignal<string | null>(state.value.error);

  // Subscribe to controller state changes (client-side only)
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    const unsubscribe = userController.subscribe((newState: CoreUserState) => {
      state.value = newState;
      user.value = newState.user;
      isLoading.value = newState.isLoading;
      error.value = newState.error;
    });

    cleanup(() => {
      unsubscribe();
    });
  });

  /**
   * Refresh user data from API
   */
  const refresh$ = $(async (): Promise<User | null> => {
    return userController.fetchCurrentUser();
  });

  /**
   * Clear user state
   */
  const clear$ = $(() => {
    userController.clearUser();
  });

  /**
   * Clear current error
   */
  const clearError$ = $(() => {
    userController.clearError();
  });

  return {
    state,
    user,
    isLoading,
    error,
    refresh$,
    clear$,
    clearError$,
  };
}
