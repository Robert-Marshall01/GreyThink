/**
 * Grey Vanilla - User Module
 * 
 * Vanilla JS state container for user data management.
 * Wraps the @grey/adapters UserController with a simple listener pattern.
 * 
 * @example
 * ```js
 * import { createUserContainer } from '@grey/vanilla';
 * 
 * const user = createUserContainer(greyClient);
 * 
 * // Subscribe to state changes
 * const unsubscribe = user.subscribe((state) => {
 *   if (state.user) {
 *     console.log('User:', state.user.name);
 *   }
 * });
 * 
 * // Fetch user data
 * await user.fetchUser();
 * 
 * // Check state
 * console.log(user.getState().user);
 * 
 * // Cleanup
 * unsubscribe();
 * ```
 */

import {
  UserController as CoreUserController,
  type UserState,
} from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';
import { isBrowser, type Listener, type Unsubscribe } from './types.js';

/**
 * Options for creating a user container
 */
export interface UserContainerOptions {
  /** The Grey client instance (from AuthContainer) */
  client: GreyClient;
  /** Whether to auto-fetch user on creation (default: false) */
  autoFetch?: boolean;
}

/**
 * User container interface
 * Provides reactive user state and actions
 */
export interface UserContainer {
  // ============================================================
  // State Access
  // ============================================================
  
  /** Get the current user state snapshot */
  getState(): UserState;
  
  /** Subscribe to user state changes */
  subscribe(listener: Listener<UserState>): Unsubscribe;
  
  // ============================================================
  // State Properties (convenience getters)
  // ============================================================
  
  /** The current user (if authenticated) */
  readonly user: User | null;
  
  /** Whether user data is being loaded */
  readonly loading: boolean;
  
  /** Current error message (if any) */
  readonly error: string | null;
  
  // ============================================================
  // Actions
  // ============================================================
  
  /**
   * Fetch current user data from API
   * @returns Promise resolving to User or null
   */
  fetchUser(): Promise<User | null>;
  
  /** Refetch user data (alias for fetchUser) */
  refetch(): Promise<User | null>;
  
  /**
   * Set user directly (useful when user data comes from auth flow)
   * @param user - User object or null to clear
   */
  setUser(user: User | null): void;
  
  /** Clear user state */
  clear(): void;
  
  /** Clear current error */
  clearError(): void;
}

/**
 * Create a user container
 * 
 * Provides a simple observable state container for user data
 * that wraps the @grey/adapters UserController.
 * 
 * @param options - Configuration options (can be just a client)
 * @returns UserContainer instance
 * 
 * @example
 * ```js
 * // Simple usage with just client
 * const user = createUserContainer({ client: auth.getClient() });
 * 
 * // With auto-fetch
 * const user = createUserContainer({
 *   client: auth.getClient(),
 *   autoFetch: true,
 * });
 * 
 * // Subscribe to changes
 * user.subscribe((state) => {
 *   updateUserUI(state);
 * });
 * ```
 */
export function createUserContainer(options: UserContainerOptions): UserContainer {
  // Internal state
  let _state: UserState;
  const listeners = new Set<Listener<UserState>>();
  
  // Initialize core controller
  const coreController = new CoreUserController(options.client);
  _state = coreController.getState();
  
  // Subscribe to core controller state changes
  coreController.subscribe((state: UserState) => {
    _state = state;
    notifyListeners();
  });
  
  /**
   * Notify all listeners of state change
   */
  function notifyListeners(): void {
    listeners.forEach((listener) => listener(_state));
  }
  
  // Auto-fetch if enabled and in browser
  if (options.autoFetch && isBrowser()) {
    coreController.fetchCurrentUser().catch(() => {
      // Fetch failed - user may not be authenticated
    });
  }
  
  // Return the container interface
  const container: UserContainer = {
    // State access
    getState() {
      return { ..._state };
    },
    
    subscribe(listener: Listener<UserState>): Unsubscribe {
      listeners.add(listener);
      // Immediately call with current state
      listener(_state);
      return () => listeners.delete(listener);
    },
    
    // Convenience getters
    get user() {
      return _state.user;
    },
    
    get loading() {
      return _state.isLoading;
    },
    
    get error() {
      return _state.error;
    },
    
    // Actions
    fetchUser(): Promise<User | null> {
      return coreController.fetchCurrentUser();
    },
    
    refetch(): Promise<User | null> {
      return coreController.fetchCurrentUser();
    },
    
    setUser(user: User | null): void {
      coreController.setUser(user);
    },
    
    clear(): void {
      coreController.clear();
    },
    
    clearError(): void {
      coreController.clearError();
    },
  };
  
  return container;
}

// Re-export related types
export type { UserState } from '@grey/adapters';
export { initialUserState, createUserController } from '@grey/adapters';
