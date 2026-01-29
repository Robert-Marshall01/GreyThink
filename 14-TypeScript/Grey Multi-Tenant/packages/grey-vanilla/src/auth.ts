/**
 * Grey Vanilla - Auth Module
 * 
 * Vanilla JS state container for authentication.
 * Wraps the @grey/adapters AuthController with a simple listener pattern.
 * 
 * @example
 * ```js
 * import { createAuthContainer } from '@grey/vanilla';
 * 
 * const auth = createAuthContainer({
 *   apiBaseUrl: 'https://api.grey.example.com',
 * });
 * 
 * // Subscribe to state changes
 * const unsubscribe = auth.subscribe((state) => {
 *   console.log('Auth state:', state);
 * });
 * 
 * // Login
 * await auth.login('user@example.com', 'password');
 * 
 * // Check state
 * console.log(auth.getState().isAuthenticated);
 * console.log(auth.getState().user);
 * 
 * // Logout
 * auth.logout();
 * 
 * // Cleanup
 * unsubscribe();
 * ```
 */

import {
  AuthController as CoreAuthController,
  BrowserTokenStorage,
  MemoryTokenStorage,
  type AuthState,
  type AuthConfig,
} from '@grey/adapters';
import type { GreyClient, AuthSession } from '@grey/core-client';
import { isBrowser, type Listener, type Unsubscribe } from './types.js';

/**
 * Options for creating an auth container
 */
export interface AuthContainerOptions {
  /** Base URL for the Grey API */
  apiBaseUrl: string;
  /** Whether to auto-restore session on creation (default: true) */
  autoRestoreSession?: boolean;
  /** Called when auth state changes */
  onAuthChange?: (state: AuthState) => void;
  /** Called when user logs out */
  onLogout?: () => void;
}

/**
 * Auth container interface
 * Provides reactive authentication state and actions
 */
export interface AuthContainer {
  // ============================================================
  // State Access
  // ============================================================
  
  /** Get the current auth state snapshot */
  getState(): AuthState;
  
  /** Subscribe to auth state changes */
  subscribe(listener: Listener<AuthState>): Unsubscribe;
  
  /** Get the Grey API client for direct API calls */
  getClient(): GreyClient;
  
  // ============================================================
  // State Properties (convenience getters)
  // ============================================================
  
  /** Whether the user is currently authenticated */
  readonly isAuthenticated: boolean;
  
  /** Whether an auth operation is in progress */
  readonly loading: boolean;
  
  /** Current error message (if any) */
  readonly error: string | null;
  
  /** The current user (if authenticated) */
  readonly user: AuthState['user'];
  
  // ============================================================
  // Actions
  // ============================================================
  
  /**
   * Login with email and password
   * @returns Promise resolving to true on success, false on failure
   */
  login(email: string, password: string): Promise<boolean>;
  
  /** Logout and clear session */
  logout(): void;
  
  /**
   * Attempt to restore session from storage
   * @returns Promise resolving to true if session was restored
   */
  restoreSession(): Promise<boolean>;
  
  /**
   * Refresh the access token
   * Note: Typically handled automatically by the core controller
   */
  refresh(): Promise<AuthSession>;
  
  /** Clear current error */
  clearError(): void;
}

/**
 * Create an auth container
 * 
 * Provides a simple observable state container for authentication
 * that wraps the @grey/adapters AuthController.
 * 
 * @param options - Configuration options
 * @returns AuthContainer instance
 * 
 * @example
 * ```js
 * const auth = createAuthContainer({
 *   apiBaseUrl: 'https://api.grey.example.com',
 * });
 * 
 * // Subscribe to changes
 * auth.subscribe((state) => {
 *   updateUI(state);
 * });
 * 
 * // Perform login
 * const success = await auth.login('email', 'password');
 * ```
 */
export function createAuthContainer(options: AuthContainerOptions): AuthContainer {
  // Internal state
  let _state: AuthState;
  const listeners = new Set<Listener<AuthState>>();
  
  // Create config for core controller
  const config: AuthConfig = {
    apiBaseUrl: options.apiBaseUrl,
    storage: isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage(),
    onAuthChange: (state: AuthState) => {
      _state = state;
      notifyListeners();
      options.onAuthChange?.(state);
    },
    onLogout: options.onLogout,
  };
  
  // Initialize core controller
  const coreController = new CoreAuthController(config);
  _state = coreController.getState();
  
  // Subscribe to core controller state changes
  coreController.subscribe((state: AuthState) => {
    _state = state;
    notifyListeners();
  });
  
  /**
   * Notify all listeners of state change
   */
  function notifyListeners(): void {
    listeners.forEach((listener) => listener(_state));
  }
  
  // Auto-restore session if enabled and in browser
  if (options.autoRestoreSession !== false && isBrowser()) {
    coreController.restoreSession().catch(() => {
      // Session restoration failed, user will need to login
    });
  }
  
  // Return the container interface
  const container: AuthContainer = {
    // State access
    getState() {
      return { ..._state };
    },
    
    subscribe(listener: Listener<AuthState>): Unsubscribe {
      listeners.add(listener);
      // Immediately call with current state
      listener(_state);
      return () => listeners.delete(listener);
    },
    
    getClient() {
      return coreController.getClient();
    },
    
    // Convenience getters
    get isAuthenticated() {
      return _state.isAuthenticated;
    },
    
    get loading() {
      return _state.isLoading;
    },
    
    get error() {
      return _state.error;
    },
    
    get user() {
      return _state.user;
    },
    
    // Actions
    login(email: string, password: string): Promise<boolean> {
      return coreController.login(email, password);
    },
    
    logout(): void {
      coreController.logout();
    },
    
    restoreSession(): Promise<boolean> {
      return coreController.restoreSession();
    },
    
    async refresh(): Promise<AuthSession> {
      const success = await coreController.restoreSession();
      if (!success) {
        throw new Error('Failed to refresh session');
      }
      // Return a placeholder session - core handles tokens internally
      return {
        access_token: '',
        refresh_token: '',
        expires_in: 0,
      };
    },
    
    clearError(): void {
      coreController.clearError();
    },
  };
  
  return container;
}

// Re-export related types
export type { AuthState, AuthConfig } from '@grey/adapters';
export { BrowserTokenStorage, MemoryTokenStorage, initialAuthState } from '@grey/adapters';
