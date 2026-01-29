/**
 * @grey/vanilla - Vanilla JavaScript bindings for Grey Multi-Tenant
 * 
 * Framework-agnostic implementation with observable state containers.
 * Works with any JavaScript project, no framework required.
 * 
 * This package provides simple state containers that wrap the @grey/adapters
 * core controllers. Each container follows a consistent pattern:
 * - getState() to get current state snapshot
 * - subscribe(listener) to react to state changes
 * - Action methods to trigger state updates
 * 
 * @example
 * ```js
 * import { createGreyProvider } from '@grey/vanilla';
 * 
 * // Create the main provider
 * const grey = createGreyProvider({
 *   apiBaseUrl: 'https://api.grey.example.com',
 *   autoRestoreSession: true,
 * });
 * 
 * // Subscribe to auth changes
 * grey.auth.subscribe((state) => {
 *   if (state.isAuthenticated) {
 *     console.log('Welcome,', state.user?.name);
 *   }
 * });
 * 
 * // Login
 * await grey.auth.login('user@example.com', 'password');
 * 
 * // Fetch data
 * await grey.user.fetchUser();
 * await grey.projects.listProjects();
 * 
 * // Access state
 * console.log(grey.projects.projects);
 * ```
 * 
 * @example
 * ```js
 * // Using individual containers
 * import { createAuthContainer, createQueryContainer } from '@grey/vanilla';
 * 
 * const auth = createAuthContainer({
 *   apiBaseUrl: 'https://api.grey.example.com',
 * });
 * 
 * const statsQuery = createQueryContainer({
 *   queryFn: () => fetch('/api/stats').then(r => r.json()),
 * });
 * 
 * // Subscribe and execute
 * statsQuery.subscribe(console.log);
 * await statsQuery.execute();
 * ```
 */

// ============================================================
// Provider (Main Entry Point)
// ============================================================

export {
  createGreyProvider,
  type GreyProvider,
  type GreyProviderConfig,
} from './provider.js';

// ============================================================
// Auth Module
// ============================================================

export {
  createAuthContainer,
  BrowserTokenStorage,
  MemoryTokenStorage,
  initialAuthState,
  type AuthContainer,
  type AuthContainerOptions,
  type AuthState,
  type AuthConfig,
} from './auth.js';

// ============================================================
// User Module
// ============================================================

export {
  createUserContainer,
  initialUserState,
  createUserController,
  type UserContainer,
  type UserContainerOptions,
  type UserState,
} from './user.js';

// ============================================================
// Projects Module
// ============================================================

export {
  createProjectsContainer,
  initialProjectsState,
  initialProjectState,
  createProjectsController,
  type ProjectsContainer,
  type ProjectsContainerOptions,
  type ProjectsState,
  type ProjectState,
  type CreateProjectInput,
  type UpdateProjectInput,
} from './projects.js';

// ============================================================
// Query Module
// ============================================================

export {
  createQueryContainer,
  initialQueryState,
  createQuery,
  type QueryContainer,
  type QueryContainerOptions,
  type QueryState,
  type QueryOptions,
} from './query.js';

// ============================================================
// Mutation Module
// ============================================================

export {
  createMutationContainer,
  initialMutationState,
  createMutation,
  type MutationContainer,
  type MutationContainerOptions,
  type MutationState,
  type MutationOptions,
} from './mutation.js';

// ============================================================
// Types
// ============================================================

export {
  isBrowser,
  type Listener,
  type Unsubscribe,
  type StateContainer,
  type Observable,
} from './types.js';

// ============================================================
// Re-exports from core client
// ============================================================

export type {
  GreyClient,
  AuthSession,
  User,
  Project,
  Pagination,
  Organization,
} from '@grey/core-client';

// ============================================================
// DOM Helpers (Vanilla JS utilities)
// ============================================================

/**
 * Bind observable state to a DOM element.
 * Updates element content when state changes.
 * 
 * @param container - State container with subscribe method
 * @param elementId - DOM element ID to bind to
 * @param render - Function that returns HTML string from state
 * @returns Unsubscribe function
 * 
 * @example
 * ```js
 * import { createGreyProvider, bindToElement } from '@grey/vanilla';
 * 
 * const grey = createGreyProvider({ apiBaseUrl: '...' });
 * 
 * // Bind auth status to a DOM element
 * bindToElement(grey.auth, 'auth-status', (state) =>
 *   state.isAuthenticated
 *     ? `<p>Welcome, ${state.user?.name}!</p>`
 *     : '<p>Not logged in</p>'
 * );
 * ```
 */
export function bindToElement<T>(
  container: { subscribe: (listener: (state: T) => void) => () => void; getState: () => T },
  elementId: string,
  render: (state: T) => string
): () => void {
  const element = document.getElementById(elementId);
  if (!element) {
    console.warn(`Element with id "${elementId}" not found`);
    return () => {};
  }

  const update = (state: T) => {
    element.innerHTML = render(state);
  };

  // Initial render
  update(container.getState());

  // Subscribe to changes
  return container.subscribe(update);
}

/**
 * Bind observable state to a callback function.
 * Calls callback on initial state and every change.
 * 
 * @param container - State container with subscribe method
 * @param callback - Function to call with state
 * @returns Unsubscribe function
 * 
 * @example
 * ```js
 * import { createGreyProvider, bindToCallback } from '@grey/vanilla';
 * 
 * const grey = createGreyProvider({ apiBaseUrl: '...' });
 * 
 * // React to projects changes
 * bindToCallback(grey.projects, (state) => {
 *   renderProjectsList(state.projects);
 *   updatePagination(state.pagination);
 * });
 * ```
 */
export function bindToCallback<T>(
  container: { subscribe: (listener: (state: T) => void) => () => void; getState: () => T },
  callback: (state: T) => void
): () => void {
  // Initial call
  callback(container.getState());

  // Subscribe to changes
  return container.subscribe(callback);
}

/**
 * Wait for a condition to be true in state.
 * Useful for waiting for auth to complete, data to load, etc.
 * 
 * @param container - State container with subscribe method
 * @param predicate - Function that returns true when condition is met
 * @param timeout - Maximum time to wait in ms (default: 30000)
 * @returns Promise that resolves with state when condition is met
 * 
 * @example
 * ```js
 * import { createGreyProvider, waitFor } from '@grey/vanilla';
 * 
 * const grey = createGreyProvider({ apiBaseUrl: '...' });
 * 
 * // Wait for auth to complete
 * const authState = await waitFor(
 *   grey.auth,
 *   (state) => !state.isLoading
 * );
 * 
 * if (authState.isAuthenticated) {
 *   console.log('Logged in as:', authState.user?.name);
 * }
 * ```
 */
export function waitFor<T>(
  container: { subscribe: (listener: (state: T) => void) => () => void; getState: () => T },
  predicate: (state: T) => boolean,
  timeout = 30000
): Promise<T> {
  return new Promise((resolve, reject) => {
    // Check if already satisfied
    const currentState = container.getState();
    if (predicate(currentState)) {
      resolve(currentState);
      return;
    }

    // Set up timeout
    const timeoutId = setTimeout(() => {
      unsubscribe();
      reject(new Error('waitFor timeout exceeded'));
    }, timeout);

    // Subscribe to changes
    const unsubscribe = container.subscribe((state) => {
      if (predicate(state)) {
        clearTimeout(timeoutId);
        unsubscribe();
        resolve(state);
      }
    });
  });
}
