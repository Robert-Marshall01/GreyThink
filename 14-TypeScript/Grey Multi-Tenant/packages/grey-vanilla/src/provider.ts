/**
 * Grey Vanilla - Provider Module
 * 
 * Aggregates all Grey state containers into a single provider object.
 * Provides a convenient way to initialize and access all Grey functionality.
 * 
 * @example
 * ```js
 * import { createGreyProvider } from '@grey/vanilla';
 * 
 * // Create the provider
 * const grey = createGreyProvider({
 *   apiBaseUrl: 'https://api.grey.example.com',
 *   autoRestoreSession: true,
 * });
 * 
 * // Access auth state
 * grey.auth.subscribe((state) => {
 *   console.log('Auth:', state.isAuthenticated);
 * });
 * 
 * // Login
 * await grey.auth.login('user@example.com', 'password');
 * 
 * // Fetch user and projects
 * await grey.user.fetchUser();
 * await grey.projects.listProjects();
 * 
 * // Create custom queries
 * const statsQuery = grey.createQuery({
 *   queryFn: () => fetchStats(),
 * });
 * 
 * // Create custom mutations
 * const updateProfile = grey.createMutation({
 *   mutationFn: (data) => api.updateProfile(data),
 *   onSuccess: () => grey.user.refetch(),
 * });
 * ```
 */

import { createAuthContainer, type AuthContainer, type AuthContainerOptions } from './auth.js';
import { createUserContainer, type UserContainer, type UserContainerOptions } from './user.js';
import { createProjectsContainer, type ProjectsContainer, type ProjectsContainerOptions } from './projects.js';
import { createQueryContainer, type QueryContainer, type QueryContainerOptions } from './query.js';
import { createMutationContainer, type MutationContainer, type MutationContainerOptions } from './mutation.js';
import type { GreyClient } from '@grey/core-client';
import type { AuthState } from '@grey/adapters';

/**
 * Configuration for creating a Grey provider
 */
export interface GreyProviderConfig {
  /** Base URL for the Grey API */
  apiBaseUrl: string;
  /** Whether to auto-restore session on initialization (default: true) */
  autoRestoreSession?: boolean;
  /** Called when auth state changes */
  onAuthChange?: (state: AuthState) => void;
  /** Called when user logs out */
  onLogout?: () => void;
}

/**
 * Grey provider interface
 * Aggregates all state containers and provides factory methods
 */
export interface GreyProvider {
  // ============================================================
  // State Containers
  // ============================================================
  
  /** Authentication state container */
  readonly auth: AuthContainer;
  
  /** User data state container */
  readonly user: UserContainer;
  
  /** Projects data state container */
  readonly projects: ProjectsContainer;
  
  // ============================================================
  // Client Access
  // ============================================================
  
  /** Get the Grey API client for direct API calls */
  getClient(): GreyClient;
  
  // ============================================================
  // Factory Methods
  // ============================================================
  
  /**
   * Create a custom query container
   * @param options - Query options
   * @returns QueryContainer instance
   */
  createQuery<T>(options: QueryContainerOptions<T>): QueryContainer<T>;
  
  /**
   * Create a custom mutation container
   * @param options - Mutation options
   * @returns MutationContainer instance
   */
  createMutation<TData, TVariables>(
    options: MutationContainerOptions<TData, TVariables>
  ): MutationContainer<TData, TVariables>;
  
  /**
   * Create an additional user container (for edge cases)
   * @param options - Optional overrides
   */
  createUserContainer(options?: Partial<UserContainerOptions>): UserContainer;
  
  /**
   * Create an additional projects container (for edge cases)
   * @param options - Optional overrides
   */
  createProjectsContainer(options?: Partial<ProjectsContainerOptions>): ProjectsContainer;
}

/**
 * Create a Grey provider
 * 
 * This is the main entry point for the Vanilla JS Grey integration.
 * It creates and aggregates all state containers and provides
 * factory methods for creating custom queries and mutations.
 * 
 * @param config - Configuration options
 * @returns GreyProvider instance
 * 
 * @example
 * ```js
 * // Basic setup
 * const grey = createGreyProvider({
 *   apiBaseUrl: 'https://api.grey.example.com',
 * });
 * 
 * // Subscribe to auth changes
 * grey.auth.subscribe((state) => {
 *   if (state.isAuthenticated) {
 *     // User is logged in
 *     grey.user.fetchUser();
 *     grey.projects.listProjects();
 *   }
 * });
 * 
 * // Login flow
 * async function handleLogin(email, password) {
 *   const success = await grey.auth.login(email, password);
 *   if (success) {
 *     console.log('Welcome,', grey.auth.user.name);
 *   } else {
 *     console.error('Login failed:', grey.auth.error);
 *   }
 * }
 * 
 * // Logout flow
 * function handleLogout() {
 *   grey.auth.logout();
 *   grey.user.clear();
 * }
 * ```
 * 
 * @example
 * ```js
 * // With custom queries
 * const grey = createGreyProvider({ apiBaseUrl: '...' });
 * 
 * const analytics = grey.createQuery({
 *   queryFn: async () => {
 *     const client = grey.getClient();
 *     // Use client for API calls
 *     return { visits: 100, users: 50 };
 *   },
 * });
 * 
 * await analytics.execute();
 * console.log('Analytics:', analytics.data);
 * ```
 */
export function createGreyProvider(config: GreyProviderConfig): GreyProvider {
  // Create auth container first (provides the client)
  const authOptions: AuthContainerOptions = {
    apiBaseUrl: config.apiBaseUrl,
    autoRestoreSession: config.autoRestoreSession,
    onAuthChange: config.onAuthChange,
    onLogout: config.onLogout,
  };
  const auth = createAuthContainer(authOptions);
  
  // Get the client from auth
  const client = auth.getClient();
  
  // Create user container
  const user = createUserContainer({
    client,
    autoFetch: false, // Don't auto-fetch, let auth handle initial user data
  });
  
  // Create projects container
  const projects = createProjectsContainer({
    client,
    autoFetch: false, // Don't auto-fetch until authenticated
  });
  
  // Return the provider
  const provider: GreyProvider = {
    // State containers
    auth,
    user,
    projects,
    
    // Client access
    getClient() {
      return client;
    },
    
    // Factory methods
    createQuery<T>(options: QueryContainerOptions<T>): QueryContainer<T> {
      return createQueryContainer(options);
    },
    
    createMutation<TData, TVariables>(
      options: MutationContainerOptions<TData, TVariables>
    ): MutationContainer<TData, TVariables> {
      return createMutationContainer(options);
    },
    
    createUserContainer(options?: Partial<UserContainerOptions>): UserContainer {
      return createUserContainer({
        client,
        ...options,
      });
    },
    
    createProjectsContainer(options?: Partial<ProjectsContainerOptions>): ProjectsContainer {
      return createProjectsContainer({
        client,
        ...options,
      });
    },
  };
  
  return provider;
}

// Re-export container types
export type {
  AuthContainer,
  AuthContainerOptions,
} from './auth.js';

export type {
  UserContainer,
  UserContainerOptions,
} from './user.js';

export type {
  ProjectsContainer,
  ProjectsContainerOptions,
} from './projects.js';

export type {
  QueryContainer,
  QueryContainerOptions,
} from './query.js';

export type {
  MutationContainer,
  MutationContainerOptions,
} from './mutation.js';
