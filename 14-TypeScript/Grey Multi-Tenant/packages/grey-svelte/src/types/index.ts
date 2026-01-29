/**
 * Grey Svelte - Type Definitions
 *
 * Shared types for the Svelte stores layer.
 */

import type { Readable, Writable } from 'svelte/store';
import type { AuthSession, User, Project, Pagination, GreyClient } from '@grey/core-client';
import type {
  AuthState as CoreAuthState,
  UserState as CoreUserState,
  ProjectsState as CoreProjectsState,
  ProjectState as CoreProjectState,
  AuthController,
  UserController,
  ProjectsController,
} from '@grey/adapters';

// Re-export core types for convenience
export type { AuthSession, User, Project, Pagination, GreyClient };

// Re-export adapter state types
export type {
  CoreAuthState,
  CoreUserState,
  CoreProjectsState,
  CoreProjectState,
};

/**
 * Grey initialization configuration
 */
export interface GreyConfig {
  /** Base URL for the Grey API (e.g., "http://localhost:8080/api/v1") */
  baseUrl: string;
  /** Optional initial auth session (for SSR hydration) */
  initialSession?: AuthSession | null;
  /** Optional initial user (for SSR hydration) */
  initialUser?: User | null;
  /** Callback when auth state changes */
  onAuthChange?: (state: CoreAuthState) => void;
  /** Callback when user logs out */
  onLogout?: () => void;
}

/**
 * Grey context value - internal state holder
 */
export interface GreyContext {
  /** Base URL for the API */
  baseUrl: string;
  /** Raw Grey API client */
  client: GreyClient;
  /** Auth controller instance */
  authController: AuthController;
  /** User controller instance */
  userController: UserController;
  /** Projects controller instance */
  projectsController: ProjectsController;
  /** Query cache for caching query results */
  queryCache: Map<string, unknown>;
  /** Invalidate cached queries by key */
  invalidateQueries: (key: string | string[]) => void;
}

/**
 * Auth store type - Svelte readable store with actions
 */
export interface AuthStore extends Readable<CoreAuthState> {
  /** Login with email and password */
  login: (email: string, password: string) => Promise<boolean>;
  /** Logout and clear session */
  logout: () => void;
  /** Restore session from storage */
  restoreSession: () => Promise<boolean>;
  /** Clear current error */
  clearError: () => void;
}

/**
 * User store type - Svelte readable store with actions
 */
export interface UserStore extends Readable<CoreUserState> {
  /** Refresh user data from API */
  refresh: () => Promise<User | null>;
  /** Clear user state */
  clear: () => void;
  /** Clear current error */
  clearError: () => void;
}

/**
 * Projects store type - Svelte readable store with actions
 */
export interface ProjectsStore extends Readable<CoreProjectsState> {
  /** Load projects list with optional pagination */
  load: (page?: number, pageSize?: number) => Promise<void>;
  /** Create a new project */
  create: (input: { name: string; description?: string }) => Promise<Project | null>;
  /** Clear list error */
  clearError: () => void;
}

/**
 * Single project store type
 */
export interface ProjectStore extends Readable<CoreProjectState> {
  /** Load or reload project */
  load: () => Promise<Project | null>;
  /** Clear error */
  clearError: () => void;
}

/**
 * Query store options
 */
export interface QueryStoreOptions<T> {
  /** Query key for caching */
  key: string | string[];
  /** Async function that fetches data */
  queryFn: () => Promise<T>;
  /** Whether to fetch on creation (default: true) */
  enabled?: boolean;
  /** Refetch interval in ms (0 = disabled) */
  refetchInterval?: number;
  /** Initial data */
  initialData?: T;
  /** Stale time in ms */
  staleTime?: number;
  /** Called on success */
  onSuccess?: (data: T) => void;
  /** Called on error */
  onError?: (error: Error) => void;
}

/**
 * Query store state
 */
export interface QueryStoreState<T> {
  /** Query result data */
  data: T | null;
  /** Whether query is loading */
  isLoading: boolean;
  /** Whether query has error */
  isError: boolean;
  /** Error object if any */
  error: Error | null;
  /** Whether query succeeded */
  isSuccess: boolean;
  /** Whether data is stale */
  isStale: boolean;
}

/**
 * Query store type
 */
export interface QueryStore<T> extends Readable<QueryStoreState<T>> {
  /** Refetch the query */
  refetch: () => Promise<T | null>;
  /** Destroy store and cleanup */
  destroy: () => void;
}

/**
 * Mutation store options
 */
export interface MutationStoreOptions<TData, TVariables> {
  /** Async function that performs the mutation */
  mutationFn: (variables: TVariables) => Promise<TData>;
  /** Called on success */
  onSuccess?: (data: TData, variables: TVariables) => void;
  /** Called on error */
  onError?: (error: Error, variables: TVariables) => void;
  /** Called when mutation starts */
  onMutate?: (variables: TVariables) => void;
  /** Called when mutation settles */
  onSettled?: (data: TData | null, error: Error | null, variables: TVariables) => void;
  /** Query keys to invalidate on success */
  invalidateKeys?: string | string[];
}

/**
 * Mutation store state
 */
export interface MutationStoreState<TData> {
  /** Mutation result data */
  data: TData | null;
  /** Whether mutation is in progress */
  isLoading: boolean;
  /** Whether mutation has error */
  isError: boolean;
  /** Error object if any */
  error: Error | null;
  /** Whether mutation succeeded */
  isSuccess: boolean;
  /** Whether mutation is idle */
  isIdle: boolean;
}

/**
 * Mutation store type
 */
export interface MutationStore<TData, TVariables> extends Readable<MutationStoreState<TData>> {
  /** Execute mutation (fire-and-forget) */
  mutate: (variables: TVariables) => void;
  /** Execute mutation and return promise */
  mutateAsync: (variables: TVariables) => Promise<TData | null>;
  /** Reset mutation state */
  reset: () => void;
}
