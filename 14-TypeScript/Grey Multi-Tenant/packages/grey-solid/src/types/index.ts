/**
 * Grey Solid - Type Definitions
 *
 * Shared types for the SolidJS signals layer.
 */

import type { Accessor, Resource } from 'solid-js';
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

// =============================================================================
// Configuration Types
// =============================================================================

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
  /** Whether to auto-restore session on mount (default: true) */
  autoRestoreSession?: boolean;
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

// =============================================================================
// Signal Types
// =============================================================================

/**
 * Auth signal return type
 */
export interface AuthSignal {
  /** Reactive auth state */
  state: Accessor<CoreAuthState>;
  /** Whether user is authenticated */
  isAuthenticated: Accessor<boolean>;
  /** Current access token */
  accessToken: Accessor<string | null>;
  /** Whether auth is loading */
  isLoading: Accessor<boolean>;
  /** Current error message */
  error: Accessor<string | null>;
  /** Current user */
  user: Accessor<User | null>;
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
 * User signal return type
 */
export interface UserSignal {
  /** Reactive user state */
  state: Accessor<CoreUserState>;
  /** Current user */
  user: Accessor<User | null>;
  /** Whether loading */
  isLoading: Accessor<boolean>;
  /** Current error */
  error: Accessor<string | null>;
  /** Refresh user data */
  refresh: () => Promise<User | null>;
  /** Clear user state */
  clear: () => void;
  /** Clear error */
  clearError: () => void;
}

/**
 * Projects signal return type
 */
export interface ProjectsSignal {
  /** Reactive projects list state */
  state: Accessor<CoreProjectsState>;
  /** Projects list */
  projects: Accessor<Project[]>;
  /** Pagination info */
  pagination: Accessor<Pagination | null>;
  /** Whether loading */
  isLoading: Accessor<boolean>;
  /** Current error */
  error: Accessor<string | null>;
  /** Load projects list */
  load: (page?: number, pageSize?: number) => Promise<void>;
  /** Create a new project */
  create: (input: { name: string; description?: string }) => Promise<Project | null>;
  /** Clear error */
  clearError: () => void;
}

/**
 * Single project signal return type
 */
export interface ProjectSignal {
  /** Reactive project state */
  state: Accessor<CoreProjectState>;
  /** Current project */
  project: Accessor<Project | null>;
  /** Whether loading */
  isLoading: Accessor<boolean>;
  /** Current error */
  error: Accessor<string | null>;
  /** Load/reload project */
  load: () => Promise<Project | null>;
  /** Clear error */
  clearError: () => void;
}

// =============================================================================
// Query/Mutation Types
// =============================================================================

/**
 * Query signal options
 */
export interface QueryOptions<T> {
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
 * Query signal state
 */
export interface QueryState<T> {
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
 * Query signal return type
 */
export interface QuerySignal<T> {
  /** Reactive query state */
  state: Accessor<QueryState<T>>;
  /** Query data */
  data: Accessor<T | null>;
  /** Whether loading */
  isLoading: Accessor<boolean>;
  /** Whether error */
  isError: Accessor<boolean>;
  /** Error object */
  error: Accessor<Error | null>;
  /** Whether success */
  isSuccess: Accessor<boolean>;
  /** Refetch the query */
  refetch: () => Promise<T | null>;
}

/**
 * Mutation signal options
 */
export interface MutationOptions<TData, TVariables> {
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
 * Mutation signal state
 */
export interface MutationState<TData> {
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
 * Mutation signal return type
 */
export interface MutationSignal<TData, TVariables> {
  /** Reactive mutation state */
  state: Accessor<MutationState<TData>>;
  /** Mutation data */
  data: Accessor<TData | null>;
  /** Whether loading */
  isLoading: Accessor<boolean>;
  /** Whether error */
  isError: Accessor<boolean>;
  /** Error object */
  error: Accessor<Error | null>;
  /** Whether success */
  isSuccess: Accessor<boolean>;
  /** Execute mutation (fire-and-forget) */
  mutate: (variables: TVariables) => void;
  /** Execute mutation and return promise */
  mutateAsync: (variables: TVariables) => Promise<TData>;
  /** Reset mutation state */
  reset: () => void;
}
