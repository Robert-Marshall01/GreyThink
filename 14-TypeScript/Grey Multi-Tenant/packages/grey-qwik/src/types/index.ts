/**
 * Grey Qwik - Type Definitions
 *
 * Shared types for the Qwik composables layer.
 */

import type { Signal, QRL } from '@builder.io/qwik';
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
// Composable Return Types
// =============================================================================

/**
 * Auth composable return type
 */
export interface AuthComposable {
  /** Reactive auth state */
  state: Signal<CoreAuthState>;
  /** Whether user is authenticated */
  isAuthenticated: Signal<boolean>;
  /** Whether auth is loading */
  isLoading: Signal<boolean>;
  /** Current error message */
  error: Signal<string | null>;
  /** Current user */
  user: Signal<User | null>;
  /** Login with email and password */
  login$: QRL<(email: string, password: string) => Promise<boolean>>;
  /** Logout and clear session */
  logout$: QRL<() => void>;
  /** Restore session from storage */
  restoreSession$: QRL<() => Promise<boolean>>;
  /** Clear current error */
  clearError$: QRL<() => void>;
}

/**
 * User composable return type
 */
export interface UserComposable {
  /** Reactive user state */
  state: Signal<CoreUserState>;
  /** Current user */
  user: Signal<User | null>;
  /** Whether loading */
  isLoading: Signal<boolean>;
  /** Current error */
  error: Signal<string | null>;
  /** Refresh user data */
  refresh$: QRL<() => Promise<User | null>>;
  /** Clear user state */
  clear$: QRL<() => void>;
  /** Clear error */
  clearError$: QRL<() => void>;
}

/**
 * Projects composable return type
 */
export interface ProjectsComposable {
  /** Reactive projects list state */
  state: Signal<CoreProjectsState>;
  /** Projects list */
  projects: Signal<Project[]>;
  /** Pagination info */
  pagination: Signal<Pagination | null>;
  /** Whether loading */
  isLoading: Signal<boolean>;
  /** Current error */
  error: Signal<string | null>;
  /** Load projects list */
  load$: QRL<(page?: number, pageSize?: number) => Promise<void>>;
  /** Create a new project */
  create$: QRL<(input: { name: string; description?: string }) => Promise<Project | null>>;
  /** Clear error */
  clearError$: QRL<() => void>;
}

/**
 * Single project composable return type
 */
export interface ProjectComposable {
  /** Reactive project state */
  state: Signal<CoreProjectState>;
  /** Current project */
  project: Signal<Project | null>;
  /** Whether loading */
  isLoading: Signal<boolean>;
  /** Current error */
  error: Signal<string | null>;
  /** Load/reload project */
  load$: QRL<() => Promise<Project | null>>;
  /** Clear error */
  clearError$: QRL<() => void>;
}

// =============================================================================
// Query/Mutation Types
// =============================================================================

/**
 * Query composable options
 */
export interface QueryOptions<T> {
  /** Query key for caching */
  key: string | string[];
  /** Async function that fetches data */
  queryFn: QRL<() => Promise<T>>;
  /** Whether to fetch on creation (default: true) */
  enabled?: boolean;
  /** Refetch interval in ms (0 = disabled) */
  refetchInterval?: number;
  /** Initial data */
  initialData?: T;
  /** Stale time in ms */
  staleTime?: number;
  /** Called on success */
  onSuccess$?: QRL<(data: T) => void>;
  /** Called on error */
  onError$?: QRL<(error: Error) => void>;
}

/**
 * Query composable state
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
 * Query composable return type
 */
export interface QueryComposable<T> {
  /** Reactive query state */
  state: Signal<QueryState<T>>;
  /** Query data */
  data: Signal<T | null>;
  /** Whether loading */
  isLoading: Signal<boolean>;
  /** Whether error */
  isError: Signal<boolean>;
  /** Error object */
  error: Signal<Error | null>;
  /** Whether success */
  isSuccess: Signal<boolean>;
  /** Refetch the query */
  refetch$: QRL<() => Promise<T | null>>;
}

/**
 * Mutation composable options
 */
export interface MutationOptions<TData, TVariables> {
  /** Async function that performs the mutation */
  mutationFn: QRL<(variables: TVariables) => Promise<TData>>;
  /** Called on success */
  onSuccess$?: QRL<(data: TData, variables: TVariables) => void>;
  /** Called on error */
  onError$?: QRL<(error: Error, variables: TVariables) => void>;
  /** Called when mutation starts */
  onMutate$?: QRL<(variables: TVariables) => void>;
  /** Called when mutation settles */
  onSettled$?: QRL<(data: TData | null, error: Error | null, variables: TVariables) => void>;
  /** Query keys to invalidate on success */
  invalidateKeys?: string | string[];
}

/**
 * Mutation composable state
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
 * Mutation composable return type
 */
export interface MutationComposable<TData, TVariables> {
  /** Reactive mutation state */
  state: Signal<MutationState<TData>>;
  /** Mutation data */
  data: Signal<TData | null>;
  /** Whether loading */
  isLoading: Signal<boolean>;
  /** Whether error */
  isError: Signal<boolean>;
  /** Error object */
  error: Signal<Error | null>;
  /** Whether success */
  isSuccess: Signal<boolean>;
  /** Execute mutation (fire-and-forget) */
  mutate$: QRL<(variables: TVariables) => void>;
  /** Execute mutation and return promise */
  mutateAsync$: QRL<(variables: TVariables) => Promise<TData>>;
  /** Reset mutation state */
  reset$: QRL<() => void>;
}
