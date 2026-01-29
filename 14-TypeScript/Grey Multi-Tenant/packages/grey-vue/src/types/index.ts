/**
 * Grey Vue - Type Definitions
 *
 * Shared types for the Vue bindings layer.
 */

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
 * Grey provider configuration options
 */
export interface GreyProviderConfig {
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
 * Grey context value - injected by GreyProvider
 */
export interface GreyContextValue {
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
 * Auth composable return type
 */
export interface UseAuthReturn {
  /** Current auth session */
  session: AuthSession | null;
  /** Current user from auth state */
  user: User | null;
  /** Whether user is authenticated */
  isAuthenticated: boolean;
  /** Whether auth operation is in progress */
  isLoading: boolean;
  /** Auth error message if any */
  error: string | null;
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
 * User composable return type
 */
export interface UseUserReturn {
  /** Current user data */
  user: User | null;
  /** Whether user fetch is in progress */
  isLoading: boolean;
  /** Error message if any */
  error: string | null;
  /** Refetch user data */
  refresh: () => Promise<User | null>;
  /** Clear user state */
  clear: () => void;
  /** Clear current error */
  clearError: () => void;
}

/**
 * Projects composable return type
 */
export interface UseProjectsReturn {
  /** List of projects */
  projects: Project[];
  /** Pagination info */
  pagination: Pagination | null;
  /** Whether list is loading */
  isLoading: boolean;
  /** Error message if any */
  error: string | null;
  /** Load projects list */
  load: (page?: number, pageSize?: number) => Promise<void>;
  /** Create a new project */
  create: (input: { name: string; description?: string }) => Promise<Project | null>;
  /** Clear list error */
  clearError: () => void;
}

/**
 * Single project composable return type
 */
export interface UseProjectReturn {
  /** Project data */
  project: Project | null;
  /** Whether project is loading */
  isLoading: boolean;
  /** Error message if any */
  error: string | null;
  /** Load or reload project */
  load: () => Promise<Project | null>;
  /** Clear error */
  clearError: () => void;
}

/**
 * Query options for useQuery composable
 */
export interface QueryOptions<T> {
  /** Query key for caching */
  key: string | string[];
  /** Async function that fetches data */
  queryFn: () => Promise<T>;
  /** Whether to fetch on mount (default: true) */
  enabled?: boolean;
  /** Refetch interval in ms */
  refetchInterval?: number;
  /** Initial data */
  initialData?: T;
  /** Called on success */
  onSuccess?: (data: T) => void;
  /** Called on error */
  onError?: (error: Error) => void;
}

/**
 * Query composable return type
 */
export interface UseQueryReturn<T> {
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
  /** Refetch the query */
  refetch: () => Promise<T | null>;
}

/**
 * Mutation options for useMutation composable
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
  /** Called when mutation settles (success or error) */
  onSettled?: (data: TData | null, error: Error | null, variables: TVariables) => void;
}

/**
 * Mutation composable return type
 */
export interface UseMutationReturn<TData, TVariables> {
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
  /** Execute the mutation */
  mutate: (variables: TVariables) => void;
  /** Execute mutation and return promise */
  mutateAsync: (variables: TVariables) => Promise<TData | null>;
  /** Reset mutation state */
  reset: () => void;
}
