/**
 * Grey Preact - Preact Hooks for Grey Multi-Tenant
 *
 * Provides Preact-idiomatic hooks wrapping @grey/adapters controllers.
 */

// =============================================================================
// Hooks
// =============================================================================

export { useAuth, type UseAuthConfig, type UseAuthResult, type GreyError } from './hooks/useAuth';
export { useUser, type UseUserResult } from './hooks/useUser';
export { useProjects, type UseProjectsResult } from './hooks/useProjects';
export { useQuery, type UseQueryResult } from './hooks/useQuery';
export { useMutation, type UseMutationResult } from './hooks/useMutation';

// =============================================================================
// Provider
// =============================================================================

export {
  GreyProvider,
  useGreyAuth,
  useGreyUser,
  useGreyProjects,
  useGreyClient,
  type GreyProviderProps,
  type GreyContextValue,
} from './provider/GreyProvider';

// =============================================================================
// Re-export types from adapters
// =============================================================================

export type {
  AuthState,
  AuthConfig,
  UserState,
  ProjectsState,
  CreateProjectInput,
  QueryOptions,
  MutationOptions,
  QueryState,
  MutationState,
} from '@grey/adapters';

export type { GreyClient, User, Project, Pagination } from '@grey/core-client';
