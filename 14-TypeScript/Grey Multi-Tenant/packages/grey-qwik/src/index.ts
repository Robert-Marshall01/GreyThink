/**
 * Grey Qwik
 *
 * Qwik hooks for the Grey Multi-Tenant API.
 *
 * @packageDocumentation
 */

// =============================================================================
// Hooks
// =============================================================================

export { useAuth } from './hooks/useAuth.js';
export type { UseAuthReturn, GreyError, AuthState, AuthConfig } from './hooks/useAuth.js';

export { useUser } from './hooks/useUser.js';
export type { UseUserReturn, UserState } from './hooks/useUser.js';

export { useProjects } from './hooks/useProjects.js';
export type { UseProjectsReturn, ProjectsData, ProjectsState, CreateProjectInput } from './hooks/useProjects.js';

export { useQuery } from './hooks/useQuery.js';
export type { UseQueryOptions, UseQueryReturn, QueryOptions, QueryState } from './hooks/useQuery.js';

export { useMutation } from './hooks/useMutation.js';
export type { UseMutationOptions, UseMutationReturn, MutationOptions, MutationState } from './hooks/useMutation.js';

// =============================================================================
// Provider
// =============================================================================

export {
  GreyProvider,
  GreyAuthContextId,
  GreyUserContextId,
  GreyProjectsContextId,
  GreyClientContextId,
} from './provider/GreyProvider.js';
export type { GreyProviderProps } from './provider/GreyProvider.js';

// =============================================================================
// Re-export types from adapters
// =============================================================================

export type {
  User,
  Project,
  Pagination,
  AuthSession,
  GreyClient,
} from '@grey/core-client';
