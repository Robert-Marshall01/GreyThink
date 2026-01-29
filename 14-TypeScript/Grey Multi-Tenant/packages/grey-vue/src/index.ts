/**
 * Grey Vue
 *
 * Vue 3 composables for the Grey Multi-Tenant API.
 *
 * @packageDocumentation
 */

// Composables
export { useAuth } from './composables/useAuth.js';
export type { UseAuthReturn, GreyError } from './composables/useAuth.js';

export { useUser } from './composables/useUser.js';
export type { UseUserReturn } from './composables/useUser.js';

export { useProjects } from './composables/useProjects.js';
export type { UseProjectsReturn, ProjectsData } from './composables/useProjects.js';

export { useQuery } from './composables/useQuery.js';
export type { UseQueryOptions, UseQueryReturn } from './composables/useQuery.js';

export { useMutation } from './composables/useMutation.js';
export type { UseMutationOptions, UseMutationReturn } from './composables/useMutation.js';

// Provider
export { default as GreyProvider } from './provider/GreyProvider.vue';
export {
  GreyAuthKey,
  GreyUserKey,
  GreyProjectsKey,
  GreyClientKey,
} from './provider/types.js';
export type {
  GreyContextValue,
  AuthContextValue,
  UserContextValue,
  ProjectsContextValue,
} from './provider/types.js';

// Re-export types from adapters
export type {
  AuthState,
  UserState,
  ProjectsState,
  User,
  Project,
  Pagination,
  AuthSession,
} from '@grey/adapters';
