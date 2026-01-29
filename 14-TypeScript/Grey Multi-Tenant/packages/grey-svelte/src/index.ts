/**
 * Grey Svelte
 *
 * Svelte stores for the Grey Multi-Tenant API.
 *
 * @packageDocumentation
 */

// =============================================================================
// Stores
// =============================================================================

export { createAuthStore } from './stores/auth.js';
export type { AuthStoreReturn, GreyError, AuthState, AuthConfig } from './stores/auth.js';

export { createUserStore } from './stores/user.js';
export type { UserStoreReturn, UserState } from './stores/user.js';

export { createProjectsStore } from './stores/projects.js';
export type { ProjectsStoreReturn, ProjectsData, ProjectsState, CreateProjectInput } from './stores/projects.js';

export { createQueryStore } from './stores/query.js';
export type { QueryStoreOptions, QueryStoreReturn, QueryOptions } from './stores/query.js';

export { createMutationStore } from './stores/mutation.js';
export type { MutationStoreOptions, MutationStoreReturn, MutationOptions } from './stores/mutation.js';

// =============================================================================
// Provider
// =============================================================================

export { default as GreyProvider } from './provider/GreyProvider.svelte';
export {
  GREY_AUTH_KEY,
  GREY_USER_KEY,
  GREY_PROJECTS_KEY,
  GREY_CLIENT_KEY,
} from './provider/types.js';
export type {
  AuthContextValue,
  UserContextValue,
  ProjectsContextValue,
} from './provider/types.js';

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
