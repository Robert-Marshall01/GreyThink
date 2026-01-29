/**
 * Grey SvelteKit - Multi-Tenant Authentication & Data Management
 *
 * Full-featured SvelteKit package for Grey Multi-Tenant system.
 * Supports both server-side and client-side (Svelte) usage.
 *
 * @packageDocumentation
 */

// ============================================================
// Stores
// ============================================================
export { createAuthStore, type AuthStoreReturn, type AuthData, type GreyError } from './stores/auth.js';
export { createUserStore, type UserStoreReturn, type UserData } from './stores/user.js';
export { createProjectsStore, type ProjectsStoreReturn, type ProjectsData } from './stores/projects.js';
export { createQueryStore, type QueryStoreReturn } from './stores/query.js';
export { createMutationStore, type MutationStoreReturn } from './stores/mutation.js';

// ============================================================
// Server
// ============================================================
export {
  login,
  logout,
  refresh,
  normalizeError,
  type AuthResult,
  type RefreshResult,
  type LogoutResult,
} from './server/auth.js';
export { fetchUser, type UserResult } from './server/user.js';
export { listProjects, createProject, type ProjectsResult, type ProjectResult } from './server/projects.js';
export { executeQuery, type QueryResult } from './server/query.js';
export { executeMutation, type MutationResult } from './server/mutation.js';

// ============================================================
// Provider
// ============================================================
export { default as GreyProvider } from './provider/GreyProvider.svelte';
export { GREY_CONTEXT_KEY, type GreyContextValue } from './provider/types.js';

// ============================================================
// Re-export core types for convenience
// ============================================================
export type {
  GreyClient,
  AuthSession,
  User,
  Project,
  Pagination,
} from '@grey/core-client';
