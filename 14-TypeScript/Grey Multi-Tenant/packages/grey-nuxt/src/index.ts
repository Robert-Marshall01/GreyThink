/**
 * Grey Nuxt - Multi-Tenant Authentication & Data Management
 *
 * Full-featured Nuxt package for Grey Multi-Tenant system.
 * Supports both server-side (Nitro) and client-side (Vue) usage.
 *
 * @packageDocumentation
 */

// ============================================================
// Composables
// ============================================================
export { useAuth, type UseAuthReturn, type AuthData, type GreyError } from './composables/useAuth.js';
export { useUser, type UseUserReturn, type UserData } from './composables/useUser.js';
export { useProjects, type UseProjectsReturn, type ProjectsData } from './composables/useProjects.js';
export { useQuery, type UseQueryReturn } from './composables/useQuery.js';
export { useMutation, type UseMutationReturn } from './composables/useMutation.js';

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
export { default as GreyProvider } from './provider/GreyProvider.vue';
export { GreyKey, type GreyContextValue } from './provider/types.js';
export { default as greyPlugin } from './provider/grey-provider.js';

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

