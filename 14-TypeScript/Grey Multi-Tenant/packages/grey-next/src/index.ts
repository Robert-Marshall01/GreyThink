/**
 * @grey/next - Next.js bindings for Grey Multi-Tenant
 *
 * Barrel exports for all hooks, server actions, and provider.
 * No logic, only exports.
 */

// =============================================================================
// Hooks (Client Components)
// =============================================================================

export { useAuth, type UseAuthReturn, type AuthData } from './hooks/useAuth.js';
export { useUser, type UseUserReturn } from './hooks/useUser.js';
export { useProjects, type UseProjectsReturn, type ProjectsData } from './hooks/useProjects.js';
export { useQuery, type UseQueryReturn } from './hooks/useQuery.js';
export { useMutation, type UseMutationReturn } from './hooks/useMutation.js';

// =============================================================================
// Server Actions
// =============================================================================

export {
  login,
  logout,
  refresh,
  type GreyError,
  type AuthResult,
} from './server/auth.js';

export { fetchUser } from './server/user.js';

export {
  listProjects,
  createProject,
  type ProjectsResult,
  type ProjectResult,
} from './server/projects.js';

export { executeQuery, type QueryResult } from './server/query.js';

export { executeMutation, type MutationResult } from './server/mutation.js';

// =============================================================================
// Provider
// =============================================================================

export {
  GreyProvider,
  useGreyContext,
  useGreyAuth,
  useGreyUser,
  useGreyProjects,
  type GreyContextValue,
  type GreyProviderProps,
} from './provider/GreyProvider.js';
