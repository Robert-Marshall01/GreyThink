/**
 * Grey Remix - Package Entry Point
 *
 * Barrel file for all exports.
 * No logic, only exports.
 */

// Hooks
export { useAuth } from './hooks/useAuth';
export type {
  UseAuthResult,
  UseAuthOptions,
  AuthData,
} from './hooks/useAuth';

export { useUser } from './hooks/useUser';
export type {
  UseUserResult,
  UseUserOptions,
} from './hooks/useUser';

export { useProjects } from './hooks/useProjects';
export type {
  UseProjectsResult,
  UseProjectsOptions,
  CreateProjectInput,
} from './hooks/useProjects';

export { useQuery } from './hooks/useQuery';
export type {
  UseQueryResult,
  UseQueryOptions,
  QueryInput,
} from './hooks/useQuery';

export { useMutation } from './hooks/useMutation';
export type {
  UseMutationResult,
  UseMutationOptions,
  MutationInput,
} from './hooks/useMutation';

// Provider
export {
  GreyProvider,
  useGrey,
  useGreyAuth,
  useGreyUser,
  useGreyProjects,
  useGreyQuery,
  useGreyMutation,
} from './provider/GreyProvider';
export type {
  GreyContextValue,
  GreyProviderProps,
} from './provider/GreyProvider';

// Server modules
export { login, logout, refresh } from './server/auth.server';
export type {
  LoginCredentials,
  TokenOptions,
  RefreshOptions,
  AuthData as ServerAuthData,
} from './server/auth.server';

export { fetchUser } from './server/user.server';
export type { FetchUserOptions } from './server/user.server';

export { listProjects, createProject } from './server/projects.server';
export type {
  ProjectsOptions,
  CreateProjectInput as ServerCreateProjectInput,
} from './server/projects.server';

export { executeQuery } from './server/query.server';
export type {
  QueryOptions as ServerQueryOptions,
  QueryInput as ServerQueryInput,
} from './server/query.server';

export { executeMutation } from './server/mutation.server';
export type {
  MutationOptions as ServerMutationOptions,
  MutationInput as ServerMutationInput,
} from './server/mutation.server';

// Shared error type
export type { GreyError } from './hooks/useAuth';
