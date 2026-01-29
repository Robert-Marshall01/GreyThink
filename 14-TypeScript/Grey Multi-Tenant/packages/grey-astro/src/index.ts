/**
 * Grey Astro - Package Entry Point
 *
 * Barrel file for all exports.
 * No logic, only exports.
 */

// Stores
export {
  createAuthStore,
  authStore,
} from './stores/auth';
export type {
  AuthData,
  AuthStoreState,
  AuthStoreOptions,
} from './stores/auth';

export {
  createUserStore,
  userStore,
} from './stores/user';
export type {
  UserStoreState,
  UserStoreOptions,
} from './stores/user';

export {
  createProjectsStore,
  projectsStore,
} from './stores/projects';
export type {
  CreateProjectInput,
  ProjectsStoreState,
  ProjectsStoreOptions,
} from './stores/projects';

export {
  createQueryStore,
  queryStore,
} from './stores/query';
export type {
  QueryInput,
  QueryStoreState,
  QueryStoreOptions,
} from './stores/query';

export {
  createMutationStore,
  mutationStore,
} from './stores/mutation';
export type {
  MutationInput,
  MutationStoreState,
  MutationStoreOptions,
} from './stores/mutation';

// Server modules
export { login, logout, refresh } from './server/auth';
export type {
  LoginCredentials,
  TokenOptions,
  RefreshOptions,
  AuthData as ServerAuthData,
} from './server/auth';

export { fetchUser } from './server/user';
export type { FetchUserOptions } from './server/user';

export { listProjects, createProject } from './server/projects';
export type {
  ProjectsOptions,
  CreateProjectInput as ServerCreateProjectInput,
} from './server/projects';

export { executeQuery } from './server/query';
export type {
  QueryOptions as ServerQueryOptions,
  QueryInput as ServerQueryInput,
} from './server/query';

export { executeMutation } from './server/mutation';
export type {
  MutationOptions as ServerMutationOptions,
  MutationInput as ServerMutationInput,
} from './server/mutation';

// Shared error type
export type { GreyError } from './stores/auth';
