/**
 * Grey Electron - Package Entry Point
 *
 * Barrel file for all exports.
 * No logic, only exports.
 */

// ============================================================
// Renderer Hooks
// ============================================================

export { useAuth } from './renderer/useAuth';
export type {
  UseAuthResult,
  AuthData,
} from './renderer/useAuth';

export { useUser } from './renderer/useUser';
export type { UseUserResult } from './renderer/useUser';

export { useProjects } from './renderer/useProjects';
export type {
  UseProjectsResult,
  CreateProjectInput,
} from './renderer/useProjects';

export { useQuery } from './renderer/useQuery';
export type {
  UseQueryResult,
  QueryInput,
} from './renderer/useQuery';

export { useMutation } from './renderer/useMutation';
export type {
  UseMutationResult,
  MutationInput,
} from './renderer/useMutation';

// ============================================================
// Renderer Provider
// ============================================================

export {
  GreyProvider,
  useGrey,
  useGreyAuth,
  useGreyUser,
  useGreyProjects,
  useGreyQuery,
  useGreyMutation,
} from './renderer/GreyProvider';
export type {
  GreyContextValue,
  GreyProviderProps,
} from './renderer/GreyProvider';

// ============================================================
// Preload Bridge Types
// ============================================================

export type { GreyBridgeApi } from './preload/bridge';

// ============================================================
// Main Process Services (optional - for main process usage)
// ============================================================

export { AuthService } from './main/auth.service';
export type {
  AuthServiceOptions,
  LoginCredentials,
  RefreshOptions,
} from './main/auth.service';

export { UserService } from './main/user.service';
export type {
  UserServiceOptions,
  FetchUserOptions,
} from './main/user.service';

export { ProjectsService } from './main/projects.service';
export type {
  ProjectsServiceOptions,
  ProjectsRequestOptions,
  CreateProjectInput as MainCreateProjectInput,
} from './main/projects.service';

export { QueryService } from './main/query.service';
export type {
  QueryServiceOptions,
  QueryRequestOptions,
  QueryInput as MainQueryInput,
} from './main/query.service';

export { MutationService } from './main/mutation.service';
export type {
  MutationServiceOptions,
  MutationRequestOptions,
  MutationInput as MainMutationInput,
} from './main/mutation.service';

// ============================================================
// IPC Handlers
// ============================================================

export {
  registerIpcHandlers,
  unregisterIpcHandlers,
  IPC_CHANNELS,
} from './main/ipc';
export type { IpcServicesConfig } from './main/ipc';

// ============================================================
// Shared Error Type
// ============================================================

export type { GreyError } from './renderer/useAuth';
