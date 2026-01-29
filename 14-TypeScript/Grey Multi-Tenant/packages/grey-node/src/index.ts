/**
 * Grey Node - Package Entry Point
 *
 * Barrel file for all exports.
 * No logic, only exports.
 */

// Services
export { AuthService } from './services/auth.service';
export type {
  AuthData,
  AuthServiceOptions,
  LoginCredentials,
  RefreshOptions,
} from './services/auth.service';

export { UserService } from './services/user.service';
export type { UserServiceOptions } from './services/user.service';

export { ProjectsService } from './services/projects.service';
export type {
  ProjectsServiceOptions,
  CreateProjectInput,
} from './services/projects.service';

export { QueryService } from './services/query.service';
export type {
  QueryServiceOptions,
  QueryInput,
} from './services/query.service';

export { MutationService } from './services/mutation.service';
export type {
  MutationServiceOptions,
  MutationInput,
} from './services/mutation.service';

// Shared error type
export type { GreyError } from './services/auth.service';
