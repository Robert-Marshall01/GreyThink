/**
 * Grey Angular - Angular Services for Grey Multi-Tenant
 *
 * Provides Angular-idiomatic services wrapping @grey/adapters controllers.
 * Uses Angular signals for reactive state management.
 */

// =============================================================================
// Services
// =============================================================================

export { AuthService, type AuthServiceConfig, type GreyError } from './services/auth.service';
export { UserService } from './services/user.service';
export { ProjectsService } from './services/projects.service';
export { QueryService } from './services/query.service';
export { MutationService } from './services/mutation.service';

// =============================================================================
// Provider
// =============================================================================

export { provideGrey, getGreyProviders, GREY_CONFIG, type GreyConfig } from './provider/grey-provider';

// =============================================================================
// Re-export types from adapters
// =============================================================================

export type {
  AuthState,
  AuthConfig,
  UserState,
  ProjectsState,
  ProjectState,
  CreateProjectInput,
  QueryOptions,
  MutationOptions,
  QueryState,
  MutationState,
} from '@grey/adapters';

export type { GreyClient, User, Project, Pagination } from '@grey/core-client';
