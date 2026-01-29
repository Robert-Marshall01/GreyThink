/**
 * Grey Adapters - Framework-Agnostic Core
 *
 * This package provides the shared logic for all Grey Multi-Tenant
 * frontend implementations. Each framework adapter wraps these
 * controllers in its idiomatic patterns.
 */

// Auth
export {
  AuthController,
  type AuthState,
  type AuthConfig,
  type TokenStorage,
  BrowserTokenStorage,
  MemoryTokenStorage,
  initialAuthState,
} from './auth.js';

// User
export {
  UserController,
  createUserController,
  type UserState,
  initialUserState,
} from './user.js';

// Projects
export {
  ProjectsController,
  createProjectsController,
  type ProjectsState,
  type ProjectState,
  type CreateProjectInput,
  type UpdateProjectInput,
  initialProjectsState,
  initialProjectState,
} from './projects.js';

// Query utilities
export {
  QueryController,
  MutationController,
  createQuery,
  createMutation,
  type QueryState,
  type MutationState,
  type QueryOptions,
  type MutationOptions,
  initialQueryState,
  initialMutationState,
} from './query.js';

// Re-export types from core client
export type {
  GreyClient,
  User,
  Organization,
  Project,
  AuthSession,
  Pagination,
} from '@grey/core-client';

// =============================================================================
// Integration Layer
// =============================================================================

// Adapter Core
export * from './adapter_core.js';

// Error Normalization
export * from './error_normalizer.js';

// HTTP Gateway
export * from './http_gateway/index.js';

// gRPC Service
export * from './grpc_service/index.js';

// Integration Manifest (import as JSON)
// import integrationManifest from './integration_manifest.json' assert { type: 'json' };
// export { integrationManifest };
