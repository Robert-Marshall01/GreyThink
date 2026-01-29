/**
 * Grey Next.js - Server Module
 *
 * Server-side utilities for Next.js.
 * All exports in this module are safe to use in:
 * - Server Components
 * - Server Actions
 * - API Routes
 * - Edge Runtime
 * - Middleware
 *
 * No React, no hooks, no browser APIs.
 */

// Auth
export {
  createServerClient,
  login,
  logout,
  refresh,
  validateToken,
  createAuthMiddleware,
  normalizeError,
} from './auth.js';

// User
export {
  fetchUser,
  getUserById,
  updateUser,
} from './user.js';

// Projects
export {
  listProjects,
  getProject,
  createProject,
  updateProject,
  deleteProject,
} from './projects.js';

// Query
export {
  query,
  queryWithController,
  batchQuery,
  queryWithRetry,
} from './query.js';

// Mutation
export {
  mutate,
  mutateWithVariables,
  mutateWithController,
  mutateSequence,
  mutateParallel,
  mutateWithVersion,
} from './mutation.js';

// Re-export types
export type {
  GreyClient,
  User,
  Project,
  AuthSession,
  Pagination,
} from '@grey/core-client';

export type {
  GreyError,
  ServerAuthResult,
  ServerUserResult,
  ServerProjectsResult,
  ServerQueryOptions,
  ServerQueryResult,
  ServerMutationOptions,
  ServerMutationResult,
  ServerClientOptions,
  MiddlewareConfig,
  CreateProjectInput,
} from '../types/index.js';
