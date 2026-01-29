/**
 * Grey Integration Layer - HTTP Gateway Index
 *
 * Main entry point for the HTTP gateway.
 * Registers all controllers and exports the router.
 */

import type { AdapterCore } from '../adapter_core';
import { HttpRouter, extractContext, parseQuery } from './router';
import { registerAuthRoutes } from './controllers/auth_controller';
import { registerUserRoutes } from './controllers/user_controller';
import { registerProjectsRoutes } from './controllers/projects_controller';
import { registerQueryRoutes } from './controllers/query_controller';
import { registerMutationRoutes } from './controllers/mutation_controller';

export * from './router';
export * from './controllers/auth_controller';
export * from './controllers/user_controller';
export * from './controllers/projects_controller';
export * from './controllers/query_controller';
export * from './controllers/mutation_controller';

/**
 * Create and configure the HTTP gateway router.
 */
export function createHttpGateway(core: AdapterCore): HttpRouter {
  const router = new HttpRouter();

  // Register all domain routes
  registerAuthRoutes(router, core);
  registerUserRoutes(router, core);
  registerProjectsRoutes(router, core);
  registerQueryRoutes(router, core);
  registerMutationRoutes(router, core);

  return router;
}

/**
 * API endpoint specification.
 */
export const API_ENDPOINTS = {
  auth: {
    login: { method: 'POST', path: '/api/v1/auth/login' },
    logout: { method: 'POST', path: '/api/v1/auth/logout' },
    refresh: { method: 'POST', path: '/api/v1/auth/refresh' },
  },
  user: {
    getCurrentUser: { method: 'GET', path: '/api/v1/users/me' },
    getUser: { method: 'GET', path: '/api/v1/users/:userId' },
  },
  projects: {
    listProjects: { method: 'GET', path: '/api/v1/projects' },
    createProject: { method: 'POST', path: '/api/v1/projects' },
  },
  query: {
    query: { method: 'POST', path: '/api/v1/query' },
    queryByName: { method: 'GET', path: '/api/v1/query/:queryName' },
  },
  mutation: {
    mutate: { method: 'POST', path: '/api/v1/mutate' },
    mutateByName: { method: 'POST', path: '/api/v1/mutate/:mutationName' },
  },
} as const;
