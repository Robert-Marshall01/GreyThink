/**
 * Grey Integration Layer - gRPC Service Index
 *
 * Main entry point for the gRPC service layer.
 * Registers all service implementations.
 */

import type { AdapterCore } from '../adapter_core';
import { createAuthServiceHandlers } from './services/auth_service';
import { createUserServiceHandlers } from './services/user_service';
import { createProjectsServiceHandlers } from './services/projects_service';
import { createQueryServiceHandlers } from './services/query_service';
import { createMutationServiceHandlers } from './services/mutation_service';

export * from './grpc_error';
export * from './services/auth_service';
export * from './services/user_service';
export * from './services/projects_service';
export * from './services/query_service';
export * from './services/mutation_service';

/**
 * All gRPC services with their handlers.
 */
export interface GrpcServices {
  authService: ReturnType<typeof createAuthServiceHandlers>;
  userService: ReturnType<typeof createUserServiceHandlers>;
  projectsService: ReturnType<typeof createProjectsServiceHandlers>;
  queryService: ReturnType<typeof createQueryServiceHandlers>;
  mutationService: ReturnType<typeof createMutationServiceHandlers>;
}

/**
 * Create all gRPC service handlers.
 */
export function createGrpcServices(core: AdapterCore): GrpcServices {
  return {
    authService: createAuthServiceHandlers(core),
    userService: createUserServiceHandlers(core),
    projectsService: createProjectsServiceHandlers(core),
    queryService: createQueryServiceHandlers(core),
    mutationService: createMutationServiceHandlers(core),
  };
}

/**
 * Service definitions for proto registration.
 */
export const SERVICE_DEFINITIONS = {
  'grey.auth.v1.AuthService': {
    service: 'AuthService',
    package: 'grey.auth.v1',
    proto: 'auth.proto',
    methods: ['Login', 'Logout', 'Refresh'],
  },
  'grey.user.v1.UserService': {
    service: 'UserService',
    package: 'grey.user.v1',
    proto: 'user.proto',
    methods: ['GetCurrentUser', 'GetUser'],
  },
  'grey.projects.v1.ProjectsService': {
    service: 'ProjectsService',
    package: 'grey.projects.v1',
    proto: 'projects.proto',
    methods: ['ListProjects', 'CreateProject', 'GetProject'],
  },
  'grey.query.v1.QueryService': {
    service: 'QueryService',
    package: 'grey.query.v1',
    proto: 'query.proto',
    methods: ['Query'],
  },
  'grey.mutation.v1.MutationService': {
    service: 'MutationService',
    package: 'grey.mutation.v1',
    proto: 'mutation.proto',
    methods: ['Mutate'],
  },
} as const;

/**
 * Proto file paths for code generation.
 */
export const PROTO_FILES = [
  'common.proto',
  'auth.proto',
  'user.proto',
  'projects.proto',
  'query.proto',
  'mutation.proto',
] as const;
