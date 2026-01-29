/**
 * Grey Integration Layer - Projects gRPC Service Implementation
 *
 * Implements the ProjectsService gRPC service.
 * Calls adapter-core directly, no business logic.
 */

import type { AdapterCore, Project, ListProjectsResponse } from '../../adapter_core';
import { createGrpcError, type GrpcError } from '../grpc_error';

// Proto message types
export interface ProtoListProjectsRequest {
  limit: number;
  offset: number;
  tenantId?: string;
}

export interface ProtoProject {
  projectId: string;
  name: string;
  description?: string;
  tenantId?: string;
  createdAt?: string;
  updatedAt?: string;
}

export interface ProtoListProjectsResponse {
  projects: ProtoProject[];
  total: number;
}

export interface ProtoCreateProjectRequest {
  name: string;
  description?: string;
  tenantId?: string;
}

export interface ProtoGetProjectRequest {
  projectId: string;
}

// Service call context
export interface ServiceContext {
  metadata?: Map<string, string>;
  authToken?: string;
  tenantId?: string;
}

/**
 * Projects service implementation.
 */
export class ProjectsServiceImpl {
  constructor(private readonly core: AdapterCore) {}

  /**
   * List projects with pagination.
   */
  async listProjects(
    request: ProtoListProjectsRequest,
    context: ServiceContext
  ): Promise<ProtoListProjectsResponse> {
    try {
      const response = await this.core.listProjects({
        limit: request.limit,
        offset: request.offset,
        tenantId: request.tenantId || context.tenantId,
      });

      return {
        projects: response.projects.map(this.toProtoProject),
        total: response.total,
      };
    } catch (error) {
      throw createGrpcError(error);
    }
  }

  /**
   * Create a new project.
   */
  async createProject(
    request: ProtoCreateProjectRequest,
    context: ServiceContext
  ): Promise<ProtoProject> {
    try {
      const project = await this.core.createProject({
        name: request.name,
        description: request.description,
        tenantId: request.tenantId || context.tenantId,
      });

      return this.toProtoProject(project);
    } catch (error) {
      throw createGrpcError(error);
    }
  }

  /**
   * Get a project by ID.
   */
  async getProject(
    request: ProtoGetProjectRequest,
    context: ServiceContext
  ): Promise<ProtoProject> {
    try {
      // Note: getProject is not in the base AdapterCore interface
      // This would need to be added or implemented via query
      const response = await this.core.query({
        queryName: 'getProject',
        parameters: { projectId: request.projectId },
        tenantId: context.tenantId,
      });

      return response.data as ProtoProject;
    } catch (error) {
      throw createGrpcError(error);
    }
  }

  private toProtoProject(project: Project): ProtoProject {
    return {
      projectId: project.projectId,
      name: project.name,
      description: project.description,
      tenantId: project.tenantId,
      createdAt: project.createdAt,
      updatedAt: project.updatedAt,
    };
  }
}

/**
 * Create Projects service handlers for gRPC server registration.
 */
export function createProjectsServiceHandlers(core: AdapterCore) {
  const service = new ProjectsServiceImpl(core);

  return {
    listProjects: service.listProjects.bind(service),
    createProject: service.createProject.bind(service),
    getProject: service.getProject.bind(service),
  };
}
