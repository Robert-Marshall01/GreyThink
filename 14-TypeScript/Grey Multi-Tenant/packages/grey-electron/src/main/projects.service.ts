/**
 * Grey Electron - Main Projects Service
 *
 * Main process projects service.
 * Wraps adapter-core createProjectsController with error normalization.
 *
 * Runs in trusted main process only.
 */

import { createProjectsController } from '@grey/adapters';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface ProjectsServiceOptions {
  baseUrl: string;
}

export interface ProjectsRequestOptions {
  token: string;
}

export interface CreateProjectInput {
  name: string;
  description?: string;
  [key: string]: unknown;
}

// ============================================================
// Error Normalization
// ============================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    const error = err as Error & { code?: string; status?: number };
    return {
      message: error.message,
      code: error.code,
      status: error.status,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err, raw: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

// ============================================================
// Service Class
// ============================================================

export class ProjectsService {
  private readonly baseUrl: string;

  constructor(options: ProjectsServiceOptions) {
    this.baseUrl = options.baseUrl;
  }

  /**
   * List all projects.
   */
  async listProjects(options: ProjectsRequestOptions): Promise<{ data: unknown[] | null; error: GreyError | null }> {
    try {
      if (!options.token) {
        return {
          data: null,
          error: { message: 'No authentication token provided', code: 'NO_TOKEN', status: 401 },
        };
      }

      const controller = createProjectsController({
        apiBaseUrl: this.baseUrl,
        token: options.token,
      });

      const projects = await controller.listProjects();

      return {
        data: projects,
        error: null,
      };
    } catch (err) {
      return {
        data: null,
        error: normalizeError(err),
      };
    }
  }

  /**
   * Create a new project.
   */
  async createProject(
    options: ProjectsRequestOptions,
    input: CreateProjectInput
  ): Promise<{ data: unknown | null; error: GreyError | null }> {
    try {
      if (!options.token) {
        return {
          data: null,
          error: { message: 'No authentication token provided', code: 'NO_TOKEN', status: 401 },
        };
      }

      const controller = createProjectsController({
        apiBaseUrl: this.baseUrl,
        token: options.token,
      });

      const project = await controller.createProject(input);

      return {
        data: project,
        error: null,
      };
    } catch (err) {
      return {
        data: null,
        error: normalizeError(err),
      };
    }
  }
}
