/**
 * Grey Node - Projects Service
 *
 * Server-side projects service for Node.js applications.
 * Wraps adapter-core createProjectsController with error normalization.
 *
 * Pure ESM, no browser APIs, dependency-injection-friendly.
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
  token?: string;
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
  private token?: string;

  // Internal state
  public data: unknown[] | null = null;
  public loading: boolean = false;
  public error: GreyError | null = null;

  constructor(options: ProjectsServiceOptions) {
    this.baseUrl = options.baseUrl;
    this.token = options.token;
  }

  /**
   * Set the authentication token.
   */
  setToken(token: string): void {
    this.token = token;
  }

  /**
   * List all projects.
   */
  async listProjects(): Promise<unknown[] | null> {
    this.loading = true;
    this.error = null;

    try {
      if (!this.token) {
        this.error = { message: 'No authentication token provided', code: 'NO_TOKEN', status: 401 };
        this.data = null;
        return null;
      }

      const controller = createProjectsController({
        apiBaseUrl: this.baseUrl,
        token: this.token,
      });

      const projects = await controller.listProjects();

      this.data = projects;
      return this.data;
    } catch (err) {
      this.error = normalizeError(err);
      this.data = null;
      return null;
    } finally {
      this.loading = false;
    }
  }

  /**
   * Create a new project.
   */
  async createProject(input: CreateProjectInput): Promise<unknown | null> {
    this.loading = true;
    this.error = null;

    try {
      if (!this.token) {
        this.error = { message: 'No authentication token provided', code: 'NO_TOKEN', status: 401 };
        return null;
      }

      const controller = createProjectsController({
        apiBaseUrl: this.baseUrl,
        token: this.token,
      });

      const project = await controller.createProject(input);

      // Refresh the list after creation
      await this.listProjects();

      return project;
    } catch (err) {
      this.error = normalizeError(err);
      return null;
    } finally {
      this.loading = false;
    }
  }
}
