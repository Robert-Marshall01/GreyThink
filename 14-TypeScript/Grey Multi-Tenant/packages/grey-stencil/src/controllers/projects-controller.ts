/**
 * ProjectsController - Stencil Controller for projects state
 *
 * Wraps @grey/adapters ProjectsController for Stencil integration.
 * Exposes: data, loading, error, and domain actions.
 */

import {
  ProjectsController as CoreProjectsController,
  type ProjectsState,
  type CreateProjectInput,
} from '@grey/adapters';
import type { GreyClient, Project, Pagination } from '@grey/core-client';
import type { GreyError, StencilHost } from './auth-controller';

// =============================================================================
// Normalize Errors
// =============================================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      code: (err as { code?: string }).code,
      status: (err as { status?: number }).status,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err, raw: err };
  }
  return { message: 'Unknown error', raw: err };
}

// =============================================================================
// ProjectsController
// =============================================================================

export class ProjectsController {
  private host: StencilHost;
  private coreController: CoreProjectsController;
  private unsubscribe: (() => void) | null = null;

  // Reactive state
  private _data: Project[] = [];
  private _loading: boolean = false;
  private _error: GreyError | null = null;
  private _pagination: Pagination | null = null;

  constructor(host: StencilHost, client: GreyClient) {
    this.host = host;
    this.coreController = new CoreProjectsController(client);
  }

  connect(): void {
    this.unsubscribe = this.coreController.subscribeToList((state: ProjectsState) => {
      this._data = state.projects;
      this._loading = state.isLoading;
      this._error = state.error ? normalizeError(state.error) : null;
      this._pagination = state.pagination;
      this.host.forceUpdate();
    });
  }

  disconnect(): void {
    this.unsubscribe?.();
    this.unsubscribe = null;
  }

  // =============================================================================
  // Reactive State
  // =============================================================================

  get data(): Project[] {
    return this._data;
  }

  get loading(): boolean {
    return this._loading;
  }

  get error(): GreyError | null {
    return this._error;
  }

  get pagination(): Pagination | null {
    return this._pagination;
  }

  // =============================================================================
  // Domain Actions
  // =============================================================================

  async listProjects(page = 1, pageSize = 20): Promise<Project[]> {
    this._loading = true;
    this._error = null;
    this.host.forceUpdate();

    try {
      await this.coreController.loadList(page, pageSize);
      return this._data;
    } catch (err) {
      this._error = normalizeError(err);
      this.host.forceUpdate();
      return [];
    } finally {
      this._loading = false;
      this.host.forceUpdate();
    }
  }

  async createProject(input: CreateProjectInput): Promise<Project | null> {
    this._loading = true;
    this._error = null;
    this.host.forceUpdate();

    try {
      const result = await this.coreController.createProject(input);
      return result;
    } catch (err) {
      this._error = normalizeError(err);
      this.host.forceUpdate();
      return null;
    } finally {
      this._loading = false;
      this.host.forceUpdate();
    }
  }
}
