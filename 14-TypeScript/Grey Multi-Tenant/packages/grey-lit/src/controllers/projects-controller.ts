/**
 * ProjectsController - Lit Reactive Controller for projects management
 *
 * Wraps @grey/adapters ProjectsController using Lit ReactiveController pattern.
 * Exposes: data, loading, error, and domain actions.
 */

import type { ReactiveController, ReactiveControllerHost } from 'lit';
import {
  ProjectsController as CoreProjectsController,
  type ProjectsState,
  type CreateProjectInput,
} from '@grey/adapters';
import type { GreyClient, Project, Pagination } from '@grey/core-client';
import type { GreyError } from './auth-controller';

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

export class ProjectsController implements ReactiveController {
  private host: ReactiveControllerHost;
  private coreController: CoreProjectsController;
  private unsubscribe: (() => void) | null = null;

  // Reactive state
  private _data: Project[] = [];
  private _loading: boolean = false;
  private _error: GreyError | null = null;
  private _pagination: Pagination | null = null;

  constructor(host: ReactiveControllerHost, client: GreyClient) {
    this.host = host;
    host.addController(this);

    this.coreController = new CoreProjectsController(client);
  }

  hostConnected(): void {
    this.unsubscribe = this.coreController.subscribeToList((state: ProjectsState) => {
      this._data = state.projects;
      this._loading = state.isLoading;
      this._error = state.error ? normalizeError(state.error) : null;
      this._pagination = state.pagination;
      this.host.requestUpdate();
    });
  }

  hostDisconnected(): void {
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

  async listProjects(page?: number, pageSize?: number): Promise<void> {
    this._loading = true;
    this._error = null;
    this.host.requestUpdate();

    try {
      await this.coreController.loadList(page, pageSize);
    } catch (err) {
      this._error = normalizeError(err);
      this.host.requestUpdate();
      throw err;
    } finally {
      this._loading = false;
      this.host.requestUpdate();
    }
  }

  async createProject(input: CreateProjectInput): Promise<void> {
    this._loading = true;
    this._error = null;
    this.host.requestUpdate();

    try {
      await this.coreController.createProject(input);
    } catch (err) {
      this._error = normalizeError(err);
      this.host.requestUpdate();
      throw err;
    } finally {
      this._loading = false;
      this.host.requestUpdate();
    }
  }
}
