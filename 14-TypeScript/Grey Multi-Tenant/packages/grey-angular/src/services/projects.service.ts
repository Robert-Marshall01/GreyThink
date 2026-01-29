/**
 * ProjectsService - Angular service for projects management
 *
 * Wraps @grey/adapters ProjectsController using Angular signals.
 * Exposes: data, loading, error, and domain actions.
 */

import { Injectable, signal, inject, type OnDestroy } from '@angular/core';
import {
  ProjectsController,
  type ProjectsState,
  type CreateProjectInput,
} from '@grey/adapters';
import type { GreyClient, Project, Pagination } from '@grey/core-client';
import { AuthService, type GreyError } from './auth.service';

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
// ProjectsService
// =============================================================================

@Injectable({ providedIn: 'root' })
export class ProjectsService implements OnDestroy {
  private controller?: ProjectsController;
  private unsubscribe?: () => void;
  private authService = inject(AuthService);

  // Signals
  private readonly _data = signal<Project[]>([]);
  private readonly _loading = signal<boolean>(false);
  private readonly _error = signal<GreyError | null>(null);
  private readonly _pagination = signal<Pagination | null>(null);

  // Public readonly signals
  readonly data = this._data.asReadonly();
  readonly loading = this._loading.asReadonly();
  readonly error = this._error.asReadonly();
  readonly pagination = this._pagination.asReadonly();

  /**
   * Initialize the projects service.
   * Called automatically when needed.
   */
  initialize(): void {
    const client = this.authService.getClient();
    this.controller = new ProjectsController(client);

    this.unsubscribe = this.controller.subscribeToList((state: ProjectsState) => {
      this._data.set(state.projects);
      this._loading.set(state.isLoading);
      this._error.set(state.error ? normalizeError(state.error) : null);
      this._pagination.set(state.pagination);
    });
  }

  ngOnDestroy(): void {
    this.unsubscribe?.();
  }

  /**
   * List projects with optional pagination.
   */
  async listProjects(page?: number, pageSize?: number): Promise<void> {
    if (!this.controller) {
      this.initialize();
    }

    this._loading.set(true);
    this._error.set(null);

    try {
      await this.controller!.loadList(page, pageSize);
    } catch (err) {
      this._error.set(normalizeError(err));
      throw err;
    } finally {
      this._loading.set(false);
    }
  }

  /**
   * Create a new project.
   */
  async createProject(input: CreateProjectInput): Promise<void> {
    if (!this.controller) {
      this.initialize();
    }

    this._loading.set(true);
    this._error.set(null);

    try {
      await this.controller!.createProject(input);
    } catch (err) {
      this._error.set(normalizeError(err));
      throw err;
    } finally {
      this._loading.set(false);
    }
  }
}
