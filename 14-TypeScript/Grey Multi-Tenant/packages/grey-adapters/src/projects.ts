/**
 * Grey Adapters - Projects Core
 *
 * Framework-agnostic project management logic.
 */

import type { GreyClient, Project, Pagination } from '@grey/core-client';

/**
 * Projects list state
 */
export interface ProjectsState {
  projects: Project[];
  isLoading: boolean;
  error: string | null;
  pagination: Pagination | null;
}

/**
 * Initial projects state
 */
export const initialProjectsState: ProjectsState = {
  projects: [],
  isLoading: false,
  error: null,
  pagination: null,
};

/**
 * Single project state
 */
export interface ProjectState {
  project: Project | null;
  isLoading: boolean;
  error: string | null;
}

/**
 * Initial single project state
 */
export const initialProjectState: ProjectState = {
  project: null,
  isLoading: false,
  error: null,
};

/**
 * Create project input
 */
export interface CreateProjectInput {
  name: string;
  description?: string;
}

/**
 * Update project input
 */
export interface UpdateProjectInput {
  name?: string;
  description?: string;
  status?: 'active' | 'archived' | 'draft';
}

/**
 * Projects controller for framework-agnostic project operations.
 */
export class ProjectsController {
  private client: GreyClient;
  private listState: ProjectsState = { ...initialProjectsState };
  private detailState: Map<string, ProjectState> = new Map();
  private listListeners: Set<(state: ProjectsState) => void> = new Set();
  private detailListeners: Map<string, Set<(state: ProjectState) => void>> = new Map();

  constructor(client: GreyClient) {
    this.client = client;
  }

  /**
   * Get projects list state
   */
  getListState(): ProjectsState {
    return { ...this.listState };
  }

  /**
   * Get project detail state
   */
  getDetailState(id: string): ProjectState {
    return this.detailState.get(id) ?? { ...initialProjectState };
  }

  /**
   * Subscribe to projects list changes
   */
  subscribeToList(listener: (state: ProjectsState) => void): () => void {
    this.listListeners.add(listener);
    listener(this.listState);
    return () => this.listListeners.delete(listener);
  }

  /**
   * Subscribe to project detail changes
   */
  subscribeToDetail(id: string, listener: (state: ProjectState) => void): () => void {
    if (!this.detailListeners.has(id)) {
      this.detailListeners.set(id, new Set());
    }
    this.detailListeners.get(id)!.add(listener);
    listener(this.getDetailState(id));
    return () => this.detailListeners.get(id)?.delete(listener);
  }

  /**
   * Update list state and notify listeners
   */
  private setListState(updates: Partial<ProjectsState>): void {
    this.listState = { ...this.listState, ...updates };
    this.listListeners.forEach((listener) => listener(this.listState));
  }

  /**
   * Update detail state and notify listeners
   */
  private setDetailState(id: string, updates: Partial<ProjectState>): void {
    const current = this.detailState.get(id) ?? { ...initialProjectState };
    const newState = { ...current, ...updates };
    this.detailState.set(id, newState);
    this.detailListeners.get(id)?.forEach((listener) => listener(newState));
  }

  /**
   * Load projects list
   */
  async loadList(page = 1, pageSize = 20): Promise<void> {
    this.setListState({ isLoading: true, error: null });

    try {
      const result = await this.client.projects.list({ page, pageSize });

      if (result.error) {
        this.setListState({
          isLoading: false,
          error: 'Failed to load projects',
        });
        return;
      }

      this.setListState({
        projects: result.data?.data ?? [],
        pagination: result.data?.pagination ?? null,
        isLoading: false,
      });
    } catch (err) {
      this.setListState({
        isLoading: false,
        error: err instanceof Error ? err.message : 'Failed to load projects',
      });
    }
  }

  /**
   * Load a single project by ID
   */
  async loadProject(id: string): Promise<Project | null> {
    this.setDetailState(id, { isLoading: true, error: null });

    try {
      const result = await this.client.projects.get(id);

      if (result.error || !result.data) {
        this.setDetailState(id, {
          isLoading: false,
          error: 'Project not found',
        });
        return null;
      }

      this.setDetailState(id, {
        project: result.data,
        isLoading: false,
      });

      return result.data;
    } catch (err) {
      this.setDetailState(id, {
        isLoading: false,
        error: err instanceof Error ? err.message : 'Failed to load project',
      });
      return null;
    }
  }

  /**
   * Create a new project
   */
  async createProject(input: CreateProjectInput): Promise<Project | null> {
    try {
      const result = await this.client.projects.create(input);

      if (result.error || !result.data) {
        throw new Error('Failed to create project');
      }

      // Refresh the list
      await this.loadList();

      return result.data;
    } catch (err) {
      this.setListState({
        error: err instanceof Error ? err.message : 'Failed to create project',
      });
      return null;
    }
  }

  /**
   * Clear list error
   */
  clearListError(): void {
    this.setListState({ error: null });
  }

  /**
   * Clear detail error
   */
  clearDetailError(id: string): void {
    this.setDetailState(id, { error: null });
  }
}

/**
 * Create a projects controller
 */
export function createProjectsController(client: GreyClient): ProjectsController {
  return new ProjectsController(client);
}
