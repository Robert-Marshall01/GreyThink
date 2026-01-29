/**
 * Grey Svelte - Projects Store
 *
 * Svelte store for projects state and actions.
 * Wraps ProjectsController from @grey/adapters.
 * Exposes: data, loading, error, listProjects, createProject.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
import {
  ProjectsController,
  type ProjectsState,
  type CreateProjectInput,
} from '@grey/adapters';
import type { GreyClient, Project, Pagination } from '@grey/core-client';

/**
 * Normalized error shape
 */
export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

/**
 * Projects data structure
 */
export interface ProjectsData {
  projects: Project[];
  pagination: Pagination | null;
}

/**
 * Projects store return type
 */
export interface ProjectsStoreReturn {
  /** Projects data (list and pagination) */
  data: Readable<ProjectsData>;
  /** Loading state */
  loading: Readable<boolean>;
  /** Error state */
  error: Readable<GreyError | null>;
  /** List projects with optional pagination */
  listProjects: (page?: number, pageSize?: number) => Promise<void>;
  /** Create a new project */
  createProject: (input: CreateProjectInput) => Promise<Project | null>;
}

/**
 * Normalize error into standard shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return { message: err.message, raw: err };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

/**
 * Create a projects store
 *
 * @param client - Grey API client
 * @returns Projects store with data, loading, error, and actions
 */
export function createProjectsStore(client: GreyClient): ProjectsStoreReturn {
  const controller = new ProjectsController(client);

  // Create writable stores
  const dataStore: Writable<ProjectsData> = writable({
    projects: [],
    pagination: null,
  });
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  // Subscribe to controller state changes
  controller.subscribeToList((state: ProjectsState) => {
    dataStore.set({
      projects: state.projects,
      pagination: state.pagination,
    });
    loadingStore.set(state.isLoading);
    if (state.error) {
      errorStore.set({ message: state.error });
    }
  });

  /**
   * List projects with optional pagination
   */
  async function listProjects(page = 1, pageSize = 20): Promise<void> {
    loadingStore.set(true);
    errorStore.set(null);
    try {
      await controller.loadList(page, pageSize);
      loadingStore.set(false);
    } catch (err) {
      errorStore.set(normalizeError(err));
      loadingStore.set(false);
    }
  }

  /**
   * Create a new project
   */
  async function createProject(input: CreateProjectInput): Promise<Project | null> {
    loadingStore.set(true);
    errorStore.set(null);
    try {
      const project = await controller.createProject(input);
      loadingStore.set(false);
      return project;
    } catch (err) {
      errorStore.set(normalizeError(err));
      loadingStore.set(false);
      return null;
    }
  }

  return {
    data: { subscribe: dataStore.subscribe },
    loading: { subscribe: loadingStore.subscribe },
    error: { subscribe: errorStore.subscribe },
    listProjects,
    createProject,
  };
}

export type { ProjectsState, CreateProjectInput };
