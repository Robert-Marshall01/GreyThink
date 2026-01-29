/**
 * Grey SvelteKit - Projects Store
 *
 * Svelte store for projects state and actions.
 * Uses writable() for SSR-safe state. Wraps server routes from /server/projects.ts.
 *
 * No browser APIs. SSR-safe.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
import type { Project, Pagination } from '@grey/core-client';
import type { CreateProjectInput } from '@grey/adapters';

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
 * Projects data state
 */
export interface ProjectsData {
  projects: Project[];
  pagination: Pagination | null;
}

/**
 * Projects store return type
 */
export interface ProjectsStoreReturn {
  data: Readable<ProjectsData | null>;
  loading: Readable<boolean>;
  error: Readable<GreyError | null>;
  listProjects: (page?: number, pageSize?: number) => Promise<Project[]>;
  createProject: (input: CreateProjectInput) => Promise<Project | null>;
}

/**
 * Create a projects store
 *
 * @returns Projects store with data, loading, error, and actions
 */
export function createProjectsStore(): ProjectsStoreReturn {
  const dataStore: Writable<ProjectsData | null> = writable(null);
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  /**
   * List projects with pagination
   */
  async function listProjects(page: number = 1, pageSize: number = 20): Promise<Project[]> {
    loadingStore.set(true);
    errorStore.set(null);

    try {
      const params = new URLSearchParams({ page: String(page), pageSize: String(pageSize) });
      const response = await fetch(`/api/grey/projects?${params}`);
      const result = await response.json();

      if (result.error) {
        errorStore.set(result.error);
        loadingStore.set(false);
        return [];
      }

      dataStore.set({
        projects: result.projects,
        pagination: result.pagination,
      });

      loadingStore.set(false);
      return result.projects;
    } catch (err) {
      errorStore.set({
        message: err instanceof Error ? err.message : 'Failed to load projects',
        raw: err,
      });
      loadingStore.set(false);
      return [];
    }
  }

  /**
   * Create a new project
   */
  async function createProject(input: CreateProjectInput): Promise<Project | null> {
    loadingStore.set(true);
    errorStore.set(null);

    try {
      const response = await fetch('/api/grey/projects', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(input),
      });

      const result = await response.json();

      if (result.error) {
        errorStore.set(result.error);
        loadingStore.set(false);
        return null;
      }

      // Add the new project to the list
      if (result.project) {
        dataStore.update((prev: ProjectsData | null) => ({
          projects: [...(prev?.projects ?? []), result.project],
          pagination: prev?.pagination ?? null,
        }));
      }

      loadingStore.set(false);
      return result.project;
    } catch (err) {
      errorStore.set({
        message: err instanceof Error ? err.message : 'Failed to create project',
        raw: err,
      });
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
