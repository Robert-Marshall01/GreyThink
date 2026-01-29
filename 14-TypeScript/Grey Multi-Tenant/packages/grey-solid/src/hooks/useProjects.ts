/**
 * Grey Solid - useProjects Hook
 *
 * Solid hook for projects state and actions.
 * Wraps ProjectsController from @grey/adapters.
 * Exposes: data, loading, error, listProjects, createProject.
 */

import { createSignal, onMount, onCleanup, type Accessor } from 'solid-js';
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
 * useProjects hook return type
 */
export interface UseProjectsReturn {
  /** Projects data (list and pagination) */
  data: Accessor<ProjectsData>;
  /** Loading state */
  loading: Accessor<boolean>;
  /** Error state */
  error: Accessor<GreyError | null>;
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
 * useProjects hook
 *
 * @param client - Grey API client
 * @returns Projects hook with data, loading, error, and actions
 */
export function useProjects(client: GreyClient): UseProjectsReturn {
  const controller = new ProjectsController(client);

  // Create signals
  const [data, setData] = createSignal<ProjectsData>({
    projects: [],
    pagination: null,
  });
  const [loading, setLoading] = createSignal(false);
  const [error, setError] = createSignal<GreyError | null>(null);

  // Subscribe to controller state changes
  let unsubscribe: (() => void) | undefined;

  onMount(() => {
    unsubscribe = controller.subscribeToList((state: ProjectsState) => {
      setData({
        projects: state.projects,
        pagination: state.pagination,
      });
      setLoading(state.isLoading);
      if (state.error) {
        setError({ message: state.error });
      }
    });
  });

  onCleanup(() => {
    unsubscribe?.();
  });

  /**
   * List projects with optional pagination
   */
  async function listProjects(page = 1, pageSize = 20): Promise<void> {
    setLoading(true);
    setError(null);
    try {
      await controller.loadList(page, pageSize);
      setLoading(false);
    } catch (err) {
      setError(normalizeError(err));
      setLoading(false);
    }
  }

  /**
   * Create a new project
   */
  async function createProject(input: CreateProjectInput): Promise<Project | null> {
    setLoading(true);
    setError(null);
    try {
      const project = await controller.createProject(input);
      setLoading(false);
      return project;
    } catch (err) {
      setError(normalizeError(err));
      setLoading(false);
      return null;
    }
  }

  return {
    data,
    loading,
    error,
    listProjects,
    createProject,
  };
}

export type { ProjectsState, CreateProjectInput };
