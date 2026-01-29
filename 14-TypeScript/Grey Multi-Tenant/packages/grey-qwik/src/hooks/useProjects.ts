/**
 * Grey Qwik - useProjects Hook
 *
 * Qwik hook for projects state and actions.
 * Wraps ProjectsController from @grey/adapters.
 * Exposes: data, loading, error, listProjects, createProject.
 */

import { useSignal, useVisibleTask$, $ } from '@builder.io/qwik';
import type { Signal, QRL } from '@builder.io/qwik';
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
  data: Signal<ProjectsData>;
  /** Loading state */
  loading: Signal<boolean>;
  /** Error state */
  error: Signal<GreyError | null>;
  /** List projects with optional pagination */
  listProjects: QRL<(page?: number, pageSize?: number) => Promise<void>>;
  /** Create a new project */
  createProject: QRL<(input: CreateProjectInput) => Promise<Project | null>>;
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
  const data = useSignal<ProjectsData>({
    projects: [],
    pagination: null,
  });
  const loading = useSignal(false);
  const error = useSignal<GreyError | null>(null);

  // Subscribe to controller state changes (client-side only)
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    const unsubscribe = controller.subscribeToList((state: ProjectsState) => {
      data.value = {
        projects: state.projects,
        pagination: state.pagination,
      };
      loading.value = state.isLoading;
      if (state.error) {
        error.value = { message: state.error };
      }
    });

    cleanup(() => {
      unsubscribe();
    });
  });

  /**
   * List projects with optional pagination
   */
  const listProjects = $(async (page = 1, pageSize = 20): Promise<void> => {
    loading.value = true;
    error.value = null;
    try {
      await controller.loadList(page, pageSize);
      loading.value = false;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
    }
  });

  /**
   * Create a new project
   */
  const createProject = $(async (input: CreateProjectInput): Promise<Project | null> => {
    loading.value = true;
    error.value = null;
    try {
      const project = await controller.createProject(input);
      loading.value = false;
      return project;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
      return null;
    }
  });

  return {
    data,
    loading,
    error,
    listProjects,
    createProject,
  };
}

export type { ProjectsState, CreateProjectInput };
