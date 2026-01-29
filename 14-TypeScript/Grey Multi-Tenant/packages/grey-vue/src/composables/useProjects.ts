/**
 * Grey Vue - useProjects Composable
 *
 * Provides projects state and actions for Vue components.
 * Wraps the ProjectsController from @grey/adapters.
 */

import { ref, onMounted, onUnmounted, type Ref } from 'vue';
import {
  ProjectsController,
  type ProjectsState as CoreProjectsState,
  type CreateProjectInput,
  type GreyClient,
  type Project,
  type Pagination,
} from '@grey/adapters';

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
 * Projects data shape
 */
export interface ProjectsData {
  projects: Project[];
  pagination: Pagination | null;
}

/**
 * Projects composable return type
 */
export interface UseProjectsReturn {
  data: Ref<ProjectsData>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  listProjects: (page?: number, pageSize?: number) => Promise<void>;
  createProject: (input: CreateProjectInput) => Promise<Project | null>;
}

/**
 * Normalize any error into the standard error shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return {
    message: 'An unknown error occurred',
    raw: err,
  };
}

/**
 * useProjects Composable
 *
 * Provides projects state and actions.
 *
 * @param client - GreyClient instance
 */
export function useProjects(client?: GreyClient): UseProjectsReturn {
  const controllerRef = ref<ProjectsController | null>(null);
  const data = ref<ProjectsData>({
    projects: [],
    pagination: null,
  });
  const loading = ref(false);
  const error = ref<GreyError | null>(null);

  let unsubscribe: (() => void) | undefined;

  onMounted(() => {
    if (!client) return;

    const controller = new ProjectsController(client);
    controllerRef.value = controller;

    unsubscribe = controller.subscribeToList((state: CoreProjectsState) => {
      data.value = {
        projects: state.projects,
        pagination: state.pagination,
      };
      loading.value = state.isLoading;
      if (state.error) {
        error.value = { message: state.error };
      }
    });
  });

  onUnmounted(() => {
    unsubscribe?.();
  });

  /**
   * List projects with pagination
   */
  async function listProjects(page = 1, pageSize = 20): Promise<void> {
    const controller = controllerRef.value;
    if (!controller) {
      error.value = { message: 'Projects controller not initialized' };
      return;
    }

    loading.value = true;
    error.value = null;

    try {
      await controller.loadList(page, pageSize);
      loading.value = false;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
    }
  }

  /**
   * Create a new project
   */
  async function createProject(input: CreateProjectInput): Promise<Project | null> {
    const controller = controllerRef.value;
    if (!controller) {
      error.value = { message: 'Projects controller not initialized' };
      return null;
    }

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
  }

  return {
    data,
    loading,
    error,
    listProjects,
    createProject,
  };
}
