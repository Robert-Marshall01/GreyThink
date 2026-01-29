/**
 * Grey Capacitor - useProjects Hook
 *
 * Provides projects state and actions for Capacitor components.
 * Wraps the ProjectsController from @grey/adapters.
 * Capacitor-safe: no Node APIs, browser APIs allowed.
 */

import { useState, useEffect, useCallback, useRef } from 'react';
import {
  ProjectsController,
  type ProjectsState as CoreProjectsState,
  type CreateProjectInput,
  type GreyClient,
  type Project,
  type Pagination,
} from '@grey/adapters';

// ============================================================
// Types
// ============================================================

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
 * Projects hook return type
 */
export interface UseProjectsReturn {
  data: ProjectsData;
  loading: boolean;
  error: GreyError | null;
  listProjects: (page?: number, pageSize?: number) => Promise<void>;
  createProject: (input: CreateProjectInput) => Promise<Project | null>;
}

// ============================================================
// Error Normalization
// ============================================================

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

// ============================================================
// Hook Implementation
// ============================================================

/**
 * useProjects Hook
 *
 * Provides projects state and actions.
 * Must be used within a GreyProvider or with a client.
 *
 * @param client - Optional GreyClient for standalone usage
 */
export function useProjects(client?: GreyClient): UseProjectsReturn {
  const controllerRef = useRef<ProjectsController | null>(null);
  const [data, setData] = useState<ProjectsData>({
    projects: [],
    pagination: null,
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<GreyError | null>(null);

  // Track if component is mounted
  const mountedRef = useRef(true);
  useEffect(() => {
    mountedRef.current = true;
    return () => {
      mountedRef.current = false;
    };
  }, []);

  // Initialize controller
  useEffect(() => {
    if (!client) return;

    const controller = new ProjectsController(client);
    controllerRef.current = controller;

    const unsubscribe = controller.subscribeToList((state: CoreProjectsState) => {
      if (mountedRef.current) {
        setData({
          projects: state.projects,
          pagination: state.pagination,
        });
        setLoading(state.isLoading);
        if (state.error) {
          setError({ message: state.error });
        }
      }
    });

    return () => {
      unsubscribe();
    };
  }, [client]);

  /**
   * List projects with pagination
   */
  const listProjects = useCallback(async (page = 1, pageSize = 20): Promise<void> => {
    const controller = controllerRef.current;
    if (!controller) {
      setError({ message: 'Projects controller not initialized' });
      return;
    }

    setLoading(true);
    setError(null);

    try {
      await controller.loadList(page, pageSize);
      if (mountedRef.current) {
        setLoading(false);
      }
    } catch (err) {
      if (mountedRef.current) {
        setError(normalizeError(err));
        setLoading(false);
      }
    }
  }, []);

  /**
   * Create a new project
   */
  const createProject = useCallback(async (input: CreateProjectInput): Promise<Project | null> => {
    const controller = controllerRef.current;
    if (!controller) {
      setError({ message: 'Projects controller not initialized' });
      return null;
    }

    setLoading(true);
    setError(null);

    try {
      const project = await controller.createProject(input);
      if (mountedRef.current) {
        setLoading(false);
      }
      return project;
    } catch (err) {
      if (mountedRef.current) {
        setError(normalizeError(err));
        setLoading(false);
      }
      return null;
    }
  }, []);

  return {
    data,
    loading,
    error,
    listProjects,
    createProject,
  };
}
