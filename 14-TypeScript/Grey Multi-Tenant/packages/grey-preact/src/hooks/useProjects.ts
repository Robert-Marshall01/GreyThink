/**
 * useProjects - Preact hook for projects management
 *
 * Wraps @grey/adapters ProjectsController using Preact hooks.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useEffect, useCallback } from 'preact/hooks';
import {
  ProjectsController,
  type ProjectsState,
  type CreateProjectInput,
} from '@grey/adapters';
import type { GreyClient, Project, Pagination } from '@grey/core-client';
import type { GreyError } from './useAuth';

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
// Hook Result
// =============================================================================

export interface UseProjectsResult {
  data: Project[];
  loading: boolean;
  error: GreyError | null;
  pagination: Pagination | null;
  listProjects: (page?: number, pageSize?: number) => Promise<void>;
  createProject: (input: CreateProjectInput) => Promise<void>;
}

// =============================================================================
// useProjects Hook
// =============================================================================

export function useProjects(client: GreyClient): UseProjectsResult {
  const [data, setData] = useState<Project[]>([]);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);
  const [pagination, setPagination] = useState<Pagination | null>(null);
  const [controller, setController] = useState<ProjectsController | null>(null);

  // Initialize controller on mount
  useEffect(() => {
    const projectsController = new ProjectsController(client);
    setController(projectsController);

    const unsubscribe = projectsController.subscribeToList((state: ProjectsState) => {
      setData(state.projects);
      setLoading(state.isLoading);
      setError(state.error ? normalizeError(state.error) : null);
      setPagination(state.pagination);
    });

    return () => {
      unsubscribe();
    };
  }, [client]);

  const listProjects = useCallback(async (page?: number, pageSize?: number): Promise<void> => {
    if (!controller) {
      throw new Error('Projects controller not initialized');
    }

    setLoading(true);
    setError(null);

    try {
      await controller.loadList(page, pageSize);
    } catch (err) {
      setError(normalizeError(err));
      throw err;
    } finally {
      setLoading(false);
    }
  }, [controller]);

  const createProject = useCallback(async (input: CreateProjectInput): Promise<void> => {
    if (!controller) {
      throw new Error('Projects controller not initialized');
    }

    setLoading(true);
    setError(null);

    try {
      await controller.createProject(input);
    } catch (err) {
      setError(normalizeError(err));
      throw err;
    } finally {
      setLoading(false);
    }
  }, [controller]);

  return {
    data,
    loading,
    error,
    pagination,
    listProjects,
    createProject,
  };
}
