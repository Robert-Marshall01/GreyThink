'use client';

/**
 * useProjects - Next.js Client Hook for projects operations
 *
 * Wraps server actions from /server/projects.ts.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useCallback } from 'react';
import {
  listProjects as listProjectsAction,
  createProject as createProjectAction,
} from '../server/projects.js';
import type { GreyError } from '../server/auth.js';
import type { CreateProjectInput } from '@grey/adapters';
import type { GreyClient, Project, Pagination } from '@grey/core-client';

// =============================================================================
// Types
// =============================================================================

export interface ProjectsData {
  projects: Project[];
  pagination: Pagination | null;
}

export interface UseProjectsReturn {
  data: ProjectsData | null;
  loading: boolean;
  error: GreyError | null;
  listProjects: (client: GreyClient, page?: number, pageSize?: number) => Promise<Project[]>;
  createProject: (client: GreyClient, input: CreateProjectInput) => Promise<Project | null>;
}

// =============================================================================
// Hook
// =============================================================================

export function useProjects(): UseProjectsReturn {
  const [data, setData] = useState<ProjectsData | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<GreyError | null>(null);

  const listProjects = useCallback(async (
    client: GreyClient,
    page = 1,
    pageSize = 20
  ): Promise<Project[]> => {
    setLoading(true);
    setError(null);

    try {
      const result = await listProjectsAction(client, page, pageSize);

      if (result.error) {
        setError(result.error);
        return [];
      }

      setData({
        projects: result.projects,
        pagination: result.pagination,
      });

      return result.projects;
    } catch (err) {
      setError({
        message: err instanceof Error ? err.message : 'List projects failed',
        raw: err,
      });
      return [];
    } finally {
      setLoading(false);
    }
  }, []);

  const createProject = useCallback(async (
    client: GreyClient,
    input: CreateProjectInput
  ): Promise<Project | null> => {
    setLoading(true);
    setError(null);

    try {
      const result = await createProjectAction(client, input);

      if (result.error) {
        setError(result.error);
        return null;
      }

      // Add the new project to the list
      if (result.project) {
        setData((prev: ProjectsData | null) => ({
          projects: [...(prev?.projects ?? []), result.project!],
          pagination: prev?.pagination ?? null,
        }));
      }

      return result.project;
    } catch (err) {
      setError({
        message: err instanceof Error ? err.message : 'Create project failed',
        raw: err,
      });
      return null;
    } finally {
      setLoading(false);
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
