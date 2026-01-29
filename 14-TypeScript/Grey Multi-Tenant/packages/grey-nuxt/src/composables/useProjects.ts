/**
 * Grey Nuxt - useProjects Composable
 *
 * Provides projects state and actions for Nuxt components.
 * Uses useState() for SSR-safe state. Wraps server routes from /server/projects.ts.
 *
 * No browser APIs. SSR-safe.
 */

import { useState } from '#app';
import type { Ref } from 'vue';
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
 * Projects composable return type
 */
export interface UseProjectsReturn {
  data: Ref<ProjectsData | null>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  listProjects: (page?: number, pageSize?: number) => Promise<Project[]>;
  createProject: (input: CreateProjectInput) => Promise<Project | null>;
}

/**
 * useProjects Composable
 *
 * Provides projects state and actions via server routes.
 * Uses useState() for SSR-safe hydration.
 */
export function useProjects(): UseProjectsReturn {
  const data = useState<ProjectsData | null>('grey-projects-data', () => null);
  const loading = useState<boolean>('grey-projects-loading', () => false);
  const error = useState<GreyError | null>('grey-projects-error', () => null);

  /**
   * List projects with pagination
   */
  async function listProjects(page: number = 1, pageSize: number = 20): Promise<Project[]> {
    loading.value = true;
    error.value = null;

    try {
      const result = await $fetch<{
        projects: Project[];
        pagination: Pagination | null;
        error: GreyError | null;
      }>('/api/grey/projects', {
        query: { page, pageSize },
      });

      if (result.error) {
        error.value = result.error;
        loading.value = false;
        return [];
      }

      data.value = {
        projects: result.projects,
        pagination: result.pagination,
      };

      loading.value = false;
      return result.projects;
    } catch (err) {
      error.value = {
        message: err instanceof Error ? err.message : 'Failed to load projects',
        raw: err,
      };
      loading.value = false;
      return [];
    }
  }

  /**
   * Create a new project
   */
  async function createProject(input: CreateProjectInput): Promise<Project | null> {
    loading.value = true;
    error.value = null;

    try {
      const result = await $fetch<{
        project: Project | null;
        error: GreyError | null;
      }>('/api/grey/projects', {
        method: 'POST',
        body: input,
      });

      if (result.error) {
        error.value = result.error;
        loading.value = false;
        return null;
      }

      // Add the new project to the list
      if (result.project && data.value) {
        data.value = {
          projects: [...data.value.projects, result.project],
          pagination: data.value.pagination,
        };
      }

      loading.value = false;
      return result.project;
    } catch (err) {
      error.value = {
        message: err instanceof Error ? err.message : 'Failed to create project',
        raw: err,
      };
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
