/**
 * Grey Remix - useProjects Hook
 *
 * Client-side projects hook for Remix.
 * Uses React state and calls server loaders/actions via fetch.
 *
 * Exposes: data, loading, error, listProjects(), createProject()
 */

import { useState, useCallback } from 'react';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface CreateProjectInput {
  name: string;
  description?: string;
  [key: string]: unknown;
}

export interface UseProjectsResult {
  data: unknown[] | null;
  loading: boolean;
  error: GreyError | null;
  listProjects: () => Promise<void>;
  createProject: (input: CreateProjectInput) => Promise<unknown | null>;
}

export interface UseProjectsOptions {
  projectsEndpoint?: string;
}

// ============================================================
// Error Normalization
// ============================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    const error = err as Error & { code?: string; status?: number };
    return {
      message: error.message,
      code: error.code,
      status: error.status,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err, raw: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

// ============================================================
// Hook
// ============================================================

export function useProjects(options: UseProjectsOptions = {}): UseProjectsResult {
  const { projectsEndpoint = '/api/projects' } = options;

  const [data, setData] = useState<unknown[] | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const listProjects = useCallback(async (): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const response = await fetch(projectsEndpoint, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json' },
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        setError(result.error ?? { message: 'Failed to list projects', status: response.status });
        setData(null);
        return;
      }

      setData(result.data);
    } catch (err) {
      setError(normalizeError(err));
      setData(null);
    } finally {
      setLoading(false);
    }
  }, [projectsEndpoint]);

  const createProject = useCallback(
    async (input: CreateProjectInput): Promise<unknown | null> => {
      setLoading(true);
      setError(null);

      try {
        const response = await fetch(projectsEndpoint, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(input),
        });

        const result = await response.json();

        if (!response.ok || result.error) {
          setError(result.error ?? { message: 'Failed to create project', status: response.status });
          return null;
        }

        // Optionally refresh the list
        await listProjects();

        return result.data;
      } catch (err) {
        setError(normalizeError(err));
        return null;
      } finally {
        setLoading(false);
      }
    },
    [projectsEndpoint, listProjects]
  );

  return {
    data,
    loading,
    error,
    listProjects,
    createProject,
  };
}
