/**
 * Grey Electron - Renderer useProjects Hook
 *
 * Client-side projects hook for Electron renderer.
 * Calls preload bridge functions (never IPC directly).
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
  listProjects: (token: string) => Promise<void>;
  createProject: (token: string, input: CreateProjectInput) => Promise<unknown | null>;
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

export function useProjects(): UseProjectsResult {
  const [data, setData] = useState<unknown[] | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);

  const listProjects = useCallback(async (token: string): Promise<void> => {
    setLoading(true);
    setError(null);

    try {
      const result = await window.grey.projects.listProjects(token);

      if (result.error) {
        setError(result.error);
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
  }, []);

  const createProject = useCallback(
    async (token: string, input: CreateProjectInput): Promise<unknown | null> => {
      setLoading(true);
      setError(null);

      try {
        const result = await window.grey.projects.createProject(token, input);

        if (result.error) {
          setError(result.error);
          return null;
        }

        // Refresh the list after creation
        await listProjects(token);

        return result.data;
      } catch (err) {
        setError(normalizeError(err));
        return null;
      } finally {
        setLoading(false);
      }
    },
    [listProjects]
  );

  return {
    data,
    loading,
    error,
    listProjects,
    createProject,
  };
}
