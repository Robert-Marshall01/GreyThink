/**
 * Grey Astro - Server Projects
 *
 * Server-side projects wrapper for Astro server routes.
 * Wraps adapter-core functions directly with error normalization.
 *
 * No browser APIs, no client code.
 */

import { createProjectsController } from '@grey/adapters';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface ProjectsOptions {
  token: string;
  baseUrl: string;
}

export interface CreateProjectInput {
  name: string;
  description?: string;
  [key: string]: unknown;
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
// Server Functions
// ============================================================

/**
 * List all projects.
 */
export async function listProjects(
  options: ProjectsOptions
): Promise<{ data: unknown[] | null; error: GreyError | null }> {
  try {
    const controller = createProjectsController({
      apiBaseUrl: options.baseUrl,
      token: options.token,
    });

    const projects = await controller.listProjects();

    return {
      data: projects,
      error: null,
    };
  } catch (err) {
    return {
      data: null,
      error: normalizeError(err),
    };
  }
}

/**
 * Create a new project.
 */
export async function createProject(
  options: ProjectsOptions,
  input: CreateProjectInput
): Promise<{ data: unknown | null; error: GreyError | null }> {
  try {
    const controller = createProjectsController({
      apiBaseUrl: options.baseUrl,
      token: options.token,
    });

    const project = await controller.createProject(input);

    return {
      data: project,
      error: null,
    };
  } catch (err) {
    return {
      data: null,
      error: normalizeError(err),
    };
  }
}
