'use server';

/**
 * Server Projects - Next.js Server Actions for projects operations
 *
 * Wraps @grey/adapters ProjectsController for server-side use.
 * No React, no hooks, no browser APIs.
 */

import { ProjectsController, type CreateProjectInput } from '@grey/adapters';
import type { GreyClient, Project, Pagination } from '@grey/core-client';
import type { GreyError } from './auth.js';

// =============================================================================
// Types
// =============================================================================

export interface ProjectsResult {
  projects: Project[];
  pagination: Pagination | null;
  error: GreyError | null;
}

export interface ProjectResult {
  project: Project | null;
  error: GreyError | null;
}

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
// Server Actions
// =============================================================================

export async function listProjects(
  client: GreyClient,
  page = 1,
  pageSize = 20
): Promise<ProjectsResult> {
  try {
    const controller = new ProjectsController(client);
    await controller.loadList(page, pageSize);

    const state = controller.getListState();

    if (state.error) {
      return {
        projects: [],
        pagination: null,
        error: { message: state.error },
      };
    }

    return {
      projects: state.projects,
      pagination: state.pagination,
      error: null,
    };
  } catch (err) {
    return {
      projects: [],
      pagination: null,
      error: normalizeError(err),
    };
  }
}

export async function createProject(
  client: GreyClient,
  input: CreateProjectInput
): Promise<ProjectResult> {
  try {
    const controller = new ProjectsController(client);
    const project = await controller.createProject(input);

    if (!project) {
      return {
        project: null,
        error: { message: 'Failed to create project' },
      };
    }

    return {
      project,
      error: null,
    };
  } catch (err) {
    return {
      project: null,
      error: normalizeError(err),
    };
  }
}
