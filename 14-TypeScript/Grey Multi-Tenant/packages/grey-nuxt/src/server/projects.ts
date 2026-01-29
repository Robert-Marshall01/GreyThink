/**
 * Grey Nuxt - Server Projects
 *
 * Server-side project utilities for Nuxt Nitro.
 * Wraps @grey/adapters ProjectsController functions.
 *
 * No Vue, no composables, no browser APIs.
 */

import { createProjectsController, type CreateProjectInput } from '@grey/adapters';
import type { GreyClient, Project, Pagination } from '@grey/core-client';
import { normalizeError, type GreyError } from './auth.js';

/**
 * Projects list result type
 */
export interface ProjectsResult {
  projects: Project[];
  pagination: Pagination | null;
  error: GreyError | null;
}

/**
 * Single project result type
 */
export interface ProjectResult {
  project: Project | null;
  error: GreyError | null;
}

/**
 * List projects with pagination.
 * Wraps ProjectsController.listProjects from @grey/adapters.
 */
export async function listProjects(
  client: GreyClient,
  page: number = 1,
  pageSize: number = 20
): Promise<ProjectsResult> {
  try {
    const controller = createProjectsController(client);
    const projects = await controller.listProjects({ page, pageSize });

    const state = controller.getState();

    return {
      projects: projects ?? [],
      pagination: state.pagination,
      error: state.error ? { message: state.error } : null,
    };
  } catch (err) {
    return {
      projects: [],
      pagination: null,
      error: normalizeError(err),
    };
  }
}

/**
 * Create a new project.
 * Wraps ProjectsController.createProject from @grey/adapters.
 */
export async function createProject(
  client: GreyClient,
  input: CreateProjectInput
): Promise<ProjectResult> {
  try {
    const controller = createProjectsController(client);
    const project = await controller.createProject(input);

    if (!project) {
      const state = controller.getState();
      return {
        project: null,
        error: state.error ? { message: state.error } : { message: 'Failed to create project' },
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
