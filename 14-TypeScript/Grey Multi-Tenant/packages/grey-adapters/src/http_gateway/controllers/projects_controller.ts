/**
 * Grey Integration Layer - Projects Controller
 *
 * HTTP endpoints for project operations.
 * Calls adapter-core directly, no business logic.
 */

import type { AdapterCore } from '../../adapter_core';
import {
  type HttpRequest,
  type HttpResponse,
  type HttpRouter,
  successResponse,
  createdResponse,
  errorResponse,
} from '../router';
import { normalizeError } from '../../error_normalizer';

export function registerProjectsRoutes(router: HttpRouter, core: AdapterCore): void {
  /**
   * GET /api/v1/projects
   *
   * List projects with pagination.
   */
  router.get('/api/v1/projects', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      const response = await core.listProjects({
        limit: parseInt(req.query.limit || '10', 10),
        offset: parseInt(req.query.offset || '0', 10),
        tenantId: req.query.tenantId || req.context.tenantId,
      });

      return successResponse(response);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });

  /**
   * POST /api/v1/projects
   *
   * Create a new project.
   */
  router.post('/api/v1/projects', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      const body = req.body as {
        name?: string;
        description?: string;
        tenantId?: string;
      };

      const project = await core.createProject({
        name: body.name || '',
        description: body.description,
        tenantId: body.tenantId || req.context.tenantId,
      });

      return createdResponse(project);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });
}
