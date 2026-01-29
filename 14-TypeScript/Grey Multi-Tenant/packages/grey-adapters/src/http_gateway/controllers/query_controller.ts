/**
 * Grey Integration Layer - Query Controller
 *
 * HTTP endpoints for query operations.
 * Calls adapter-core directly, no business logic.
 */

import type { AdapterCore } from '../../adapter_core';
import {
  type HttpRequest,
  type HttpResponse,
  type HttpRouter,
  successResponse,
  errorResponse,
} from '../router';
import { normalizeError } from '../../error_normalizer';

export function registerQueryRoutes(router: HttpRouter, core: AdapterCore): void {
  /**
   * POST /api/v1/query
   *
   * Execute a query.
   */
  router.post('/api/v1/query', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      const body = req.body as {
        queryName?: string;
        parameters?: Record<string, unknown>;
        tenantId?: string;
      };

      const response = await core.query({
        queryName: body.queryName || '',
        parameters: body.parameters || {},
        tenantId: body.tenantId || req.context.tenantId,
      });

      return successResponse(response);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });

  /**
   * GET /api/v1/query/:queryName
   *
   * Execute a query by name with query string parameters.
   */
  router.get('/api/v1/query/:queryName', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      // Convert query string to parameters
      const parameters: Record<string, unknown> = {};
      for (const [key, value] of Object.entries(req.query)) {
        if (key !== 'tenantId') {
          parameters[key] = value;
        }
      }

      const response = await core.query({
        queryName: req.params.queryName,
        parameters,
        tenantId: req.query.tenantId || req.context.tenantId,
      });

      return successResponse(response);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });
}
