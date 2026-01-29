/**
 * Grey Integration Layer - Mutation Controller
 *
 * HTTP endpoints for mutation operations.
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

export function registerMutationRoutes(router: HttpRouter, core: AdapterCore): void {
  /**
   * POST /api/v1/mutate
   *
   * Execute a mutation.
   */
  router.post('/api/v1/mutate', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      const body = req.body as {
        mutationName?: string;
        parameters?: Record<string, unknown>;
        tenantId?: string;
      };

      const response = await core.mutate({
        mutationName: body.mutationName || '',
        parameters: body.parameters || {},
        tenantId: body.tenantId || req.context.tenantId,
      });

      return successResponse(response);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });

  /**
   * POST /api/v1/mutate/:mutationName
   *
   * Execute a mutation by name.
   */
  router.post('/api/v1/mutate/:mutationName', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      const body = req.body as {
        parameters?: Record<string, unknown>;
        tenantId?: string;
      };

      const response = await core.mutate({
        mutationName: req.params.mutationName,
        parameters: body.parameters || {},
        tenantId: body.tenantId || req.context.tenantId,
      });

      return successResponse(response);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });
}
