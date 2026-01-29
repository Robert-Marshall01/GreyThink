/**
 * Grey Integration Layer - User Controller
 *
 * HTTP endpoints for user operations.
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

export function registerUserRoutes(router: HttpRouter, core: AdapterCore): void {
  /**
   * GET /api/v1/users/me
   *
   * Get the current authenticated user.
   */
  router.get('/api/v1/users/me', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      const user = await core.getCurrentUser();
      return successResponse(user);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });

  /**
   * GET /api/v1/users/:userId
   *
   * Get a user by ID.
   */
  router.get('/api/v1/users/:userId', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      const user = await core.getUser({
        userId: req.params.userId,
      });

      return successResponse(user);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });
}
