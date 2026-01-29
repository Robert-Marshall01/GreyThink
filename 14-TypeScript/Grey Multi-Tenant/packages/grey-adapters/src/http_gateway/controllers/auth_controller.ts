/**
 * Grey Integration Layer - Auth Controller
 *
 * HTTP endpoints for authentication operations.
 * Calls adapter-core directly, no business logic.
 */

import type { AdapterCore } from '../../adapter_core';
import {
  type HttpRequest,
  type HttpResponse,
  type HttpRouter,
  successResponse,
  noContentResponse,
  errorResponse,
} from '../router';
import { normalizeError } from '../../error_normalizer';

export function registerAuthRoutes(router: HttpRouter, core: AdapterCore): void {
  /**
   * POST /api/v1/auth/login
   *
   * Login with email and password.
   */
  router.post('/api/v1/auth/login', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      const body = req.body as { email?: string; password?: string; tenantId?: string };

      const response = await core.login({
        email: body.email || '',
        password: body.password || '',
        tenantId: body.tenantId || req.context.tenantId,
      });

      return successResponse(response);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });

  /**
   * POST /api/v1/auth/logout
   *
   * Logout current session.
   */
  router.post('/api/v1/auth/logout', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      await core.logout({
        accessToken: req.context.authToken,
      });

      return noContentResponse();
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });

  /**
   * POST /api/v1/auth/refresh
   *
   * Refresh authentication token.
   */
  router.post('/api/v1/auth/refresh', async (req: HttpRequest): Promise<HttpResponse> => {
    try {
      const body = req.body as { refreshToken?: string };

      const response = await core.refresh({
        refreshToken: body.refreshToken || '',
      });

      return successResponse(response);
    } catch (error) {
      return errorResponse(normalizeError(error));
    }
  });
}
