/**
 * Grey Integration Layer - HTTP Gateway Router
 *
 * Central router that mounts all domain controllers.
 * Provides unified error handling and request processing.
 */

import type { IncomingMessage, ServerResponse } from 'http';
import { normalizeError, toHttpStatus, type NormalizedError } from '../error_normalizer';
import type { RequestContext } from '../adapter_core';

// ============================================================================
// Types
// ============================================================================

export interface HttpRequest {
  method: string;
  url: string;
  headers: Record<string, string | string[] | undefined>;
  body: unknown;
  params: Record<string, string>;
  query: Record<string, string>;
  context: RequestContext;
}

export interface HttpResponse {
  status: number;
  headers: Record<string, string>;
  body: unknown;
}

export type RouteHandler = (req: HttpRequest) => Promise<HttpResponse>;

export interface Route {
  method: string;
  path: string;
  handler: RouteHandler;
}

// ============================================================================
// Router
// ============================================================================

export class HttpRouter {
  private routes: Route[] = [];

  /**
   * Register a GET route.
   */
  get(path: string, handler: RouteHandler): void {
    this.routes.push({ method: 'GET', path, handler });
  }

  /**
   * Register a POST route.
   */
  post(path: string, handler: RouteHandler): void {
    this.routes.push({ method: 'POST', path, handler });
  }

  /**
   * Register a PUT route.
   */
  put(path: string, handler: RouteHandler): void {
    this.routes.push({ method: 'PUT', path, handler });
  }

  /**
   * Register a DELETE route.
   */
  delete(path: string, handler: RouteHandler): void {
    this.routes.push({ method: 'DELETE', path, handler });
  }

  /**
   * Match a request to a route.
   */
  match(method: string, url: string): { route: Route; params: Record<string, string> } | null {
    for (const route of this.routes) {
      if (route.method !== method) continue;

      const params = matchPath(route.path, url);
      if (params !== null) {
        return { route, params };
      }
    }
    return null;
  }

  /**
   * Handle an HTTP request.
   */
  async handle(req: HttpRequest): Promise<HttpResponse> {
    const urlPath = req.url.split('?')[0];
    const match = this.match(req.method, urlPath);

    if (!match) {
      return {
        status: 404,
        headers: { 'Content-Type': 'application/json' },
        body: {
          code: 'not_found',
          message: `Route ${req.method} ${urlPath} not found`,
          details: '',
        },
      };
    }

    try {
      req.params = match.params;
      return await match.route.handler(req);
    } catch (error) {
      return this.handleError(error);
    }
  }

  /**
   * Handle errors and return normalized error response.
   */
  private handleError(error: unknown): HttpResponse {
    const normalized = normalizeError(error);
    return {
      status: toHttpStatus(normalized.code),
      headers: { 'Content-Type': 'application/json' },
      body: normalized,
    };
  }
}

// ============================================================================
// Path Matching
// ============================================================================

function matchPath(pattern: string, path: string): Record<string, string> | null {
  const patternParts = pattern.split('/').filter(Boolean);
  const pathParts = path.split('/').filter(Boolean);

  if (patternParts.length !== pathParts.length) {
    return null;
  }

  const params: Record<string, string> = {};

  for (let i = 0; i < patternParts.length; i++) {
    const patternPart = patternParts[i];
    const pathPart = pathParts[i];

    if (patternPart.startsWith(':')) {
      const paramName = patternPart.slice(1);
      params[paramName] = pathPart;
    } else if (patternPart !== pathPart) {
      return null;
    }
  }

  return params;
}

// ============================================================================
// Request Parsing
// ============================================================================

export function parseQuery(url: string): Record<string, string> {
  const queryString = url.split('?')[1];
  if (!queryString) return {};

  const params: Record<string, string> = {};
  for (const pair of queryString.split('&')) {
    const [key, value] = pair.split('=');
    if (key) {
      params[decodeURIComponent(key)] = decodeURIComponent(value || '');
    }
  }
  return params;
}

export function extractContext(headers: Record<string, string | string[] | undefined>): RequestContext {
  const getHeader = (name: string): string | undefined => {
    const value = headers[name] || headers[name.toLowerCase()];
    return Array.isArray(value) ? value[0] : value;
  };

  const authHeader = getHeader('Authorization') || getHeader('authorization');
  let authToken: string | undefined;
  if (authHeader?.startsWith('Bearer ')) {
    authToken = authHeader.slice(7);
  }

  return {
    authToken,
    tenantId: getHeader('X-Tenant-ID') || getHeader('x-tenant-id'),
    correlationId: getHeader('X-Correlation-ID') || getHeader('x-correlation-id'),
  };
}

// ============================================================================
// Response Helpers
// ============================================================================

export function jsonResponse(status: number, body: unknown): HttpResponse {
  return {
    status,
    headers: { 'Content-Type': 'application/json' },
    body,
  };
}

export function successResponse(data: unknown): HttpResponse {
  return jsonResponse(200, data);
}

export function createdResponse(data: unknown): HttpResponse {
  return jsonResponse(201, data);
}

export function noContentResponse(): HttpResponse {
  return jsonResponse(204, null);
}

export function errorResponse(error: NormalizedError): HttpResponse {
  return jsonResponse(toHttpStatus(error.code), error);
}
