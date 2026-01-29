/**
 * Grey Electron - Main Query Service
 *
 * Main process query service.
 * Wraps adapter-core createQuery with error normalization.
 *
 * Runs in trusted main process only.
 */

import { createQuery } from '@grey/adapters';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface QueryServiceOptions {
  baseUrl: string;
}

export interface QueryRequestOptions {
  token?: string;
}

export interface QueryInput {
  endpoint: string;
  params?: Record<string, unknown>;
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
// Service Class
// ============================================================

export class QueryService {
  private readonly baseUrl: string;

  constructor(options: QueryServiceOptions) {
    this.baseUrl = options.baseUrl;
  }

  /**
   * Execute a generic query.
   */
  async executeQuery<T = unknown>(
    options: QueryRequestOptions,
    input: QueryInput
  ): Promise<{ data: T | null; error: GreyError | null }> {
    try {
      const query = createQuery<T>({
        apiBaseUrl: this.baseUrl,
        token: options.token,
      });

      const result = await query.execute(input.endpoint, input.params);

      return {
        data: result,
        error: null,
      };
    } catch (err) {
      return {
        data: null,
        error: normalizeError(err),
      };
    }
  }
}
