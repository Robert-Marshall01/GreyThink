/**
 * Grey Node - Query Service
 *
 * Server-side query service for Node.js applications.
 * Wraps adapter-core createQuery with error normalization.
 *
 * Pure ESM, no browser APIs, dependency-injection-friendly.
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

export class QueryService<T = unknown> {
  private readonly baseUrl: string;
  private token?: string;

  // Internal state
  public data: T | null = null;
  public loading: boolean = false;
  public error: GreyError | null = null;

  constructor(options: QueryServiceOptions) {
    this.baseUrl = options.baseUrl;
    this.token = options.token;
  }

  /**
   * Set the authentication token.
   */
  setToken(token: string): void {
    this.token = token;
  }

  /**
   * Execute a generic query.
   */
  async executeQuery(input: QueryInput): Promise<T | null> {
    this.loading = true;
    this.error = null;

    try {
      const query = createQuery<T>({
        apiBaseUrl: this.baseUrl,
        token: this.token,
      });

      const result = await query.execute(input.endpoint, input.params);

      this.data = result;
      return this.data;
    } catch (err) {
      this.error = normalizeError(err);
      this.data = null;
      return null;
    } finally {
      this.loading = false;
    }
  }
}
