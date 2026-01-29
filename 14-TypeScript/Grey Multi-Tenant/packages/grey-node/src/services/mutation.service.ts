/**
 * Grey Node - Mutation Service
 *
 * Server-side mutation service for Node.js applications.
 * Wraps adapter-core createMutation with error normalization.
 *
 * Pure ESM, no browser APIs, dependency-injection-friendly.
 */

import { createMutation } from '@grey/adapters';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface MutationServiceOptions {
  baseUrl: string;
  token?: string;
}

export interface MutationInput<TVariables = unknown> {
  endpoint: string;
  method?: 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  variables?: TVariables;
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

export class MutationService<TData = unknown, TVariables = unknown> {
  private readonly baseUrl: string;
  private token?: string;

  // Internal state
  public data: TData | null = null;
  public loading: boolean = false;
  public error: GreyError | null = null;

  constructor(options: MutationServiceOptions) {
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
   * Execute a generic mutation.
   */
  async executeMutation(input: MutationInput<TVariables>): Promise<TData | null> {
    this.loading = true;
    this.error = null;

    try {
      const mutation = createMutation<TData, TVariables>({
        apiBaseUrl: this.baseUrl,
        token: this.token,
      });

      const result = await mutation.execute(
        input.endpoint,
        input.variables as TVariables,
        input.method
      );

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

  /**
   * Reset the service state.
   */
  reset(): void {
    this.data = null;
    this.loading = false;
    this.error = null;
  }
}
