/**
 * Grey Electron - Main Mutation Service
 *
 * Main process mutation service.
 * Wraps adapter-core createMutation with error normalization.
 *
 * Runs in trusted main process only.
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
}

export interface MutationRequestOptions {
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

export class MutationService {
  private readonly baseUrl: string;

  constructor(options: MutationServiceOptions) {
    this.baseUrl = options.baseUrl;
  }

  /**
   * Execute a generic mutation.
   */
  async executeMutation<TData = unknown, TVariables = unknown>(
    options: MutationRequestOptions,
    input: MutationInput<TVariables>
  ): Promise<{ data: TData | null; error: GreyError | null }> {
    try {
      const mutation = createMutation<TData, TVariables>({
        apiBaseUrl: this.baseUrl,
        token: options.token,
      });

      const result = await mutation.execute(
        input.endpoint,
        input.variables as TVariables,
        input.method
      );

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
