/**
 * Grey Remix - Server Mutation
 *
 * Server-side mutation wrapper for Remix loaders and actions.
 * Wraps adapter-core functions directly with error normalization.
 *
 * No React, no hooks, no browser APIs.
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

export interface MutationOptions {
  token?: string;
  baseUrl: string;
}

export interface MutationInput {
  endpoint: string;
  method?: 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  body?: unknown;
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
// Server Functions
// ============================================================

/**
 * Execute a generic mutation.
 */
export async function executeMutation<TData = unknown, TVariables = unknown>(
  options: MutationOptions,
  input: MutationInput & { variables?: TVariables }
): Promise<{ data: TData | null; error: GreyError | null }> {
  try {
    const mutation = createMutation<TData, TVariables>({
      apiBaseUrl: options.baseUrl,
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
