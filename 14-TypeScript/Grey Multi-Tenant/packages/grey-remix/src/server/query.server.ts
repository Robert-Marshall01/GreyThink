/**
 * Grey Remix - Server Query
 *
 * Server-side query wrapper for Remix loaders and actions.
 * Wraps adapter-core functions directly with error normalization.
 *
 * No React, no hooks, no browser APIs.
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

export interface QueryOptions {
  token?: string;
  baseUrl: string;
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
// Server Functions
// ============================================================

/**
 * Execute a generic query.
 */
export async function executeQuery<T = unknown>(
  options: QueryOptions,
  input: QueryInput
): Promise<{ data: T | null; error: GreyError | null }> {
  try {
    const query = createQuery<T>({
      apiBaseUrl: options.baseUrl,
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
