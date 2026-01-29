'use server';

/**
 * Server Query - Next.js Server Actions for generic queries
 *
 * Wraps @grey/adapters QueryController for server-side use.
 * No React, no hooks, no browser APIs.
 */

import { QueryController, type QueryOptions } from '@grey/adapters';
import type { GreyError } from './auth.js';

// =============================================================================
// Types
// =============================================================================

export interface QueryResult<T> {
  data: T | null;
  error: GreyError | null;
}

// =============================================================================
// Normalize Errors
// =============================================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      code: (err as { code?: string }).code,
      status: (err as { status?: number }).status,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err, raw: err };
  }
  return { message: 'Unknown error', raw: err };
}

// =============================================================================
// Server Actions
// =============================================================================

export async function executeQuery<T>(
  options: QueryOptions<T>
): Promise<QueryResult<T>> {
  try {
    const controller = new QueryController<T>(options);
    const data = await controller.execute();

    const state = controller.getState();

    if (state.isError || !data) {
      return {
        data: null,
        error: state.error ? { message: state.error.message } : { message: 'Query failed' },
      };
    }

    return {
      data,
      error: null,
    };
  } catch (err) {
    return {
      data: null,
      error: normalizeError(err),
    };
  }
}
