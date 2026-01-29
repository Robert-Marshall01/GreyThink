'use server';

/**
 * Server Mutation - Next.js Server Actions for generic mutations
 *
 * Wraps @grey/adapters MutationController for server-side use.
 * No React, no hooks, no browser APIs.
 */

import { MutationController, type MutationOptions } from '@grey/adapters';
import type { GreyError } from './auth.js';

// =============================================================================
// Types
// =============================================================================

export interface MutationResult<T> {
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

export async function executeMutation<TData, TVariables>(
  variables: TVariables,
  options: MutationOptions<TData, TVariables>
): Promise<MutationResult<TData>> {
  try {
    const controller = new MutationController<TData, TVariables>(options);
    const data = await controller.mutate(variables);

    const state = controller.getState();

    if (state.isError || !data) {
      return {
        data: null,
        error: state.error ? { message: state.error.message } : { message: 'Mutation failed' },
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
