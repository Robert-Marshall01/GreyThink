'use server';

/**
 * Server User - Next.js Server Actions for user operations
 *
 * Wraps @grey/adapters UserController for server-side use.
 * No React, no hooks, no browser APIs.
 */

import { UserController } from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';
import type { GreyError } from './auth.js';

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

export async function fetchUser(
  client: GreyClient
): Promise<{ user: User | null; error: GreyError | null }> {
  try {
    const controller = new UserController(client);
    const user = await controller.fetchCurrentUser();

    if (!user) {
      const state = controller.getState();
      return {
        user: null,
        error: state.error ? { message: state.error } : { message: 'User not found' },
      };
    }

    return {
      user,
      error: null,
    };
  } catch (err) {
    return {
      user: null,
      error: normalizeError(err),
    };
  }
}
