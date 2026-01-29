/**
 * Grey Remix - Server User
 *
 * Server-side user wrapper for Remix loaders and actions.
 * Wraps adapter-core functions directly with error normalization.
 *
 * No React, no hooks, no browser APIs.
 */

import { createUserController } from '@grey/adapters';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface FetchUserOptions {
  token: string;
  baseUrl: string;
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
 * Fetch the current user.
 */
export async function fetchUser(
  options: FetchUserOptions
): Promise<{ data: unknown | null; error: GreyError | null }> {
  try {
    const controller = createUserController({
      apiBaseUrl: options.baseUrl,
      token: options.token,
    });

    const user = await controller.fetchUser();

    return {
      data: user,
      error: null,
    };
  } catch (err) {
    return {
      data: null,
      error: normalizeError(err),
    };
  }
}
