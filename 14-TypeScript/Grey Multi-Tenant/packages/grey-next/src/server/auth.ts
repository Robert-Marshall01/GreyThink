'use server';

/**
 * Server Auth - Next.js Server Actions for authentication
 *
 * Wraps @grey/adapters AuthController for server-side use.
 * No React, no hooks, no browser APIs.
 */

import {
  AuthController,
  MemoryTokenStorage,
  type AuthConfig,
} from '@grey/adapters';
import type { User, AuthSession } from '@grey/core-client';

// =============================================================================
// Types
// =============================================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface AuthResult {
  session: AuthSession | null;
  user: User | null;
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

export async function login(
  baseUrl: string,
  email: string,
  password: string
): Promise<AuthResult> {
  try {
    const config: AuthConfig = {
      apiBaseUrl: baseUrl,
      storage: new MemoryTokenStorage(),
    };

    const controller = new AuthController(config);
    const success = await controller.login(email, password);

    if (!success) {
      return {
        session: null,
        user: null,
        error: { message: 'Invalid credentials' },
      };
    }

    const state = controller.getState();
    const client = controller.getClient();
    const tokens = client.tokens.getTokens();

    const session: AuthSession | null = tokens
      ? {
          access_token: tokens.accessToken ?? '',
          refresh_token: tokens.refreshToken ?? '',
          expires_in: tokens.expiresIn ?? 3600,
        }
      : null;

    return {
      session,
      user: state.user,
      error: null,
    };
  } catch (err) {
    return {
      session: null,
      user: null,
      error: normalizeError(err),
    };
  }
}

export async function logout(baseUrl: string, token?: string): Promise<{ error: GreyError | null }> {
  try {
    if (!token) {
      return { error: null };
    }

    const config: AuthConfig = {
      apiBaseUrl: baseUrl,
      storage: new MemoryTokenStorage(),
    };

    const controller = new AuthController(config);
    controller.logout();

    return { error: null };
  } catch (err) {
    return { error: normalizeError(err) };
  }
}

export async function refresh(
  baseUrl: string,
  refreshToken?: string
): Promise<{ session: AuthSession | null; error: GreyError | null }> {
  try {
    if (!refreshToken) {
      return {
        session: null,
        error: { message: 'No refresh token provided' },
      };
    }

    const config: AuthConfig = {
      apiBaseUrl: baseUrl,
      storage: new MemoryTokenStorage(),
    };

    const controller = new AuthController(config);
    const success = await controller.restoreSession();

    if (!success) {
      return {
        session: null,
        error: { message: 'Token refresh failed' },
      };
    }

    const client = controller.getClient();
    const tokens = client.tokens.getTokens();

    const session: AuthSession | null = tokens
      ? {
          access_token: tokens.accessToken ?? '',
          refresh_token: tokens.refreshToken ?? '',
          expires_in: tokens.expiresIn ?? 3600,
        }
      : null;

    return {
      session,
      error: null,
    };
  } catch (err) {
    return {
      session: null,
      error: normalizeError(err),
    };
  }
}
