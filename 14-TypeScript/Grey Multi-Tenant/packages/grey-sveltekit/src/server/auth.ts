/**
 * Grey SvelteKit - Server Auth
 *
 * Server-side authentication utilities for SvelteKit.
 * Wraps @grey/adapters AuthController.
 *
 * No Svelte stores, no browser APIs.
 */

import { AuthController, MemoryTokenStorage } from '@grey/adapters';
import { createGreyClient, type User, type AuthSession } from '@grey/core-client';

/**
 * Normalized error shape
 */
export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

/**
 * Auth result type
 */
export interface AuthResult {
  session: AuthSession | null;
  user: User | null;
  error: GreyError | null;
}

/**
 * Refresh result type
 */
export interface RefreshResult {
  session: AuthSession | null;
  error: GreyError | null;
}

/**
 * Logout result type
 */
export interface LogoutResult {
  error: GreyError | null;
}

/**
 * Normalize any error into a consistent GreyError shape
 */
export function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      code: (err as { code?: string }).code,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

/**
 * Login with email and password.
 * Wraps AuthController.login from @grey/adapters.
 */
export async function login(
  baseUrl: string,
  email: string,
  password: string
): Promise<AuthResult> {
  try {
    const controller = new AuthController({
      apiBaseUrl: baseUrl,
      storage: new MemoryTokenStorage(),
    });

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

    // Get the tokens from the client
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

/**
 * Logout and invalidate session.
 * Wraps AuthController.logout from @grey/adapters.
 */
export async function logout(
  baseUrl: string,
  token?: string
): Promise<LogoutResult> {
  try {
    if (!token) {
      return { error: null };
    }

    const client = createGreyClient({
      baseUrl,
      token,
    });

    await client.auth.logout();
    return { error: null };
  } catch {
    // Logout errors are typically ignored
    return { error: null };
  }
}

/**
 * Refresh the access token.
 * Wraps token refresh from @grey/adapters.
 */
export async function refresh(
  baseUrl: string,
  refreshToken?: string
): Promise<RefreshResult> {
  try {
    if (!refreshToken) {
      return {
        session: null,
        error: { message: 'No refresh token provided' },
      };
    }

    const client = createGreyClient({
      baseUrl,
    });

    const result = await client.auth.refresh({ refresh_token: refreshToken });

    if (result.error || !result.data?.data) {
      return {
        session: null,
        error: { message: 'Token refresh failed' },
      };
    }

    return {
      session: result.data.data,
      error: null,
    };
  } catch (err) {
    return {
      session: null,
      error: normalizeError(err),
    };
  }
}
