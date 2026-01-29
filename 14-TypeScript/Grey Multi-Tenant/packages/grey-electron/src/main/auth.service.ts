/**
 * Grey Electron - Main Auth Service
 *
 * Main process authentication service.
 * Wraps adapter-core AuthController with error normalization.
 *
 * Runs in trusted main process only.
 */

import {
  AuthController,
  MemoryTokenStorage,
} from '@grey/adapters';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface AuthData {
  user: unknown | null;
  session: {
    accessToken: string;
    refreshToken?: string;
    expiresIn?: number;
  } | null;
}

export interface AuthServiceOptions {
  baseUrl: string;
}

export interface LoginCredentials {
  email: string;
  password: string;
}

export interface RefreshOptions {
  refreshToken: string;
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

export class AuthService {
  private readonly baseUrl: string;

  constructor(options: AuthServiceOptions) {
    this.baseUrl = options.baseUrl;
  }

  /**
   * Login with email and password.
   */
  async login(credentials: LoginCredentials): Promise<{ data: AuthData | null; error: GreyError | null }> {
    try {
      const controller = new AuthController({
        apiBaseUrl: this.baseUrl,
        storage: new MemoryTokenStorage(),
      });

      const success = await controller.login(credentials.email, credentials.password);

      if (!success) {
        return {
          data: null,
          error: { message: 'Invalid credentials', code: 'INVALID_CREDENTIALS', status: 401 },
        };
      }

      const state = controller.getState();
      const client = controller.getClient();
      const tokens = client.tokens.getTokens();

      return {
        data: {
          user: state.user,
          session: tokens
            ? {
                accessToken: tokens.accessToken ?? '',
                refreshToken: tokens.refreshToken,
                expiresIn: tokens.expiresIn,
              }
            : null,
        },
        error: null,
      };
    } catch (err) {
      return {
        data: null,
        error: normalizeError(err),
      };
    }
  }

  /**
   * Logout and invalidate session.
   */
  async logout(token?: string): Promise<{ error: GreyError | null }> {
    try {
      if (token) {
        const controller = new AuthController({
          apiBaseUrl: this.baseUrl,
          storage: new MemoryTokenStorage(),
        });

        const client = controller.getClient();
        client.tokens.setTokens({ accessToken: token });

        await controller.logout();
      }

      return { error: null };
    } catch {
      return { error: null };
    }
  }

  /**
   * Refresh the access token.
   */
  async refresh(options: RefreshOptions): Promise<{ data: AuthData | null; error: GreyError | null }> {
    try {
      const controller = new AuthController({
        apiBaseUrl: this.baseUrl,
        storage: new MemoryTokenStorage(),
      });

      const client = controller.getClient();
      client.tokens.setTokens({ refreshToken: options.refreshToken });

      const success = await controller.refresh();

      if (!success) {
        return {
          data: null,
          error: { message: 'Token refresh failed', code: 'REFRESH_FAILED', status: 401 },
        };
      }

      const state = controller.getState();
      const tokens = client.tokens.getTokens();

      return {
        data: {
          user: state.user,
          session: tokens
            ? {
                accessToken: tokens.accessToken ?? '',
                refreshToken: tokens.refreshToken,
                expiresIn: tokens.expiresIn,
              }
            : null,
        },
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
