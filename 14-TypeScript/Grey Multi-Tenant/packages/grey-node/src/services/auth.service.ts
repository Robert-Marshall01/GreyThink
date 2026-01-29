/**
 * Grey Node - Auth Service
 *
 * Server-side authentication service for Node.js applications.
 * Wraps adapter-core AuthController with error normalization.
 *
 * Pure ESM, no browser APIs, dependency-injection-friendly.
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

  // Internal state
  public data: AuthData | null = null;
  public loading: boolean = false;
  public error: GreyError | null = null;

  constructor(options: AuthServiceOptions) {
    this.baseUrl = options.baseUrl;
  }

  /**
   * Login with email and password.
   */
  async login(credentials: LoginCredentials): Promise<AuthData | null> {
    this.loading = true;
    this.error = null;

    try {
      const controller = new AuthController({
        apiBaseUrl: this.baseUrl,
        storage: new MemoryTokenStorage(),
      });

      const success = await controller.login(credentials.email, credentials.password);

      if (!success) {
        this.error = { message: 'Invalid credentials', code: 'INVALID_CREDENTIALS', status: 401 };
        this.data = null;
        return null;
      }

      const state = controller.getState();
      const client = controller.getClient();
      const tokens = client.tokens.getTokens();

      this.data = {
        user: state.user,
        session: tokens
          ? {
              accessToken: tokens.accessToken ?? '',
              refreshToken: tokens.refreshToken,
              expiresIn: tokens.expiresIn,
            }
          : null,
      };

      return this.data;
    } catch (err) {
      this.error = normalizeError(err);
      this.data = null;
      return null;
    } finally {
      this.loading = false;
    }
  }

  /**
   * Logout and invalidate session.
   */
  async logout(token?: string): Promise<void> {
    this.loading = true;
    this.error = null;

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

      this.data = null;
    } catch {
      // Logout errors are typically ignored
      this.data = null;
    } finally {
      this.loading = false;
    }
  }

  /**
   * Refresh the access token.
   */
  async refresh(options: RefreshOptions): Promise<AuthData | null> {
    this.loading = true;
    this.error = null;

    try {
      const controller = new AuthController({
        apiBaseUrl: this.baseUrl,
        storage: new MemoryTokenStorage(),
      });

      const client = controller.getClient();
      client.tokens.setTokens({ refreshToken: options.refreshToken });

      const success = await controller.refresh();

      if (!success) {
        this.error = { message: 'Token refresh failed', code: 'REFRESH_FAILED', status: 401 };
        this.data = null;
        return null;
      }

      const state = controller.getState();
      const tokens = client.tokens.getTokens();

      this.data = {
        user: state.user,
        session: tokens
          ? {
              accessToken: tokens.accessToken ?? '',
              refreshToken: tokens.refreshToken,
              expiresIn: tokens.expiresIn,
            }
          : null,
      };

      return this.data;
    } catch (err) {
      this.error = normalizeError(err);
      this.data = null;
      return null;
    } finally {
      this.loading = false;
    }
  }
}
