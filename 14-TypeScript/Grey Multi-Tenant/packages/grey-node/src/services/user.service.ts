/**
 * Grey Node - User Service
 *
 * Server-side user service for Node.js applications.
 * Wraps adapter-core createUserController with error normalization.
 *
 * Pure ESM, no browser APIs, dependency-injection-friendly.
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

export interface UserServiceOptions {
  baseUrl: string;
  token?: string;
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

export class UserService {
  private readonly baseUrl: string;
  private token?: string;

  // Internal state
  public data: unknown | null = null;
  public loading: boolean = false;
  public error: GreyError | null = null;

  constructor(options: UserServiceOptions) {
    this.baseUrl = options.baseUrl;
    this.token = options.token;
  }

  /**
   * Set the authentication token.
   */
  setToken(token: string): void {
    this.token = token;
  }

  /**
   * Fetch the current user.
   */
  async fetchUser(): Promise<unknown | null> {
    this.loading = true;
    this.error = null;

    try {
      if (!this.token) {
        this.error = { message: 'No authentication token provided', code: 'NO_TOKEN', status: 401 };
        this.data = null;
        return null;
      }

      const controller = createUserController({
        apiBaseUrl: this.baseUrl,
        token: this.token,
      });

      const user = await controller.fetchUser();

      this.data = user;
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
