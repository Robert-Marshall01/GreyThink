/**
 * Grey Electron - Main User Service
 *
 * Main process user service.
 * Wraps adapter-core createUserController with error normalization.
 *
 * Runs in trusted main process only.
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
}

export interface FetchUserOptions {
  token: string;
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

  constructor(options: UserServiceOptions) {
    this.baseUrl = options.baseUrl;
  }

  /**
   * Fetch the current user.
   */
  async fetchUser(options: FetchUserOptions): Promise<{ data: unknown | null; error: GreyError | null }> {
    try {
      if (!options.token) {
        return {
          data: null,
          error: { message: 'No authentication token provided', code: 'NO_TOKEN', status: 401 },
        };
      }

      const controller = createUserController({
        apiBaseUrl: this.baseUrl,
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
}
