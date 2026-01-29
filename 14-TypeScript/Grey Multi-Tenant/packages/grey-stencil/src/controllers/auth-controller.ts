/**
 * AuthController - Stencil Controller for authentication
 *
 * Wraps @grey/adapters AuthController for Stencil integration.
 * Exposes: data, loading, error, and domain actions.
 */

import {
  AuthController as CoreAuthController,
  BrowserTokenStorage,
  MemoryTokenStorage,
  type AuthState,
  type AuthConfig,
} from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';

// =============================================================================
// Types
// =============================================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface StencilHost {
  forceUpdate(): void;
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

function isBrowser(): boolean {
  return typeof window !== 'undefined' && typeof document !== 'undefined';
}

// =============================================================================
// AuthController
// =============================================================================

export class AuthController {
  private host: StencilHost;
  private coreController: CoreAuthController;
  private unsubscribe: (() => void) | null = null;

  // Reactive state
  private _data: { user: User | null; isAuthenticated: boolean } | null = null;
  private _loading: boolean = false;
  private _error: GreyError | null = null;

  constructor(host: StencilHost, config: AuthConfig) {
    this.host = host;

    const fullConfig: AuthConfig = {
      ...config,
      storage: config.storage ?? (isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage()),
    };

    this.coreController = new CoreAuthController(fullConfig);
  }

  connect(): void {
    this.unsubscribe = this.coreController.subscribe((state: AuthState) => {
      this._data = { user: state.user, isAuthenticated: state.isAuthenticated };
      this._loading = state.isLoading;
      this._error = state.error ? normalizeError(state.error) : null;
      this.host.forceUpdate();
    });
  }

  disconnect(): void {
    this.unsubscribe?.();
    this.unsubscribe = null;
  }

  // =============================================================================
  // Reactive State
  // =============================================================================

  get data(): { user: User | null; isAuthenticated: boolean } | null {
    return this._data;
  }

  get loading(): boolean {
    return this._loading;
  }

  get error(): GreyError | null {
    return this._error;
  }

  get isAuthenticated(): boolean {
    return this._data?.isAuthenticated ?? false;
  }

  getClient(): GreyClient {
    return this.coreController.getClient();
  }

  // =============================================================================
  // Domain Actions
  // =============================================================================

  async login(email: string, password: string): Promise<boolean> {
    this._loading = true;
    this._error = null;
    this.host.forceUpdate();

    try {
      const result = await this.coreController.login(email, password);
      return result;
    } catch (err) {
      this._error = normalizeError(err);
      this.host.forceUpdate();
      return false;
    } finally {
      this._loading = false;
      this.host.forceUpdate();
    }
  }

  async logout(): Promise<void> {
    this._loading = true;
    this._error = null;
    this.host.forceUpdate();

    try {
      this.coreController.logout();
    } catch (err) {
      this._error = normalizeError(err);
    } finally {
      this._loading = false;
      this.host.forceUpdate();
    }
  }

  async refresh(): Promise<boolean> {
    this._loading = true;
    this._error = null;
    this.host.forceUpdate();

    try {
      const result = await this.coreController.restoreSession();
      return result;
    } catch (err) {
      this._error = normalizeError(err);
      this.host.forceUpdate();
      return false;
    } finally {
      this._loading = false;
      this.host.forceUpdate();
    }
  }
}
