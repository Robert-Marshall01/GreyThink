/**
 * AuthController - Lit Reactive Controller for authentication
 *
 * Wraps @grey/adapters AuthController using Lit ReactiveController pattern.
 * Exposes: data, loading, error, and domain actions.
 */

import type { ReactiveController, ReactiveControllerHost } from 'lit';
import {
  AuthController as CoreAuthController,
  BrowserTokenStorage,
  MemoryTokenStorage,
  type AuthState,
  type AuthConfig,
} from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';

// =============================================================================
// Error Shape
// =============================================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

// =============================================================================
// SSR Safety
// =============================================================================

function isBrowser(): boolean {
  return typeof window !== 'undefined' && typeof document !== 'undefined';
}

function createPlatformStorage(): BrowserTokenStorage | MemoryTokenStorage {
  return isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage();
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
// Controller Options
// =============================================================================

export interface AuthControllerOptions {
  baseUrl: string;
  onAuthChange?: (user: User | null) => void;
  onLogout?: () => void;
}

// =============================================================================
// AuthController
// =============================================================================

export class AuthController implements ReactiveController {
  private host: ReactiveControllerHost;
  private coreController: CoreAuthController;
  private unsubscribe: (() => void) | null = null;

  // Reactive state
  private _data: User | null = null;
  private _loading: boolean = false;
  private _error: GreyError | null = null;
  private _isAuthenticated: boolean = false;

  constructor(host: ReactiveControllerHost, options: AuthControllerOptions) {
    this.host = host;
    host.addController(this);

    const storage = createPlatformStorage();

    const config: AuthConfig = {
      apiBaseUrl: options.baseUrl,
      storage,
      onAuthChange: (state: AuthState) => {
        this._data = state.user;
        this._isAuthenticated = state.isAuthenticated;
        this._loading = state.isLoading;
        this._error = state.error ? normalizeError(state.error) : null;
        this.host.requestUpdate();
        options.onAuthChange?.(state.user);
      },
      onLogout: options.onLogout,
    };

    this.coreController = new CoreAuthController(config);
  }

  hostConnected(): void {
    this.unsubscribe = this.coreController.subscribe((state: AuthState) => {
      this._data = state.user;
      this._isAuthenticated = state.isAuthenticated;
      this._loading = state.isLoading;
      this._error = state.error ? normalizeError(state.error) : null;
      this.host.requestUpdate();
    });
  }

  hostDisconnected(): void {
    this.unsubscribe?.();
    this.unsubscribe = null;
  }

  // =============================================================================
  // Reactive State
  // =============================================================================

  get data(): User | null {
    return this._data;
  }

  get loading(): boolean {
    return this._loading;
  }

  get error(): GreyError | null {
    return this._error;
  }

  get isAuthenticated(): boolean {
    return this._isAuthenticated;
  }

  // =============================================================================
  // Client Access
  // =============================================================================

  getClient(): GreyClient {
    return this.coreController.getClient();
  }

  // =============================================================================
  // Domain Actions
  // =============================================================================

  async login(email: string, password: string): Promise<void> {
    this._loading = true;
    this._error = null;
    this.host.requestUpdate();

    try {
      await this.coreController.login(email, password);
    } catch (err) {
      this._error = normalizeError(err);
      this.host.requestUpdate();
      throw err;
    } finally {
      this._loading = false;
      this.host.requestUpdate();
    }
  }

  async logout(): Promise<void> {
    this._loading = true;
    this._error = null;
    this.host.requestUpdate();

    try {
      this.coreController.logout();
    } catch (err) {
      this._error = normalizeError(err);
      this.host.requestUpdate();
      throw err;
    } finally {
      this._loading = false;
      this.host.requestUpdate();
    }
  }

  async refresh(): Promise<void> {
    this._loading = true;
    this._error = null;
    this.host.requestUpdate();

    try {
      await this.coreController.restoreSession();
    } catch (err) {
      this._error = normalizeError(err);
      this.host.requestUpdate();
      throw err;
    } finally {
      this._loading = false;
      this.host.requestUpdate();
    }
  }
}
