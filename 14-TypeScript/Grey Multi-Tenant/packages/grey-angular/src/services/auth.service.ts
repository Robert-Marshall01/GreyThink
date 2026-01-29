/**
 * AuthService - Angular service for authentication
 *
 * Wraps @grey/adapters AuthController using Angular signals.
 * Exposes: data, loading, error, and domain actions.
 */

import { Injectable, signal, computed, inject, type OnDestroy } from '@angular/core';
import {
  AuthController,
  type AuthState,
  type AuthConfig,
  BrowserTokenStorage,
  MemoryTokenStorage,
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
// Configuration
// =============================================================================

export interface AuthServiceConfig {
  baseUrl: string;
  onAuthChange?: (user: User | null) => void;
  onLogout?: () => void;
}

// =============================================================================
// AuthService
// =============================================================================

@Injectable({ providedIn: 'root' })
export class AuthService implements OnDestroy {
  private controller!: AuthController;
  private unsubscribe?: () => void;
  private config?: AuthServiceConfig;

  // Signals
  private readonly _data = signal<User | null>(null);
  private readonly _loading = signal<boolean>(false);
  private readonly _error = signal<GreyError | null>(null);
  private readonly _isAuthenticated = signal<boolean>(false);

  // Public readonly signals
  readonly data = this._data.asReadonly();
  readonly loading = this._loading.asReadonly();
  readonly error = this._error.asReadonly();
  readonly isAuthenticated = this._isAuthenticated.asReadonly();

  /**
   * Initialize the auth service with configuration.
   * Must be called before using the service.
   */
  initialize(config: AuthServiceConfig): void {
    this.config = config;
    const storage = createPlatformStorage();

    this.controller = new AuthController({
      apiBaseUrl: config.baseUrl,
      storage,
      onAuthChange: (state: AuthState) => {
        this._data.set(state.user);
        this._isAuthenticated.set(state.isAuthenticated);
        this._loading.set(state.isLoading);
        this._error.set(state.error ? normalizeError(state.error) : null);
        config.onAuthChange?.(state.user);
      },
      onLogout: config.onLogout,
    });

    this.unsubscribe = this.controller.subscribe((state: AuthState) => {
      this._data.set(state.user);
      this._isAuthenticated.set(state.isAuthenticated);
      this._loading.set(state.isLoading);
      this._error.set(state.error ? normalizeError(state.error) : null);
    });
  }

  ngOnDestroy(): void {
    this.unsubscribe?.();
  }

  /**
   * Get the Grey client instance for use by other services.
   */
  getClient(): GreyClient {
    if (!this.controller) {
      throw new Error('AuthService not initialized. Call initialize() first.');
    }
    return this.controller.getClient();
  }

  /**
   * Login with email and password.
   */
  async login(email: string, password: string): Promise<void> {
    if (!this.controller) {
      throw new Error('AuthService not initialized. Call initialize() first.');
    }

    this._loading.set(true);
    this._error.set(null);

    try {
      await this.controller.login(email, password);
    } catch (err) {
      this._error.set(normalizeError(err));
      throw err;
    } finally {
      this._loading.set(false);
    }
  }

  /**
   * Logout and clear session.
   */
  async logout(): Promise<void> {
    if (!this.controller) {
      throw new Error('AuthService not initialized. Call initialize() first.');
    }

    this._loading.set(true);
    this._error.set(null);

    try {
      this.controller.logout();
    } catch (err) {
      this._error.set(normalizeError(err));
      throw err;
    } finally {
      this._loading.set(false);
    }
  }

  /**
   * Refresh the current session.
   */
  async refresh(): Promise<void> {
    if (!this.controller) {
      throw new Error('AuthService not initialized. Call initialize() first.');
    }

    this._loading.set(true);
    this._error.set(null);

    try {
      await this.controller.restoreSession();
    } catch (err) {
      this._error.set(normalizeError(err));
      throw err;
    } finally {
      this._loading.set(false);
    }
  }
}
