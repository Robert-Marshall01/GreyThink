/**
 * UserService - Angular service for user management
 *
 * Wraps @grey/adapters UserController using Angular signals.
 * Exposes: data, loading, error, and domain actions.
 */

import { Injectable, signal, inject, type OnDestroy } from '@angular/core';
import { UserController, type UserState } from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';
import { AuthService, type GreyError } from './auth.service';

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
// UserService
// =============================================================================

@Injectable({ providedIn: 'root' })
export class UserService implements OnDestroy {
  private controller?: UserController;
  private unsubscribe?: () => void;
  private authService = inject(AuthService);

  // Signals
  private readonly _data = signal<User | null>(null);
  private readonly _loading = signal<boolean>(false);
  private readonly _error = signal<GreyError | null>(null);

  // Public readonly signals
  readonly data = this._data.asReadonly();
  readonly loading = this._loading.asReadonly();
  readonly error = this._error.asReadonly();

  /**
   * Initialize the user service.
   * Called automatically when AuthService is initialized.
   */
  initialize(): void {
    const client = this.authService.getClient();
    this.controller = new UserController(client);

    this.unsubscribe = this.controller.subscribe((state: UserState) => {
      this._data.set(state.user);
      this._loading.set(state.isLoading);
      this._error.set(state.error ? normalizeError(state.error) : null);
    });
  }

  ngOnDestroy(): void {
    this.unsubscribe?.();
  }

  /**
   * Fetch the current user.
   */
  async fetchUser(): Promise<void> {
    if (!this.controller) {
      this.initialize();
    }

    this._loading.set(true);
    this._error.set(null);

    try {
      await this.controller!.fetchCurrentUser();
    } catch (err) {
      this._error.set(normalizeError(err));
      throw err;
    } finally {
      this._loading.set(false);
    }
  }
}
