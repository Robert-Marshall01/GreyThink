/**
 * UserController - Lit Reactive Controller for user management
 *
 * Wraps @grey/adapters UserController using Lit ReactiveController pattern.
 * Exposes: data, loading, error, and domain actions.
 */

import type { ReactiveController, ReactiveControllerHost } from 'lit';
import { UserController as CoreUserController, type UserState } from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';
import type { GreyError } from './auth-controller';

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
// UserController
// =============================================================================

export class UserController implements ReactiveController {
  private host: ReactiveControllerHost;
  private coreController: CoreUserController;
  private unsubscribe: (() => void) | null = null;

  // Reactive state
  private _data: User | null = null;
  private _loading: boolean = false;
  private _error: GreyError | null = null;

  constructor(host: ReactiveControllerHost, client: GreyClient) {
    this.host = host;
    host.addController(this);

    this.coreController = new CoreUserController(client);
  }

  hostConnected(): void {
    this.unsubscribe = this.coreController.subscribe((state: UserState) => {
      this._data = state.user;
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

  // =============================================================================
  // Domain Actions
  // =============================================================================

  async fetchUser(): Promise<void> {
    this._loading = true;
    this._error = null;
    this.host.requestUpdate();

    try {
      await this.coreController.fetchCurrentUser();
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
