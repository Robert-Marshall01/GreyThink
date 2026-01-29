/**
 * UserController - Stencil Controller for user state
 *
 * Wraps @grey/adapters UserController for Stencil integration.
 * Exposes: data, loading, error, and domain actions.
 */

import { UserController as CoreUserController, type UserState } from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';
import type { GreyError, StencilHost } from './auth-controller';

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

export class UserController {
  private host: StencilHost;
  private coreController: CoreUserController;
  private unsubscribe: (() => void) | null = null;

  // Reactive state
  private _data: User | null = null;
  private _loading: boolean = false;
  private _error: GreyError | null = null;

  constructor(host: StencilHost, client: GreyClient) {
    this.host = host;
    this.coreController = new CoreUserController(client);
  }

  connect(): void {
    this.unsubscribe = this.coreController.subscribe((state: UserState) => {
      this._data = state.user;
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

  async fetchUser(): Promise<User | null> {
    this._loading = true;
    this._error = null;
    this.host.forceUpdate();

    try {
      const result = await this.coreController.fetchCurrentUser();
      return result;
    } catch (err) {
      this._error = normalizeError(err);
      this.host.forceUpdate();
      return null;
    } finally {
      this._loading = false;
      this.host.forceUpdate();
    }
  }
}
