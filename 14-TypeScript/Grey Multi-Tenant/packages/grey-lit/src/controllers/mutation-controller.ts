/**
 * MutationController - Lit Reactive Controller for generic mutations
 *
 * Wraps @grey/adapters MutationController using Lit ReactiveController pattern.
 * Exposes: data, loading, error, and domain actions.
 */

import type { ReactiveController, ReactiveControllerHost } from 'lit';
import {
  MutationController as CoreMutationController,
  type MutationOptions,
  type MutationState,
} from '@grey/adapters';
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
// MutationController
// =============================================================================

export class MutationController<TData, TVariables> implements ReactiveController {
  private host: ReactiveControllerHost;
  private coreController: CoreMutationController<TData, TVariables>;
  private unsubscribe: (() => void) | null = null;

  // Reactive state
  private _data: TData | null = null;
  private _loading: boolean = false;
  private _error: GreyError | null = null;

  constructor(host: ReactiveControllerHost, options: MutationOptions<TData, TVariables>) {
    this.host = host;
    host.addController(this);

    this.coreController = new CoreMutationController<TData, TVariables>(options);
  }

  hostConnected(): void {
    this.unsubscribe = this.coreController.subscribe((state: MutationState<TData>) => {
      this._data = state.data;
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

  get data(): TData | null {
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

  async executeMutation(variables: TVariables): Promise<TData | null> {
    this._loading = true;
    this._error = null;
    this.host.requestUpdate();

    try {
      const result = await this.coreController.mutate(variables);
      return result;
    } catch (err) {
      this._error = normalizeError(err);
      this.host.requestUpdate();
      return null;
    } finally {
      this._loading = false;
      this.host.requestUpdate();
    }
  }

  reset(): void {
    this.coreController.reset();
    this._data = null;
    this._loading = false;
    this._error = null;
    this.host.requestUpdate();
  }
}
