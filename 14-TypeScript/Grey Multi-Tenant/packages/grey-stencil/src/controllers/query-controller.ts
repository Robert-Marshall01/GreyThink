/**
 * QueryController - Stencil Controller for generic queries
 *
 * Wraps @grey/adapters QueryController for Stencil integration.
 * Exposes: data, loading, error, and domain actions.
 */

import {
  QueryController as CoreQueryController,
  type QueryOptions,
  type QueryState,
} from '@grey/adapters';
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
// QueryController
// =============================================================================

export class QueryController<T> {
  private host: StencilHost;
  private coreController: CoreQueryController<T>;
  private unsubscribe: (() => void) | null = null;

  // Reactive state
  private _data: T | null = null;
  private _loading: boolean = false;
  private _error: GreyError | null = null;

  constructor(host: StencilHost, options: QueryOptions<T>) {
    this.host = host;
    this.coreController = new CoreQueryController<T>(options);
  }

  connect(): void {
    this.unsubscribe = this.coreController.subscribe((state: QueryState<T>) => {
      this._data = state.data;
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

  get data(): T | null {
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

  async executeQuery(): Promise<T | null> {
    this._loading = true;
    this._error = null;
    this.host.forceUpdate();

    try {
      const result = await this.coreController.execute();
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
