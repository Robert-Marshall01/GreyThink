/**
 * QueryService - Angular service for generic queries
 *
 * Wraps @grey/adapters QueryController using Angular signals.
 * Exposes: data, loading, error, and domain actions.
 */

import { Injectable, signal } from '@angular/core';
import { QueryController, type QueryOptions, type QueryState } from '@grey/adapters';
import { type GreyError } from './auth.service';

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
// QueryResult Interface
// =============================================================================

export interface QueryResult<T> {
  /** Current data */
  readonly data: typeof signal<T | null>;
  /** Loading state */
  readonly loading: typeof signal<boolean>;
  /** Error state */
  readonly error: typeof signal<GreyError | null>;
  /** Execute the query */
  executeQuery: () => Promise<T | null>;
  /** Destroy and cleanup */
  destroy: () => void;
}

// =============================================================================
// QueryService
// =============================================================================

@Injectable({ providedIn: 'root' })
export class QueryService {
  /**
   * Create a query instance with signals.
   *
   * @example
   * ```typescript
   * private query = this.queryService.createQuery({
   *   queryFn: () => this.authService.getClient().users.me(),
   * });
   *
   * readonly data = this.query.data;
   * readonly loading = this.query.loading;
   * readonly error = this.query.error;
   *
   * ngOnInit() {
   *   this.query.executeQuery();
   * }
   *
   * ngOnDestroy() {
   *   this.query.destroy();
   * }
   * ```
   */
  createQuery<T>(options: QueryOptions<T>): {
    data: ReturnType<typeof signal<T | null>>;
    loading: ReturnType<typeof signal<boolean>>;
    error: ReturnType<typeof signal<GreyError | null>>;
    executeQuery: () => Promise<T | null>;
    destroy: () => void;
  } {
    const controller = new QueryController(options);

    const _data = signal<T | null>(null);
    const _loading = signal<boolean>(false);
    const _error = signal<GreyError | null>(null);

    const unsubscribe = controller.subscribe((state: QueryState<T>) => {
      _data.set(state.data);
      _loading.set(state.isLoading);
      _error.set(state.error ? normalizeError(state.error) : null);
    });

    const executeQuery = async (): Promise<T | null> => {
      _loading.set(true);
      _error.set(null);

      try {
        const result = await controller.execute();
        return result;
      } catch (err) {
        _error.set(normalizeError(err));
        return null;
      } finally {
        _loading.set(false);
      }
    };

    return {
      data: _data,
      loading: _loading,
      error: _error,
      executeQuery,
      destroy: () => unsubscribe(),
    };
  }
}
