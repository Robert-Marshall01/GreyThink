/**
 * MutationService - Angular service for generic mutations
 *
 * Wraps @grey/adapters MutationController using Angular signals.
 * Exposes: data, loading, error, and domain actions.
 */

import { Injectable, signal } from '@angular/core';
import { MutationController, type MutationOptions, type MutationState } from '@grey/adapters';
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
// MutationService
// =============================================================================

@Injectable({ providedIn: 'root' })
export class MutationService {
  /**
   * Create a mutation instance with signals.
   *
   * @example
   * ```typescript
   * private mutation = this.mutationService.createMutation({
   *   mutationFn: (data: CreateProjectInput) =>
   *     this.authService.getClient().projects.create(data),
   *   onSuccess: () => this.projectsService.listProjects(),
   * });
   *
   * readonly data = this.mutation.data;
   * readonly loading = this.mutation.loading;
   * readonly error = this.mutation.error;
   *
   * async createProject(input: CreateProjectInput) {
   *   await this.mutation.executeMutation(input);
   * }
   *
   * ngOnDestroy() {
   *   this.mutation.destroy();
   * }
   * ```
   */
  createMutation<TData, TVariables>(options: MutationOptions<TData, TVariables>): {
    data: ReturnType<typeof signal<TData | null>>;
    loading: ReturnType<typeof signal<boolean>>;
    error: ReturnType<typeof signal<GreyError | null>>;
    executeMutation: (variables: TVariables) => Promise<TData | null>;
    reset: () => void;
    destroy: () => void;
  } {
    const controller = new MutationController(options);

    const _data = signal<TData | null>(null);
    const _loading = signal<boolean>(false);
    const _error = signal<GreyError | null>(null);

    const unsubscribe = controller.subscribe((state: MutationState<TData>) => {
      _data.set(state.data);
      _loading.set(state.isLoading);
      _error.set(state.error ? normalizeError(state.error) : null);
    });

    const executeMutation = async (variables: TVariables): Promise<TData | null> => {
      _loading.set(true);
      _error.set(null);

      try {
        const result = await controller.mutate(variables);
        return result;
      } catch (err) {
        _error.set(normalizeError(err));
        return null;
      } finally {
        _loading.set(false);
      }
    };

    const reset = (): void => {
      controller.reset();
      _data.set(null);
      _loading.set(false);
      _error.set(null);
    };

    return {
      data: _data,
      loading: _loading,
      error: _error,
      executeMutation,
      reset,
      destroy: () => unsubscribe(),
    };
  }
}
