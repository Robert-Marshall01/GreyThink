/**
 * Grey Solid - Mutation Signal
 *
 * Generic mutation signal for data mutations with optimistic updates.
 * Similar to TanStack Query patterns but SolidJS-native.
 *
 * @example
 * ```tsx
 * import { createMutationSignal, useClient } from '@grey/solid';
 *
 * function CreateProjectForm() {
 *   const client = useClient();
 *
 *   const createMutation = createMutationSignal({
 *     mutationFn: async (data: { name: string }) => {
 *       const result = await client.projects.create(data);
 *       return result.data!;
 *     },
 *     onSuccess: () => {
 *       // Invalidate projects cache
 *       invalidateQueries('projects');
 *     },
 *   });
 *
 *   return (
 *     <form onSubmit={(e) => {
 *       e.preventDefault();
 *       createMutation.mutate({ name: 'New Project' });
 *     }}>
 *       <button type="submit" disabled={createMutation.isLoading()}>
 *         {createMutation.isLoading() ? 'Creating...' : 'Create'}
 *       </button>
 *       <Show when={createMutation.error()}>
 *         <p class="error">{createMutation.error()?.message}</p>
 *       </Show>
 *     </form>
 *   );
 * }
 * ```
 */

import { createSignal } from 'solid-js';
import { MutationController } from '@grey/adapters';
import { useGreyContext } from '../context/index.js';
import type { MutationOptions, MutationState, MutationSignal } from '../types/index.js';

/**
 * Create a mutation signal for data mutations
 *
 * @param options - Mutation options
 * @returns Mutation signal with state accessors and actions
 *
 * @example
 * ```tsx
 * const updateUser = createMutationSignal({
 *   mutationFn: (data: { name: string }) =>
 *     client.users.update(userId, data).then(r => r.data!),
 *   onSuccess: (data) => {
 *     console.log('Updated:', data);
 *     invalidateQueries(['user', userId]);
 *   },
 *   onError: (error) => {
 *     console.error('Failed:', error);
 *   },
 * });
 *
 * // Fire-and-forget
 * updateUser.mutate({ name: 'New Name' });
 *
 * // Or await the result
 * const result = await updateUser.mutateAsync({ name: 'New Name' });
 * ```
 */
export function createMutationSignal<TData, TVariables>(
  options: MutationOptions<TData, TVariables>
): MutationSignal<TData, TVariables> {
  const context = useGreyContext();

  // Initial state
  const initialState: MutationState<TData> = {
    data: null,
    isLoading: false,
    isError: false,
    error: null,
    isSuccess: false,
    isIdle: true,
  };

  const [state, setState] = createSignal<MutationState<TData>>(initialState);

  // Track current variables for callbacks
  let currentVariables: TVariables | null = null;

  /**
   * Execute the mutation and return a promise
   */
  async function mutateAsync(variables: TVariables): Promise<TData> {
    currentVariables = variables;

    // Call onMutate callback
    options.onMutate?.(variables);

    setState({
      data: null,
      isLoading: true,
      isError: false,
      error: null,
      isSuccess: false,
      isIdle: false,
    });

    try {
      const result = await options.mutationFn(variables);

      setState({
        data: result,
        isLoading: false,
        isError: false,
        error: null,
        isSuccess: true,
        isIdle: false,
      });

      // Call onSuccess callback
      options.onSuccess?.(result, variables);

      // Invalidate queries if specified
      if (options.invalidateKeys) {
        context.invalidateQueries(options.invalidateKeys);
      }

      // Call onSettled callback
      options.onSettled?.(result, null, variables);

      return result;
    } catch (err) {
      const error = err instanceof Error ? err : new Error(String(err));

      setState({
        data: null,
        isLoading: false,
        isError: true,
        error,
        isSuccess: false,
        isIdle: false,
      });

      // Call onError callback
      options.onError?.(error, variables);

      // Call onSettled callback
      options.onSettled?.(null, error, variables);

      throw error;
    }
  }

  /**
   * Execute the mutation (fire-and-forget)
   */
  function mutate(variables: TVariables): void {
    mutateAsync(variables).catch(() => {
      // Error is already handled and stored in state
    });
  }

  /**
   * Reset mutation state to initial
   */
  function reset(): void {
    setState(initialState);
    currentVariables = null;
  }

  // Derived accessors
  const data = () => state().data;
  const isLoading = () => state().isLoading;
  const isError = () => state().isError;
  const error = () => state().error;
  const isSuccess = () => state().isSuccess;

  return {
    state,
    data,
    isLoading,
    isError,
    error,
    isSuccess,
    mutate,
    mutateAsync,
    reset,
  };
}
