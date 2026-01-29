/**
 * Grey Qwik - useMutation Composable
 *
 * Generic mutation composable for data mutations with optimistic updates.
 * Similar to TanStack Query patterns but Qwik-native.
 *
 * @example
 * ```tsx
 * import { component$ } from '@builder.io/qwik';
 * import { useMutation, useClient, invalidateQueries } from '@grey/qwik';
 *
 * export default component$(() => {
 *   const client = useClient();
 *
 *   const createMutation = useMutation({
 *     mutationFn: $(async (data: { name: string }) => {
 *       const result = await client.projects.create(data);
 *       return result.data!;
 *     }),
 *     onSuccess$: $(() => {
 *       invalidateQueries('projects');
 *     }),
 *   });
 *
 *   return (
 *     <form preventdefault:submit onSubmit$={() => {
 *       createMutation.mutate$({ name: 'New Project' });
 *     }}>
 *       <button type="submit" disabled={createMutation.isLoading.value}>
 *         {createMutation.isLoading.value ? 'Creating...' : 'Create'}
 *       </button>
 *       {createMutation.error.value && (
 *         <p class="error">{createMutation.error.value.message}</p>
 *       )}
 *     </form>
 *   );
 * });
 * ```
 */

import { useSignal, $ } from '@builder.io/qwik';
import { useGreyContext } from '../context/index.js';
import type { MutationOptions, MutationState, MutationComposable } from '../types/index.js';

/**
 * useMutation Composable
 *
 * Create a mutation composable for data mutations.
 *
 * @param options - Mutation options
 * @returns Mutation composable with state signals and actions
 *
 * @example
 * ```tsx
 * const updateUser = useMutation({
 *   mutationFn: $((data: { name: string }) =>
 *     client.users.update(userId, data).then(r => r.data!)),
 *   onSuccess$: $((data) => {
 *     console.log('Updated:', data);
 *     invalidateQueries(['user', userId]);
 *   }),
 *   onError$: $((error) => {
 *     console.error('Failed:', error);
 *   }),
 * });
 *
 * // Fire-and-forget
 * updateUser.mutate$({ name: 'New Name' });
 *
 * // Or await the result
 * const result = await updateUser.mutateAsync$({ name: 'New Name' });
 * ```
 */
export function useMutation<TData, TVariables>(
  options: MutationOptions<TData, TVariables>
): MutationComposable<TData, TVariables> {
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

  const state = useSignal<MutationState<TData>>(initialState);
  const data = useSignal<TData | null>(null);
  const isLoading = useSignal(false);
  const isError = useSignal(false);
  const error = useSignal<Error | null>(null);
  const isSuccess = useSignal(false);

  /**
   * Execute the mutation and return a promise
   */
  const mutateAsync$ = $(async (variables: TVariables): Promise<TData> => {
    // Call onMutate callback
    if (options.onMutate$) {
      await options.onMutate$(variables);
    }

    // Update state to loading
    isLoading.value = true;
    isError.value = false;
    error.value = null;
    isSuccess.value = false;
    data.value = null;

    state.value = {
      data: null,
      isLoading: true,
      isError: false,
      error: null,
      isSuccess: false,
      isIdle: false,
    };

    try {
      const result = await options.mutationFn(variables);

      // Update state to success
      data.value = result;
      isLoading.value = false;
      isSuccess.value = true;
      isError.value = false;
      error.value = null;

      state.value = {
        data: result,
        isLoading: false,
        isError: false,
        error: null,
        isSuccess: true,
        isIdle: false,
      };

      // Call onSuccess callback
      if (options.onSuccess$) {
        await options.onSuccess$(result, variables);
      }

      // Invalidate queries if specified
      if (options.invalidateKeys) {
        context.invalidateQueries(options.invalidateKeys);
      }

      // Call onSettled callback
      if (options.onSettled$) {
        await options.onSettled$(result, null, variables);
      }

      return result;
    } catch (err) {
      const errorObj = err instanceof Error ? err : new Error(String(err));

      // Update state to error
      isLoading.value = false;
      isError.value = true;
      error.value = errorObj;
      isSuccess.value = false;

      state.value = {
        data: null,
        isLoading: false,
        isError: true,
        error: errorObj,
        isSuccess: false,
        isIdle: false,
      };

      // Call onError callback
      if (options.onError$) {
        await options.onError$(errorObj, variables);
      }

      // Call onSettled callback
      if (options.onSettled$) {
        await options.onSettled$(null, errorObj, variables);
      }

      throw errorObj;
    }
  });

  /**
   * Execute the mutation (fire-and-forget)
   */
  const mutate$ = $(async (variables: TVariables): Promise<void> => {
    try {
      await mutateAsync$(variables);
    } catch {
      // Error is already handled and stored in state
    }
  });

  /**
   * Reset mutation state to initial
   */
  const reset$ = $(() => {
    data.value = null;
    isLoading.value = false;
    isError.value = false;
    error.value = null;
    isSuccess.value = false;

    state.value = initialState;
  });

  return {
    state,
    data,
    isLoading,
    isError,
    error,
    isSuccess,
    mutate$,
    mutateAsync$,
    reset$,
  };
}
