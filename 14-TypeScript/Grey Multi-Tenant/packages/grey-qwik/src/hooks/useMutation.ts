/**
 * Grey Qwik - useMutation Hook
 *
 * Qwik hook for generic data mutations.
 * Wraps MutationController from @grey/adapters.
 * Exposes: data, loading, error, executeMutation.
 */

import { useSignal, useVisibleTask$, $ } from '@builder.io/qwik';
import type { Signal, QRL } from '@builder.io/qwik';
import { MutationController, type MutationOptions, type MutationState } from '@grey/adapters';

/**
 * Normalized error shape
 */
export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

/**
 * Mutation hook options
 */
export interface UseMutationOptions<TData, TVariables> {
  /** Async function that performs the mutation */
  mutationFn: (variables: TVariables) => Promise<TData>;
  /** Called on success */
  onSuccess?: (data: TData, variables: TVariables) => void;
  /** Called on error */
  onError?: (error: Error, variables: TVariables) => void;
  /** Called when mutation settles */
  onSettled?: (data: TData | null, error: Error | null, variables: TVariables) => void;
}

/**
 * useMutation hook return type
 */
export interface UseMutationReturn<TData, TVariables> {
  /** Mutation result data */
  data: Signal<TData | null>;
  /** Loading state */
  loading: Signal<boolean>;
  /** Error state */
  error: Signal<GreyError | null>;
  /** Execute the mutation */
  executeMutation: QRL<(variables: TVariables) => Promise<TData | null>>;
}

/**
 * Normalize error into standard shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return { message: err.message, raw: err };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

/**
 * useMutation hook
 *
 * @param options - Mutation options
 * @returns Mutation hook with data, loading, error, and executeMutation action
 */
export function useMutation<TData, TVariables>(
  options: UseMutationOptions<TData, TVariables>
): UseMutationReturn<TData, TVariables> {
  const controller = new MutationController<TData, TVariables>({
    mutationFn: options.mutationFn,
    onSuccess: options.onSuccess,
    onError: options.onError,
    onSettled: options.onSettled,
  });

  // Create signals
  const data = useSignal<TData | null>(null);
  const loading = useSignal(false);
  const error = useSignal<GreyError | null>(null);

  // Subscribe to controller state changes (client-side only)
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    const unsubscribe = controller.subscribe((state: MutationState<TData>) => {
      data.value = state.data;
      loading.value = state.isLoading;
      if (state.error) {
        error.value = normalizeError(state.error);
      } else {
        error.value = null;
      }
    });

    cleanup(() => {
      unsubscribe();
    });
  });

  /**
   * Execute the mutation
   */
  const executeMutation = $(async (variables: TVariables): Promise<TData | null> => {
    loading.value = true;
    error.value = null;
    try {
      const result = await controller.mutate(variables);
      loading.value = false;
      return result;
    } catch (err) {
      error.value = normalizeError(err);
      loading.value = false;
      return null;
    }
  });

  return {
    data,
    loading,
    error,
    executeMutation,
  };
}

export type { MutationOptions, MutationState };
