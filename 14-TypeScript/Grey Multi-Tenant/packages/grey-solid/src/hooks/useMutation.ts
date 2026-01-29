/**
 * Grey Solid - useMutation Hook
 *
 * Solid hook for generic data mutations.
 * Wraps MutationController from @grey/adapters.
 * Exposes: data, loading, error, executeMutation.
 */

import { createSignal, onMount, onCleanup, type Accessor } from 'solid-js';
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
  data: Accessor<TData | null>;
  /** Loading state */
  loading: Accessor<boolean>;
  /** Error state */
  error: Accessor<GreyError | null>;
  /** Execute the mutation */
  executeMutation: (variables: TVariables) => Promise<TData | null>;
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
  const [data, setData] = createSignal<TData | null>(null);
  const [loading, setLoading] = createSignal(false);
  const [error, setError] = createSignal<GreyError | null>(null);

  // Subscribe to controller state changes
  let unsubscribe: (() => void) | undefined;

  onMount(() => {
    unsubscribe = controller.subscribe((state: MutationState<TData>) => {
      setData(() => state.data);
      setLoading(state.isLoading);
      if (state.error) {
        setError(normalizeError(state.error));
      } else {
        setError(null);
      }
    });
  });

  onCleanup(() => {
    unsubscribe?.();
  });

  /**
   * Execute the mutation
   */
  async function executeMutation(variables: TVariables): Promise<TData | null> {
    setLoading(true);
    setError(null);
    try {
      const result = await controller.mutate(variables);
      setLoading(false);
      return result;
    } catch (err) {
      setError(normalizeError(err));
      setLoading(false);
      return null;
    }
  }

  return {
    data,
    loading,
    error,
    executeMutation,
  };
}

export type { MutationOptions, MutationState };
