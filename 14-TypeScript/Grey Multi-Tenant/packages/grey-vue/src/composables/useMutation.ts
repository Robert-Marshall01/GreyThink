/**
 * Grey Vue - useMutation Composable
 *
 * Generic mutation composable with loading and error states.
 * Wraps the MutationController from @grey/adapters.
 */

import { ref, onMounted, onUnmounted, type Ref } from 'vue';
import {
  MutationController,
  type MutationState as CoreMutationState,
  type MutationOptions as CoreMutationOptions,
} from '@grey/adapters';

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
 * Mutation composable options
 */
export interface UseMutationOptions<TData, TVariables> {
  mutationFn: (variables: TVariables) => Promise<TData>;
  onSuccess?: (data: TData, variables: TVariables) => void;
  onError?: (error: Error, variables: TVariables) => void;
  onSettled?: (data: TData | null, error: Error | null, variables: TVariables) => void;
}

/**
 * Mutation composable return type
 */
export interface UseMutationReturn<TData, TVariables> {
  data: Ref<TData | null>;
  loading: Ref<boolean>;
  error: Ref<GreyError | null>;
  executeMutation: (variables: TVariables) => Promise<TData | null>;
}

/**
 * Normalize any error into the standard error shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return {
    message: 'An unknown error occurred',
    raw: err,
  };
}

/**
 * useMutation Composable
 *
 * Perform data mutations with loading states and callbacks.
 *
 * @param options - Mutation options including mutationFn
 */
export function useMutation<TData, TVariables = void>(
  options: UseMutationOptions<TData, TVariables>
): UseMutationReturn<TData, TVariables> {
  const { mutationFn, onSuccess, onError, onSettled } = options;

  const controllerRef = ref<MutationController<TData, TVariables> | null>(null);
  const data = ref<TData | null>(null) as Ref<TData | null>;
  const loading = ref(false);
  const error = ref<GreyError | null>(null);

  let unsubscribe: (() => void) | undefined;

  onMounted(() => {
    const coreOptions: CoreMutationOptions<TData, TVariables> = {
      mutationFn,
      onSuccess,
      onError,
      onSettled,
    };

    const controller = new MutationController(coreOptions);
    controllerRef.value = controller;

    unsubscribe = controller.subscribe((state: CoreMutationState<TData>) => {
      data.value = state.data;
      loading.value = state.isLoading;
      if (state.error) {
        error.value = normalizeError(state.error);
      } else {
        error.value = null;
      }
    });
  });

  onUnmounted(() => {
    unsubscribe?.();
  });

  /**
   * Execute the mutation
   */
  async function executeMutation(variables: TVariables): Promise<TData | null> {
    const controller = controllerRef.value;
    if (!controller) {
      error.value = { message: 'Mutation controller not initialized' };
      return null;
    }

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
  }

  return {
    data,
    loading,
    error,
    executeMutation,
  };
}
