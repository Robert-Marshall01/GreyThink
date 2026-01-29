/**
 * Grey Svelte - Mutation Store
 *
 * Svelte store for generic data mutations.
 * Wraps MutationController from @grey/adapters.
 * Exposes: data, loading, error, executeMutation.
 */

import { writable, type Readable, type Writable } from 'svelte/store';
import { MutationController, type MutationOptions } from '@grey/adapters';

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
 * Mutation store options
 */
export interface MutationStoreOptions<TData, TVariables> {
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
 * Mutation store return type
 */
export interface MutationStoreReturn<TData, TVariables> {
  /** Mutation result data */
  data: Readable<TData | null>;
  /** Loading state */
  loading: Readable<boolean>;
  /** Error state */
  error: Readable<GreyError | null>;
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
 * Create a mutation store
 *
 * @param options - Mutation options
 * @returns Mutation store with data, loading, error, and executeMutation action
 */
export function createMutationStore<TData, TVariables>(
  options: MutationStoreOptions<TData, TVariables>
): MutationStoreReturn<TData, TVariables> {
  const controller = new MutationController<TData, TVariables>({
    mutationFn: options.mutationFn,
    onSuccess: options.onSuccess,
    onError: options.onError,
    onSettled: options.onSettled,
  });

  // Create writable stores
  const dataStore: Writable<TData | null> = writable(null);
  const loadingStore: Writable<boolean> = writable(false);
  const errorStore: Writable<GreyError | null> = writable(null);

  // Subscribe to controller state changes
  controller.subscribe((state) => {
    dataStore.set(state.data);
    loadingStore.set(state.isLoading);
    if (state.error) {
      errorStore.set(normalizeError(state.error));
    } else {
      errorStore.set(null);
    }
  });

  /**
   * Execute the mutation
   */
  async function executeMutation(variables: TVariables): Promise<TData | null> {
    loadingStore.set(true);
    errorStore.set(null);
    try {
      const result = await controller.mutate(variables);
      loadingStore.set(false);
      return result;
    } catch (err) {
      errorStore.set(normalizeError(err));
      loadingStore.set(false);
      return null;
    }
  }

  return {
    data: { subscribe: dataStore.subscribe },
    loading: { subscribe: loadingStore.subscribe },
    error: { subscribe: errorStore.subscribe },
    executeMutation,
  };
}

export type { MutationOptions };
