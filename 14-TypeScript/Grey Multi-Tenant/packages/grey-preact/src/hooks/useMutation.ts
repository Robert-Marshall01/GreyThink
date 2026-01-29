/**
 * useMutation - Preact hook for generic mutations
 *
 * Wraps @grey/adapters MutationController using Preact hooks.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useEffect, useCallback } from 'preact/hooks';
import { MutationController, type MutationOptions, type MutationState } from '@grey/adapters';
import type { GreyError } from './useAuth';

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
// Hook Result
// =============================================================================

export interface UseMutationResult<TData, TVariables> {
  data: TData | null;
  loading: boolean;
  error: GreyError | null;
  executeMutation: (variables: TVariables) => Promise<TData | null>;
  reset: () => void;
}

// =============================================================================
// useMutation Hook
// =============================================================================

export function useMutation<TData, TVariables>(
  options: MutationOptions<TData, TVariables>
): UseMutationResult<TData, TVariables> {
  const [data, setData] = useState<TData | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);
  const [controller, setController] = useState<MutationController<TData, TVariables> | null>(null);

  // Initialize controller on mount
  useEffect(() => {
    const mutationController = new MutationController(options);
    setController(mutationController);

    const unsubscribe = mutationController.subscribe((state: MutationState<TData>) => {
      setData(state.data);
      setLoading(state.isLoading);
      setError(state.error ? normalizeError(state.error) : null);
    });

    return () => {
      unsubscribe();
    };
  }, []);

  const executeMutation = useCallback(async (variables: TVariables): Promise<TData | null> => {
    if (!controller) {
      throw new Error('Mutation controller not initialized');
    }

    setLoading(true);
    setError(null);

    try {
      const result = await controller.mutate(variables);
      return result;
    } catch (err) {
      setError(normalizeError(err));
      return null;
    } finally {
      setLoading(false);
    }
  }, [controller]);

  const reset = useCallback((): void => {
    if (controller) {
      controller.reset();
    }
    setData(null);
    setLoading(false);
    setError(null);
  }, [controller]);

  return {
    data,
    loading,
    error,
    executeMutation,
    reset,
  };
}
