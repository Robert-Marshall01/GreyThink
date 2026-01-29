/**
 * Grey Capacitor - useMutation Hook
 *
 * Generic mutation hook with loading and error states.
 * Wraps the MutationController from @grey/adapters.
 * Capacitor-safe: no Node APIs, browser APIs allowed.
 */

import { useState, useCallback, useRef, useEffect } from 'react';
import {
  MutationController,
  type MutationState as CoreMutationState,
  type MutationOptions as CoreMutationOptions,
} from '@grey/adapters';

// ============================================================
// Types
// ============================================================

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
  mutationFn: (variables: TVariables) => Promise<TData>;
  onSuccess?: (data: TData, variables: TVariables) => void;
  onError?: (error: Error, variables: TVariables) => void;
  onSettled?: (data: TData | null, error: Error | null, variables: TVariables) => void;
}

/**
 * Mutation hook return type
 */
export interface UseMutationReturn<TData, TVariables> {
  data: TData | null;
  loading: boolean;
  error: GreyError | null;
  executeMutation: (variables: TVariables) => Promise<TData | null>;
}

// ============================================================
// Error Normalization
// ============================================================

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

// ============================================================
// Hook Implementation
// ============================================================

/**
 * useMutation Hook
 *
 * Perform data mutations with loading states and callbacks.
 *
 * @param options - Mutation options including mutationFn
 */
export function useMutation<TData, TVariables = void>(
  options: UseMutationOptions<TData, TVariables>
): UseMutationReturn<TData, TVariables> {
  const { mutationFn, onSuccess, onError, onSettled } = options;

  const controllerRef = useRef<MutationController<TData, TVariables> | null>(null);
  const [data, setData] = useState<TData | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<GreyError | null>(null);

  // Track if component is mounted
  const mountedRef = useRef(true);
  useEffect(() => {
    mountedRef.current = true;
    return () => {
      mountedRef.current = false;
    };
  }, []);

  // Initialize controller
  useEffect(() => {
    const coreOptions: CoreMutationOptions<TData, TVariables> = {
      mutationFn,
      onSuccess,
      onError,
      onSettled,
    };

    const controller = new MutationController(coreOptions);
    controllerRef.current = controller;

    const unsubscribe = controller.subscribe((state: CoreMutationState<TData>) => {
      if (mountedRef.current) {
        setData(state.data);
        setLoading(state.isLoading);
        if (state.error) {
          setError(normalizeError(state.error));
        } else {
          setError(null);
        }
      }
    });

    return () => {
      unsubscribe();
    };
  }, [mutationFn, onSuccess, onError, onSettled]);

  /**
   * Execute the mutation
   */
  const executeMutation = useCallback(async (variables: TVariables): Promise<TData | null> => {
    const controller = controllerRef.current;
    if (!controller) {
      setError({ message: 'Mutation controller not initialized' });
      return null;
    }

    setLoading(true);
    setError(null);

    try {
      const result = await controller.mutate(variables);
      if (mountedRef.current) {
        setLoading(false);
      }
      return result;
    } catch (err) {
      if (mountedRef.current) {
        setError(normalizeError(err));
        setLoading(false);
      }
      return null;
    }
  }, []);

  return {
    data,
    loading,
    error,
    executeMutation,
  };
}
