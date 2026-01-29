/**
 * useUser - Preact hook for user management
 *
 * Wraps @grey/adapters UserController using Preact hooks.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useEffect, useCallback } from 'preact/hooks';
import { UserController, type UserState } from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';
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

export interface UseUserResult {
  data: User | null;
  loading: boolean;
  error: GreyError | null;
  fetchUser: () => Promise<void>;
}

// =============================================================================
// useUser Hook
// =============================================================================

export function useUser(client: GreyClient): UseUserResult {
  const [data, setData] = useState<User | null>(null);
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<GreyError | null>(null);
  const [controller, setController] = useState<UserController | null>(null);

  // Initialize controller on mount
  useEffect(() => {
    const userController = new UserController(client);
    setController(userController);

    const unsubscribe = userController.subscribe((state: UserState) => {
      setData(state.user);
      setLoading(state.isLoading);
      setError(state.error ? normalizeError(state.error) : null);
    });

    return () => {
      unsubscribe();
    };
  }, [client]);

  const fetchUser = useCallback(async (): Promise<void> => {
    if (!controller) {
      throw new Error('User controller not initialized');
    }

    setLoading(true);
    setError(null);

    try {
      await controller.fetchCurrentUser();
    } catch (err) {
      setError(normalizeError(err));
      throw err;
    } finally {
      setLoading(false);
    }
  }, [controller]);

  return {
    data,
    loading,
    error,
    fetchUser,
  };
}
