/**
 * Grey Expo - useUser Hook
 *
 * Provides current user state and actions for Expo components.
 * Wraps the UserController from @grey/adapters.
 * Expo-safe: no DOM, no Node APIs.
 */

import { useState, useEffect, useCallback, useRef } from 'react';
import {
  UserController,
  type UserState as CoreUserState,
  type GreyClient,
  type User,
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
 * User hook return type
 */
export interface UseUserReturn {
  data: User | null;
  loading: boolean;
  error: GreyError | null;
  fetchUser: () => Promise<User | null>;
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
 * useUser Hook
 *
 * Provides current user state and actions.
 * Must be used within a GreyProvider or with a client.
 *
 * @param client - Optional GreyClient for standalone usage
 */
export function useUser(client?: GreyClient): UseUserReturn {
  const controllerRef = useRef<UserController | null>(null);
  const [data, setData] = useState<User | null>(null);
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
    if (!client) return;

    const controller = new UserController(client);
    controllerRef.current = controller;

    const unsubscribe = controller.subscribe((state: CoreUserState) => {
      if (mountedRef.current) {
        setData(state.user);
        setLoading(state.isLoading);
        if (state.error) {
          setError({ message: state.error });
        }
      }
    });

    return () => {
      unsubscribe();
    };
  }, [client]);

  /**
   * Fetch the current user
   */
  const fetchUser = useCallback(async (): Promise<User | null> => {
    const controller = controllerRef.current;
    if (!controller) {
      setError({ message: 'User controller not initialized' });
      return null;
    }

    setLoading(true);
    setError(null);

    try {
      const user = await controller.fetchCurrentUser();
      if (mountedRef.current) {
        setLoading(false);
      }
      return user;
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
    fetchUser,
  };
}
