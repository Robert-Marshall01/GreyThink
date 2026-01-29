'use client';

/**
 * useUser - Next.js Client Hook for user operations
 *
 * Wraps server actions from /server/user.ts.
 * Exposes: data, loading, error, and domain actions.
 */

import { useState, useCallback } from 'react';
import { fetchUser as fetchUserAction } from '../server/user.js';
import type { GreyError } from '../server/auth.js';
import type { GreyClient, User } from '@grey/core-client';

// =============================================================================
// Types
// =============================================================================

export interface UseUserReturn {
  data: User | null;
  loading: boolean;
  error: GreyError | null;
  fetchUser: (client: GreyClient) => Promise<User | null>;
}

// =============================================================================
// Hook
// =============================================================================

export function useUser(): UseUserReturn {
  const [data, setData] = useState<User | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<GreyError | null>(null);

  const fetchUser = useCallback(async (client: GreyClient): Promise<User | null> => {
    setLoading(true);
    setError(null);

    try {
      const result = await fetchUserAction(client);

      if (result.error) {
        setError(result.error);
        return null;
      }

      setData(result.user);
      return result.user;
    } catch (err) {
      setError({
        message: err instanceof Error ? err.message : 'Fetch user failed',
        raw: err,
      });
      return null;
    } finally {
      setLoading(false);
    }
  }, []);

  return {
    data,
    loading,
    error,
    fetchUser,
  };
}
