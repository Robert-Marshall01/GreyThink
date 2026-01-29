/**
 * Grey Astro - User Store
 *
 * Client-side user store for Astro islands.
 * Uses nanostores for framework-agnostic reactive state.
 * Calls server endpoints via fetch.
 *
 * Exposes: data, loading, error, fetchUser()
 */

import { atom, computed } from 'nanostores';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface UserStoreState {
  data: unknown | null;
  loading: boolean;
  error: GreyError | null;
}

export interface UserStoreOptions {
  userEndpoint?: string;
}

// ============================================================
// Error Normalization
// ============================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    const error = err as Error & { code?: string; status?: number };
    return {
      message: error.message,
      code: error.code,
      status: error.status,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err, raw: err };
  }
  return { message: 'An unknown error occurred', raw: err };
}

// ============================================================
// Store Factory
// ============================================================

export function createUserStore(options: UserStoreOptions = {}) {
  const { userEndpoint = '/api/user' } = options;

  // Atoms
  const $data = atom<unknown | null>(null);
  const $loading = atom<boolean>(false);
  const $error = atom<GreyError | null>(null);

  // Computed state
  const $state = computed(
    [$data, $loading, $error],
    (data: unknown | null, loading: boolean, error: GreyError | null) => ({
      data,
      loading,
      error,
    })
  );

  // Actions
  async function fetchUser(): Promise<void> {
    $loading.set(true);
    $error.set(null);

    try {
      const response = await fetch(userEndpoint, {
        method: 'GET',
        headers: { 'Content-Type': 'application/json' },
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        $error.set(result.error ?? { message: 'Failed to fetch user', status: response.status });
        $data.set(null);
        return;
      }

      $data.set(result.data);
    } catch (err) {
      $error.set(normalizeError(err));
      $data.set(null);
    } finally {
      $loading.set(false);
    }
  }

  return {
    // Stores
    $data,
    $loading,
    $error,
    $state,
    // Actions
    fetchUser,
  };
}

// Default instance
export const userStore = createUserStore();
