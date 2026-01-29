/**
 * Grey Astro - Auth Store
 *
 * Client-side authentication store for Astro islands.
 * Uses nanostores for framework-agnostic reactive state.
 * Calls server endpoints via fetch.
 *
 * Exposes: data, loading, error, login(), logout(), refresh()
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

export interface AuthData {
  user: unknown | null;
  session: {
    accessToken: string;
    refreshToken?: string;
    expiresIn?: number;
  } | null;
}

export interface AuthStoreState {
  data: AuthData | null;
  loading: boolean;
  error: GreyError | null;
}

export interface AuthStoreOptions {
  loginEndpoint?: string;
  logoutEndpoint?: string;
  refreshEndpoint?: string;
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

export function createAuthStore(options: AuthStoreOptions = {}) {
  const {
    loginEndpoint = '/api/auth/login',
    logoutEndpoint = '/api/auth/logout',
    refreshEndpoint = '/api/auth/refresh',
  } = options;

  // Atoms
  const $data = atom<AuthData | null>(null);
  const $loading = atom<boolean>(false);
  const $error = atom<GreyError | null>(null);

  // Computed state
  const $state = computed(
    [$data, $loading, $error],
    (data: AuthData | null, loading: boolean, error: GreyError | null) => ({
      data,
      loading,
      error,
    })
  );

  // Actions
  async function login(email: string, password: string): Promise<void> {
    $loading.set(true);
    $error.set(null);

    try {
      const response = await fetch(loginEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ email, password }),
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        $error.set(result.error ?? { message: 'Login failed', status: response.status });
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

  async function logout(): Promise<void> {
    $loading.set(true);
    $error.set(null);

    try {
      const response = await fetch(logoutEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        $error.set(result.error ?? { message: 'Logout failed', status: response.status });
        return;
      }

      $data.set(null);
    } catch (err) {
      $error.set(normalizeError(err));
    } finally {
      $loading.set(false);
    }
  }

  async function refresh(): Promise<void> {
    $loading.set(true);
    $error.set(null);

    try {
      const response = await fetch(refreshEndpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
      });

      const result = await response.json();

      if (!response.ok || result.error) {
        $error.set(result.error ?? { message: 'Refresh failed', status: response.status });
        return;
      }

      $data.set(result.data);
    } catch (err) {
      $error.set(normalizeError(err));
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
    login,
    logout,
    refresh,
  };
}

// Default instance
export const authStore = createAuthStore();
