/**
 * Grey Svelte - Context Management
 *
 * Provides initialization and context utilities for the Grey SDK.
 * Uses a module-level singleton pattern for Svelte's store-based architecture.
 *
 * @example
 * ```ts
 * // In your app's initialization (e.g., +layout.ts or main.ts)
 * import { initGrey } from '@grey/svelte';
 *
 * initGrey({
 *   baseUrl: 'http://localhost:8080/api/v1',
 * });
 * ```
 */

import {
  AuthController,
  UserController,
  ProjectsController,
  BrowserTokenStorage,
  MemoryTokenStorage,
} from '@grey/adapters';
import type { GreyClient } from '@grey/core-client';
import type { GreyConfig, GreyContext } from '../types/index.js';

// =============================================================================
// SSR Safety Utilities
// =============================================================================

/**
 * SSR-safe check for browser environment
 */
export function isBrowser(): boolean {
  return typeof window !== 'undefined' && typeof document !== 'undefined';
}

/**
 * SSR-safe localStorage wrapper
 */
export const safeStorage = {
  getItem(key: string): string | null {
    if (!isBrowser()) return null;
    try {
      return localStorage.getItem(key);
    } catch {
      return null;
    }
  },
  setItem(key: string, value: string): void {
    if (!isBrowser()) return;
    try {
      localStorage.setItem(key, value);
    } catch {
      // Storage may be full or disabled
    }
  },
  removeItem(key: string): void {
    if (!isBrowser()) return;
    try {
      localStorage.removeItem(key);
    } catch {
      // Ignore errors
    }
  },
};

// =============================================================================
// Module-Level Context (Singleton)
// =============================================================================

let _context: GreyContext | null = null;
let _initialized = false;

/**
 * Initialize Grey SDK
 *
 * Must be called once at application startup before using any stores.
 * In SvelteKit, call this in your root +layout.ts or +layout.svelte.
 *
 * @param config - Configuration options
 * @returns The Grey context (for advanced usage)
 *
 * @example
 * ```svelte
 * <script lang="ts">
 * import { onMount } from 'svelte';
 * import { initGrey, authStore } from '@grey/svelte';
 *
 * onMount(() => {
 *   initGrey({
 *     baseUrl: import.meta.env.VITE_GREY_API_URL || 'http://localhost:8080/api/v1',
 *     onLogout: () => {
 *       // Redirect to login
 *       goto('/login');
 *     },
 *   });
 *
 *   // Restore session on mount
 *   authStore().restoreSession();
 * });
 * </script>
 * ```
 */
export function initGrey(config: GreyConfig): GreyContext {
  if (_initialized && _context) {
    console.warn('[Grey Svelte] Already initialized. Returning existing context.');
    return _context;
  }

  // Use browser storage in browser, memory storage during SSR
  const storage = isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage();

  // Initialize auth controller with config
  const authController = new AuthController({
    apiBaseUrl: config.baseUrl,
    storage,
    onAuthChange: config.onAuthChange,
    onLogout: config.onLogout,
  });

  // Get the API client from auth controller
  const client = authController.getClient();

  // Initialize user controller
  const userController = new UserController(client);

  // Initialize projects controller
  const projectsController = new ProjectsController(client);

  // Query cache for query stores
  const queryCache = new Map<string, unknown>();

  /**
   * Invalidate queries by key prefix
   */
  function invalidateQueries(key: string | string[]): void {
    const normalizedKey = Array.isArray(key) ? key.join(':') : key;

    // Remove exact match
    queryCache.delete(normalizedKey);

    // Remove keys that start with the prefix
    for (const cachedKey of queryCache.keys()) {
      if (cachedKey.startsWith(normalizedKey)) {
        queryCache.delete(cachedKey);
      }
    }

    // TODO: Implement query subscription system to trigger refetch
    // This would require tracking active query store instances
  }

  // Build context
  _context = {
    baseUrl: config.baseUrl,
    client,
    authController,
    userController,
    projectsController,
    queryCache,
    invalidateQueries,
  };

  _initialized = true;

  // Handle initial session/user if provided
  if (config.initialSession) {
    storage.setTokens(config.initialSession);
  }
  if (config.initialUser) {
    userController.setUser(config.initialUser);
  }

  return _context;
}

/**
 * Get the Grey context
 *
 * @throws Error if Grey is not initialized
 * @returns The Grey context
 */
export function getGreyContext(): GreyContext {
  if (!_context) {
    throw new Error(
      '[Grey Svelte] Grey not initialized. Call initGrey() first in your app initialization.'
    );
  }
  return _context;
}

/**
 * Get the Grey context or null if not initialized
 *
 * @returns The Grey context or null
 */
export function getGreyContextOrNull(): GreyContext | null {
  return _context;
}

/**
 * Get the Grey API client
 *
 * @throws Error if Grey is not initialized
 * @returns The Grey client instance
 *
 * @example
 * ```ts
 * const client = getClient();
 * const result = await client.projects.list();
 * ```
 */
export function getClient(): GreyClient {
  return getGreyContext().client;
}

/**
 * Check if Grey is initialized
 *
 * @returns True if initialized
 */
export function isGreyInitialized(): boolean {
  return _initialized && _context !== null;
}

/**
 * Reset Grey context (primarily for testing)
 *
 * @internal
 */
export function _resetGrey(): void {
  _context = null;
  _initialized = false;
}

// Re-export storage implementations
export { BrowserTokenStorage, MemoryTokenStorage } from '@grey/adapters';
