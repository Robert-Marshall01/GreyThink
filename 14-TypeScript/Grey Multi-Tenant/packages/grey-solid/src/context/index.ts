/**
 * Grey Solid - Context Management
 *
 * Provides initialization and context utilities for the Grey SDK.
 * Uses SolidJS context API for dependency injection.
 */

import { createContext, useContext } from 'solid-js';
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

/**
 * Create appropriate storage for the current platform
 */
export function createPlatformStorage(): BrowserTokenStorage | MemoryTokenStorage {
  return isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage();
}

// =============================================================================
// SolidJS Context
// =============================================================================

/**
 * Grey context for dependency injection
 */
export const GreyContextValue = createContext<GreyContext | null>(null);

/**
 * Get the Grey context
 *
 * @throws Error if Grey is not initialized (no provider found)
 * @returns The Grey context
 */
export function useGreyContext(): GreyContext {
  const context = useContext(GreyContextValue);
  if (!context) {
    throw new Error(
      '[Grey Solid] Grey context not found. Wrap your app with <GreyProvider>.'
    );
  }
  return context;
}

/**
 * Get the Grey context or null if not initialized
 */
export function useGreyContextOptional(): GreyContext | null {
  return useContext(GreyContextValue);
}

/**
 * Get the Grey API client
 *
 * @throws Error if Grey is not initialized
 * @returns The Grey client instance
 *
 * @example
 * ```tsx
 * function MyComponent() {
 *   const client = useClient();
 *   // Use client for custom API calls
 *   return <div>...</div>;
 * }
 * ```
 */
export function useClient(): GreyClient {
  return useGreyContext().client;
}

// =============================================================================
// Context Factory
// =============================================================================

/**
 * Create Grey context value from config
 *
 * This is used internally by GreyProvider to create the context value.
 * You typically don't need to call this directly.
 *
 * @param config - Configuration options
 * @returns The Grey context value
 */
export function createGreyContext(config: GreyConfig): GreyContext {
  // Use browser storage in browser, memory storage during SSR
  const storage = createPlatformStorage();

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

  // Query cache for query signals
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
  }

  // Handle initial session/user if provided
  if (config.initialSession) {
    storage.setTokens(config.initialSession);
  }

  return {
    baseUrl: config.baseUrl,
    client,
    authController,
    userController,
    projectsController,
    queryCache,
    invalidateQueries,
  };
}

// Re-export storage implementations
export { BrowserTokenStorage, MemoryTokenStorage } from '@grey/adapters';
