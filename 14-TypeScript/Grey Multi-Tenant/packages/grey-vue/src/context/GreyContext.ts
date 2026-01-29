/**
 * Grey Vue - Context and Injection
 *
 * Provides Vue injection keys and context utilities for the Grey system.
 * Uses Vue 3's provide/inject pattern for dependency injection.
 */

import { inject, type InjectionKey } from 'vue';
import type { GreyContextValue } from '../types/index.js';

/**
 * Injection key for Grey context
 * @internal
 */
export const GreyKey: InjectionKey<GreyContextValue> = Symbol('GreyContext');

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
 * Inject Grey context - throws if not provided
 *
 * @throws Error if Grey context is not provided
 * @returns Grey context value
 *
 * @example
 * ```ts
 * // Inside a composable or setup function
 * const context = useGreyContext();
 * const client = context.client;
 * ```
 */
export function useGreyContext(): GreyContextValue {
  const context = inject(GreyKey);
  if (!context) {
    throw new Error(
      '[Grey Vue] Grey context not found. ' +
        'Make sure to call provideGrey() in a parent component or use the GreyPlugin.'
    );
  }
  return context;
}

/**
 * Try to inject Grey context - returns null if not provided
 *
 * @returns Grey context value or null
 *
 * @example
 * ```ts
 * const context = useGreyContextOrNull();
 * if (context) {
 *   // Context available
 * }
 * ```
 */
export function useGreyContextOrNull(): GreyContextValue | null {
  return inject(GreyKey, null);
}
