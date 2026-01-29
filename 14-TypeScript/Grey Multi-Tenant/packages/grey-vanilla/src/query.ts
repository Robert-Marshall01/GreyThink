/**
 * Grey Vanilla - Query Module
 * 
 * Generic async query state container for custom data fetching.
 * Wraps the @grey/adapters QueryController with a simple listener pattern.
 * Similar to TanStack Query / React Query patterns.
 * 
 * @example
 * ```js
 * import { createQueryContainer } from '@grey/vanilla';
 * 
 * const statsQuery = createQueryContainer({
 *   queryFn: () => fetch('/api/stats').then(r => r.json()),
 *   enabled: true,
 *   onSuccess: (data) => console.log('Stats loaded:', data),
 * });
 * 
 * // Subscribe to state changes
 * const unsubscribe = statsQuery.subscribe((state) => {
 *   if (state.isLoading) {
 *     showLoader();
 *   } else if (state.isError) {
 *     showError(state.error);
 *   } else {
 *     renderStats(state.data);
 *   }
 * });
 * 
 * // Execute the query
 * await statsQuery.execute();
 * 
 * // Refetch later
 * await statsQuery.refetch();
 * 
 * // Cleanup
 * unsubscribe();
 * ```
 */

import {
  QueryController as CoreQueryController,
  type QueryState,
  type QueryOptions as CoreQueryOptions,
} from '@grey/adapters';
import { isBrowser, type Listener, type Unsubscribe } from './types.js';

/**
 * Options for creating a query container
 */
export interface QueryContainerOptions<T> {
  /** Async function that fetches data */
  queryFn: () => Promise<T>;
  /** Whether to auto-execute on creation (default: false) */
  enabled?: boolean;
  /** Called on successful fetch */
  onSuccess?: (data: T) => void;
  /** Called on error */
  onError?: (error: Error) => void;
}

/**
 * Query container interface
 * Provides reactive query state and actions
 */
export interface QueryContainer<T> {
  // ============================================================
  // State Access
  // ============================================================
  
  /** Get the current query state snapshot */
  getState(): QueryState<T>;
  
  /** Subscribe to query state changes */
  subscribe(listener: Listener<QueryState<T>>): Unsubscribe;
  
  // ============================================================
  // State Properties (convenience getters)
  // ============================================================
  
  /** The fetched data (if any) */
  readonly data: T | null;
  
  /** Whether the query is loading */
  readonly loading: boolean;
  
  /** Current error (if any) */
  readonly error: Error | null;
  
  /** Whether the query completed successfully */
  readonly isSuccess: boolean;
  
  /** Whether the query resulted in an error */
  readonly isError: boolean;
  
  // ============================================================
  // Actions
  // ============================================================
  
  /**
   * Execute the query
   * @returns Promise resolving to the data or null
   */
  execute(): Promise<T | null>;
  
  /**
   * Refetch data (alias for execute)
   * @returns Promise resolving to the data or null
   */
  refetch(): Promise<T | null>;
}

/**
 * Create a query container
 * 
 * Provides a simple observable state container for async queries
 * that wraps the @grey/adapters QueryController.
 * 
 * @param options - Configuration options
 * @returns QueryContainer instance
 * 
 * @example
 * ```js
 * const userStats = createQueryContainer({
 *   queryFn: async () => {
 *     const res = await fetch('/api/user/stats');
 *     return res.json();
 *   },
 *   enabled: true, // Auto-execute on creation
 *   onSuccess: (data) => console.log('Stats:', data),
 *   onError: (err) => console.error('Failed:', err),
 * });
 * 
 * // Subscribe to changes
 * userStats.subscribe((state) => {
 *   updateUI(state);
 * });
 * ```
 */
export function createQueryContainer<T>(
  options: QueryContainerOptions<T>
): QueryContainer<T> {
  // Internal state
  let _state: QueryState<T>;
  const listeners = new Set<Listener<QueryState<T>>>();
  
  // Convert options for core controller
  const coreOptions: CoreQueryOptions<T> = {
    queryFn: options.queryFn,
    enabled: options.enabled,
    onSuccess: options.onSuccess,
    onError: options.onError,
  };
  
  // Initialize core controller
  const coreController = new CoreQueryController(coreOptions);
  _state = coreController.getState();
  
  // Subscribe to core controller state changes
  coreController.subscribe((state: QueryState<T>) => {
    _state = state;
    notifyListeners();
  });
  
  /**
   * Notify all listeners of state change
   */
  function notifyListeners(): void {
    listeners.forEach((listener) => listener(_state));
  }
  
  // Auto-execute if enabled and in browser
  if (options.enabled && isBrowser()) {
    coreController.execute().catch(() => {
      // Query failed - error is in state
    });
  }
  
  // Return the container interface
  const container: QueryContainer<T> = {
    // State access
    getState() {
      return { ..._state };
    },
    
    subscribe(listener: Listener<QueryState<T>>): Unsubscribe {
      listeners.add(listener);
      // Immediately call with current state
      listener(_state);
      return () => listeners.delete(listener);
    },
    
    // Convenience getters
    get data() {
      return _state.data;
    },
    
    get loading() {
      return _state.isLoading;
    },
    
    get error() {
      return _state.error;
    },
    
    get isSuccess() {
      return _state.isSuccess;
    },
    
    get isError() {
      return _state.isError;
    },
    
    // Actions
    execute(): Promise<T | null> {
      return coreController.execute();
    },
    
    refetch(): Promise<T | null> {
      return coreController.refetch();
    },
  };
  
  return container;
}

// Re-export related types
export type { QueryState, QueryOptions } from '@grey/adapters';
export { initialQueryState, createQuery } from '@grey/adapters';
