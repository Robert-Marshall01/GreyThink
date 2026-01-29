/**
 * Grey Vanilla - Mutation Module
 * 
 * Generic async mutation state container for data modification operations.
 * Wraps the @grey/adapters MutationController with a simple listener pattern.
 * Similar to TanStack Query / React Query mutation patterns.
 * 
 * @example
 * ```js
 * import { createMutationContainer } from '@grey/vanilla';
 * 
 * const updateProfile = createMutationContainer({
 *   mutationFn: (data) => fetch('/api/profile', {
 *     method: 'PUT',
 *     body: JSON.stringify(data),
 *   }).then(r => r.json()),
 *   onSuccess: (result) => {
 *     console.log('Profile updated:', result);
 *     userContainer.refetch(); // Refresh user data
 *   },
 *   onError: (error) => console.error('Failed:', error),
 * });
 * 
 * // Subscribe to state changes
 * const unsubscribe = updateProfile.subscribe((state) => {
 *   if (state.isLoading) {
 *     showSavingIndicator();
 *   } else if (state.isSuccess) {
 *     showSuccessMessage();
 *   }
 * });
 * 
 * // Execute the mutation
 * await updateProfile.mutate({ name: 'New Name' });
 * 
 * // Reset state
 * updateProfile.reset();
 * 
 * // Cleanup
 * unsubscribe();
 * ```
 */

import {
  MutationController as CoreMutationController,
  type MutationState,
  type MutationOptions as CoreMutationOptions,
} from '@grey/adapters';
import type { Listener, Unsubscribe } from './types.js';

/**
 * Options for creating a mutation container
 */
export interface MutationContainerOptions<TData, TVariables> {
  /** Async function that performs the mutation */
  mutationFn: (variables: TVariables) => Promise<TData>;
  /** Called on success */
  onSuccess?: (data: TData, variables: TVariables) => void;
  /** Called on error */
  onError?: (error: Error, variables: TVariables) => void;
  /** Called when mutation starts */
  onMutate?: (variables: TVariables) => void;
  /** Called when mutation settles (success or error) */
  onSettled?: (data: TData | null, error: Error | null, variables: TVariables) => void;
}

/**
 * Mutation container interface
 * Provides reactive mutation state and actions
 */
export interface MutationContainer<TData, TVariables> {
  // ============================================================
  // State Access
  // ============================================================
  
  /** Get the current mutation state snapshot */
  getState(): MutationState<TData>;
  
  /** Subscribe to mutation state changes */
  subscribe(listener: Listener<MutationState<TData>>): Unsubscribe;
  
  // ============================================================
  // State Properties (convenience getters)
  // ============================================================
  
  /** The mutation result data (if successful) */
  readonly data: TData | null;
  
  /** Whether the mutation is loading */
  readonly loading: boolean;
  
  /** Current error (if any) */
  readonly error: Error | null;
  
  /** Whether the mutation completed successfully */
  readonly isSuccess: boolean;
  
  /** Whether the mutation resulted in an error */
  readonly isError: boolean;
  
  // ============================================================
  // Actions
  // ============================================================
  
  /**
   * Execute the mutation
   * @param variables - Variables to pass to the mutation function
   * @returns Promise resolving to the data or null
   */
  mutate(variables: TVariables): Promise<TData | null>;
  
  /** Reset mutation state to initial values */
  reset(): void;
}

/**
 * Create a mutation container
 * 
 * Provides a simple observable state container for async mutations
 * that wraps the @grey/adapters MutationController.
 * 
 * @param options - Configuration options
 * @returns MutationContainer instance
 * 
 * @example
 * ```js
 * const createProject = createMutationContainer({
 *   mutationFn: async (input) => {
 *     const res = await fetch('/api/projects', {
 *       method: 'POST',
 *       headers: { 'Content-Type': 'application/json' },
 *       body: JSON.stringify(input),
 *     });
 *     return res.json();
 *   },
 *   onSuccess: (project) => {
 *     console.log('Created:', project);
 *     projectsContainer.refetch();
 *   },
 * });
 * 
 * // Subscribe to changes
 * createProject.subscribe((state) => {
 *   submitButton.disabled = state.isLoading;
 * });
 * 
 * // Execute mutation
 * await createProject.mutate({ name: 'New Project' });
 * ```
 */
export function createMutationContainer<TData, TVariables>(
  options: MutationContainerOptions<TData, TVariables>
): MutationContainer<TData, TVariables> {
  // Internal state
  let _state: MutationState<TData>;
  const listeners = new Set<Listener<MutationState<TData>>>();
  
  // Convert options for core controller
  const coreOptions: CoreMutationOptions<TData, TVariables> = {
    mutationFn: options.mutationFn,
    onSuccess: options.onSuccess,
    onError: options.onError,
    onSettled: options.onSettled,
  };
  
  // Initialize core controller
  const coreController = new CoreMutationController(coreOptions);
  _state = coreController.getState();
  
  // Subscribe to core controller state changes
  coreController.subscribe((state: MutationState<TData>) => {
    _state = state;
    notifyListeners();
  });
  
  /**
   * Notify all listeners of state change
   */
  function notifyListeners(): void {
    listeners.forEach((listener) => listener(_state));
  }
  
  // Return the container interface
  const container: MutationContainer<TData, TVariables> = {
    // State access
    getState() {
      return { ..._state };
    },
    
    subscribe(listener: Listener<MutationState<TData>>): Unsubscribe {
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
    mutate(variables: TVariables): Promise<TData | null> {
      options.onMutate?.(variables);
      return coreController.mutate(variables);
    },
    
    reset(): void {
      coreController.reset();
    },
  };
  
  return container;
}

// Re-export related types
export type { MutationState, MutationOptions } from '@grey/adapters';
export { initialMutationState, createMutation } from '@grey/adapters';
