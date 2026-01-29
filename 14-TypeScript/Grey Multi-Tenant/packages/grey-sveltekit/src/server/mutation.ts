/**
 * Grey SvelteKit - Server Mutation
 *
 * Server-side mutation utilities for SvelteKit.
 * Wraps @grey/adapters MutationController.
 *
 * No Svelte stores, no browser APIs.
 */

import { createMutation, type MutationOptions } from '@grey/adapters';
import { normalizeError, type GreyError } from './auth.js';

/**
 * Mutation result type
 */
export interface MutationResult<T> {
  data: T | null;
  error: GreyError | null;
}

/**
 * Execute a server-side mutation.
 * Wraps MutationController from @grey/adapters.
 */
export async function executeMutation<TData, TVariables>(
  variables: TVariables,
  options: MutationOptions<TData, TVariables>
): Promise<MutationResult<TData>> {
  try {
    const controller = createMutation(options);
    const data = await controller.mutate(variables);

    const state = controller.getState();

    if (state.isError || !data) {
      return {
        data: null,
        error: state.error ? { message: state.error.message } : { message: 'Mutation failed' },
      };
    }

    return {
      data,
      error: null,
    };
  } catch (err) {
    return {
      data: null,
      error: normalizeError(err),
    };
  }
}
