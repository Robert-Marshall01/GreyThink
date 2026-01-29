/**
 * Grey SvelteKit - Server Query
 *
 * Server-side query utilities for SvelteKit.
 * Wraps @grey/adapters QueryController.
 *
 * No Svelte stores, no browser APIs.
 */

import { createQuery, type QueryOptions } from '@grey/adapters';
import { normalizeError, type GreyError } from './auth.js';

/**
 * Query result type
 */
export interface QueryResult<T> {
  data: T | null;
  error: GreyError | null;
}

/**
 * Execute a server-side query.
 * Wraps QueryController from @grey/adapters.
 */
export async function executeQuery<T>(
  options: QueryOptions<T>
): Promise<QueryResult<T>> {
  try {
    const controller = createQuery(options);
    const data = await controller.execute();

    const state = controller.getState();

    if (state.isError || !data) {
      return {
        data: null,
        error: state.error ? { message: state.error.message } : { message: 'Query failed' },
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
