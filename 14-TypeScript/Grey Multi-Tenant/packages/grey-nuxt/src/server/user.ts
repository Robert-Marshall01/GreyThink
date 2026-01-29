/**
 * Grey Nuxt - Server User
 *
 * Server-side user utilities for Nuxt Nitro.
 * Wraps @grey/adapters UserController functions.
 *
 * No Vue, no composables, no browser APIs.
 */

import { createUserController } from '@grey/adapters';
import type { GreyClient, User } from '@grey/core-client';
import { normalizeError, type GreyError } from './auth.js';

/**
 * User result type
 */
export interface UserResult {
  user: User | null;
  error: GreyError | null;
}

/**
 * Fetch the current user.
 * Wraps UserController.fetchCurrentUser from @grey/adapters.
 */
export async function fetchUser(client: GreyClient): Promise<UserResult> {
  try {
    const controller = createUserController(client);
    const user = await controller.fetchCurrentUser();

    if (!user) {
      const state = controller.getState();
      return {
        user: null,
        error: state.error ? { message: state.error } : { message: 'User not found' },
      };
    }

    return {
      user,
      error: null,
    };
  } catch (err) {
    return {
      user: null,
      error: normalizeError(err),
    };
  }
}
