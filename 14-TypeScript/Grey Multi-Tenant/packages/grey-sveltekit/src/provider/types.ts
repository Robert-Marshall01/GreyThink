/**
 * Grey SvelteKit - Provider Types
 *
 * Type definitions for GreyProvider context.
 */

import type { AuthStoreReturn } from '../stores/auth.js';
import type { UserStoreReturn } from '../stores/user.js';
import type { ProjectsStoreReturn } from '../stores/projects.js';

/**
 * Grey context value type
 */
export interface GreyContextValue {
  auth: AuthStoreReturn;
  user: UserStoreReturn;
  projects: ProjectsStoreReturn;
}

/**
 * Grey context key
 */
export const GREY_CONTEXT_KEY = Symbol('grey');
