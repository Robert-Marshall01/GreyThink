/**
 * Grey Nuxt - Provider Types
 *
 * Type definitions for GreyProvider context.
 */

import type { InjectionKey } from 'vue';
import type { UseAuthReturn } from '../composables/useAuth.js';
import type { UseUserReturn } from '../composables/useUser.js';
import type { UseProjectsReturn } from '../composables/useProjects.js';

/**
 * Grey context value type
 */
export interface GreyContextValue {
  auth: UseAuthReturn;
  user: UseUserReturn;
  projects: UseProjectsReturn;
}

/**
 * Grey context injection key
 */
export const GreyKey: InjectionKey<GreyContextValue> = Symbol('grey');
