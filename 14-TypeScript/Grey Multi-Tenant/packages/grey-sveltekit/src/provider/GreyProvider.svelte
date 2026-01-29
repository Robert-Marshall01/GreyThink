<script lang="ts">
/**
 * Grey SvelteKit - GreyProvider Component
 *
 * Root provider component for Grey Multi-Tenant.
 * Provides Grey stores to all child components via Svelte's setContext.
 *
 * Uses the PROVIDER TEMPLATE (Svelte variant).
 * No UI logic, only wiring. SSR-safe.
 */

import { setContext } from 'svelte';
import { createAuthStore } from '../stores/auth.js';
import { createUserStore } from '../stores/user.js';
import { createProjectsStore } from '../stores/projects.js';
import { GREY_CONTEXT_KEY, type GreyContextValue } from './types.js';

/**
 * Props
 */
export let baseUrl: string = '';

// Initialize stores
const auth = createAuthStore(baseUrl);
const user = createUserStore();
const projects = createProjectsStore();

// Provide context to child components
const contextValue: GreyContextValue = {
  auth,
  user,
  projects,
};

setContext(GREY_CONTEXT_KEY, contextValue);
</script>

<slot />
