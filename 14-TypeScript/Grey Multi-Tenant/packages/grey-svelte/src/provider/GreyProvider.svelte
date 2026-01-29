<!--
  Grey Svelte - GreyProvider

  Root provider component that wraps the application and provides
  Grey state and actions to all child components via Svelte setContext.
  No UI logic, only wiring.
  Must be SSR-safe.
-->
<script lang="ts">
  import { setContext, onMount } from 'svelte';
  import { createAuthStore, type AuthStoreReturn } from '../stores/auth.js';
  import { createUserStore, type UserStoreReturn } from '../stores/user.js';
  import { createProjectsStore, type ProjectsStoreReturn } from '../stores/projects.js';
  import {
    GREY_AUTH_KEY,
    GREY_USER_KEY,
    GREY_PROJECTS_KEY,
    GREY_CLIENT_KEY,
  } from './types.js';
  import type { GreyClient } from '@grey/core-client';

  /**
   * Base URL for the Grey API (required)
   */
  export let apiBaseUrl: string;

  // Initialize stores
  const authStore = createAuthStore({ apiBaseUrl });
  const client: GreyClient = authStore.getClient();
  const userStore = createUserStore(client);
  const projectsStore = createProjectsStore(client);

  // Provide stores via context
  setContext<AuthStoreReturn>(GREY_AUTH_KEY, authStore);
  setContext<UserStoreReturn>(GREY_USER_KEY, userStore);
  setContext<ProjectsStoreReturn>(GREY_PROJECTS_KEY, projectsStore);
  setContext<GreyClient>(GREY_CLIENT_KEY, client);

  // Restore session on mount (SSR-safe)
  onMount(() => {
    authStore.refresh();
  });
</script>

<slot />
