/**
 * Grey Nuxt - Nuxt Plugin
 *
 * Nuxt plugin that provides Grey context to the application.
 * Uses defineNuxtPlugin from #app.
 *
 * Injects:
 * - auth
 * - user
 * - projects
 * - query
 * - mutation
 */

import { defineNuxtPlugin } from '#app';
import { useAuth } from '../composables/useAuth.js';
import { useUser } from '../composables/useUser.js';
import { useProjects } from '../composables/useProjects.js';
import { useQuery } from '../composables/useQuery.js';
import { useMutation } from '../composables/useMutation.js';

export default defineNuxtPlugin(() => {
  // Initialize composables
  const auth = useAuth();
  const user = useUser();
  const projects = useProjects();

  return {
    provide: {
      grey: {
        auth,
        user,
        projects,
        useQuery,
        useMutation,
      },
    },
  };
});
