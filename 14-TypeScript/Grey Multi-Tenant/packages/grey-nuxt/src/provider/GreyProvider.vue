<script lang="ts">
/**
 * Grey Nuxt - GreyProvider Component
 *
 * Root provider component for Grey Multi-Tenant.
 * Provides Grey context to all child components via Vue's provide/inject.
 *
 * Uses the PROVIDER TEMPLATE (Vue variant).
 * No UI logic, only wiring.
 */

import { defineComponent, provide } from 'vue';
import { useAuth } from '../composables/useAuth.js';
import { useUser } from '../composables/useUser.js';
import { useProjects } from '../composables/useProjects.js';
import { GreyKey, type GreyContextValue } from './types.js';

export default defineComponent({
  name: 'GreyProvider',

  setup(_, { slots }) {
    // Initialize composables
    const auth = useAuth();
    const user = useUser();
    const projects = useProjects();

    // Provide context to child components
    const contextValue: GreyContextValue = {
      auth,
      user,
      projects,
    };

    provide(GreyKey, contextValue);

    // Render slot content
    return () => {
      return slots.default ? slots.default() : null;
    };
  },
});
</script>
