/**
 * Grey Vue - Provider
 *
 * Provides Grey state to the Vue component tree using Vue 3's provide/inject.
 * Must be called in a parent component's setup function.
 *
 * @example
 * ```vue
 * <script setup lang="ts">
 * import { provideGrey } from '@grey/vue';
 *
 * // Call in root App.vue setup
 * provideGrey({
 *   baseUrl: 'http://localhost:8080/api/v1',
 * });
 * </script>
 *
 * <template>
 *   <RouterView />
 * </template>
 * ```
 */

import { provide, onMounted } from 'vue';
import {
  AuthController,
  UserController,
  ProjectsController,
  BrowserTokenStorage,
  MemoryTokenStorage,
} from '@grey/adapters';
import { GreyKey, isBrowser } from '../context/GreyContext.js';
import type { GreyProviderConfig, GreyContextValue } from '../types/index.js';

/**
 * Provide Grey context to child components
 *
 * Call this function in your root component's setup to initialize
 * the Grey SDK and provide it to all child components.
 *
 * @param config - Provider configuration options
 * @returns The context value (for advanced usage)
 *
 * @example
 * ```vue
 * <script setup lang="ts">
 * import { provideGrey } from '@grey/vue';
 *
 * const grey = provideGrey({
 *   baseUrl: import.meta.env.VITE_GREY_API_URL || 'http://localhost:8080/api/v1',
 *   onLogout: () => {
 *     router.push('/login');
 *   },
 * });
 * </script>
 * ```
 */
export function provideGrey(config: GreyProviderConfig): GreyContextValue {
  // Use browser storage in browser, memory storage during SSR
  const storage = isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage();

  // Initialize auth controller with config
  const authController = new AuthController({
    apiBaseUrl: config.baseUrl,
    storage,
    onAuthChange: config.onAuthChange,
    onLogout: config.onLogout,
  });

  // Get the API client from auth controller
  const client = authController.getClient();

  // Initialize user controller
  const userController = new UserController(client);

  // Initialize projects controller
  const projectsController = new ProjectsController(client);

  // Query cache for useQuery composable
  const queryCache = new Map<string, unknown>();

  /**
   * Invalidate queries by key prefix
   */
  function invalidateQueries(key: string | string[]): void {
    const normalizedKey = Array.isArray(key) ? key.join(':') : key;

    // Remove exact match
    queryCache.delete(normalizedKey);

    // Remove keys that start with the prefix
    for (const cachedKey of queryCache.keys()) {
      if (cachedKey.startsWith(normalizedKey)) {
        queryCache.delete(cachedKey);
      }
    }

    // TODO: Implement query subscription system to trigger refetch
    // This would require tracking active query instances
  }

  // Build context value
  const context: GreyContextValue = {
    baseUrl: config.baseUrl,
    client,
    authController,
    userController,
    projectsController,
    queryCache,
    invalidateQueries,
  };

  // Provide context to child components
  provide(GreyKey, context);

  // Restore session on mount (client-side only)
  onMounted(() => {
    if (isBrowser() && !config.initialSession) {
      // Attempt to restore session from storage
      authController.restoreSession().catch(() => {
        // Session restoration failed - user needs to login
      });
    }
  });

  // If initial session provided, set it
  if (config.initialSession) {
    storage.setTokens(config.initialSession);
  }

  // If initial user provided, set it
  if (config.initialUser) {
    userController.setUser(config.initialUser);
  }

  return context;
}

/**
 * Vue plugin for Grey SDK
 *
 * Alternative to provideGrey() - use app.use() to install globally.
 *
 * @example
 * ```ts
 * import { createApp } from 'vue';
 * import { createGreyPlugin } from '@grey/vue';
 * import App from './App.vue';
 *
 * const app = createApp(App);
 *
 * app.use(createGreyPlugin({
 *   baseUrl: 'http://localhost:8080/api/v1',
 * }));
 *
 * app.mount('#app');
 * ```
 */
export function createGreyPlugin(config: GreyProviderConfig) {
  return {
    install(app: { provide: typeof provide }) {
      // Use memory storage for SSR safety in plugin context
      const storage = isBrowser() ? new BrowserTokenStorage() : new MemoryTokenStorage();

      const authController = new AuthController({
        apiBaseUrl: config.baseUrl,
        storage,
        onAuthChange: config.onAuthChange,
        onLogout: config.onLogout,
      });

      const client = authController.getClient();
      const userController = new UserController(client);
      const projectsController = new ProjectsController(client);
      const queryCache = new Map<string, unknown>();

      function invalidateQueries(key: string | string[]): void {
        const normalizedKey = Array.isArray(key) ? key.join(':') : key;
        queryCache.delete(normalizedKey);
        for (const cachedKey of queryCache.keys()) {
          if (cachedKey.startsWith(normalizedKey)) {
            queryCache.delete(cachedKey);
          }
        }
      }

      const context: GreyContextValue = {
        baseUrl: config.baseUrl,
        client,
        authController,
        userController,
        projectsController,
        queryCache,
        invalidateQueries,
      };

      app.provide(GreyKey, context);

      // Handle initial session/user if provided
      if (config.initialSession) {
        storage.setTokens(config.initialSession);
      }
      if (config.initialUser) {
        userController.setUser(config.initialUser);
      }
    },
  };
}

// Re-export storage implementations
export { BrowserTokenStorage, MemoryTokenStorage } from '@grey/adapters';
