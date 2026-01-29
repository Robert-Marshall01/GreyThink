<script lang="ts">
/**
 * Grey Vue - GreyProvider
 *
 * Root provider component that wraps the application and provides
 * Grey state and actions to all child components via Vue provide/inject.
 * No UI logic, only wiring.
 */

import { defineComponent, provide, ref, onMounted, onUnmounted } from 'vue';
import {
  AuthController,
  UserController,
  ProjectsController,
  type AuthState,
  type UserState,
  type ProjectsState,
  type AuthConfig,
  type GreyClient,
  type User,
  type Project,
  type Pagination,
} from '@grey/adapters';
import {
  GreyAuthKey,
  GreyUserKey,
  GreyProjectsKey,
  GreyClientKey,
  type GreyError,
} from './types.js';

/**
 * Normalize any error into the standard error shape
 */
function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    return {
      message: err.message,
      raw: err,
    };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return {
    message: 'An unknown error occurred',
    raw: err,
  };
}

export default defineComponent({
  name: 'GreyProvider',
  props: {
    apiBaseUrl: {
      type: String,
      required: true,
    },
  },
  setup(props, { slots }) {
    // Controllers
    let authController: AuthController | null = null;
    let userController: UserController | null = null;
    let projectsController: ProjectsController | null = null;

    // Auth state
    const authData = ref<AuthState>({
      user: null,
      isAuthenticated: false,
      isLoading: false,
      error: null,
    });
    const authLoading = ref(false);
    const authError = ref<GreyError | null>(null);

    // User state
    const userData = ref<User | null>(null);
    const userLoading = ref(false);
    const userError = ref<GreyError | null>(null);

    // Projects state
    const projectsData = ref<{ projects: Project[]; pagination: Pagination | null }>({
      projects: [],
      pagination: null,
    });
    const projectsLoading = ref(false);
    const projectsError = ref<GreyError | null>(null);

    // Client reference
    const client = ref<GreyClient | null>(null);

    // Subscriptions
    let unsubAuth: (() => void) | undefined;
    let unsubUser: (() => void) | undefined;
    let unsubProjects: (() => void) | undefined;

    onMounted(() => {
      const authConfig: AuthConfig = {
        apiBaseUrl: props.apiBaseUrl,
      };

      authController = new AuthController(authConfig);
      client.value = authController.getClient();

      // Subscribe to auth state
      unsubAuth = authController.subscribe((state) => {
        authData.value = state;
        authLoading.value = state.isLoading;
        if (state.error) {
          authError.value = { message: state.error };
        }
      });

      // Initialize user controller
      userController = new UserController(client.value);
      unsubUser = userController.subscribe((state: UserState) => {
        userData.value = state.user;
        userLoading.value = state.isLoading;
        if (state.error) {
          userError.value = { message: state.error };
        }
      });

      // Initialize projects controller
      projectsController = new ProjectsController(client.value);
      unsubProjects = projectsController.subscribeToList((state: ProjectsState) => {
        projectsData.value = {
          projects: state.projects,
          pagination: state.pagination,
        };
        projectsLoading.value = state.isLoading;
        if (state.error) {
          projectsError.value = { message: state.error };
        }
      });

      // Restore session on mount
      authController.restoreSession().catch(() => {
        // Session restore failed
      });
    });

    onUnmounted(() => {
      unsubAuth?.();
      unsubUser?.();
      unsubProjects?.();
    });

    // Auth actions
    async function login(email: string, password: string): Promise<boolean> {
      if (!authController) return false;
      authLoading.value = true;
      authError.value = null;
      try {
        const result = await authController.login(email, password);
        authLoading.value = false;
        return result;
      } catch (err) {
        authError.value = normalizeError(err);
        authLoading.value = false;
        return false;
      }
    }

    async function logout(): Promise<void> {
      if (!authController) return;
      authLoading.value = true;
      authError.value = null;
      try {
        authController.logout();
        authLoading.value = false;
      } catch (err) {
        authError.value = normalizeError(err);
        authLoading.value = false;
      }
    }

    async function refresh(): Promise<boolean> {
      if (!authController) return false;
      authLoading.value = true;
      authError.value = null;
      try {
        const result = await authController.restoreSession();
        authLoading.value = false;
        return result;
      } catch (err) {
        authError.value = normalizeError(err);
        authLoading.value = false;
        return false;
      }
    }

    // User actions
    async function fetchUser(): Promise<User | null> {
      if (!userController) return null;
      userLoading.value = true;
      userError.value = null;
      try {
        const user = await userController.fetchCurrentUser();
        userLoading.value = false;
        return user;
      } catch (err) {
        userError.value = normalizeError(err);
        userLoading.value = false;
        return null;
      }
    }

    // Projects actions
    async function listProjects(page = 1, pageSize = 20): Promise<void> {
      if (!projectsController) return;
      projectsLoading.value = true;
      projectsError.value = null;
      try {
        await projectsController.loadList(page, pageSize);
        projectsLoading.value = false;
      } catch (err) {
        projectsError.value = normalizeError(err);
        projectsLoading.value = false;
      }
    }

    async function createProject(input: { name: string; description?: string }): Promise<Project | null> {
      if (!projectsController) return null;
      projectsLoading.value = true;
      projectsError.value = null;
      try {
        const project = await projectsController.createProject(input);
        projectsLoading.value = false;
        return project;
      } catch (err) {
        projectsError.value = normalizeError(err);
        projectsLoading.value = false;
        return null;
      }
    }

    // Provide context values
    provide(GreyAuthKey, {
      get data() { return authData.value; },
      get loading() { return authLoading.value; },
      get error() { return authError.value; },
      login,
      logout,
      refresh,
    });

    provide(GreyUserKey, {
      get data() { return userData.value; },
      get loading() { return userLoading.value; },
      get error() { return userError.value; },
      fetchUser,
    });

    provide(GreyProjectsKey, {
      get data() { return projectsData.value; },
      get loading() { return projectsLoading.value; },
      get error() { return projectsError.value; },
      listProjects,
      createProject,
    });

    provide(GreyClientKey, client.value);

    return () => slots.default?.();
  },
});
</script>

<template>
  <slot />
</template>
