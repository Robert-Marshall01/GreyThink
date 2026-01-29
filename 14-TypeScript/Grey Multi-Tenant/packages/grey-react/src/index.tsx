/**
 * Grey React - React Hooks for Grey Multi-Tenant
 *
 * Provides React-idiomatic hooks wrapping @grey/adapters controllers.
 */

import { useState, useEffect, useCallback, useContext, createContext, useMemo } from 'react';
import type { ReactNode } from 'react';
import {
  AuthController,
  UserController,
  ProjectsController,
  QueryController,
  MutationController,
  BrowserTokenStorage,
  type AuthState,
  type AuthConfig,
  type UserState,
  type ProjectsState,
  type ProjectState,
  type CreateProjectInput,
  type QueryOptions,
  type MutationOptions,
} from '@grey/adapters';
import type { GreyClient } from '@grey/core-client';

// ============================================
// Context
// ============================================

interface GreyContextValue {
  client: GreyClient;
  authController: AuthController;
  userController: UserController;
  projectsController: ProjectsController;
}

const GreyContext = createContext<GreyContextValue | null>(null);

interface GreyProviderProps {
  config: AuthConfig;
  children: ReactNode;
}

/**
 * Grey Provider - Wrap your app with this to enable Grey hooks
 */
export function GreyProvider({ config, children }: GreyProviderProps) {
  const value = useMemo(() => {
    const authController = new AuthController(config);
    const client = authController.getClient();
    const userController = new UserController(client);
    const projectsController = new ProjectsController(client);

    return { client, authController, userController, projectsController };
  }, [config.apiBaseUrl]);

  return <GreyContext.Provider value={value}>{children}</GreyContext.Provider>;
}

function useGreyContext(): GreyContextValue {
  const context = useContext(GreyContext);
  if (!context) {
    throw new Error('useGreyContext must be used within a GreyProvider');
  }
  return context;
}

// ============================================
// Hooks
// ============================================

/**
 * useAuth - Authentication hook
 */
export function useAuth() {
  const { authController } = useGreyContext();
  const [state, setState] = useState<AuthState>(authController.getState());

  useEffect(() => {
    return authController.subscribe(setState);
  }, [authController]);

  const login = useCallback(
    (email: string, password: string) => authController.login(email, password),
    [authController]
  );

  const logout = useCallback(() => authController.logout(), [authController]);

  const restoreSession = useCallback(
    () => authController.restoreSession(),
    [authController]
  );

  return {
    ...state,
    login,
    logout,
    restoreSession,
  };
}

/**
 * useUser - Current user hook
 */
export function useUser() {
  const { userController } = useGreyContext();
  const [state, setState] = useState<UserState>(userController.getState());

  useEffect(() => {
    return userController.subscribe(setState);
  }, [userController]);

  const refresh = useCallback(
    () => userController.fetchCurrentUser(),
    [userController]
  );

  return {
    ...state,
    refresh,
  };
}

/**
 * useProjects - Projects list hook
 */
export function useProjects() {
  const { projectsController } = useGreyContext();
  const [state, setState] = useState<ProjectsState>(projectsController.getListState());

  useEffect(() => {
    return projectsController.subscribeToList(setState);
  }, [projectsController]);

  const load = useCallback(
    (page?: number, pageSize?: number) => projectsController.loadList(page, pageSize),
    [projectsController]
  );

  const create = useCallback(
    (input: CreateProjectInput) => projectsController.createProject(input),
    [projectsController]
  );

  return {
    ...state,
    load,
    create,
  };
}

/**
 * useProject - Single project hook
 */
export function useProject(id: string) {
  const { projectsController } = useGreyContext();
  const [state, setState] = useState<ProjectState>(projectsController.getDetailState(id));

  useEffect(() => {
    return projectsController.subscribeToDetail(id, setState);
  }, [projectsController, id]);

  const load = useCallback(
    () => projectsController.loadProject(id),
    [projectsController, id]
  );

  return {
    ...state,
    load,
  };
}

/**
 * useQuery - Generic query hook for data fetching
 */
export function useQuery<T>(options: QueryOptions<T>) {
  const controller = useMemo(
    () => new QueryController(options),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [options.enabled]
  );

  const [state, setState] = useState(controller.getState());

  useEffect(() => {
    return controller.subscribe(setState);
  }, [controller]);

  useEffect(() => {
    if (options.enabled !== false) {
      controller.execute();
    }
  }, [controller, options.enabled]);

  const refetch = useCallback(() => controller.refetch(), [controller]);

  return {
    ...state,
    refetch,
  };
}

/**
 * useMutation - Generic mutation hook for data mutations
 */
export function useMutation<TData, TVariables>(
  options: MutationOptions<TData, TVariables>
) {
  const controller = useMemo(
    () => new MutationController(options),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    []
  );

  const [state, setState] = useState(controller.getState());

  useEffect(() => {
    return controller.subscribe(setState);
  }, [controller]);

  const mutate = useCallback(
    (variables: TVariables) => controller.mutate(variables),
    [controller]
  );

  const reset = useCallback(() => controller.reset(), [controller]);

  return {
    ...state,
    mutate,
    mutateAsync: mutate,
    reset,
  };
}

/**
 * useClient - Get the raw Grey API client
 */
export function useClient(): GreyClient {
  const { client } = useGreyContext();
  return client;
}

// Re-export types
export type {
  AuthState,
  UserState,
  ProjectsState,
  ProjectState,
  CreateProjectInput,
  QueryOptions,
  MutationOptions,
};
export { BrowserTokenStorage, type AuthConfig };
