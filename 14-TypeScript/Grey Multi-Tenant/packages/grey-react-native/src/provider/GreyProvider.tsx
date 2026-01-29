/**
 * Grey React Native - GreyProvider
 *
 * Root provider component that wraps the application and provides
 * Grey state and actions to all child components via React Context.
 * Mobile-safe: no browser/DOM APIs.
 * No UI logic, only wiring.
 */

import React, { createContext, useContext, useMemo, useRef, useEffect } from 'react';
import {
  AuthController,
  UserController,
  ProjectsController,
  MemoryTokenStorage,
  type AuthState,
  type UserState,
  type ProjectsState,
  type AuthConfig,
  type GreyClient,
  type User,
  type Project,
  type Pagination,
} from '@grey/adapters';

// ============================================================
// Types
// ============================================================

/**
 * Normalized error shape
 */
export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

/**
 * Auth context value
 */
export interface AuthContextValue {
  data: AuthState;
  loading: boolean;
  error: GreyError | null;
  login: (email: string, password: string) => Promise<boolean>;
  logout: () => Promise<void>;
  refresh: () => Promise<boolean>;
}

/**
 * User context value
 */
export interface UserContextValue {
  data: User | null;
  loading: boolean;
  error: GreyError | null;
  fetchUser: () => Promise<User | null>;
}

/**
 * Projects context value
 */
export interface ProjectsContextValue {
  data: {
    projects: Project[];
    pagination: Pagination | null;
  };
  loading: boolean;
  error: GreyError | null;
  listProjects: (page?: number, pageSize?: number) => Promise<void>;
  createProject: (input: { name: string; description?: string }) => Promise<Project | null>;
}

/**
 * Query context value
 */
export interface QueryContextValue {
  // Query context provides hook factory - implemented via useQuery hook
}

/**
 * Mutation context value
 */
export interface MutationContextValue {
  // Mutation context provides hook factory - implemented via useMutation hook
}

/**
 * Combined Grey context value
 */
export interface GreyContextValue {
  auth: AuthContextValue;
  user: UserContextValue;
  projects: ProjectsContextValue;
  client: GreyClient | null;
}

/**
 * Grey provider props
 */
export interface GreyProviderProps {
  apiBaseUrl: string;
  children: React.ReactNode;
}

// ============================================================
// Contexts
// ============================================================

/**
 * Create contexts for each domain
 */
const AuthContext = createContext<AuthContextValue | null>(null);
const UserContext = createContext<UserContextValue | null>(null);
const ProjectsContext = createContext<ProjectsContextValue | null>(null);
const GreyClientContext = createContext<GreyClient | null>(null);

// ============================================================
// Context Hooks
// ============================================================

/**
 * Context hooks for accessing domain state
 */
export function useGreyAuth(): AuthContextValue {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error('useGreyAuth must be used within a GreyProvider');
  }
  return context;
}

export function useGreyUser(): UserContextValue {
  const context = useContext(UserContext);
  if (!context) {
    throw new Error('useGreyUser must be used within a GreyProvider');
  }
  return context;
}

export function useGreyProjects(): ProjectsContextValue {
  const context = useContext(ProjectsContext);
  if (!context) {
    throw new Error('useGreyProjects must be used within a GreyProvider');
  }
  return context;
}

export function useGreyClient(): GreyClient | null {
  return useContext(GreyClientContext);
}

// ============================================================
// Error Normalization
// ============================================================

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

// ============================================================
// Provider Component
// ============================================================

/**
 * GreyProvider Component
 *
 * Provides Grey authentication, user, and project state to the component tree.
 * Must wrap your application at the root level.
 * Uses MemoryTokenStorage by default (mobile-safe).
 */
export function GreyProvider({ apiBaseUrl, children }: GreyProviderProps): React.ReactElement {
  // Controller refs
  const authControllerRef = useRef<AuthController | null>(null);
  const userControllerRef = useRef<UserController | null>(null);
  const projectsControllerRef = useRef<ProjectsController | null>(null);

  // State
  const [authState, setAuthState] = React.useState<AuthState>({
    user: null,
    isAuthenticated: false,
    isLoading: false,
    error: null,
  });
  const [authLoading, setAuthLoading] = React.useState(false);
  const [authError, setAuthError] = React.useState<GreyError | null>(null);

  const [userData, setUserData] = React.useState<User | null>(null);
  const [userLoading, setUserLoading] = React.useState(false);
  const [userError, setUserError] = React.useState<GreyError | null>(null);

  const [projectsData, setProjectsData] = React.useState<{
    projects: Project[];
    pagination: Pagination | null;
  }>({ projects: [], pagination: null });
  const [projectsLoading, setProjectsLoading] = React.useState(false);
  const [projectsError, setProjectsError] = React.useState<GreyError | null>(null);

  // Track mount state
  const mountedRef = useRef(true);
  useEffect(() => {
    mountedRef.current = true;
    return () => {
      mountedRef.current = false;
    };
  }, []);

  // Initialize controllers
  useEffect(() => {
    // Use MemoryTokenStorage for mobile-safe token storage
    const authConfig: AuthConfig = {
      apiBaseUrl,
      tokenStorage: new MemoryTokenStorage(),
    };

    const authController = new AuthController(authConfig);
    authControllerRef.current = authController;

    // Subscribe to auth state
    const unsubAuth = authController.subscribe((state: AuthState) => {
      if (mountedRef.current) {
        setAuthState(state);
        setAuthLoading(state.isLoading);
        if (state.error) {
          setAuthError({ message: state.error });
        }
      }
    });

    // Get client from auth controller
    const client = authController.getClient();

    // Initialize user controller
    const userController = new UserController(client);
    userControllerRef.current = userController;

    const unsubUser = userController.subscribe((state: UserState) => {
      if (mountedRef.current) {
        setUserData(state.user);
        setUserLoading(state.isLoading);
        if (state.error) {
          setUserError({ message: state.error });
        }
      }
    });

    // Initialize projects controller
    const projectsController = new ProjectsController(client);
    projectsControllerRef.current = projectsController;

    const unsubProjects = projectsController.subscribeToList((state: ProjectsState) => {
      if (mountedRef.current) {
        setProjectsData({
          projects: state.projects,
          pagination: state.pagination,
        });
        setProjectsLoading(state.isLoading);
        if (state.error) {
          setProjectsError({ message: state.error });
        }
      }
    });

    // Restore session on mount
    authController.restoreSession().catch(() => {
      // Session restore failed
    });

    return () => {
      unsubAuth();
      unsubUser();
      unsubProjects();
    };
  }, [apiBaseUrl]);

  // Auth actions
  const login = React.useCallback(async (email: string, password: string): Promise<boolean> => {
    const controller = authControllerRef.current;
    if (!controller) return false;

    setAuthLoading(true);
    setAuthError(null);

    try {
      const result = await controller.login(email, password);
      if (mountedRef.current) {
        setAuthLoading(false);
      }
      return result;
    } catch (err) {
      if (mountedRef.current) {
        setAuthError(normalizeError(err));
        setAuthLoading(false);
      }
      return false;
    }
  }, []);

  const logout = React.useCallback(async (): Promise<void> => {
    const controller = authControllerRef.current;
    if (!controller) return;

    setAuthLoading(true);
    setAuthError(null);

    try {
      controller.logout();
      if (mountedRef.current) {
        setAuthLoading(false);
      }
    } catch (err) {
      if (mountedRef.current) {
        setAuthError(normalizeError(err));
        setAuthLoading(false);
      }
    }
  }, []);

  const refresh = React.useCallback(async (): Promise<boolean> => {
    const controller = authControllerRef.current;
    if (!controller) return false;

    setAuthLoading(true);
    setAuthError(null);

    try {
      const result = await controller.restoreSession();
      if (mountedRef.current) {
        setAuthLoading(false);
      }
      return result;
    } catch (err) {
      if (mountedRef.current) {
        setAuthError(normalizeError(err));
        setAuthLoading(false);
      }
      return false;
    }
  }, []);

  // User actions
  const fetchUser = React.useCallback(async (): Promise<User | null> => {
    const controller = userControllerRef.current;
    if (!controller) return null;

    setUserLoading(true);
    setUserError(null);

    try {
      const user = await controller.fetchCurrentUser();
      if (mountedRef.current) {
        setUserLoading(false);
      }
      return user;
    } catch (err) {
      if (mountedRef.current) {
        setUserError(normalizeError(err));
        setUserLoading(false);
      }
      return null;
    }
  }, []);

  // Projects actions
  const listProjects = React.useCallback(async (page = 1, pageSize = 20): Promise<void> => {
    const controller = projectsControllerRef.current;
    if (!controller) return;

    setProjectsLoading(true);
    setProjectsError(null);

    try {
      await controller.loadList(page, pageSize);
      if (mountedRef.current) {
        setProjectsLoading(false);
      }
    } catch (err) {
      if (mountedRef.current) {
        setProjectsError(normalizeError(err));
        setProjectsLoading(false);
      }
    }
  }, []);

  const createProject = React.useCallback(async (input: { name: string; description?: string }): Promise<Project | null> => {
    const controller = projectsControllerRef.current;
    if (!controller) return null;

    setProjectsLoading(true);
    setProjectsError(null);

    try {
      const project = await controller.createProject(input);
      if (mountedRef.current) {
        setProjectsLoading(false);
      }
      return project;
    } catch (err) {
      if (mountedRef.current) {
        setProjectsError(normalizeError(err));
        setProjectsLoading(false);
      }
      return null;
    }
  }, []);

  // Memoize context values
  const authContextValue = useMemo<AuthContextValue>(() => ({
    data: authState,
    loading: authLoading,
    error: authError,
    login,
    logout,
    refresh,
  }), [authState, authLoading, authError, login, logout, refresh]);

  const userContextValue = useMemo<UserContextValue>(() => ({
    data: userData,
    loading: userLoading,
    error: userError,
    fetchUser,
  }), [userData, userLoading, userError, fetchUser]);

  const projectsContextValue = useMemo<ProjectsContextValue>(() => ({
    data: projectsData,
    loading: projectsLoading,
    error: projectsError,
    listProjects,
    createProject,
  }), [projectsData, projectsLoading, projectsError, listProjects, createProject]);

  const client = authControllerRef.current?.getClient() ?? null;

  return (
    <GreyClientContext.Provider value={client}>
      <AuthContext.Provider value={authContextValue}>
        <UserContext.Provider value={userContextValue}>
          <ProjectsContext.Provider value={projectsContextValue}>
            {children}
          </ProjectsContext.Provider>
        </UserContext.Provider>
      </AuthContext.Provider>
    </GreyClientContext.Provider>
  );
}
