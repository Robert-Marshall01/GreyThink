/**
 * Grey Electron - Preload Bridge
 *
 * Secure bridge between renderer and main process.
 * Uses contextBridge to expose typed API to renderer.
 *
 * Never exposes ipcRenderer directly.
 */

import { contextBridge, ipcRenderer } from 'electron';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface AuthData {
  user: unknown | null;
  session: {
    accessToken: string;
    refreshToken?: string;
    expiresIn?: number;
  } | null;
}

export interface LoginCredentials {
  email: string;
  password: string;
}

export interface RefreshOptions {
  refreshToken: string;
}

export interface CreateProjectInput {
  name: string;
  description?: string;
  [key: string]: unknown;
}

export interface QueryInput {
  endpoint: string;
  params?: Record<string, unknown>;
}

export interface MutationInput<TVariables = unknown> {
  endpoint: string;
  method?: 'POST' | 'PUT' | 'PATCH' | 'DELETE';
  variables?: TVariables;
}

export interface GreyBridgeApi {
  auth: {
    login: (credentials: LoginCredentials) => Promise<{ data: AuthData | null; error: GreyError | null }>;
    logout: (token?: string) => Promise<{ error: GreyError | null }>;
    refresh: (options: RefreshOptions) => Promise<{ data: AuthData | null; error: GreyError | null }>;
  };
  user: {
    fetchUser: (token: string) => Promise<{ data: unknown | null; error: GreyError | null }>;
  };
  projects: {
    listProjects: (token: string) => Promise<{ data: unknown[] | null; error: GreyError | null }>;
    createProject: (token: string, input: CreateProjectInput) => Promise<{ data: unknown | null; error: GreyError | null }>;
  };
  query: {
    executeQuery: <T = unknown>(token: string | undefined, input: QueryInput) => Promise<{ data: T | null; error: GreyError | null }>;
  };
  mutation: {
    executeMutation: <TData = unknown, TVariables = unknown>(
      token: string | undefined,
      input: MutationInput<TVariables>
    ) => Promise<{ data: TData | null; error: GreyError | null }>;
  };
}

// ============================================================
// IPC Channel Names (must match main/ipc.ts)
// ============================================================

const IPC_CHANNELS = {
  AUTH_LOGIN: 'auth:login',
  AUTH_LOGOUT: 'auth:logout',
  AUTH_REFRESH: 'auth:refresh',
  USER_FETCH: 'user:fetch',
  PROJECTS_LIST: 'projects:list',
  PROJECTS_CREATE: 'projects:create',
  QUERY_EXECUTE: 'query:execute',
  MUTATION_EXECUTE: 'mutation:execute',
} as const;

// ============================================================
// Bridge API
// ============================================================

const greyApi: GreyBridgeApi = {
  auth: {
    login: async (credentials: LoginCredentials) => {
      return ipcRenderer.invoke(IPC_CHANNELS.AUTH_LOGIN, {
        email: credentials.email,
        password: credentials.password,
      });
    },

    logout: async (token?: string) => {
      return ipcRenderer.invoke(IPC_CHANNELS.AUTH_LOGOUT, { token });
    },

    refresh: async (options: RefreshOptions) => {
      return ipcRenderer.invoke(IPC_CHANNELS.AUTH_REFRESH, {
        refreshToken: options.refreshToken,
      });
    },
  },

  user: {
    fetchUser: async (token: string) => {
      return ipcRenderer.invoke(IPC_CHANNELS.USER_FETCH, { token });
    },
  },

  projects: {
    listProjects: async (token: string) => {
      return ipcRenderer.invoke(IPC_CHANNELS.PROJECTS_LIST, { token });
    },

    createProject: async (token: string, input: CreateProjectInput) => {
      return ipcRenderer.invoke(IPC_CHANNELS.PROJECTS_CREATE, {
        token,
        project: input,
      });
    },
  },

  query: {
    executeQuery: async <T = unknown>(token: string | undefined, input: QueryInput) => {
      return ipcRenderer.invoke(IPC_CHANNELS.QUERY_EXECUTE, {
        token,
        query: input,
      }) as Promise<{ data: T | null; error: GreyError | null }>;
    },
  },

  mutation: {
    executeMutation: async <TData = unknown, TVariables = unknown>(
      token: string | undefined,
      input: MutationInput<TVariables>
    ) => {
      return ipcRenderer.invoke(IPC_CHANNELS.MUTATION_EXECUTE, {
        token,
        mutation: input,
      }) as Promise<{ data: TData | null; error: GreyError | null }>;
    },
  },
};

// ============================================================
// Expose to Renderer
// ============================================================

contextBridge.exposeInMainWorld('grey', greyApi);

// ============================================================
// Global Type Declaration
// ============================================================

declare global {
  interface Window {
    grey: GreyBridgeApi;
  }
}
