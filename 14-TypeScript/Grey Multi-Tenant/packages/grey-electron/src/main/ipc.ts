/**
 * Grey Electron - IPC Handlers
 *
 * Main process IPC registration for secure communication.
 * Validates inputs and normalizes errors before sending to renderer.
 *
 * Runs in trusted main process only.
 */

import { ipcMain, IpcMainInvokeEvent } from 'electron';
import { AuthService } from './auth.service';
import { UserService } from './user.service';
import { ProjectsService } from './projects.service';
import { QueryService } from './query.service';
import { MutationService } from './mutation.service';

// ============================================================
// Types
// ============================================================

export interface GreyError {
  message: string;
  code?: string;
  status?: number;
  raw?: unknown;
}

export interface IpcServicesConfig {
  baseUrl: string;
}

// ============================================================
// Error Normalization
// ============================================================

function normalizeError(err: unknown): GreyError {
  if (err instanceof Error) {
    const error = err as Error & { code?: string; status?: number };
    return {
      message: error.message,
      code: error.code,
      status: error.status,
    };
  }
  if (typeof err === 'string') {
    return { message: err };
  }
  return { message: 'An unknown error occurred' };
}

// ============================================================
// Input Validation
// ============================================================

function validateString(value: unknown, fieldName: string): string {
  if (typeof value !== 'string' || value.trim() === '') {
    throw new Error(`Invalid ${fieldName}: must be a non-empty string`);
  }
  return value.trim();
}

function validateObject(value: unknown, fieldName: string): Record<string, unknown> {
  if (typeof value !== 'object' || value === null || Array.isArray(value)) {
    throw new Error(`Invalid ${fieldName}: must be an object`);
  }
  return value as Record<string, unknown>;
}

function validateOptionalString(value: unknown): string | undefined {
  if (value === undefined || value === null) {
    return undefined;
  }
  if (typeof value !== 'string') {
    throw new Error('Invalid value: must be a string or undefined');
  }
  return value;
}

// ============================================================
// IPC Channel Names
// ============================================================

export const IPC_CHANNELS = {
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
// Register IPC Handlers
// ============================================================

export function registerIpcHandlers(config: IpcServicesConfig): void {
  const authService = new AuthService({ baseUrl: config.baseUrl });
  const userService = new UserService({ baseUrl: config.baseUrl });
  const projectsService = new ProjectsService({ baseUrl: config.baseUrl });
  const queryService = new QueryService({ baseUrl: config.baseUrl });
  const mutationService = new MutationService({ baseUrl: config.baseUrl });

  // Auth: Login
  ipcMain.handle(IPC_CHANNELS.AUTH_LOGIN, async (_event: IpcMainInvokeEvent, args: unknown) => {
    try {
      const input = validateObject(args, 'args');
      const email = validateString(input.email, 'email');
      const password = validateString(input.password, 'password');

      return await authService.login({ email, password });
    } catch (err) {
      return { data: null, error: normalizeError(err) };
    }
  });

  // Auth: Logout
  ipcMain.handle(IPC_CHANNELS.AUTH_LOGOUT, async (_event: IpcMainInvokeEvent, args: unknown) => {
    try {
      const input = validateObject(args, 'args');
      const token = validateOptionalString(input.token);

      return await authService.logout(token);
    } catch (err) {
      return { error: normalizeError(err) };
    }
  });

  // Auth: Refresh
  ipcMain.handle(IPC_CHANNELS.AUTH_REFRESH, async (_event: IpcMainInvokeEvent, args: unknown) => {
    try {
      const input = validateObject(args, 'args');
      const refreshToken = validateString(input.refreshToken, 'refreshToken');

      return await authService.refresh({ refreshToken });
    } catch (err) {
      return { data: null, error: normalizeError(err) };
    }
  });

  // User: Fetch
  ipcMain.handle(IPC_CHANNELS.USER_FETCH, async (_event: IpcMainInvokeEvent, args: unknown) => {
    try {
      const input = validateObject(args, 'args');
      const token = validateString(input.token, 'token');

      return await userService.fetchUser({ token });
    } catch (err) {
      return { data: null, error: normalizeError(err) };
    }
  });

  // Projects: List
  ipcMain.handle(IPC_CHANNELS.PROJECTS_LIST, async (_event: IpcMainInvokeEvent, args: unknown) => {
    try {
      const input = validateObject(args, 'args');
      const token = validateString(input.token, 'token');

      return await projectsService.listProjects({ token });
    } catch (err) {
      return { data: null, error: normalizeError(err) };
    }
  });

  // Projects: Create
  ipcMain.handle(IPC_CHANNELS.PROJECTS_CREATE, async (_event: IpcMainInvokeEvent, args: unknown) => {
    try {
      const input = validateObject(args, 'args');
      const token = validateString(input.token, 'token');
      const projectInput = validateObject(input.project, 'project');
      const name = validateString(projectInput.name, 'name');

      return await projectsService.createProject({ token }, { name, ...projectInput });
    } catch (err) {
      return { data: null, error: normalizeError(err) };
    }
  });

  // Query: Execute
  ipcMain.handle(IPC_CHANNELS.QUERY_EXECUTE, async (_event: IpcMainInvokeEvent, args: unknown) => {
    try {
      const input = validateObject(args, 'args');
      const token = validateOptionalString(input.token);
      const queryInput = validateObject(input.query, 'query');
      const endpoint = validateString(queryInput.endpoint, 'endpoint');

      return await queryService.executeQuery(
        { token },
        { endpoint, params: queryInput.params as Record<string, unknown> | undefined }
      );
    } catch (err) {
      return { data: null, error: normalizeError(err) };
    }
  });

  // Mutation: Execute
  ipcMain.handle(IPC_CHANNELS.MUTATION_EXECUTE, async (_event: IpcMainInvokeEvent, args: unknown) => {
    try {
      const input = validateObject(args, 'args');
      const token = validateOptionalString(input.token);
      const mutationInput = validateObject(input.mutation, 'mutation');
      const endpoint = validateString(mutationInput.endpoint, 'endpoint');
      const method = validateOptionalString(mutationInput.method) as 'POST' | 'PUT' | 'PATCH' | 'DELETE' | undefined;

      return await mutationService.executeMutation(
        { token },
        { endpoint, method, variables: mutationInput.variables }
      );
    } catch (err) {
      return { data: null, error: normalizeError(err) };
    }
  });
}

/**
 * Unregister all IPC handlers (for cleanup).
 */
export function unregisterIpcHandlers(): void {
  Object.values(IPC_CHANNELS).forEach((channel) => {
    ipcMain.removeHandler(channel);
  });
}
