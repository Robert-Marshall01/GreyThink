/**
 * @grey/flutter - Flutter Web TypeScript interop layer
 * 
 * This package provides a JavaScript bridge for Flutter Web.
 * It exposes global functions that can be called from Dart.
 * 
 * The package is designed to be bundled into a single JS file
 * that can be included in Flutter's web/index.html.
 */

import { createGreyClient, type GreyClient } from '@grey/core-client';
import type { AuthSession, User, Project } from '@grey/core-client';

// ============================================================
// Global State
// ============================================================

let greyClient: GreyClient | null = null;
let currentSession: AuthSession | null = null;

// ============================================================
// Initialization
// ============================================================

/**
 * Initialize the Grey client.
 * Call this from Dart: js.context.callMethod('greyInit', [baseUrl]);
 */
export function greyInit(baseUrl: string, token?: string): void {
  greyClient = createGreyClient({ baseUrl, token });
  console.log('[Grey] Client initialized with base URL:', baseUrl);
}

function getClient(): GreyClient {
  if (!greyClient) {
    throw new Error('[Grey] Client not initialized. Call greyInit first.');
  }
  return greyClient;
}

// ============================================================
// Auth Functions
// ============================================================

/**
 * Login with email and password.
 * Returns true on success, throws on error.
 * 
 * Dart usage:
 * ```dart
 * final success = await js.context.callMethod('greyLogin', [email, password]);
 * ```
 */
export async function greyLogin(
  email: string,
  password: string
): Promise<boolean> {
  const client = getClient();
  const result = await client.auth.login({ email, password });

  if (result.error) {
    throw new Error(result.error.message);
  }

  if (result.data) {
    currentSession = result.data.data;
    return true;
  }

  return false;
}

/**
 * Logout the current user.
 * 
 * Dart usage:
 * ```dart
 * await js.context.callMethod('greyLogout', []);
 * ```
 */
export async function greyLogout(): Promise<void> {
  const client = getClient();
  await client.auth.logout();
  currentSession = null;
}

/**
 * Refresh the auth token.
 * 
 * Dart usage:
 * ```dart
 * await js.context.callMethod('greyRefresh', []);
 * ```
 */
export async function greyRefresh(): Promise<boolean> {
  if (!currentSession?.refresh_token) {
    return false;
  }

  const client = getClient();
  const result = await client.auth.refresh({
    refresh_token: currentSession.refresh_token,
  });

  if (result.data) {
    currentSession = result.data.data;
    return true;
  }

  return false;
}

/**
 * Check if user is authenticated.
 * 
 * Dart usage:
 * ```dart
 * final isAuth = js.context.callMethod('greyIsAuthenticated', []);
 * ```
 */
export function greyIsAuthenticated(): boolean {
  return currentSession !== null;
}

/**
 * Get current session info.
 * Returns null if not authenticated.
 * 
 * Dart usage:
 * ```dart
 * final session = js.context.callMethod('greyGetSession', []);
 * ```
 */
export function greyGetSession(): AuthSession | null {
  return currentSession;
}

// ============================================================
// User Functions
// ============================================================

/**
 * Get the current user.
 * Returns user object or null.
 * 
 * Dart usage:
 * ```dart
 * final user = await js.context.callMethod('greyGetUser', []);
 * if (user != null) {
 *   print('Hello, ${user['name']}');
 * }
 * ```
 */
export async function greyGetUser(): Promise<User | null> {
  const client = getClient();
  const result = await client.users.me();
  return result.data ?? null;
}

// ============================================================
// Projects Functions
// ============================================================

/**
 * Get all projects.
 * Returns array of project objects.
 * 
 * Dart usage:
 * ```dart
 * final projects = await js.context.callMethod('greyGetProjects', []);
 * for (final project in projects) {
 *   print(project['name']);
 * }
 * ```
 */
export async function greyGetProjects(): Promise<Project[]> {
  const client = getClient();
  const result = await client.projects.list();
  return result.data ?? [];
}

/**
 * Get a single project by ID.
 * 
 * Dart usage:
 * ```dart
 * final project = await js.context.callMethod('greyGetProject', [id]);
 * ```
 */
export async function greyGetProject(id: string): Promise<Project | null> {
  const client = getClient();
  const result = await client.projects.get(id);
  return result.data ?? null;
}

/**
 * Create a new project.
 * Returns the created project.
 * 
 * Dart usage:
 * ```dart
 * final project = await js.context.callMethod(
 *   'greyCreateProject',
 *   [name, description]
 * );
 * ```
 */
export async function greyCreateProject(
  name: string,
  description?: string
): Promise<Project> {
  const client = getClient();
  const result = await client.projects.create({ name, description });

  if (result.error) {
    throw new Error(result.error.message);
  }

  return result.data!;
}

/**
 * Update an existing project.
 * 
 * Dart usage:
 * ```dart
 * final project = await js.context.callMethod(
 *   'greyUpdateProject',
 *   [id, name, description]
 * );
 * ```
 */
export async function greyUpdateProject(
  id: string,
  name?: string,
  description?: string
): Promise<Project> {
  const client = getClient();
  const result = await client.projects.update(id, { name, description });

  if (result.error) {
    throw new Error(result.error.message);
  }

  return result.data!;
}

/**
 * Delete a project.
 * 
 * Dart usage:
 * ```dart
 * await js.context.callMethod('greyDeleteProject', [id]);
 * ```
 */
export async function greyDeleteProject(id: string): Promise<void> {
  const client = getClient();
  const result = await client.projects.delete(id);

  if (result.error) {
    throw new Error(result.error.message);
  }
}

// ============================================================
// Register Global Functions
// ============================================================

/**
 * Register all functions on the global window object.
 * This makes them accessible from Dart's js.context.
 */
function registerGlobals(): void {
  if (typeof window === 'undefined') {
    console.warn('[Grey] Not in browser environment, skipping global registration');
    return;
  }

  const globals = {
    greyInit,
    greyLogin,
    greyLogout,
    greyRefresh,
    greyIsAuthenticated,
    greyGetSession,
    greyGetUser,
    greyGetProjects,
    greyGetProject,
    greyCreateProject,
    greyUpdateProject,
    greyDeleteProject,
  };

  Object.assign(window, globals);
  console.log('[Grey] Global functions registered');
}

// Auto-register on load
registerGlobals();

// ============================================================
// Module Exports
// ============================================================

export {
  greyInit as init,
  greyLogin as login,
  greyLogout as logout,
  greyRefresh as refresh,
  greyIsAuthenticated as isAuthenticated,
  greyGetSession as getSession,
  greyGetUser as getUser,
  greyGetProjects as getProjects,
  greyGetProject as getProject,
  greyCreateProject as createProject,
  greyUpdateProject as updateProject,
  greyDeleteProject as deleteProject,
};

// Type exports
export type { GreyClient, AuthSession, User, Project } from '@grey/core-client';
