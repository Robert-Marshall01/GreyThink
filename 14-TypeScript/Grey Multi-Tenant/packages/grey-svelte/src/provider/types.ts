/**
 * Grey Svelte - Provider Types and Context Keys
 *
 * Exports context keys and types for the GreyProvider.
 */

import type { Readable } from 'svelte/store';
import type { AuthState } from '@grey/adapters';
import type { GreyClient, User, Project, Pagination } from '@grey/core-client';

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
 * Auth store return type
 */
export interface AuthContextValue {
  data: Readable<AuthState>;
  loading: Readable<boolean>;
  error: Readable<GreyError | null>;
  login: (email: string, password: string) => Promise<boolean>;
  logout: () => Promise<void>;
  refresh: () => Promise<boolean>;
  getClient: () => GreyClient;
}

/**
 * User store return type
 */
export interface UserContextValue {
  data: Readable<User | null>;
  loading: Readable<boolean>;
  error: Readable<GreyError | null>;
  fetchUser: () => Promise<User | null>;
}

/**
 * Projects data structure
 */
export interface ProjectsData {
  projects: Project[];
  pagination: Pagination | null;
}

/**
 * Projects store return type
 */
export interface ProjectsContextValue {
  data: Readable<ProjectsData>;
  loading: Readable<boolean>;
  error: Readable<GreyError | null>;
  listProjects: (page?: number, pageSize?: number) => Promise<void>;
  createProject: (input: { name: string; description?: string }) => Promise<Project | null>;
}

/**
 * Context keys for setContext/getContext
 */
export const GREY_AUTH_KEY = Symbol('GreyAuth');
export const GREY_USER_KEY = Symbol('GreyUser');
export const GREY_PROJECTS_KEY = Symbol('GreyProjects');
export const GREY_CLIENT_KEY = Symbol('GreyClient');
