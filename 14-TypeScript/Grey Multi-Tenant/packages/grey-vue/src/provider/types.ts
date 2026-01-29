/**
 * Grey Vue - Provider Types
 *
 * Types and injection keys for the GreyProvider.
 */

import type { InjectionKey } from 'vue';
import type {
  AuthState,
  User,
  Project,
  Pagination,
  GreyClient,
} from '@grey/adapters';

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
 * Grey context value
 */
export interface GreyContextValue {
  auth: AuthContextValue;
  user: UserContextValue;
  projects: ProjectsContextValue;
  client: GreyClient | null;
}

/**
 * Injection keys
 */
export const GreyAuthKey: InjectionKey<AuthContextValue> = Symbol('GreyAuth');
export const GreyUserKey: InjectionKey<UserContextValue> = Symbol('GreyUser');
export const GreyProjectsKey: InjectionKey<ProjectsContextValue> = Symbol('GreyProjects');
export const GreyClientKey: InjectionKey<GreyClient | null> = Symbol('GreyClient');
