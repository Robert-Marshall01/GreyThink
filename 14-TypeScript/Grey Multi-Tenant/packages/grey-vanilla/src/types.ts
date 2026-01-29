/**
 * Grey Vanilla - Type Definitions
 * 
 * Shared types for the Vanilla JS state containers.
 */

// Re-export types from core client
export type {
  GreyClient,
  AuthSession,
  User,
  Project,
  Pagination,
  Organization,
} from '@grey/core-client';

// Re-export adapter types
export type {
  AuthState,
  AuthConfig,
  TokenStorage,
  UserState,
  ProjectsState,
  ProjectState,
  CreateProjectInput,
  UpdateProjectInput,
  QueryState,
  MutationState,
  QueryOptions,
  MutationOptions,
} from '@grey/adapters';

/**
 * SSR-safe check for browser environment
 */
export function isBrowser(): boolean {
  return typeof window !== 'undefined';
}

/**
 * Listener function type for state subscriptions
 */
export type Listener<T> = (state: T) => void;

/**
 * Unsubscribe function returned by subscribe
 */
export type Unsubscribe = () => void;

/**
 * Base state container interface
 * All modules (auth, user, projects, etc.) follow this pattern
 */
export interface StateContainer<T> {
  /** Get the current state snapshot */
  getState(): T;
  /** Subscribe to state changes */
  subscribe(listener: Listener<T>): Unsubscribe;
}

/**
 * Observable state container with notify capability
 */
export interface Observable<T> extends StateContainer<T> {
  /** Current state (getter) */
  readonly state: T;
  /** Notify all subscribers of state change */
  notify(): void;
}

/**
 * Configuration for creating the Grey provider
 */
export interface GreyProviderConfig {
  /** Base URL for the Grey API */
  apiBaseUrl: string;
  /** Whether to auto-restore session on initialization (default: true) */
  autoRestoreSession?: boolean;
  /** Called when auth state changes */
  onAuthChange?: (state: import('@grey/adapters').AuthState) => void;
  /** Called when user logs out */
  onLogout?: () => void;
}
