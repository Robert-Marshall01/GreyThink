/**
 * Grey Electron - Renderer GreyProvider
 *
 * React context provider for Electron renderer.
 * Initializes hooks and provides them via context.
 *
 * No UI logic, client-side only.
 */

import React, {
  createContext,
  useContext,
  useMemo,
  type ReactNode,
} from 'react';
import { useAuth, type UseAuthResult } from './useAuth';
import { useUser, type UseUserResult } from './useUser';
import { useProjects, type UseProjectsResult } from './useProjects';
import { useQuery, type UseQueryResult } from './useQuery';
import { useMutation, type UseMutationResult } from './useMutation';

// ============================================================
// Types
// ============================================================

export interface GreyContextValue {
  auth: UseAuthResult;
  user: UseUserResult;
  projects: UseProjectsResult;
  query: UseQueryResult;
  mutation: UseMutationResult;
}

export interface GreyProviderProps {
  children: ReactNode;
}

// ============================================================
// Context
// ============================================================

const GreyContext = createContext<GreyContextValue | null>(null);

// ============================================================
// Provider Component
// ============================================================

export function GreyProvider({ children }: GreyProviderProps): React.ReactElement {
  const auth = useAuth();
  const user = useUser();
  const projects = useProjects();
  const query = useQuery();
  const mutation = useMutation();

  const value = useMemo<GreyContextValue>(
    () => ({
      auth,
      user,
      projects,
      query,
      mutation,
    }),
    [auth, user, projects, query, mutation]
  );

  return React.createElement(GreyContext.Provider, { value }, children);
}

// ============================================================
// Context Hooks
// ============================================================

export function useGrey(): GreyContextValue {
  const context = useContext(GreyContext);
  if (!context) {
    throw new Error('useGrey must be used within a GreyProvider');
  }
  return context;
}

export function useGreyAuth(): UseAuthResult {
  const { auth } = useGrey();
  return auth;
}

export function useGreyUser(): UseUserResult {
  const { user } = useGrey();
  return user;
}

export function useGreyProjects(): UseProjectsResult {
  const { projects } = useGrey();
  return projects;
}

export function useGreyQuery<T = unknown>(): UseQueryResult<T> {
  const { query } = useGrey();
  return query as UseQueryResult<T>;
}

export function useGreyMutation<TData = unknown, TVariables = unknown>(): UseMutationResult<TData, TVariables> {
  const { mutation } = useGrey();
  return mutation as UseMutationResult<TData, TVariables>;
}
