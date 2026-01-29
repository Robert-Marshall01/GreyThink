/**
 * Grey Remix - GreyProvider
 *
 * React context provider for Remix applications.
 * Provides auth, user, projects, query, and mutation contexts.
 *
 * Client-side only. No UI logic.
 */

import React, {
  createContext,
  useContext,
  useMemo,
  type ReactNode,
} from 'react';
import { useAuth, type UseAuthResult, type UseAuthOptions } from '../hooks/useAuth';
import { useUser, type UseUserResult, type UseUserOptions } from '../hooks/useUser';
import { useProjects, type UseProjectsResult, type UseProjectsOptions } from '../hooks/useProjects';
import { useQuery, type UseQueryResult, type UseQueryOptions } from '../hooks/useQuery';
import { useMutation, type UseMutationResult, type UseMutationOptions } from '../hooks/useMutation';

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
  authOptions?: UseAuthOptions;
  userOptions?: UseUserOptions;
  projectsOptions?: UseProjectsOptions;
  queryOptions?: UseQueryOptions;
  mutationOptions?: UseMutationOptions;
}

// ============================================================
// Context
// ============================================================

const GreyContext = createContext<GreyContextValue | null>(null);

// ============================================================
// Provider Component
// ============================================================

export function GreyProvider({
  children,
  authOptions,
  userOptions,
  projectsOptions,
  queryOptions,
  mutationOptions,
}: GreyProviderProps): React.ReactElement {
  const auth = useAuth(authOptions);
  const user = useUser(userOptions);
  const projects = useProjects(projectsOptions);
  const query = useQuery(queryOptions);
  const mutation = useMutation(mutationOptions);

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
