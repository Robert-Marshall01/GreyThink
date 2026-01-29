/**
 * Grey Expo
 *
 * React Native hooks and provider for Grey Multi-Tenant SDK.
 * Expo-safe: no DOM, no Node APIs.
 *
 * @packageDocumentation
 */

// Hooks
export { useAuth } from './hooks/useAuth';
export type { UseAuthReturn, GreyError } from './hooks/useAuth';

export { useUser } from './hooks/useUser';
export type { UseUserReturn } from './hooks/useUser';

export { useProjects } from './hooks/useProjects';
export type { UseProjectsReturn, ProjectsData } from './hooks/useProjects';

export { useQuery } from './hooks/useQuery';
export type { UseQueryReturn, UseQueryOptions } from './hooks/useQuery';

export { useMutation } from './hooks/useMutation';
export type { UseMutationReturn, UseMutationOptions } from './hooks/useMutation';

// Provider
export {
  GreyProvider,
  useGreyAuth,
  useGreyUser,
  useGreyProjects,
  useGreyClient,
} from './provider/GreyProvider';
export type {
  GreyProviderProps,
  AuthContextValue,
  UserContextValue,
  ProjectsContextValue,
  QueryContextValue,
  MutationContextValue,
  GreyContextValue,
} from './provider/GreyProvider';
