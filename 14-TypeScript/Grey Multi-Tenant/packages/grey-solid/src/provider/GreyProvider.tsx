/**
 * Grey Solid - GreyProvider
 *
 * Root provider component that wraps the application and provides
 * Grey state and actions to all child components via Solid context.
 * No UI logic, only wiring.
 */

import { createContext, useContext, type JSX, type Accessor } from 'solid-js';
import { useAuth, type UseAuthReturn, type AuthConfig } from '../hooks/useAuth.js';
import { useUser, type UseUserReturn } from '../hooks/useUser.js';
import { useProjects, type UseProjectsReturn } from '../hooks/useProjects.js';
import type { GreyClient } from '@grey/core-client';

/**
 * Context types
 */
export interface GreyAuthContextValue extends UseAuthReturn {}
export interface GreyUserContextValue extends UseUserReturn {}
export interface GreyProjectsContextValue extends UseProjectsReturn {}

/**
 * Create contexts
 */
export const GreyAuthContext = createContext<GreyAuthContextValue>();
export const GreyUserContext = createContext<GreyUserContextValue>();
export const GreyProjectsContext = createContext<GreyProjectsContextValue>();
export const GreyClientContext = createContext<GreyClient>();

/**
 * GreyProvider props
 */
export interface GreyProviderProps {
  /** Base URL for the Grey API (required) */
  apiBaseUrl: string;
  /** Children */
  children: JSX.Element;
}

/**
 * GreyProvider component
 *
 * Root provider that initializes all Grey hooks and provides them
 * via Solid's context system to all child components.
 */
export function GreyProvider(props: GreyProviderProps): JSX.Element {
  // Initialize auth hook
  const auth = useAuth({ apiBaseUrl: props.apiBaseUrl });
  const client = auth.getClient();

  // Initialize user and projects hooks
  const user = useUser(client);
  const projects = useProjects(client);

  return (
    <GreyAuthContext.Provider value={auth}>
      <GreyUserContext.Provider value={user}>
        <GreyProjectsContext.Provider value={projects}>
          <GreyClientContext.Provider value={client}>
            {props.children}
          </GreyClientContext.Provider>
        </GreyProjectsContext.Provider>
      </GreyUserContext.Provider>
    </GreyAuthContext.Provider>
  );
}

/**
 * useGreyAuth - Get auth context
 */
export function useGreyAuth(): GreyAuthContextValue {
  const context = useContext(GreyAuthContext);
  if (!context) {
    throw new Error('[Grey Solid] useGreyAuth must be used within a GreyProvider');
  }
  return context;
}

/**
 * useGreyUser - Get user context
 */
export function useGreyUser(): GreyUserContextValue {
  const context = useContext(GreyUserContext);
  if (!context) {
    throw new Error('[Grey Solid] useGreyUser must be used within a GreyProvider');
  }
  return context;
}

/**
 * useGreyProjects - Get projects context
 */
export function useGreyProjects(): GreyProjectsContextValue {
  const context = useContext(GreyProjectsContext);
  if (!context) {
    throw new Error('[Grey Solid] useGreyProjects must be used within a GreyProvider');
  }
  return context;
}

/**
 * useGreyClient - Get Grey API client
 */
export function useGreyClient(): GreyClient {
  const context = useContext(GreyClientContext);
  if (!context) {
    throw new Error('[Grey Solid] useGreyClient must be used within a GreyProvider');
  }
  return context;
}

export default GreyProvider;
