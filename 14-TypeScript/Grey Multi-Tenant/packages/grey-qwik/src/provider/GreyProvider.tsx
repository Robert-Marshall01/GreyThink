/**
 * Grey Qwik - GreyProvider
 *
 * Root provider component that wraps the application and provides
 * Grey state and actions to all child components via Qwik context.
 * No UI logic, only wiring.
 */

import { component$, Slot, useContextProvider, createContextId, useVisibleTask$ } from '@builder.io/qwik';
import { useAuth, type UseAuthReturn, type AuthConfig } from '../hooks/useAuth.js';
import { useUser, type UseUserReturn } from '../hooks/useUser.js';
import { useProjects, type UseProjectsReturn } from '../hooks/useProjects.js';
import type { GreyClient } from '@grey/core-client';

/**
 * Context IDs for Qwik's context system
 */
export const GreyAuthContextId = createContextId<UseAuthReturn>('grey-auth');
export const GreyUserContextId = createContextId<UseUserReturn>('grey-user');
export const GreyProjectsContextId = createContextId<UseProjectsReturn>('grey-projects');
export const GreyClientContextId = createContextId<GreyClient>('grey-client');

/**
 * GreyProvider props
 */
export interface GreyProviderProps {
  /** Base URL for the Grey API (required) */
  apiBaseUrl: string;
}

/**
 * GreyProvider component
 *
 * Root provider that initializes all Grey hooks and provides them
 * via Qwik's context system to all child components.
 */
export const GreyProvider = component$<GreyProviderProps>((props) => {
  // Initialize auth hook
  const auth = useAuth({ apiBaseUrl: props.apiBaseUrl });
  const client = auth.getClient();

  // Initialize user and projects hooks
  const user = useUser(client);
  const projects = useProjects(client);

  // Provide context values
  useContextProvider(GreyAuthContextId, auth);
  useContextProvider(GreyUserContextId, user);
  useContextProvider(GreyProjectsContextId, projects);
  useContextProvider(GreyClientContextId, client);

  // Restore session on mount (SSR-safe)
  useVisibleTask$(async () => {
    await auth.refresh();
  });

  return <Slot />;
});

export default GreyProvider;
