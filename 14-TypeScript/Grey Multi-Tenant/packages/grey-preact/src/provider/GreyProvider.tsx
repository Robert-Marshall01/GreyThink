/**
 * GreyProvider - Preact context provider for Grey Multi-Tenant
 *
 * Provides all Grey services via Preact context.
 * SSR-safe, no UI logic.
 */

import { h } from 'preact';
import { createContext } from 'preact';
import { useContext, useMemo } from 'preact/hooks';
import type { ComponentChildren, VNode } from 'preact';
import { useAuth, type UseAuthResult, type UseAuthConfig, type GreyError } from '../hooks/useAuth';
import { useUser, type UseUserResult } from '../hooks/useUser';
import { useProjects, type UseProjectsResult } from '../hooks/useProjects';
import type { GreyClient } from '@grey/core-client';

// =============================================================================
// Context Types
// =============================================================================

export interface GreyContextValue {
  auth: UseAuthResult;
  user: UseUserResult;
  projects: UseProjectsResult;
  client: GreyClient | null;
}

// =============================================================================
// Contexts
// =============================================================================

const GreyAuthContext = createContext<UseAuthResult | null>(null);
const GreyUserContext = createContext<UseUserResult | null>(null);
const GreyProjectsContext = createContext<UseProjectsResult | null>(null);
const GreyClientContext = createContext<GreyClient | null>(null);

// =============================================================================
// Provider Props
// =============================================================================

export interface GreyProviderProps {
  baseUrl: string;
  onAuthChange?: (user: unknown) => void;
  onLogout?: () => void;
  children: ComponentChildren;
}

// =============================================================================
// GreyProvider Component
// =============================================================================

export function GreyProvider(props: GreyProviderProps): VNode {
  const { baseUrl, onAuthChange, onLogout, children } = props;

  // Initialize auth hook
  const auth = useAuth({
    baseUrl,
    onAuthChange,
    onLogout,
  });

  // Get client from auth (may be null initially)
  const client = useMemo(() => {
    try {
      return auth.getClient();
    } catch {
      return null;
    }
  }, [auth]);

  // Initialize user hook (requires client)
  const user = client ? useUser(client) : {
    data: null,
    loading: false,
    error: null,
    fetchUser: async () => {},
  } as UseUserResult;

  // Initialize projects hook (requires client)
  const projects = client ? useProjects(client) : {
    data: [],
    loading: false,
    error: null,
    pagination: null,
    listProjects: async () => {},
    createProject: async () => {},
  } as UseProjectsResult;

  return (
    <GreyAuthContext.Provider value={auth}>
      <GreyUserContext.Provider value={user}>
        <GreyProjectsContext.Provider value={projects}>
          <GreyClientContext.Provider value={client}>
            {children}
          </GreyClientContext.Provider>
        </GreyProjectsContext.Provider>
      </GreyUserContext.Provider>
    </GreyAuthContext.Provider>
  );
}

// =============================================================================
// Context Hooks
// =============================================================================

export function useGreyAuth(): UseAuthResult {
  const context = useContext(GreyAuthContext);
  if (!context) {
    throw new Error('useGreyAuth must be used within a GreyProvider');
  }
  return context;
}

export function useGreyUser(): UseUserResult {
  const context = useContext(GreyUserContext);
  if (!context) {
    throw new Error('useGreyUser must be used within a GreyProvider');
  }
  return context;
}

export function useGreyProjects(): UseProjectsResult {
  const context = useContext(GreyProjectsContext);
  if (!context) {
    throw new Error('useGreyProjects must be used within a GreyProvider');
  }
  return context;
}

export function useGreyClient(): GreyClient {
  const context = useContext(GreyClientContext);
  if (!context) {
    throw new Error('useGreyClient must be used within a GreyProvider');
  }
  return context;
}
