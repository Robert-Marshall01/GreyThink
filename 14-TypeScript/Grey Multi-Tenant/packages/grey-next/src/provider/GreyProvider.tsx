'use client';

/**
 * GreyProvider - Next.js Client Component for context provision
 *
 * Provides Grey contexts for auth, user, projects, query, and mutation.
 * Renders children directly.
 * No UI logic.
 */

import React, { createContext, useContext, useMemo, type ReactNode } from 'react';
import { useAuth, type UseAuthReturn } from '../hooks/useAuth.js';
import { useUser, type UseUserReturn } from '../hooks/useUser.js';
import { useProjects, type UseProjectsReturn } from '../hooks/useProjects.js';

// =============================================================================
// Context Types
// =============================================================================

export interface GreyContextValue {
  auth: UseAuthReturn;
  user: UseUserReturn;
  projects: UseProjectsReturn;
}

// =============================================================================
// Context
// =============================================================================

const GreyContext = createContext<GreyContextValue | null>(null);

// =============================================================================
// Provider
// =============================================================================

export interface GreyProviderProps {
  children: ReactNode;
}

export function GreyProvider({ children }: GreyProviderProps): React.JSX.Element {
  const auth = useAuth();
  const user = useUser();
  const projects = useProjects();

  const value = useMemo<GreyContextValue>(
    () => ({
      auth,
      user,
      projects,
    }),
    [auth, user, projects]
  );

  return (
    <GreyContext.Provider value={value}>
      {children}
    </GreyContext.Provider>
  );
}

// =============================================================================
// Context Hooks
// =============================================================================

export function useGreyContext(): GreyContextValue {
  const context = useContext(GreyContext);
  if (!context) {
    throw new Error('useGreyContext must be used within a GreyProvider');
  }
  return context;
}

export function useGreyAuth(): UseAuthReturn {
  return useGreyContext().auth;
}

export function useGreyUser(): UseUserReturn {
  return useGreyContext().user;
}

export function useGreyProjects(): UseProjectsReturn {
  return useGreyContext().projects;
}
