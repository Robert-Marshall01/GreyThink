/**
 * Organization Context
 * 
 * Manages the active organization for multi-tenancy.
 * Stores selected organization in localStorage for persistence.
 */

import { createContext, useContext, useState, useCallback, useEffect, type ReactNode } from 'react';
import type { Organization } from '@grey/core-client';

// Storage key for persisting active organization
const STORAGE_KEY = 'grey_active_organization';

/**
 * Organization context value shape
 */
interface OrganizationContextValue {
  /** Currently active organization */
  activeOrganization: Organization | null;
  /** Set the active organization */
  setActiveOrganization: (org: Organization | null) => void;
  /** Clear the active organization */
  clearActiveOrganization: () => void;
}

// Create context with undefined default (must be used within provider)
const OrganizationContext = createContext<OrganizationContextValue | undefined>(undefined);

/**
 * Organization Provider Props
 */
interface OrganizationProviderProps {
  children: ReactNode;
}

/**
 * OrganizationProvider Component
 * 
 * Provides active organization state to the component tree.
 * Persists selection to localStorage.
 */
export function OrganizationProvider({ children }: OrganizationProviderProps) {
  // Initialize from localStorage
  const [activeOrganization, setActiveOrganizationState] = useState<Organization | null>(() => {
    if (typeof window === 'undefined') return null;
    
    try {
      const stored = localStorage.getItem(STORAGE_KEY);
      return stored ? JSON.parse(stored) : null;
    } catch {
      return null;
    }
  });

  // Sync to localStorage when organization changes
  useEffect(() => {
    if (typeof window === 'undefined') return;
    
    if (activeOrganization) {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(activeOrganization));
    } else {
      localStorage.removeItem(STORAGE_KEY);
    }
  }, [activeOrganization]);

  const setActiveOrganization = useCallback((org: Organization | null) => {
    setActiveOrganizationState(org);
  }, []);

  const clearActiveOrganization = useCallback(() => {
    setActiveOrganizationState(null);
  }, []);

  return (
    <OrganizationContext.Provider
      value={{
        activeOrganization,
        setActiveOrganization,
        clearActiveOrganization,
      }}
    >
      {children}
    </OrganizationContext.Provider>
  );
}

/**
 * useOrganization Hook
 * 
 * Access active organization state from any component.
 * Must be used within OrganizationProvider.
 * 
 * @example
 * ```tsx
 * function ProjectList() {
 *   const { activeOrganization } = useOrganization();
 *   
 *   if (!activeOrganization) {
 *     return <div>Please select an organization</div>;
 *   }
 *   
 *   // Fetch projects for this organization...
 * }
 * ```
 */
export function useOrganization(): OrganizationContextValue {
  const context = useContext(OrganizationContext);
  
  if (context === undefined) {
    throw new Error('useOrganization must be used within an OrganizationProvider');
  }
  
  return context;
}
