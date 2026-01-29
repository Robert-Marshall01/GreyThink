/**
 * Grey React - GreyProvider
 * 
 * Root provider component that wraps the application and provides
 * Grey state and actions to all child components via React Context.
 */

import React, { useMemo, useCallback, useRef } from 'react';
import { createGreyClient } from '@grey/core-client';
import { GreyContext } from '../context/GreyContext.js';
import { useAuthInternal } from '../hooks/useAuth.js';
import { useUserInternal } from '../hooks/useUser.js';
import { useProjectsInternal } from '../hooks/useProjects.js';
import type { GreyProviderProps, GreyContextValue } from '../types/index.js';

/**
 * GreyProvider Component
 * 
 * Provides Grey authentication, user, and project state to the component tree.
 * Must wrap your application at the root level.
 * 
 * @example
 * ```tsx
 * // Basic usage
 * function App() {
 *   return (
 *     <GreyProvider baseUrl="https://api.grey.example.com">
 *       <YourApp />
 *     </GreyProvider>
 *   );
 * }
 * 
 * // With SSR hydration
 * function App({ initialSession, initialUser }) {
 *   return (
 *     <GreyProvider
 *       baseUrl="https://api.grey.example.com"
 *       initialSession={initialSession}
 *       initialUser={initialUser}
 *     >
 *       <YourApp />
 *     </GreyProvider>
 *   );
 * }
 * ```
 */
export function GreyProvider({
  baseUrl,
  initialSession,
  initialUser,
  children,
}: GreyProviderProps): React.ReactElement {
  // Create Grey client instance (memoized)
  const client = useMemo(() => {
    return createGreyClient({
      baseUrl,
    });
  }, [baseUrl]);
  
  // Query cache for useQuery hook
  const queryCacheRef = useRef(new Map<string, unknown>());
  
  // Initialize auth state
  const auth = useAuthInternal(client, initialSession);
  
  // Initialize user state (depends on auth session)
  const user = useUserInternal(client, auth.session, initialUser);
  
  // Initialize projects state (depends on auth session)
  const projects = useProjectsInternal(client, auth.session);
  
  /**
   * Invalidate queries by key
   */
  const invalidateQueries = useCallback((key: string | string[]) => {
    const normalizedKey = Array.isArray(key) ? key.join(':') : key;
    
    // Remove exact match
    queryCacheRef.current.delete(normalizedKey);
    
    // Remove keys that start with the prefix
    for (const cachedKey of queryCacheRef.current.keys()) {
      if (cachedKey.startsWith(normalizedKey)) {
        queryCacheRef.current.delete(cachedKey);
      }
    }
    
    // TODO: Trigger re-fetch for active queries with this key
    // This would require a subscription system
  }, []);
  
  // Build context value
  const contextValue = useMemo<GreyContextValue>(() => ({
    baseUrl,
    auth,
    user,
    projects,
    queryCache: queryCacheRef.current,
    invalidateQueries,
  }), [baseUrl, auth, user, projects, invalidateQueries]);
  
  return (
    <GreyContext.Provider value={contextValue}>
      {children}
    </GreyContext.Provider>
  );
}

/**
 * Higher-order component to wrap a component with GreyProvider
 * 
 * @example
 * ```tsx
 * const App = withGrey({ baseUrl: 'https://api.grey.example.com' })(MyApp);
 * ```
 */
export function withGrey(providerProps: Omit<GreyProviderProps, 'children'>) {
  return function <P extends object>(WrappedComponent: React.ComponentType<P>) {
    function WithGreyWrapper(props: P) {
      return (
        <GreyProvider {...providerProps}>
          <WrappedComponent {...props} />
        </GreyProvider>
      );
    }
    
    WithGreyWrapper.displayName = `WithGrey(${WrappedComponent.displayName || WrappedComponent.name || 'Component'})`;
    
    return WithGreyWrapper;
  };
}