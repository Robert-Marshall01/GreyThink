/**
 * Grey Solid - GreyProvider Component
 *
 * Provider component that initializes Grey context and makes
 * all signals available to child components.
 *
 * @example
 * ```tsx
 * import { GreyProvider } from '@grey/solid';
 *
 * function App() {
 *   return (
 *     <GreyProvider
 *       baseUrl="http://localhost:8080/api/v1"
 *       autoRestoreSession={true}
 *       onLogout={() => navigate('/login')}
 *     >
 *       <Router />
 *     </GreyProvider>
 *   );
 * }
 * ```
 */

import type { ParentProps, JSX } from 'solid-js';
import { onMount } from 'solid-js';
import { GreyContextValue, createGreyContext, isBrowser } from '../context/index.js';
import type { GreyConfig } from '../types/index.js';

/**
 * Props for GreyProvider component
 */
export interface GreyProviderProps extends GreyConfig {
  /** Child components */
  children?: JSX.Element;
}

/**
 * GreyProvider Component
 *
 * Wraps your application to provide Grey context to all child components.
 * This is required for all Grey signals and hooks to work.
 *
 * @param props - Provider configuration
 * @returns Provider component with children
 *
 * @example Basic usage
 * ```tsx
 * import { GreyProvider } from '@grey/solid';
 *
 * function App() {
 *   return (
 *     <GreyProvider baseUrl={import.meta.env.VITE_API_URL}>
 *       <MyApp />
 *     </GreyProvider>
 *   );
 * }
 * ```
 *
 * @example With SSR hydration
 * ```tsx
 * import { GreyProvider } from '@grey/solid';
 *
 * function App(props: { session?: AuthSession; user?: User }) {
 *   return (
 *     <GreyProvider
 *       baseUrl={import.meta.env.VITE_API_URL}
 *       initialSession={props.session}
 *       initialUser={props.user}
 *       autoRestoreSession={false} // Don't restore if we have initial data
 *     >
 *       <MyApp />
 *     </GreyProvider>
 *   );
 * }
 * ```
 *
 * @example With callbacks
 * ```tsx
 * import { GreyProvider } from '@grey/solid';
 * import { useNavigate } from '@solidjs/router';
 *
 * function App() {
 *   const navigate = useNavigate();
 *
 *   return (
 *     <GreyProvider
 *       baseUrl={import.meta.env.VITE_API_URL}
 *       onLogout={() => navigate('/login')}
 *       onAuthChange={(state) => {
 *         if (state.isAuthenticated) {
 *           navigate('/dashboard');
 *         }
 *       }}
 *     >
 *       <MyApp />
 *     </GreyProvider>
 *   );
 * }
 * ```
 */
export function GreyProvider(props: GreyProviderProps) {
  // Create context with config
  const context = createGreyContext({
    baseUrl: props.baseUrl,
    initialSession: props.initialSession,
    initialUser: props.initialUser,
    onAuthChange: props.onAuthChange,
    onLogout: props.onLogout,
    autoRestoreSession: props.autoRestoreSession,
  });

  // Auto-restore session on mount if enabled and in browser
  onMount(() => {
    if (isBrowser() && props.autoRestoreSession !== false) {
      // Restore session in the background
      context.authController.restoreSession().catch(() => {
        // Silently fail - user will need to login
      });
    }

    // If initial user was provided, set it in the user controller
    if (props.initialUser) {
      context.userController.setUser(props.initialUser);
    }
  });

  return (
    <GreyContextValue.Provider value={context}>
      {props.children}
    </GreyContextValue.Provider>
  );
}
