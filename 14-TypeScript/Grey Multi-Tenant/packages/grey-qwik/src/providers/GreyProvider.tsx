/**
 * Grey Qwik - GreyProvider Component
 *
 * Provider component that initializes Grey context and makes
 * all composables available to child components.
 *
 * @example
 * ```tsx
 * import { component$ } from '@builder.io/qwik';
 * import { GreyProvider } from '@grey/qwik';
 *
 * export default component$(() => {
 *   return (
 *     <GreyProvider
 *       baseUrl="http://localhost:8080/api/v1"
 *       autoRestoreSession={true}
 *     >
 *       <App />
 *     </GreyProvider>
 *   );
 * });
 * ```
 */

import {
  component$,
  useContextProvider,
  useVisibleTask$,
  Slot,
} from '@builder.io/qwik';
import { GreyContextId, createGreyContext, isBrowser } from '../context/index.js';
import type { GreyConfig } from '../types/index.js';

/**
 * Props for GreyProvider component
 */
export interface GreyProviderProps extends GreyConfig {}

/**
 * GreyProvider Component
 *
 * Wraps your application to provide Grey context to all child components.
 * This is required for all Grey composables to work.
 *
 * @param props - Provider configuration
 * @returns Provider component with children
 *
 * @example Basic usage
 * ```tsx
 * import { component$ } from '@builder.io/qwik';
 * import { GreyProvider } from '@grey/qwik';
 *
 * export default component$(() => {
 *   return (
 *     <GreyProvider baseUrl={import.meta.env.VITE_API_URL}>
 *       <MyApp />
 *     </GreyProvider>
 *   );
 * });
 * ```
 *
 * @example With SSR hydration
 * ```tsx
 * import { component$ } from '@builder.io/qwik';
 * import { GreyProvider } from '@grey/qwik';
 *
 * export default component$((props: { session?: AuthSession; user?: User }) => {
 *   return (
 *     <GreyProvider
 *       baseUrl={import.meta.env.VITE_API_URL}
 *       initialSession={props.session}
 *       initialUser={props.user}
 *       autoRestoreSession={false}
 *     >
 *       <MyApp />
 *     </GreyProvider>
 *   );
 * });
 * ```
 *
 * @example With callbacks
 * ```tsx
 * import { component$ } from '@builder.io/qwik';
 * import { GreyProvider } from '@grey/qwik';
 * import { useNavigate } from '@builder.io/qwik-city';
 *
 * export default component$(() => {
 *   const nav = useNavigate();
 *
 *   return (
 *     <GreyProvider
 *       baseUrl={import.meta.env.VITE_API_URL}
 *       onLogout={() => nav('/login')}
 *       onAuthChange={(state) => {
 *         if (state.isAuthenticated) {
 *           nav('/dashboard');
 *         }
 *       }}
 *     >
 *       <MyApp />
 *     </GreyProvider>
 *   );
 * });
 * ```
 */
export const GreyProvider = component$<GreyProviderProps>((props) => {
  // Create context with config
  const context = createGreyContext({
    baseUrl: props.baseUrl,
    initialSession: props.initialSession,
    initialUser: props.initialUser,
    onAuthChange: props.onAuthChange,
    onLogout: props.onLogout,
    autoRestoreSession: props.autoRestoreSession,
  });

  // Provide context to children
  useContextProvider(GreyContextId, context);

  // Auto-restore session on mount if enabled and in browser
  // useVisibleTask$ ensures this only runs on the client
  useVisibleTask$(async () => {
    if (isBrowser() && props.autoRestoreSession !== false) {
      // Restore session in the background
      try {
        await context.authController.restoreSession();
      } catch {
        // Silently fail - user will need to login
      }
    }

    // If initial user was provided, set it in the user controller
    if (props.initialUser) {
      context.userController.setUser(props.initialUser);
    }
  });

  return <Slot />;
});
