/**
 * Svelte SFC shims for TypeScript
 *
 * Allows TypeScript to understand .svelte single-file component imports.
 */

declare module '*.svelte' {
  import type { SvelteComponent } from 'svelte';

  const component: typeof SvelteComponent<Record<string, unknown>>;
  export default component;

  // Named exports for module context
  export function getGreyContext(): import('./types/index.js').GreyContextValue;
  export function getGreyContextSafe(): import('./types/index.js').GreyContextValue | null;
}
