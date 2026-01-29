/**
 * Grey Solid - Query Signal
 *
 * Generic query signal for data fetching with caching support.
 * Similar to TanStack Query patterns but SolidJS-native.
 *
 * @example
 * ```tsx
 * import { createQuerySignal, useClient } from '@grey/solid';
 *
 * function OrganizationList() {
 *   const client = useClient();
 *
 *   const orgsQuery = createQuerySignal({
 *     key: ['organizations'],
 *     queryFn: async () => {
 *       const result = await client.organizations.list();
 *       return result.data ?? [];
 *     },
 *   });
 *
 *   return (
 *     <Show when={!orgsQuery.isLoading()} fallback={<Spinner />}>
 *       <Show when={orgsQuery.isError()}>
 *         <p class="error">{orgsQuery.error()?.message}</p>
 *       </Show>
 *       <For each={orgsQuery.data() ?? []}>
 *         {(org) => <OrgCard org={org} />}
 *       </For>
 *     </Show>
 *   );
 * }
 * ```
 */

import { createSignal, onCleanup, onMount } from 'solid-js';
import { QueryController } from '@grey/adapters';
import { useGreyContext, isBrowser } from '../context/index.js';
import type { QueryOptions, QueryState, QuerySignal } from '../types/index.js';

/**
 * Normalize query key to string
 */
function normalizeKey(key: string | string[]): string {
  return Array.isArray(key) ? key.join(':') : key;
}

/**
 * Create a query signal for data fetching
 *
 * @param options - Query options
 * @returns Query signal with state accessors and actions
 *
 * @example
 * ```tsx
 * const usersQuery = createQuerySignal({
 *   key: ['users', orgId],
 *   queryFn: async () => {
 *     const result = await client.users.list({ org_id: orgId });
 *     return result.data ?? [];
 *   },
 *   staleTime: 5000, // 5 seconds
 *   onSuccess: (data) => console.log('Loaded:', data.length),
 * });
 * ```
 */
export function createQuerySignal<T>(options: QueryOptions<T>): QuerySignal<T> {
  const { queryCache } = useGreyContext();
  const key = normalizeKey(options.key);

  // Initial state
  const initialState: QueryState<T> = {
    data: options.initialData ?? null,
    isLoading: false,
    isError: false,
    error: null,
    isSuccess: false,
    isStale: true,
  };

  const [state, setState] = createSignal<QueryState<T>>(initialState);

  // Track fetch time for staleness
  let lastFetchedAt: number | null = null;
  let intervalId: ReturnType<typeof setInterval> | undefined;
  let controller: QueryController<T> | null = null;
  let unsubscribe: (() => void) | undefined;

  /**
   * Execute the query
   */
  async function execute(): Promise<T | null> {
    // Check cache first
    if (queryCache.has(key) && lastFetchedAt) {
      const staleTime = options.staleTime ?? 0;
      const isStale = Date.now() - lastFetchedAt > staleTime;
      if (!isStale) {
        const cachedData = queryCache.get(key) as T;
        setState((s: QueryState<T>) => ({ ...s, data: cachedData, isStale: false }));
        return cachedData;
      }
    }

    // Create controller if needed
    if (!controller) {
      controller = new QueryController<T>({
        queryFn: options.queryFn,
        enabled: options.enabled,
        onSuccess: (result: T) => {
          // Cache the result
          queryCache.set(key, result);
          lastFetchedAt = Date.now();
          options.onSuccess?.(result);
        },
        onError: (err: Error) => {
          options.onError?.(err);
        },
      });

      // Subscribe to controller state
      unsubscribe = controller.subscribe((controllerState: { data: T | null; isLoading: boolean; isError: boolean; error: Error | null; isSuccess: boolean }) => {
        setState((s: QueryState<T>) => ({
          ...s,
          data: controllerState.data,
          isLoading: controllerState.isLoading,
          isError: controllerState.isError,
          error: controllerState.error,
          isSuccess: controllerState.isSuccess,
          isStale: lastFetchedAt
            ? Date.now() - lastFetchedAt > (options.staleTime ?? 0)
            : true,
        }));
      });
    }

    return controller.execute();
  }

  /**
   * Refetch the query
   */
  async function refetch(): Promise<T | null> {
    if (!controller) {
      return execute();
    }
    setState((s: QueryState<T>) => ({ ...s, isLoading: true }));
    return controller.refetch();
  }

  // Cleanup on unmount
  onCleanup(() => {
    unsubscribe?.();
    if (intervalId) {
      clearInterval(intervalId);
    }
  });

  // Execute on mount if enabled and in browser
  onMount(() => {
    if (isBrowser() && options.enabled !== false) {
      execute();
    }

    // Set up refetch interval if specified
    if (isBrowser() && options.refetchInterval && options.refetchInterval > 0) {
      intervalId = setInterval(() => {
        if (options.enabled !== false) {
          refetch();
        }
      }, options.refetchInterval);
    }
  });

  // Derived accessors
  const data = () => state().data;
  const isLoading = () => state().isLoading;
  const isError = () => state().isError;
  const error = () => state().error;
  const isSuccess = () => state().isSuccess;

  return {
    state,
    data,
    isLoading,
    isError,
    error,
    isSuccess,
    refetch,
  };
}

/**
 * Prefetch data and populate the cache without creating a signal
 *
 * @param options - Query options (without enabled)
 * @returns Promise that resolves when prefetch completes
 *
 * @example
 * ```tsx
 * // Prefetch on hover
 * function ProjectLink(props: { id: string }) {
 *   const client = useClient();
 *
 *   function onHover() {
 *     prefetchQuery({
 *       key: ['project', props.id],
 *       queryFn: () => client.projects.get(props.id).then(r => r.data),
 *     });
 *   }
 *
 *   return <a onMouseEnter={onHover} href={`/projects/${props.id}`}>View</a>;
 * }
 * ```
 */
export async function prefetchQuery<T>(
  options: Omit<QueryOptions<T>, 'enabled'>
): Promise<void> {
  const { queryCache } = useGreyContext();
  const key = normalizeKey(options.key);

  // Skip if already cached
  if (queryCache.has(key)) {
    return;
  }

  try {
    const data = await options.queryFn();
    queryCache.set(key, data);
    options.onSuccess?.(data);
  } catch (err) {
    // Silently fail prefetch but call error callback
    options.onError?.(err instanceof Error ? err : new Error(String(err)));
  }
}

/**
 * Invalidate queries by key prefix
 *
 * @param key - Query key or key prefix to invalidate
 *
 * @example
 * ```tsx
 * // Invalidate all project queries after mutation
 * async function deleteProject(id: string) {
 *   await client.projects.delete(id);
 *   invalidateQueries('projects');
 * }
 * ```
 */
export function invalidateQueries(key: string | string[]): void {
  const { invalidateQueries: invalidate } = useGreyContext();
  invalidate(key);
}
