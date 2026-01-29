/**
 * Grey Qwik - useQuery Composable
 *
 * Generic query composable for data fetching with caching support.
 * Similar to TanStack Query patterns but Qwik-native.
 *
 * @example
 * ```tsx
 * import { component$ } from '@builder.io/qwik';
 * import { useQuery, useClient } from '@grey/qwik';
 *
 * export default component$(() => {
 *   const client = useClient();
 *
 *   const orgsQuery = useQuery({
 *     key: ['organizations'],
 *     queryFn: $(async () => {
 *       const result = await client.organizations.list();
 *       return result.data ?? [];
 *     }),
 *   });
 *
 *   return (
 *     <div>
 *       {orgsQuery.isLoading.value ? (
 *         <p>Loading...</p>
 *       ) : orgsQuery.isError.value ? (
 *         <p class="error">{orgsQuery.error.value?.message}</p>
 *       ) : (
 *         <ul>
 *           {orgsQuery.data.value?.map((org) => (
 *             <li key={org.id}>{org.name}</li>
 *           ))}
 *         </ul>
 *       )}
 *     </div>
 *   );
 * });
 * ```
 */

import { useSignal, useVisibleTask$, useTask$, $ } from '@builder.io/qwik';
import { QueryController } from '@grey/adapters';
import { useGreyContext, isBrowser } from '../context/index.js';
import type { QueryOptions, QueryState, QueryComposable } from '../types/index.js';

/**
 * Normalize query key to string
 */
function normalizeKey(key: string | string[]): string {
  return Array.isArray(key) ? key.join(':') : key;
}

/**
 * useQuery Composable
 *
 * Create a query composable for data fetching with caching.
 *
 * @param options - Query options
 * @returns Query composable with state signals and actions
 *
 * @example
 * ```tsx
 * const usersQuery = useQuery({
 *   key: ['users', orgId],
 *   queryFn: $(async () => {
 *     const result = await client.users.list({ org_id: orgId });
 *     return result.data ?? [];
 *   }),
 *   staleTime: 5000, // 5 seconds
 *   onSuccess$: $((data) => console.log('Loaded:', data.length)),
 * });
 * ```
 */
export function useQuery<T>(options: QueryOptions<T>): QueryComposable<T> {
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

  const state = useSignal<QueryState<T>>(initialState);
  const data = useSignal<T | null>(initialState.data);
  const isLoading = useSignal(false);
  const isError = useSignal(false);
  const error = useSignal<Error | null>(null);
  const isSuccess = useSignal(false);

  // Track fetch time for staleness
  let lastFetchedAt: number | null = null;

  /**
   * Execute the query
   */
  const execute = async (): Promise<T | null> => {
    // Check cache first
    if (queryCache.has(key) && lastFetchedAt) {
      const staleTime = options.staleTime ?? 0;
      const isStaleNow = Date.now() - lastFetchedAt > staleTime;
      if (!isStaleNow) {
        const cachedData = queryCache.get(key) as T;
        data.value = cachedData;
        state.value = { ...state.value, data: cachedData, isStale: false };
        return cachedData;
      }
    }

    // Execute query
    isLoading.value = true;
    error.value = null;
    isError.value = false;
    state.value = { ...state.value, isLoading: true, error: null, isError: false };

    try {
      const result = await options.queryFn();

      // Cache the result
      queryCache.set(key, result);
      lastFetchedAt = Date.now();

      // Update signals
      data.value = result;
      isLoading.value = false;
      isSuccess.value = true;
      isError.value = false;
      error.value = null;

      state.value = {
        data: result,
        isLoading: false,
        isError: false,
        error: null,
        isSuccess: true,
        isStale: false,
      };

      // Call success callback
      if (options.onSuccess$) {
        await options.onSuccess$(result);
      }

      return result;
    } catch (err) {
      const errorObj = err instanceof Error ? err : new Error(String(err));

      // Update signals
      isLoading.value = false;
      isError.value = true;
      error.value = errorObj;
      isSuccess.value = false;

      state.value = {
        ...state.value,
        isLoading: false,
        isError: true,
        error: errorObj,
        isSuccess: false,
      };

      // Call error callback
      if (options.onError$) {
        await options.onError$(errorObj);
      }

      return null;
    }
  };

  /**
   * Refetch the query
   */
  const refetch$ = $(async (): Promise<T | null> => {
    return execute();
  });

  // Execute on mount if enabled and in browser
  useVisibleTask$(({ cleanup }: { cleanup: (fn: () => void) => void }) => {
    if (options.enabled !== false) {
      execute();
    }

    // Set up refetch interval if specified
    let intervalId: ReturnType<typeof setInterval> | undefined;
    if (options.refetchInterval && options.refetchInterval > 0) {
      intervalId = setInterval(() => {
        if (options.enabled !== false) {
          execute();
        }
      }, options.refetchInterval);
    }

    cleanup(() => {
      if (intervalId) {
        clearInterval(intervalId);
      }
    });
  });

  return {
    state,
    data,
    isLoading,
    isError,
    error,
    isSuccess,
    refetch$,
  };
}

/**
 * Prefetch data and populate the cache without creating signals
 *
 * @param options - Query options (without enabled)
 * @returns Promise that resolves when prefetch completes
 *
 * @example
 * ```tsx
 * // Prefetch on hover
 * const onHover$ = $(async (projectId: string) => {
 *   await prefetchQuery({
 *     key: ['project', projectId],
 *     queryFn: $(() => client.projects.get(projectId).then(r => r.data)),
 *   });
 * });
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
    if (options.onSuccess$) {
      await options.onSuccess$(data);
    }
  } catch (err) {
    // Silently fail prefetch but call error callback
    if (options.onError$) {
      await options.onError$(err instanceof Error ? err : new Error(String(err)));
    }
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
 * const deleteProject$ = $(async (id: string) => {
 *   await client.projects.delete(id);
 *   invalidateQueries('projects');
 * });
 * ```
 */
export function invalidateQueries(key: string | string[]): void {
  const { invalidateQueries: invalidate } = useGreyContext();
  invalidate(key);
}
