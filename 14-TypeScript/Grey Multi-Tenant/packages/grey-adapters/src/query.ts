/**
 * Grey Adapters - Query Core
 *
 * Framework-agnostic async state management for API queries.
 * Similar to React Query / TanStack Query patterns.
 */

/**
 * Query state
 */
export interface QueryState<T> {
  data: T | null;
  isLoading: boolean;
  isError: boolean;
  error: Error | null;
  isSuccess: boolean;
}

/**
 * Initial query state
 */
export function initialQueryState<T>(): QueryState<T> {
  return {
    data: null,
    isLoading: false,
    isError: false,
    error: null,
    isSuccess: false,
  };
}

/**
 * Mutation state
 */
export interface MutationState<T> {
  data: T | null;
  isLoading: boolean;
  isError: boolean;
  error: Error | null;
  isSuccess: boolean;
}

/**
 * Initial mutation state
 */
export function initialMutationState<T>(): MutationState<T> {
  return {
    data: null,
    isLoading: false,
    isError: false,
    error: null,
    isSuccess: false,
  };
}

/**
 * Query options
 */
export interface QueryOptions<T> {
  queryFn: () => Promise<T>;
  enabled?: boolean;
  onSuccess?: (data: T) => void;
  onError?: (error: Error) => void;
}

/**
 * Mutation options
 */
export interface MutationOptions<TData, TVariables> {
  mutationFn: (variables: TVariables) => Promise<TData>;
  onSuccess?: (data: TData, variables: TVariables) => void;
  onError?: (error: Error, variables: TVariables) => void;
  onSettled?: (data: TData | null, error: Error | null, variables: TVariables) => void;
}

/**
 * Query controller for managing async data fetching.
 */
export class QueryController<T> {
  private state: QueryState<T> = initialQueryState();
  private listeners: Set<(state: QueryState<T>) => void> = new Set();
  private options: QueryOptions<T>;

  constructor(options: QueryOptions<T>) {
    this.options = options;
  }

  /**
   * Get current state
   */
  getState(): QueryState<T> {
    return { ...this.state };
  }

  /**
   * Subscribe to state changes
   */
  subscribe(listener: (state: QueryState<T>) => void): () => void {
    this.listeners.add(listener);
    listener(this.state);
    return () => this.listeners.delete(listener);
  }

  /**
   * Update state and notify
   */
  private setState(updates: Partial<QueryState<T>>): void {
    this.state = { ...this.state, ...updates };
    this.listeners.forEach((l) => l(this.state));
  }

  /**
   * Execute the query
   */
  async execute(): Promise<T | null> {
    if (this.options.enabled === false) {
      return null;
    }

    this.setState({ isLoading: true, isError: false, error: null });

    try {
      const data = await this.options.queryFn();
      this.setState({
        data,
        isLoading: false,
        isSuccess: true,
      });
      this.options.onSuccess?.(data);
      return data;
    } catch (err) {
      const error = err instanceof Error ? err : new Error(String(err));
      this.setState({
        isLoading: false,
        isError: true,
        error,
        isSuccess: false,
      });
      this.options.onError?.(error);
      return null;
    }
  }

  /**
   * Refetch data
   */
  async refetch(): Promise<T | null> {
    return this.execute();
  }
}

/**
 * Mutation controller for managing async mutations.
 */
export class MutationController<TData, TVariables> {
  private state: MutationState<TData> = initialMutationState();
  private listeners: Set<(state: MutationState<TData>) => void> = new Set();
  private options: MutationOptions<TData, TVariables>;

  constructor(options: MutationOptions<TData, TVariables>) {
    this.options = options;
  }

  /**
   * Get current state
   */
  getState(): MutationState<TData> {
    return { ...this.state };
  }

  /**
   * Subscribe to state changes
   */
  subscribe(listener: (state: MutationState<TData>) => void): () => void {
    this.listeners.add(listener);
    listener(this.state);
    return () => this.listeners.delete(listener);
  }

  /**
   * Update state and notify
   */
  private setState(updates: Partial<MutationState<TData>>): void {
    this.state = { ...this.state, ...updates };
    this.listeners.forEach((l) => l(this.state));
  }

  /**
   * Execute the mutation
   */
  async mutate(variables: TVariables): Promise<TData | null> {
    this.setState({ isLoading: true, isError: false, error: null, isSuccess: false });

    try {
      const data = await this.options.mutationFn(variables);
      this.setState({
        data,
        isLoading: false,
        isSuccess: true,
      });
      this.options.onSuccess?.(data, variables);
      this.options.onSettled?.(data, null, variables);
      return data;
    } catch (err) {
      const error = err instanceof Error ? err : new Error(String(err));
      this.setState({
        isLoading: false,
        isError: true,
        error,
        isSuccess: false,
      });
      this.options.onError?.(error, variables);
      this.options.onSettled?.(null, error, variables);
      return null;
    }
  }

  /**
   * Reset mutation state
   */
  reset(): void {
    this.setState(initialMutationState());
  }
}

/**
 * Create a query controller
 */
export function createQuery<T>(options: QueryOptions<T>): QueryController<T> {
  return new QueryController(options);
}

/**
 * Create a mutation controller
 */
export function createMutation<TData, TVariables>(
  options: MutationOptions<TData, TVariables>
): MutationController<TData, TVariables> {
  return new MutationController(options);
}
