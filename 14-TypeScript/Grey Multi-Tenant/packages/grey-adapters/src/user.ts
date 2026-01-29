/**
 * Grey Adapters - User Core
 *
 * Framework-agnostic user management logic.
 */

import type { GreyClient, User } from '@grey/core-client';

/**
 * User state
 */
export interface UserState {
  user: User | null;
  isLoading: boolean;
  error: string | null;
}

/**
 * Initial user state
 */
export const initialUserState: UserState = {
  user: null,
  isLoading: false,
  error: null,
};

/**
 * User controller for framework-agnostic user operations.
 */
export class UserController {
  private client: GreyClient;
  private state: UserState = { ...initialUserState };
  private listeners: Set<(state: UserState) => void> = new Set();

  constructor(client: GreyClient) {
    this.client = client;
  }

  /**
   * Get current user state
   */
  getState(): UserState {
    return { ...this.state };
  }

  /**
   * Subscribe to user state changes
   */
  subscribe(listener: (state: UserState) => void): () => void {
    this.listeners.add(listener);
    listener(this.state);
    return () => this.listeners.delete(listener);
  }

  /**
   * Update state and notify listeners
   */
  private setState(updates: Partial<UserState>): void {
    this.state = { ...this.state, ...updates };
    this.listeners.forEach((listener) => listener(this.state));
  }

  /**
   * Fetch current user
   */
  async fetchCurrentUser(): Promise<User | null> {
    this.setState({ isLoading: true, error: null });

    try {
      const result = await this.client.users.me();

      if (result.error || !result.data) {
        this.setState({
          isLoading: false,
          error: 'Failed to fetch user',
        });
        return null;
      }

      this.setState({
        user: result.data,
        isLoading: false,
      });

      return result.data;
    } catch (err) {
      this.setState({
        isLoading: false,
        error: err instanceof Error ? err.message : 'Failed to fetch user',
      });
      return null;
    }
  }

  /**
   * Set user directly (from auth flow)
   */
  setUser(user: User | null): void {
    this.setState({ user, error: null });
  }

  /**
   * Clear user state
   */
  clear(): void {
    this.setState({ ...initialUserState });
  }

  /**
   * Clear error
   */
  clearError(): void {
    this.setState({ error: null });
  }
}

/**
 * Create a user controller
 */
export function createUserController(client: GreyClient): UserController {
  return new UserController(client);
}
