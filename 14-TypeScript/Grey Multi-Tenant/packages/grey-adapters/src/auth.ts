/**
 * Grey Adapters - Auth State Core
 *
 * Framework-agnostic authentication state management.
 * Each framework adapter wraps this core logic.
 */

import {
  createGreyClient,
  type GreyClient,
  type AuthSession,
  type User,
  shouldRefreshToken,
} from '@grey/core-client';

/**
 * Auth state interface
 */
export interface AuthState {
  user: User | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  error: string | null;
}

/**
 * Initial auth state
 */
export const initialAuthState: AuthState = {
  user: null,
  isAuthenticated: false,
  isLoading: false,
  error: null,
};

/**
 * Auth configuration options
 */
export interface AuthConfig {
  apiBaseUrl: string;
  storage?: TokenStorage;
  onAuthChange?: (state: AuthState) => void;
  onLogout?: () => void;
}

/**
 * Token storage interface for different environments
 */
export interface TokenStorage {
  getAccessToken(): string | null;
  getRefreshToken(): string | null;
  getExpiresIn(): number | null;
  getIssuedAt(): number | null;
  setTokens(session: AuthSession): void;
  clearTokens(): void;
}

/**
 * Browser localStorage implementation
 */
export class BrowserTokenStorage implements TokenStorage {
  private readonly ACCESS_KEY = 'grey_access_token';
  private readonly REFRESH_KEY = 'grey_refresh_token';
  private readonly EXPIRES_IN_KEY = 'grey_token_expires_in';
  private readonly ISSUED_AT_KEY = 'grey_token_issued_at';

  getAccessToken(): string | null {
    if (typeof localStorage === 'undefined') return null;
    return localStorage.getItem(this.ACCESS_KEY);
  }

  getRefreshToken(): string | null {
    if (typeof localStorage === 'undefined') return null;
    return localStorage.getItem(this.REFRESH_KEY);
  }

  getExpiresIn(): number | null {
    if (typeof localStorage === 'undefined') return null;
    const val = localStorage.getItem(this.EXPIRES_IN_KEY);
    return val ? parseInt(val, 10) : null;
  }

  getIssuedAt(): number | null {
    if (typeof localStorage === 'undefined') return null;
    const val = localStorage.getItem(this.ISSUED_AT_KEY);
    return val ? parseInt(val, 10) : null;
  }

  setTokens(session: AuthSession): void {
    if (typeof localStorage === 'undefined') return;
    localStorage.setItem(this.ACCESS_KEY, session.access_token);
    localStorage.setItem(this.REFRESH_KEY, session.refresh_token);
    localStorage.setItem(this.EXPIRES_IN_KEY, String(session.expires_in));
    localStorage.setItem(this.ISSUED_AT_KEY, String(Date.now()));
  }

  clearTokens(): void {
    if (typeof localStorage === 'undefined') return;
    localStorage.removeItem(this.ACCESS_KEY);
    localStorage.removeItem(this.REFRESH_KEY);
    localStorage.removeItem(this.EXPIRES_IN_KEY);
    localStorage.removeItem(this.ISSUED_AT_KEY);
  }
}

/**
 * In-memory token storage (for SSR or testing)
 */
export class MemoryTokenStorage implements TokenStorage {
  private accessToken: string | null = null;
  private refreshToken: string | null = null;
  private expiresIn: number | null = null;
  private issuedAt: number | null = null;

  getAccessToken(): string | null {
    return this.accessToken;
  }

  getRefreshToken(): string | null {
    return this.refreshToken;
  }

  getExpiresIn(): number | null {
    return this.expiresIn;
  }

  getIssuedAt(): number | null {
    return this.issuedAt;
  }

  setTokens(session: AuthSession): void {
    this.accessToken = session.access_token;
    this.refreshToken = session.refresh_token;
    this.expiresIn = session.expires_in;
    this.issuedAt = Date.now();
  }

  clearTokens(): void {
    this.accessToken = null;
    this.refreshToken = null;
    this.expiresIn = null;
    this.issuedAt = null;
  }
}

/**
 * Core auth controller that manages authentication state.
 * This is the framework-agnostic logic that all adapters use.
 */
export class AuthController {
  private client: GreyClient;
  private storage: TokenStorage;
  private state: AuthState = { ...initialAuthState };
  private listeners: Set<(state: AuthState) => void> = new Set();
  private config: AuthConfig;

  constructor(config: AuthConfig) {
    this.config = config;
    this.storage = config.storage ?? new BrowserTokenStorage();

    this.client = createGreyClient({
      baseUrl: config.apiBaseUrl,
      onTokenRefresh: (session) => {
        this.storage.setTokens(session);
      },
      onAuthError: () => {
        this.logout();
      },
    });

    // Restore tokens if available
    const storedSession = this.getStoredSession();
    if (storedSession) {
      this.client.tokens.setTokens(storedSession);
    }
  }

  /**
   * Get stored session from storage
   */
  private getStoredSession(): AuthSession | null {
    const accessToken = this.storage.getAccessToken();
    const refreshToken = this.storage.getRefreshToken();
    const expiresIn = this.storage.getExpiresIn();
    if (!accessToken || !refreshToken || !expiresIn) {
      return null;
    }
    return { access_token: accessToken, refresh_token: refreshToken, expires_in: expiresIn };
  }

  /**
   * Get the current auth state
   */
  getState(): AuthState {
    return { ...this.state };
  }

  /**
   * Get the API client
   */
  getClient(): GreyClient {
    return this.client;
  }

  /**
   * Subscribe to auth state changes
   */
  subscribe(listener: (state: AuthState) => void): () => void {
    this.listeners.add(listener);
    listener(this.state);
    return () => this.listeners.delete(listener);
  }

  /**
   * Update state and notify listeners
   */
  private setState(updates: Partial<AuthState>): void {
    this.state = { ...this.state, ...updates };
    this.listeners.forEach((listener) => listener(this.state));
    this.config.onAuthChange?.(this.state);
  }

  /**
   * Check if there's a valid stored session and restore it
   */
  async restoreSession(): Promise<boolean> {
    const accessToken = this.storage.getAccessToken();
    const expiresIn = this.storage.getExpiresIn();
    const issuedAt = this.storage.getIssuedAt();

    if (!accessToken) {
      return false;
    }

    // Check if token is expired
    if (expiresIn && issuedAt && shouldRefreshToken(expiresIn, new Date(issuedAt))) {
      // Try to refresh
      const refreshToken = this.storage.getRefreshToken();
      if (refreshToken) {
        try {
          await this.client.auth.refresh(refreshToken);
        } catch {
          this.storage.clearTokens();
          return false;
        }
      } else {
        this.storage.clearTokens();
        return false;
      }
    }

    // Fetch current user
    try {
      this.setState({ isLoading: true, error: null });
      const result = await this.client.users.me();

      if (result.data) {
        this.setState({
          user: result.data,
          isAuthenticated: true,
          isLoading: false,
        });
        return true;
      }
    } catch {
      this.storage.clearTokens();
    }

    this.setState({ isLoading: false });
    return false;
  }

  /**
   * Login with email and password
   */
  async login(email: string, password: string): Promise<boolean> {
    this.setState({ isLoading: true, error: null });

    try {
      const loginResult = await this.client.auth.login({ email, password });

      if (loginResult.error || !loginResult.data?.data) {
        this.setState({
          isLoading: false,
          error: 'Invalid credentials',
        });
        return false;
      }

      this.storage.setTokens(loginResult.data.data);

      // Fetch user profile
      const userResult = await this.client.users.me();

      if (userResult.data) {
        this.setState({
          user: userResult.data,
          isAuthenticated: true,
          isLoading: false,
        });
        return true;
      }

      this.setState({
        isLoading: false,
        error: 'Failed to fetch user profile',
      });
      return false;
    } catch (err) {
      this.setState({
        isLoading: false,
        error: err instanceof Error ? err.message : 'Login failed',
      });
      return false;
    }
  }

  /**
   * Logout and clear tokens
   */
  logout(): void {
    this.client.auth.logout();
    this.storage.clearTokens();
    this.setState({
      user: null,
      isAuthenticated: false,
      error: null,
    });
    this.config.onLogout?.();
  }

  /**
   * Clear current error
   */
  clearError(): void {
    this.setState({ error: null });
  }
}
