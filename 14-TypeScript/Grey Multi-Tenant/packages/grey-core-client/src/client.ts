/**
 * Grey Core Client - API Client
 *
 * Provides a typed API client for the Grey Multi-Tenant platform.
 * Uses openapi-fetch for type-safe API calls.
 */

import createClient, { type Middleware } from "openapi-fetch";
import type {
  paths,
  AuthSession,
  CreateUserRequest,
  CreateOrganizationRequest,
  CreateProjectRequest,
} from "./generated/schema.js";

// =============================================================================
// CONFIGURATION
// =============================================================================

/**
 * Default API URL - can be overridden by environment variable or config
 */
const DEFAULT_API_URL = "http://localhost:8080/api/v1";

/**
 * Resolves the API URL from environment or fallback to default.
 * Works in both Node.js and browser environments.
 */
function resolveApiUrl(): string {
  // Node.js environment
  if (typeof process !== "undefined" && process.env?.GREY_API_URL) {
    return process.env.GREY_API_URL;
  }
  // Browser environment with Vite
  if (typeof import.meta !== "undefined" && (import.meta as any).env?.VITE_GREY_API_URL) {
    return (import.meta as any).env.VITE_GREY_API_URL;
  }
  return DEFAULT_API_URL;
}

/**
 * Configuration options for the Grey API client.
 */
export interface GreyClientConfig {
  /** Base URL for the API (e.g., "http://localhost:8080/api/v1"). Defaults to GREY_API_URL env var or localhost. */
  baseUrl?: string;
  /** Optional initial access token (for SSR or restoration from storage) */
  accessToken?: string;
  /** Optional organization ID to scope all requests */
  organizationId?: string;
  /** Optional callback when tokens are refreshed */
  onTokenRefresh?: (session: AuthSession) => void;
  /** Optional callback when authentication fails */
  onAuthError?: (error: Error) => void;
  /** Custom fetch implementation */
  fetch?: typeof fetch;
}

// =============================================================================
// TOKEN STORE
// =============================================================================

/**
 * Token storage interface for managing auth tokens.
 */
export interface TokenStore {
  getAccessToken(): string | null;
  getRefreshToken(): string | null;
  getExpiresIn(): number | null;
  setTokens(session: AuthSession): void;
  clearTokens(): void;
  isAuthenticated(): boolean;
}

/**
 * In-memory token store implementation.
 */
export class InMemoryTokenStore implements TokenStore {
  private accessToken: string | null = null;
  private refreshToken: string | null = null;
  private expiresIn: number | null = null;
  private tokenSetAt: number | null = null;

  getAccessToken(): string | null {
    return this.accessToken;
  }

  getRefreshToken(): string | null {
    return this.refreshToken;
  }

  getExpiresIn(): number | null {
    return this.expiresIn;
  }

  setTokens(session: AuthSession): void {
    this.accessToken = session.access_token;
    this.refreshToken = session.refresh_token;
    this.expiresIn = session.expires_in;
    this.tokenSetAt = Date.now();
  }

  clearTokens(): void {
    this.accessToken = null;
    this.refreshToken = null;
    this.expiresIn = null;
    this.tokenSetAt = null;
  }

  isAuthenticated(): boolean {
    if (!this.accessToken || !this.expiresIn || !this.tokenSetAt) {
      return false;
    }
    // Check if token is expired (with 60 second buffer)
    const expiresAt = this.tokenSetAt + this.expiresIn * 1000;
    return Date.now() < expiresAt - 60000;
  }
}

// =============================================================================
// ORGANIZATION STORE
// =============================================================================

/**
 * Organization store for managing the current organization context.
 */
class OrganizationStore {
  private organizationId: string | null = null;

  getOrganizationId(): string | null {
    return this.organizationId;
  }

  setOrganizationId(id: string | null): void {
    this.organizationId = id;
  }
}

// =============================================================================
// AUTH MIDDLEWARE
// =============================================================================

/**
 * Creates an authentication middleware that automatically injects tokens,
 * organization headers, and handles token refresh on 401 responses.
 */
function createAuthMiddleware(
  tokenStore: TokenStore,
  orgStore: OrganizationStore,
  config: GreyClientConfig,
  resolvedBaseUrl: string
): Middleware {
  let isRefreshing = false;

  return {
    async onRequest({ request }) {
      // Add Authorization header
      const token = tokenStore.getAccessToken();
      if (token) {
        request.headers.set("Authorization", `Bearer ${token}`);
      }

      // Add X-Organization-ID header if set
      const orgId = orgStore.getOrganizationId();
      if (orgId) {
        request.headers.set("X-Organization-ID", orgId);
      }

      return request;
    },

    async onResponse({ response }) {
      // Handle 401 errors - attempt token refresh
      if (response.status === 401 && !isRefreshing) {
        const refreshToken = tokenStore.getRefreshToken();
        if (refreshToken) {
          isRefreshing = true;
          try {
            const refreshResponse = await fetch(
              `${resolvedBaseUrl}/auth/refresh`,
              {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({ refresh_token: refreshToken }),
              }
            );

            if (refreshResponse.ok) {
              const responseData = (await refreshResponse.json()) as { data: AuthSession };
              const session = responseData.data;
              tokenStore.setTokens(session);
              config.onTokenRefresh?.(session);
              // Note: In a more advanced implementation, you would retry
              // the original request here with the new token
            } else {
              tokenStore.clearTokens();
              config.onAuthError?.(new Error("Token refresh failed"));
            }
          } catch (error) {
            tokenStore.clearTokens();
            config.onAuthError?.(error as Error);
          } finally {
            isRefreshing = false;
          }
        } else {
          config.onAuthError?.(new Error("No refresh token available"));
        }
      }
      return response;
    },
  };
}

// =============================================================================
// API CLIENT
// =============================================================================

/**
 * Creates a Grey API client instance.
 *
 * @example
 * ```typescript
 * const client = createGreyClient({
 *   baseUrl: "http://localhost:8080/api/v1",
 * });
 *
 * // Login
 * const { data } = await client.auth.login({
 *   email: "user@example.com",
 *   password: "password123",
 * });
 *
 * // Get current user
 * const { data: user } = await client.users.me();
 *
 * // List projects with pagination
 * const { data: response } = await client.projects.list({ page: 1, pageSize: 20 });
 * console.log(response.data, response.pagination);
 * ```
 */
export function createGreyClient(config: GreyClientConfig = {}) {
  // Resolve base URL from config or environment
  const resolvedBaseUrl = config.baseUrl ?? resolveApiUrl();
  
  // Initialize stores
  const tokenStore = new InMemoryTokenStore();
  const orgStore = new OrganizationStore();

  // Apply initial access token if provided
  if (config.accessToken) {
    // Create a minimal session to set the token
    tokenStore.setTokens({
      access_token: config.accessToken,
      refresh_token: "",
      expires_in: 3600, // Default 1 hour
    });
  }

  // Apply initial organization ID if provided
  if (config.organizationId) {
    orgStore.setOrganizationId(config.organizationId);
  }

  const client = createClient<paths>({
    baseUrl: resolvedBaseUrl,
    fetch: config.fetch,
  });

  // Add auth middleware
  client.use(createAuthMiddleware(tokenStore, orgStore, config, resolvedBaseUrl));

  return {
    /**
     * Raw openapi-fetch client for advanced usage.
     */
    raw: client,

    /**
     * Token store for manual token management.
     */
    tokens: tokenStore,

    /**
     * Get the resolved base URL.
     */
    baseUrl: resolvedBaseUrl,

    /**
     * Check if the client is authenticated.
     */
    isAuthenticated(): boolean {
      return tokenStore.isAuthenticated();
    },

    /**
     * Get the current access token.
     */
    getAccessToken(): string | null {
      return tokenStore.getAccessToken();
    },

    /**
     * Set the current organization ID for all subsequent requests.
     */
    setOrganizationId(id: string | null): void {
      orgStore.setOrganizationId(id);
    },

    /**
     * Get the current organization ID.
     */
    getOrganizationId(): string | null {
      return orgStore.getOrganizationId();
    },

    // =========================================================================
    // AUTH ENDPOINTS
    // =========================================================================
    auth: {
      /**
       * Authenticate with email and password.
       * Automatically stores tokens on success.
       */
      async login(credentials: { email: string; password: string }) {
        const response = await client.POST("/auth/login", {
          body: credentials,
        });

        if (response.data?.data) {
          tokenStore.setTokens(response.data.data);
          config.onTokenRefresh?.(response.data.data);
        }

        return response;
      },

      /**
       * Refresh the access token using a refresh token.
       * Automatically stores new tokens on success.
       */
      async refresh(refreshToken?: string) {
        const token = refreshToken ?? tokenStore.getRefreshToken();
        if (!token) {
          throw new Error("No refresh token available");
        }

        const response = await client.POST("/auth/refresh", {
          body: { refresh_token: token },
        });

        if (response.data?.data) {
          tokenStore.setTokens(response.data.data);
          config.onTokenRefresh?.(response.data.data);
        }

        return response;
      },

      /**
       * Clear tokens and log out.
       */
      logout(): void {
        tokenStore.clearTokens();
      },
    },

    // =========================================================================
    // USER ENDPOINTS
    // =========================================================================
    users: {
      /**
       * Create a new user in the current organization.
       */
      async create(user: CreateUserRequest) {
        return client.POST("/users", {
          body: user,
        });
      },

      /**
       * Get the current authenticated user.
       */
      async me() {
        return client.GET("/users/me");
      },
    },

    // =========================================================================
    // ORGANIZATION ENDPOINTS
    // =========================================================================
    organizations: {
      /**
       * Create a new organization.
       */
      async create(org: CreateOrganizationRequest) {
        return client.POST("/organizations", {
          body: org,
        });
      },

      /**
       * Get an organization by ID.
       */
      async get(id: string) {
        return client.GET("/organizations/{id}", {
          params: { path: { id } },
        });
      },
    },

    // =========================================================================
    // PROJECT ENDPOINTS
    // =========================================================================
    projects: {
      /**
       * Create a new project in the current organization.
       */
      async create(project: CreateProjectRequest) {
        return client.POST("/projects", {
          body: project,
        });
      },

      /**
       * List projects in the current organization with pagination.
       */
      async list(options?: { page?: number; pageSize?: number }) {
        return client.GET("/projects", {
          params: {
            query: {
              page: options?.page,
              page_size: options?.pageSize,
            },
          },
        });
      },

      /**
       * Get a project by ID.
       */
      async get(id: string) {
        return client.GET("/projects/{id}", {
          params: { path: { id } },
        });
      },
    },
  };
}

/**
 * Type for the Grey API client.
 */
export type GreyClient = ReturnType<typeof createGreyClient>;
