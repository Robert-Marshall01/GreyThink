/**
 * Grey Core Client - Auth Helpers
 *
 * Utilities for managing authentication state and tokens.
 */

import type { AuthSession } from "./generated/schema.js";

// =============================================================================
// JWT UTILITIES
// =============================================================================

/**
 * JWT claims from a Grey access token.
 */
export interface GreyJwtClaims {
  /** User ID */
  user_id: string;
  /** Organization ID */
  organization_id: string;
  /** User email */
  email: string;
  /** User role (if included) */
  role?: string;
  /** Expiration timestamp (Unix seconds) */
  exp: number;
  /** Issued at timestamp (Unix seconds) */
  iat: number;
  /** Issuer */
  iss?: string;
  /** Subject (typically user ID) */
  sub: string;
}

/**
 * Parse a JWT token and extract the payload.
 * NOTE: This does NOT verify the token signature - only use for reading claims.
 *
 * @param token - JWT token string
 * @returns Parsed token payload or null if invalid
 */
export function parseJwt<T = Record<string, unknown>>(token: string): T | null {
  try {
    const parts = token.split(".");
    if (parts.length !== 3) {
      return null;
    }

    // Decode base64url to base64
    const base64 = parts[1].replace(/-/g, "+").replace(/_/g, "/");
    // Pad if necessary
    const padded = base64 + "=".repeat((4 - (base64.length % 4)) % 4);

    // Decode and parse
    const decoded = atob(padded);
    return JSON.parse(decoded) as T;
  } catch {
    return null;
  }
}

/**
 * Extract claims from a Grey access token.
 *
 * @param accessToken - JWT access token
 * @returns Token claims or null if invalid
 */
export function getTokenClaims(accessToken: string): GreyJwtClaims | null {
  return parseJwt<GreyJwtClaims>(accessToken);
}

/**
 * Get the user ID from an access token.
 *
 * @param accessToken - JWT access token
 * @returns User ID or null if invalid
 */
export function getUserIdFromToken(accessToken: string): string | null {
  const claims = getTokenClaims(accessToken);
  return claims?.user_id ?? claims?.sub ?? null;
}

/**
 * Get the organization ID from an access token.
 *
 * @param accessToken - JWT access token
 * @returns Organization ID or null if invalid
 */
export function getOrganizationIdFromToken(accessToken: string): string | null {
  const claims = getTokenClaims(accessToken);
  return claims?.organization_id ?? null;
}

/**
 * Check if a token is expired based on its exp claim.
 *
 * @param accessToken - JWT access token
 * @param bufferSeconds - Seconds before actual expiry to consider expired (default: 60)
 * @returns true if expired or will expire within buffer
 */
export function isTokenExpiredFromJwt(
  accessToken: string,
  bufferSeconds = 60
): boolean {
  const claims = getTokenClaims(accessToken);
  if (!claims?.exp) {
    return true; // Treat invalid tokens as expired
  }
  const expiresAt = claims.exp * 1000; // Convert to milliseconds
  const now = Date.now();
  return now >= expiresAt - bufferSeconds * 1000;
}

// =============================================================================
// TOKEN EXPIRATION HELPERS
// =============================================================================

/**
 * Calculate when a token will expire based on expires_in seconds.
 *
 * @param expiresIn - Number of seconds until expiration
 * @param issuedAt - When the token was issued (default: now)
 * @returns Date when the token expires
 */
export function calculateExpiration(
  expiresIn: number,
  issuedAt: Date = new Date()
): Date {
  return new Date(issuedAt.getTime() + expiresIn * 1000);
}

/**
 * Check if a token should be refreshed based on expires_in.
 *
 * @param expiresIn - Number of seconds until expiration
 * @param issuedAt - When the token was issued
 * @param bufferSeconds - Seconds before expiry to trigger refresh (default: 60)
 * @returns true if the token should be refreshed
 */
export function shouldRefreshToken(
  expiresIn: number,
  issuedAt: Date,
  bufferSeconds = 60
): boolean {
  const expiresAt = calculateExpiration(expiresIn, issuedAt);
  const now = new Date();
  return now.getTime() >= expiresAt.getTime() - bufferSeconds * 1000;
}

// =============================================================================
// BROWSER STORAGE (Optional - for browser environments)
// =============================================================================

/**
 * Storage key names for auth tokens.
 */
export const AUTH_STORAGE_KEYS = {
  ACCESS_TOKEN: "grey_access_token",
  REFRESH_TOKEN: "grey_refresh_token",
  EXPIRES_IN: "grey_expires_in",
  TOKEN_ISSUED_AT: "grey_token_issued_at",
} as const;

/**
 * Browser localStorage token store.
 * Only use in browser environments.
 */
export const browserAuthStorage = {
  /**
   * Store tokens in localStorage.
   */
  setTokens(session: AuthSession): void {
    if (typeof localStorage === "undefined") return;
    localStorage.setItem(AUTH_STORAGE_KEYS.ACCESS_TOKEN, session.access_token);
    localStorage.setItem(AUTH_STORAGE_KEYS.REFRESH_TOKEN, session.refresh_token);
    localStorage.setItem(AUTH_STORAGE_KEYS.EXPIRES_IN, String(session.expires_in));
    localStorage.setItem(AUTH_STORAGE_KEYS.TOKEN_ISSUED_AT, String(Date.now()));
  },

  /**
   * Get the access token from localStorage.
   */
  getAccessToken(): string | null {
    if (typeof localStorage === "undefined") return null;
    return localStorage.getItem(AUTH_STORAGE_KEYS.ACCESS_TOKEN);
  },

  /**
   * Get the refresh token from localStorage.
   */
  getRefreshToken(): string | null {
    if (typeof localStorage === "undefined") return null;
    return localStorage.getItem(AUTH_STORAGE_KEYS.REFRESH_TOKEN);
  },

  /**
   * Get all stored session data.
   */
  getSession(): AuthSession | null {
    const accessToken = this.getAccessToken();
    const refreshToken = this.getRefreshToken();
    const expiresIn = localStorage.getItem(AUTH_STORAGE_KEYS.EXPIRES_IN);

    if (!accessToken || !refreshToken || !expiresIn) {
      return null;
    }

    return {
      access_token: accessToken,
      refresh_token: refreshToken,
      expires_in: parseInt(expiresIn, 10),
    };
  },

  /**
   * Clear all tokens from localStorage.
   */
  clearTokens(): void {
    if (typeof localStorage === "undefined") return;
    localStorage.removeItem(AUTH_STORAGE_KEYS.ACCESS_TOKEN);
    localStorage.removeItem(AUTH_STORAGE_KEYS.REFRESH_TOKEN);
    localStorage.removeItem(AUTH_STORAGE_KEYS.EXPIRES_IN);
    localStorage.removeItem(AUTH_STORAGE_KEYS.TOKEN_ISSUED_AT);
  },

  /**
   * Check if tokens are stored and not expired.
   */
  hasValidTokens(): boolean {
    const accessToken = this.getAccessToken();
    const expiresIn = localStorage.getItem(AUTH_STORAGE_KEYS.EXPIRES_IN);
    const issuedAt = localStorage.getItem(AUTH_STORAGE_KEYS.TOKEN_ISSUED_AT);

    if (!accessToken || !expiresIn || !issuedAt) {
      return false;
    }

    return !shouldRefreshToken(
      parseInt(expiresIn, 10),
      new Date(parseInt(issuedAt, 10))
    );
  },

  /**
   * Check if the user is authenticated (has any tokens).
   */
  isAuthenticated(): boolean {
    return this.getAccessToken() !== null;
  },
};

// =============================================================================
// AUTH STATE HELPERS
// =============================================================================

/**
 * Create an auth state manager for tracking authentication status.
 */
export interface AuthState {
  isAuthenticated: boolean;
  accessToken: string | null;
  refreshToken: string | null;
  expiresIn: number | null;
  issuedAt: Date | null;
}

/**
 * Create initial auth state.
 */
export function createAuthState(): AuthState {
  return {
    isAuthenticated: false,
    accessToken: null,
    refreshToken: null,
    expiresIn: null,
    issuedAt: null,
  };
}

/**
 * Update auth state with a new session.
 */
export function updateAuthState(
  _state: AuthState,
  session: AuthSession
): AuthState {
  return {
    isAuthenticated: true,
    accessToken: session.access_token,
    refreshToken: session.refresh_token,
    expiresIn: session.expires_in,
    issuedAt: new Date(),
  };
}

/**
 * Clear auth state (logout).
 */
export function clearAuthState(): AuthState {
  return createAuthState();
}
