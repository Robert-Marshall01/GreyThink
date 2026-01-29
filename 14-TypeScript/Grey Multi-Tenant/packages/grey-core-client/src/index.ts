/**
 * Grey Core Client - Main Export
 *
 * TypeScript SDK for the Grey Multi-Tenant platform.
 *
 * @packageDocumentation
 */

// =============================================================================
// CLIENT
// =============================================================================

export {
  createGreyClient,
  InMemoryTokenStore,
  type GreyClient,
  type GreyClientConfig,
  type TokenStore,
} from "./client.js";

// =============================================================================
// AUTH HELPERS
// =============================================================================

export {
  // JWT utilities
  parseJwt,
  getTokenClaims,
  getUserIdFromToken,
  getOrganizationIdFromToken,
  isTokenExpiredFromJwt,
  // Expiration helpers
  calculateExpiration,
  shouldRefreshToken,
  // Browser storage
  browserAuthStorage,
  AUTH_STORAGE_KEYS,
  // Auth state
  createAuthState,
  updateAuthState,
  clearAuthState,
  // Types
  type GreyJwtClaims,
  type AuthState,
} from "./auth.js";

// =============================================================================
// ERROR HELPERS
// =============================================================================

export {
  // Error class
  GreyApiError,
  // Type guards
  isErrorResponse,
  isGreyApiError,
  // Parsing
  parseErrorResponse,
  fromErrorResponse,
  // Message extraction
  getErrorMessage,
  getErrorCode,
  // Helper functions
  isUnauthorized,
  isForbidden,
  isNotFound,
  isValidationError,
  isServerError,
  // Constants
  ErrorCodes,
  type ErrorCode,
} from "./errors.js";

// =============================================================================
// GENERATED TYPES
// =============================================================================

export type {
  // Path definitions
  paths as GreyApiPaths,
  // Core schemas
  User,
  Organization,
  Project,
  AuthSession,
  ErrorResponse,
  Pagination,
  // Request schemas
  LoginRequest,
  RefreshTokenRequest,
  CreateUserRequest,
  CreateOrganizationRequest,
  CreateProjectRequest,
  // Response wrappers
  PaginatedResponse,
  ProjectListResponse,
} from "./generated/schema.js";
