/**
 * Grey Core Client - TypeScript SDK
 *
 * Auto-generated types from OpenAPI specification.
 * Run `pnpm generate` to regenerate this file from /contracts/openapi.yaml.
 *
 * DO NOT EDIT MANUALLY - This file is generated.
 */

// =============================================================================
// PATH DEFINITIONS (openapi-fetch compatible)
// =============================================================================

export interface paths {
  "/auth/login": {
    post: {
      requestBody: {
        content: {
          "application/json": LoginRequest;
        };
      };
      responses: {
        200: {
          content: {
            "application/json": {
              data: AuthSession;
            };
          };
        };
        400: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        401: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        500: {
          content: {
            "application/json": ErrorResponse;
          };
        };
      };
    };
  };
  "/auth/refresh": {
    post: {
      requestBody: {
        content: {
          "application/json": RefreshTokenRequest;
        };
      };
      responses: {
        200: {
          content: {
            "application/json": {
              data: AuthSession;
            };
          };
        };
        400: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        401: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        500: {
          content: {
            "application/json": ErrorResponse;
          };
        };
      };
    };
  };
  "/users": {
    post: {
      requestBody: {
        content: {
          "application/json": CreateUserRequest;
        };
      };
      responses: {
        201: {
          content: {
            "application/json": User;
          };
        };
        400: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        401: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        409: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        500: {
          content: {
            "application/json": ErrorResponse;
          };
        };
      };
    };
  };
  "/users/me": {
    get: {
      responses: {
        200: {
          content: {
            "application/json": User;
          };
        };
        401: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        500: {
          content: {
            "application/json": ErrorResponse;
          };
        };
      };
    };
  };
  "/organizations": {
    post: {
      requestBody: {
        content: {
          "application/json": CreateOrganizationRequest;
        };
      };
      responses: {
        201: {
          content: {
            "application/json": Organization;
          };
        };
        400: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        401: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        500: {
          content: {
            "application/json": ErrorResponse;
          };
        };
      };
    };
  };
  "/organizations/{id}": {
    get: {
      parameters: {
        path: {
          id: string;
        };
      };
      responses: {
        200: {
          content: {
            "application/json": Organization;
          };
        };
        401: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        404: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        500: {
          content: {
            "application/json": ErrorResponse;
          };
        };
      };
    };
  };
  "/projects": {
    get: {
      parameters: {
        query?: {
          page?: number;
          page_size?: number;
        };
      };
      responses: {
        200: {
          content: {
            "application/json": {
              data: Project[];
              pagination: Pagination;
            };
          };
        };
        401: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        500: {
          content: {
            "application/json": ErrorResponse;
          };
        };
      };
    };
    post: {
      requestBody: {
        content: {
          "application/json": CreateProjectRequest;
        };
      };
      responses: {
        201: {
          content: {
            "application/json": Project;
          };
        };
        400: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        401: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        500: {
          content: {
            "application/json": ErrorResponse;
          };
        };
      };
    };
  };
  "/projects/{id}": {
    get: {
      parameters: {
        path: {
          id: string;
        };
      };
      responses: {
        200: {
          content: {
            "application/json": Project;
          };
        };
        401: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        404: {
          content: {
            "application/json": ErrorResponse;
          };
        };
        500: {
          content: {
            "application/json": ErrorResponse;
          };
        };
      };
    };
  };
}

// =============================================================================
// CORE SCHEMAS - Matching OpenAPI components/schemas
// =============================================================================

/**
 * User resource - a member of an organization.
 */
export interface User {
  /** Unique identifier (UUID) */
  id: string;
  /** Email address */
  email: string;
  /** Display name */
  name: string;
  /** Organization this user belongs to */
  organization_id: string;
  /** Creation timestamp (ISO 8601) */
  created_at: string;
  /** Last update timestamp (ISO 8601) */
  updated_at: string;
}

/**
 * Organization (tenant) - the top-level isolation boundary.
 */
export interface Organization {
  /** Unique identifier (UUID) */
  id: string;
  /** Organization display name */
  name: string;
  /** Creation timestamp (ISO 8601) */
  created_at: string;
  /** Last update timestamp (ISO 8601) */
  updated_at: string;
}

/**
 * Project - a resource within an organization.
 */
export interface Project {
  /** Unique identifier (UUID) */
  id: string;
  /** Organization this project belongs to */
  organization_id: string;
  /** Project name */
  name: string;
  /** Project description (optional) */
  description?: string;
  /** Creation timestamp (ISO 8601) */
  created_at: string;
  /** Last update timestamp (ISO 8601) */
  updated_at: string;
}

/**
 * Authentication session - tokens returned on login/refresh.
 */
export interface AuthSession {
  /** JWT access token for API authorization */
  access_token: string;
  /** Refresh token for obtaining new access tokens */
  refresh_token: string;
  /** Number of seconds until the access token expires */
  expires_in: number;
}

/**
 * Standard error response.
 */
export interface ErrorResponse {
  /** Error code (e.g., "not_found", "validation_error") */
  error: string;
  /** Human-readable error message */
  message: string;
}

/**
 * Pagination metadata.
 */
export interface Pagination {
  /** Current page number (1-indexed) */
  page: number;
  /** Number of items per page */
  page_size: number;
  /** Total number of items across all pages */
  total: number;
}

// =============================================================================
// REQUEST SCHEMAS
// =============================================================================

/**
 * Login credentials.
 */
export interface LoginRequest {
  /** User's email address */
  email: string;
  /** User's password (min 8 characters) */
  password: string;
}

/**
 * Refresh token request.
 */
export interface RefreshTokenRequest {
  /** Refresh token obtained from login */
  refresh_token: string;
}

/**
 * Request to create a new user.
 */
export interface CreateUserRequest {
  /** User's email address */
  email: string;
  /** User's display name */
  name: string;
  /** User's password (min 8 characters) */
  password: string;
}

/**
 * Request to create a new organization.
 */
export interface CreateOrganizationRequest {
  /** Organization display name */
  name: string;
}

/**
 * Request to create a new project.
 */
export interface CreateProjectRequest {
  /** Project name */
  name: string;
  /** Project description (optional) */
  description?: string;
}

// =============================================================================
// RESPONSE WRAPPERS
// =============================================================================

/**
 * Paginated list response.
 */
export interface PaginatedResponse<T> {
  data: T[];
  pagination: Pagination;
}

/**
 * Project list response (alias).
 */
export type ProjectListResponse = PaginatedResponse<Project>;

// =============================================================================
// TYPE ALIASES FOR CONVENIENCE
// =============================================================================

export type GreyApiPaths = paths;
