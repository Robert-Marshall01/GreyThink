/**
 * Grey Core Client - Error Helpers
 *
 * Utilities for handling API errors in a consistent way.
 * Matches the ErrorResponse schema from the OpenAPI contract:
 * { error: string, message: string }
 */

import type { ErrorResponse } from "./generated/schema.js";

// =============================================================================
// ERROR CLASS
// =============================================================================

/**
 * Custom error class for Grey API errors.
 *
 * @example
 * ```typescript
 * try {
 *   await client.users.me();
 * } catch (e) {
 *   if (e instanceof GreyApiError && e.isUnauthorized()) {
 *     // Redirect to login
 *   }
 * }
 * ```
 */
export class GreyApiError extends Error {
  /** HTTP status code */
  readonly status: number;
  /** Error code from the API (e.g., "not_found", "validation_error") */
  readonly code: string;
  /** Original response (for advanced handling) */
  readonly response?: Response;

  constructor(
    message: string,
    status: number,
    code: string,
    response?: Response
  ) {
    super(message);
    this.name = "GreyApiError";
    this.status = status;
    this.code = code;
    this.response = response;

    // Maintains proper stack trace for where error was thrown
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, GreyApiError);
    }
  }

  /**
   * Check if the error is an authentication error (401).
   */
  isUnauthorized(): boolean {
    return this.status === 401;
  }

  /**
   * Check if the error is a permission error (403).
   */
  isForbidden(): boolean {
    return this.status === 403;
  }

  /**
   * Check if the error is a not found error (404).
   */
  isNotFound(): boolean {
    return this.status === 404;
  }

  /**
   * Check if the error is a validation error (400).
   */
  isValidationError(): boolean {
    return this.status === 400;
  }

  /**
   * Check if the error is a conflict error (409 - e.g., duplicate resource).
   */
  isConflict(): boolean {
    return this.status === 409;
  }

  /**
   * Check if the error is a server error (5xx).
   */
  isServerError(): boolean {
    return this.status >= 500;
  }

  /**
   * Convert to a plain object for serialization.
   */
  toJSON(): { status: number; code: string; message: string } {
    return {
      status: this.status,
      code: this.code,
      message: this.message,
    };
  }
}

// =============================================================================
// TYPE GUARDS
// =============================================================================

/**
 * Type guard to check if a value is an ErrorResponse from the API.
 * ErrorResponse schema: { error: string, message: string }
 */
export function isErrorResponse(value: unknown): value is ErrorResponse {
  return (
    typeof value === "object" &&
    value !== null &&
    "error" in value &&
    "message" in value &&
    typeof (value as ErrorResponse).error === "string" &&
    typeof (value as ErrorResponse).message === "string"
  );
}

/**
 * Type guard to check if an error is a GreyApiError.
 */
export function isGreyApiError(error: unknown): error is GreyApiError {
  return error instanceof GreyApiError;
}

// =============================================================================
// ERROR PARSING
// =============================================================================

/**
 * Parse an error response into a GreyApiError.
 *
 * @param response - The fetch Response object
 * @returns A GreyApiError with parsed details
 */
export async function parseErrorResponse(
  response: Response
): Promise<GreyApiError> {
  try {
    const body = await response.json();

    if (isErrorResponse(body)) {
      return new GreyApiError(
        body.message,
        response.status,
        body.error,
        response
      );
    }

    // Fallback for non-standard error responses
    return new GreyApiError(
      "An unexpected error occurred",
      response.status,
      response.statusText || "unknown_error",
      response
    );
  } catch {
    // Failed to parse JSON
    return new GreyApiError(
      "Failed to parse error response",
      response.status,
      "parse_error",
      response
    );
  }
}

/**
 * Create a GreyApiError from an ErrorResponse object.
 */
export function fromErrorResponse(
  errorResponse: ErrorResponse,
  status: number
): GreyApiError {
  return new GreyApiError(errorResponse.message, status, errorResponse.error);
}

// =============================================================================
// ERROR MESSAGE EXTRACTION
// =============================================================================

/**
 * Extract a user-friendly error message from various error types.
 *
 * @param error - Any error value
 * @returns A human-readable error message
 */
export function getErrorMessage(error: unknown): string {
  if (error instanceof GreyApiError) {
    return error.message;
  }

  if (error instanceof Error) {
    return error.message;
  }

  if (typeof error === "string") {
    return error;
  }

  if (isErrorResponse(error)) {
    return error.message;
  }

  return "An unknown error occurred";
}

/**
 * Extract the error code from various error types.
 *
 * @param error - Any error value
 * @returns The error code or "unknown"
 */
export function getErrorCode(error: unknown): string {
  if (error instanceof GreyApiError) {
    return error.code;
  }

  if (isErrorResponse(error)) {
    return error.error;
  }

  return "unknown";
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

/**
 * Check if an error indicates the user is not authenticated.
 */
export function isUnauthorized(error: unknown): boolean {
  if (error instanceof GreyApiError) {
    return error.isUnauthorized();
  }
  return false;
}

/**
 * Check if an error indicates insufficient permissions.
 */
export function isForbidden(error: unknown): boolean {
  if (error instanceof GreyApiError) {
    return error.isForbidden();
  }
  return false;
}

/**
 * Check if an error indicates the resource was not found.
 */
export function isNotFound(error: unknown): boolean {
  if (error instanceof GreyApiError) {
    return error.isNotFound();
  }
  return false;
}

/**
 * Check if an error indicates a validation error.
 */
export function isValidationError(error: unknown): boolean {
  if (error instanceof GreyApiError) {
    return error.isValidationError();
  }
  return false;
}

/**
 * Check if an error is a server error (5xx).
 */
export function isServerError(error: unknown): boolean {
  if (error instanceof GreyApiError) {
    return error.isServerError();
  }
  return false;
}

// =============================================================================
// ERROR CODES
// =============================================================================

/**
 * Common error codes returned by the API.
 */
export const ErrorCodes = {
  BAD_REQUEST: "bad_request",
  VALIDATION_ERROR: "validation_error",
  UNAUTHORIZED: "unauthorized",
  INVALID_CREDENTIALS: "invalid_credentials",
  INVALID_TOKEN: "invalid_token",
  FORBIDDEN: "forbidden",
  NOT_FOUND: "not_found",
  CONFLICT: "conflict",
  EMAIL_EXISTS: "email_exists",
  INTERNAL_ERROR: "internal_error",
} as const;

export type ErrorCode = (typeof ErrorCodes)[keyof typeof ErrorCodes];
