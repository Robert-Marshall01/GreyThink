/**
 * Grey Integration Layer - Error Normalizer
 *
 * Normalizes all errors into the unified error shape:
 * { code: string, message: string, details: string }
 */

export interface NormalizedError {
  code: string;
  message: string;
  details: string;
}

export enum ErrorCode {
  UNAUTHORIZED = 'unauthorized',
  FORBIDDEN = 'forbidden',
  NOT_FOUND = 'not_found',
  VALIDATION_ERROR = 'validation_error',
  NETWORK_ERROR = 'network_error',
  TIMEOUT = 'timeout',
  SERVER_ERROR = 'server_error',
  UNKNOWN = 'unknown',
}

/**
 * Normalize any error into the standard shape.
 */
export function normalizeError(error: unknown): NormalizedError {
  if (isNormalizedError(error)) {
    return error;
  }

  if (error instanceof Error) {
    return {
      code: inferErrorCode(error),
      message: error.message,
      details: error.stack || '',
    };
  }

  if (typeof error === 'object' && error !== null) {
    const obj = error as Record<string, unknown>;
    return {
      code: String(obj.code || ErrorCode.UNKNOWN),
      message: String(obj.message || 'Unknown error'),
      details: String(obj.details || ''),
    };
  }

  return {
    code: ErrorCode.UNKNOWN,
    message: String(error),
    details: '',
  };
}

/**
 * Type guard for NormalizedError.
 */
export function isNormalizedError(error: unknown): error is NormalizedError {
  if (typeof error !== 'object' || error === null) {
    return false;
  }
  const obj = error as Record<string, unknown>;
  return (
    typeof obj.code === 'string' &&
    typeof obj.message === 'string' &&
    typeof obj.details === 'string'
  );
}

/**
 * Infer error code from Error instance.
 */
function inferErrorCode(error: Error): string {
  const message = error.message.toLowerCase();
  const name = error.name.toLowerCase();

  if (message.includes('unauthorized') || message.includes('unauthenticated')) {
    return ErrorCode.UNAUTHORIZED;
  }
  if (message.includes('forbidden') || message.includes('permission denied')) {
    return ErrorCode.FORBIDDEN;
  }
  if (message.includes('not found')) {
    return ErrorCode.NOT_FOUND;
  }
  if (message.includes('validation') || message.includes('invalid')) {
    return ErrorCode.VALIDATION_ERROR;
  }
  if (message.includes('network') || message.includes('connection')) {
    return ErrorCode.NETWORK_ERROR;
  }
  if (message.includes('timeout') || message.includes('timed out')) {
    return ErrorCode.TIMEOUT;
  }
  if (name.includes('typeerror') || name.includes('referenceerror')) {
    return ErrorCode.SERVER_ERROR;
  }

  return ErrorCode.UNKNOWN;
}

/**
 * Map HTTP status code to error code.
 */
export function fromHttpStatus(status: number): string {
  switch (status) {
    case 401:
      return ErrorCode.UNAUTHORIZED;
    case 403:
      return ErrorCode.FORBIDDEN;
    case 404:
      return ErrorCode.NOT_FOUND;
    case 400:
    case 422:
      return ErrorCode.VALIDATION_ERROR;
    case 408:
    case 504:
      return ErrorCode.TIMEOUT;
    default:
      if (status >= 500) {
        return ErrorCode.SERVER_ERROR;
      }
      return ErrorCode.UNKNOWN;
  }
}

/**
 * Map gRPC status code to error code.
 */
export function fromGrpcStatus(status: number): string {
  switch (status) {
    case 16: // UNAUTHENTICATED
      return ErrorCode.UNAUTHORIZED;
    case 7: // PERMISSION_DENIED
      return ErrorCode.FORBIDDEN;
    case 5: // NOT_FOUND
      return ErrorCode.NOT_FOUND;
    case 3: // INVALID_ARGUMENT
      return ErrorCode.VALIDATION_ERROR;
    case 4: // DEADLINE_EXCEEDED
      return ErrorCode.TIMEOUT;
    case 14: // UNAVAILABLE
      return ErrorCode.NETWORK_ERROR;
    case 13: // INTERNAL
      return ErrorCode.SERVER_ERROR;
    default:
      return ErrorCode.UNKNOWN;
  }
}

/**
 * Map error code to HTTP status.
 */
export function toHttpStatus(code: string): number {
  switch (code) {
    case ErrorCode.UNAUTHORIZED:
      return 401;
    case ErrorCode.FORBIDDEN:
      return 403;
    case ErrorCode.NOT_FOUND:
      return 404;
    case ErrorCode.VALIDATION_ERROR:
      return 400;
    case ErrorCode.TIMEOUT:
      return 408;
    case ErrorCode.NETWORK_ERROR:
      return 503;
    case ErrorCode.SERVER_ERROR:
      return 500;
    default:
      return 500;
  }
}

/**
 * Map error code to gRPC status.
 */
export function toGrpcStatus(code: string): number {
  switch (code) {
    case ErrorCode.UNAUTHORIZED:
      return 16; // UNAUTHENTICATED
    case ErrorCode.FORBIDDEN:
      return 7; // PERMISSION_DENIED
    case ErrorCode.NOT_FOUND:
      return 5; // NOT_FOUND
    case ErrorCode.VALIDATION_ERROR:
      return 3; // INVALID_ARGUMENT
    case ErrorCode.TIMEOUT:
      return 4; // DEADLINE_EXCEEDED
    case ErrorCode.NETWORK_ERROR:
      return 14; // UNAVAILABLE
    case ErrorCode.SERVER_ERROR:
      return 13; // INTERNAL
    default:
      return 2; // UNKNOWN
  }
}
