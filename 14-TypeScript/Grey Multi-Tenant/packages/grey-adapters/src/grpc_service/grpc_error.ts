/**
 * Grey Integration Layer - gRPC Error Normalizer
 *
 * Converts errors to gRPC status codes and normalized error shape.
 */

import { normalizeError, toGrpcStatus, fromGrpcStatus, type NormalizedError } from '../error_normalizer';

// gRPC status codes
export enum GrpcStatus {
  OK = 0,
  CANCELLED = 1,
  UNKNOWN = 2,
  INVALID_ARGUMENT = 3,
  DEADLINE_EXCEEDED = 4,
  NOT_FOUND = 5,
  ALREADY_EXISTS = 6,
  PERMISSION_DENIED = 7,
  RESOURCE_EXHAUSTED = 8,
  FAILED_PRECONDITION = 9,
  ABORTED = 10,
  OUT_OF_RANGE = 11,
  UNIMPLEMENTED = 12,
  INTERNAL = 13,
  UNAVAILABLE = 14,
  DATA_LOSS = 15,
  UNAUTHENTICATED = 16,
}

/**
 * gRPC error with status code.
 */
export interface GrpcError {
  code: GrpcStatus;
  message: string;
  details: NormalizedError;
}

/**
 * Create a gRPC error from any error.
 */
export function createGrpcError(error: unknown): GrpcError {
  const normalized = normalizeError(error);
  return {
    code: toGrpcStatus(normalized.code) as GrpcStatus,
    message: normalized.message,
    details: normalized,
  };
}

/**
 * Create a gRPC error from status code and message.
 */
export function grpcError(code: GrpcStatus, message: string, details: string = ''): GrpcError {
  const errorCode = fromGrpcStatus(code);
  return {
    code,
    message,
    details: {
      code: errorCode,
      message,
      details,
    },
  };
}

/**
 * Throw a gRPC error (for use in service implementations).
 */
export function throwGrpcError(code: GrpcStatus, message: string, details: string = ''): never {
  const error = grpcError(code, message, details);
  const err = new Error(message) as Error & { code: GrpcStatus; details: NormalizedError };
  err.code = error.code;
  err.details = error.details;
  throw err;
}

// Convenience error creators
export const grpcErrors = {
  unauthenticated: (message: string = 'Authentication required') =>
    grpcError(GrpcStatus.UNAUTHENTICATED, message),

  permissionDenied: (message: string = 'Permission denied') =>
    grpcError(GrpcStatus.PERMISSION_DENIED, message),

  notFound: (message: string = 'Resource not found') =>
    grpcError(GrpcStatus.NOT_FOUND, message),

  invalidArgument: (message: string, field: string = '') =>
    grpcError(GrpcStatus.INVALID_ARGUMENT, message, field),

  internal: (message: string = 'Internal server error') =>
    grpcError(GrpcStatus.INTERNAL, message),

  unavailable: (message: string = 'Service unavailable') =>
    grpcError(GrpcStatus.UNAVAILABLE, message),

  deadlineExceeded: (message: string = 'Request timed out') =>
    grpcError(GrpcStatus.DEADLINE_EXCEEDED, message),
};
