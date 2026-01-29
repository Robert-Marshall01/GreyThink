/// Error handling types for Grey SDK.
library;

import 'package:grpc/grpc.dart';

/// Normalized error shape for all Grey SDK operations.
class GreyError implements Exception {
  /// Creates a new GreyError.
  const GreyError({
    required this.code,
    required this.message,
    this.details,
  });

  /// Creates a GreyError from a gRPC error.
  factory GreyError.fromGrpcError(GrpcError error) {
    final code = _grpcStatusToCode(error.code);
    return GreyError(
      code: code,
      message: error.message ?? 'Unknown gRPC error',
      details: <String, dynamic>{
        'grpcCode': error.code,
        'grpcCodeName': error.codeName,
        if (error.details != null) 'grpcDetails': error.details,
      },
    );
  }

  /// Creates a GreyError from a generic exception.
  factory GreyError.fromException(Object exception) {
    if (exception is GreyError) {
      return exception;
    }
    if (exception is GrpcError) {
      return GreyError.fromGrpcError(exception);
    }
    return GreyError(
      code: 'UNKNOWN',
      message: exception.toString(),
    );
  }

  /// Error code identifying the type of error.
  final String code;

  /// Human-readable error message.
  final String message;

  /// Optional additional error details.
  final Map<String, dynamic>? details;

  /// Common error codes.
  static const String unauthorized = 'UNAUTHORIZED';
  static const String forbidden = 'FORBIDDEN';
  static const String notFound = 'NOT_FOUND';
  static const String validationError = 'VALIDATION_ERROR';
  static const String networkError = 'NETWORK_ERROR';
  static const String timeout = 'TIMEOUT';
  static const String serverError = 'SERVER_ERROR';
  static const String unknown = 'UNKNOWN';

  @override
  String toString() => 'GreyError(code: $code, message: $message)';

  /// Copies this error with optional modifications.
  GreyError copyWith({
    String? code,
    String? message,
    Map<String, dynamic>? details,
  }) {
    return GreyError(
      code: code ?? this.code,
      message: message ?? this.message,
      details: details ?? this.details,
    );
  }
}

/// Maps gRPC status codes to Grey error codes.
String _grpcStatusToCode(int grpcCode) {
  return switch (grpcCode) {
    StatusCode.unauthenticated => GreyError.unauthorized,
    StatusCode.permissionDenied => GreyError.forbidden,
    StatusCode.notFound => GreyError.notFound,
    StatusCode.invalidArgument => GreyError.validationError,
    StatusCode.failedPrecondition => GreyError.validationError,
    StatusCode.unavailable => GreyError.networkError,
    StatusCode.deadlineExceeded => GreyError.timeout,
    StatusCode.internal => GreyError.serverError,
    StatusCode.unknown => GreyError.unknown,
    _ => GreyError.unknown,
  };
}

/// Result type for operations that can fail.
sealed class GreyResult<T> {
  const GreyResult._();

  /// Creates a success result.
  const factory GreyResult.success(T value) = GreySuccess<T>;

  /// Creates a failure result.
  const factory GreyResult.failure(GreyError error) = GreyFailure<T>;

  /// Whether this result is a success.
  bool get isSuccess => this is GreySuccess<T>;

  /// Whether this result is a failure.
  bool get isFailure => this is GreyFailure<T>;

  /// Gets the value if success, otherwise throws.
  T get value {
    return switch (this) {
      GreySuccess(:final value) => value,
      GreyFailure(:final error) => throw error,
    };
  }

  /// Gets the value if success, otherwise returns null.
  T? get valueOrNull {
    return switch (this) {
      GreySuccess(:final value) => value,
      GreyFailure() => null,
    };
  }

  /// Gets the error if failure, otherwise returns null.
  GreyError? get errorOrNull {
    return switch (this) {
      GreySuccess() => null,
      GreyFailure(:final error) => error,
    };
  }

  /// Maps the success value to a new type.
  GreyResult<U> map<U>(U Function(T value) transform) {
    return switch (this) {
      GreySuccess(:final value) => GreyResult.success(transform(value)),
      GreyFailure(:final error) => GreyResult.failure(error),
    };
  }

  /// Applies a function that returns a result to the success value.
  GreyResult<U> flatMap<U>(GreyResult<U> Function(T value) transform) {
    return switch (this) {
      GreySuccess(:final value) => transform(value),
      GreyFailure(:final error) => GreyResult.failure(error),
    };
  }

  /// Folds the result into a single value.
  U fold<U>({
    required U Function(T value) onSuccess,
    required U Function(GreyError error) onFailure,
  }) {
    return switch (this) {
      GreySuccess(:final value) => onSuccess(value),
      GreyFailure(:final error) => onFailure(error),
    };
  }
}

/// Success variant of GreyResult.
final class GreySuccess<T> extends GreyResult<T> {
  /// Creates a success result with the given value.
  const GreySuccess(this.value) : super._();

  /// The success value.
  @override
  final T value;
}

/// Failure variant of GreyResult.
final class GreyFailure<T> extends GreyResult<T> {
  /// Creates a failure result with the given error.
  const GreyFailure(this.error) : super._();

  /// The error that caused the failure.
  final GreyError error;
}
