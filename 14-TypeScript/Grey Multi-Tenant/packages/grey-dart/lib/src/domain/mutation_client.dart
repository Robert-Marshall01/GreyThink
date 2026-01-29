/// Mutation domain client for Grey SDK.
library;

import 'dart:async';

import 'package:grpc/grpc.dart';

import '../error/grey_error.dart';
import '../grpc/grey_channel.dart';
import '../grpc/mutation_grpc_service.dart';
import '../grpc/types.dart';

/// Client for generic mutation operations.
class MutationClient {
  /// Creates a new mutation client.
  MutationClient(this._channel, this._service);

  final GreyChannel _channel;
  final MutationGrpcService _service;

  /// Executes a mutation against the specified endpoint.
  ///
  /// [endpoint] is the mutation endpoint path.
  /// [method] is the HTTP method equivalent (default: 'POST').
  /// [body] is the optional request body.
  ///
  /// Throws [GreyError] if not authenticated or on failure.
  Future<MutationData> mutate({
    required String endpoint,
    String method = 'POST',
    Object? body,
  }) async {
    // Validate inputs
    if (endpoint.isEmpty) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Endpoint is required',
      );
    }

    // Validate method
    const validMethods = ['POST', 'PUT', 'PATCH', 'DELETE'];
    if (!validMethods.contains(method.toUpperCase())) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Method must be one of: ${validMethods.join(', ')}',
      );
    }

    // Check authentication
    if (!_channel.isAuthenticated) {
      throw GreyError(
        code: GreyError.unauthorized,
        message: 'Not authenticated',
      );
    }

    try {
      return await _service.mutate(
        endpoint: endpoint,
        method: method.toUpperCase(),
        body: body,
      );
    } on GrpcError catch (e) {
      throw GreyError.fromGrpcError(e);
    } catch (e) {
      throw GreyError.fromException(e);
    }
  }
}
