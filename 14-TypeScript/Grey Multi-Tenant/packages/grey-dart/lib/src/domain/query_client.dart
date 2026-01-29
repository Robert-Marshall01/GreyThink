/// Query domain client for Grey SDK.
library;

import 'dart:async';

import 'package:grpc/grpc.dart';

import '../error/grey_error.dart';
import '../grpc/grey_channel.dart';
import '../grpc/query_grpc_service.dart';
import '../grpc/types.dart';

/// Client for generic query operations.
class QueryClient {
  /// Creates a new query client.
  QueryClient(this._channel, this._service);

  final GreyChannel _channel;
  final QueryGrpcService _service;

  /// Executes a query against the specified endpoint.
  ///
  /// [endpoint] is the query endpoint path.
  /// [params] are optional query parameters.
  /// [requireAuth] specifies whether authentication is required (default: true).
  ///
  /// Throws [GreyError] on failure.
  Future<QueryData> query({
    required String endpoint,
    Map<String, String>? params,
    bool requireAuth = true,
  }) async {
    // Validate inputs
    if (endpoint.isEmpty) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Endpoint is required',
      );
    }

    // Check authentication if required
    if (requireAuth && !_channel.isAuthenticated) {
      throw GreyError(
        code: GreyError.unauthorized,
        message: 'Not authenticated',
      );
    }

    try {
      return await _service.query(
        endpoint: endpoint,
        params: params,
        requireAuth: requireAuth,
      );
    } on GrpcError catch (e) {
      throw GreyError.fromGrpcError(e);
    } catch (e) {
      throw GreyError.fromException(e);
    }
  }
}
