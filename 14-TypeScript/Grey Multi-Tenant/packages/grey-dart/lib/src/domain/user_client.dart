/// User domain client for Grey SDK.
library;

import 'dart:async';

import 'package:grpc/grpc.dart';

import '../error/grey_error.dart';
import '../grpc/grey_channel.dart';
import '../grpc/types.dart';
import '../grpc/user_grpc_service.dart';

/// Client for user operations.
class UserClient {
  /// Creates a new user client.
  UserClient(this._channel, this._service);

  final GreyChannel _channel;
  final UserGrpcService _service;

  User? _cachedUser;

  /// Gets the current authenticated user.
  ///
  /// Returns cached data unless [forceRefresh] is true.
  ///
  /// Throws [GreyError] if not authenticated or on failure.
  Future<User> getUser({bool forceRefresh = false}) async {
    if (!_channel.isAuthenticated) {
      throw GreyError(
        code: GreyError.unauthorized,
        message: 'Not authenticated',
      );
    }

    // Return cached user if available and not forcing refresh
    if (!forceRefresh && _cachedUser != null) {
      return _cachedUser!;
    }

    try {
      final user = await _service.getUser(forceRefresh: forceRefresh);
      _cachedUser = user;
      return user;
    } on GrpcError catch (e) {
      throw GreyError.fromGrpcError(e);
    } catch (e) {
      throw GreyError.fromException(e);
    }
  }

  /// Clears the cached user data.
  void clearCache() {
    _cachedUser = null;
  }

  /// Gets the cached user without making a request.
  User? get cachedUser => _cachedUser;
}
