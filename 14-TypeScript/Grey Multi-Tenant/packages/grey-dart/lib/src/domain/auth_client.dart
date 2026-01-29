/// Auth domain client for Grey SDK.
library;

import 'dart:async';

import 'package:grpc/grpc.dart';

import '../error/grey_error.dart';
import '../grpc/auth_grpc_service.dart';
import '../grpc/grey_channel.dart';
import '../grpc/types.dart';

/// Client for authentication operations.
class AuthClient {
  /// Creates a new auth client.
  AuthClient(this._channel, this._service);

  final GreyChannel _channel;
  final AuthGrpcService _service;

  /// Authenticates a user with email and password.
  ///
  /// On success, stores the access token for subsequent authenticated requests.
  ///
  /// Throws [GreyError] on failure.
  Future<AuthData> login({
    required String email,
    required String password,
  }) async {
    // Validate inputs
    if (email.isEmpty) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Email is required',
      );
    }
    if (password.isEmpty) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Password is required',
      );
    }

    try {
      final result = await _service.login(
        email: email,
        password: password,
      );

      // Store the access token
      _channel.setAccessToken(result.accessToken);

      return result;
    } on GrpcError catch (e) {
      throw GreyError.fromGrpcError(e);
    } catch (e) {
      throw GreyError.fromException(e);
    }
  }

  /// Logs out the current user.
  ///
  /// Clears the stored access token.
  ///
  /// Throws [GreyError] on failure.
  Future<void> logout() async {
    try {
      await _service.logout();
    } on GrpcError catch (e) {
      throw GreyError.fromGrpcError(e);
    } catch (e) {
      throw GreyError.fromException(e);
    } finally {
      // Always clear the token
      _channel.setAccessToken(null);
    }
  }

  /// Refreshes the authentication tokens.
  ///
  /// If [refreshToken] is provided, uses it; otherwise uses the last known
  /// refresh token.
  ///
  /// Throws [GreyError] on failure.
  Future<AuthData> refresh({String? refreshToken}) async {
    final token = refreshToken;
    if (token == null || token.isEmpty) {
      throw GreyError(
        code: GreyError.validationError,
        message: 'Refresh token is required',
      );
    }

    try {
      final result = await _service.refresh(refreshToken: token);

      // Update the access token
      _channel.setAccessToken(result.accessToken);

      return result;
    } on GrpcError catch (e) {
      throw GreyError.fromGrpcError(e);
    } catch (e) {
      throw GreyError.fromException(e);
    }
  }

  /// Whether the client has a valid access token.
  bool get isAuthenticated => _channel.isAuthenticated;
}
