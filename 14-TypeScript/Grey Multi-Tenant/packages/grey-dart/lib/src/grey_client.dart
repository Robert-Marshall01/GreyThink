/// Main Grey SDK client facade.
library;

import 'config/grey_options.dart';
import 'domain/auth_client.dart';
import 'domain/mutation_client.dart';
import 'domain/projects_client.dart';
import 'domain/query_client.dart';
import 'domain/user_client.dart';
import 'grpc/auth_grpc_service.dart';
import 'grpc/grey_channel.dart';
import 'grpc/mutation_grpc_service.dart';
import 'grpc/projects_grpc_service.dart';
import 'grpc/query_grpc_service.dart';
import 'grpc/user_grpc_service.dart';

/// Main entry point for the Grey SDK.
///
/// Provides access to all domain clients for interacting with the Grey API.
///
/// Example:
/// ```dart
/// final client = GreyClient(
///   GreyOptions(host: 'api.grey.example.com'),
/// );
///
/// final auth = await client.auth.login(
///   email: 'user@example.com',
///   password: 'password',
/// );
///
/// final user = await client.user.getUser();
///
/// await client.shutdown();
/// ```
class GreyClient {
  /// Creates a new Grey client with the specified options.
  GreyClient(GreyOptions options) : _channel = GreyChannel(options) {
    // Initialize gRPC services
    final authService = AuthGrpcService(_channel);
    final userService = UserGrpcService(_channel);
    final projectsService = ProjectsGrpcService(_channel);
    final queryService = QueryGrpcService(_channel);
    final mutationService = MutationGrpcService(_channel);

    // Initialize domain clients
    _auth = AuthClient(_channel, authService);
    _user = UserClient(_channel, userService);
    _projects = ProjectsClient(_channel, projectsService);
    _query = QueryClient(_channel, queryService);
    _mutation = MutationClient(_channel, mutationService);
  }

  final GreyChannel _channel;

  late final AuthClient _auth;
  late final UserClient _user;
  late final ProjectsClient _projects;
  late final QueryClient _query;
  late final MutationClient _mutation;

  /// Client for authentication operations (login, logout, refresh).
  AuthClient get auth => _auth;

  /// Client for user operations (getUser).
  UserClient get user => _user;

  /// Client for project operations (listProjects, createProject).
  ProjectsClient get projects => _projects;

  /// Client for generic query operations.
  QueryClient get query => _query;

  /// Client for generic mutation operations.
  MutationClient get mutation => _mutation;

  /// Whether the client has a valid access token.
  bool get isAuthenticated => _channel.isAuthenticated;

  /// The current access token, if any.
  String? get accessToken => _channel.accessToken;

  /// Sets the access token manually.
  ///
  /// Useful for restoring a session from stored credentials.
  void setAccessToken(String? token) {
    _channel.setAccessToken(token);
  }

  /// Gracefully shuts down the client and closes connections.
  Future<void> shutdown() async {
    _user.clearCache();
    await _channel.shutdown();
  }

  /// Immediately terminates all connections.
  Future<void> terminate() async {
    _user.clearCache();
    await _channel.terminate();
  }
}
