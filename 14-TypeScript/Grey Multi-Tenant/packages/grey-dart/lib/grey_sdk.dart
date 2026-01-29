/// Grey Multi-Tenant SDK for Dart.
///
/// A Dart SDK for the Grey Multi-Tenant platform using gRPC transport.
///
/// ## Getting Started
///
/// ```dart
/// import 'package:grey_sdk/grey_sdk.dart';
///
/// void main() async {
///   final client = GreyClient(
///     GreyOptions(host: 'api.grey.example.com'),
///   );
///
///   try {
///     final auth = await client.auth.login(
///       email: 'user@example.com',
///       password: 'password',
///     );
///     print('Logged in: ${auth.accessToken}');
///
///     final user = await client.user.getUser();
///     print('User: ${user.name}');
///   } on GreyError catch (e) {
///     print('Error: [${e.code}] ${e.message}');
///   } finally {
///     await client.shutdown();
///   }
/// }
/// ```
library grey_sdk;

// Config
export 'src/config/grey_options.dart';

// Error handling
export 'src/error/grey_error.dart';

// Types
export 'src/grpc/types.dart';

// Main client
export 'src/grey_client.dart';

// Domain clients (for type access)
export 'src/domain/auth_client.dart';
export 'src/domain/mutation_client.dart';
export 'src/domain/projects_client.dart';
export 'src/domain/query_client.dart';
export 'src/domain/user_client.dart';
