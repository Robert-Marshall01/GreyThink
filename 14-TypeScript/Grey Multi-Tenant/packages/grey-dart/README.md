# Grey Multi-Tenant SDK - Dart

Dart SDK for the Grey Multi-Tenant platform using gRPC transport.

## Installation

Add the dependency to your `pubspec.yaml`:

```yaml
dependencies:
  grey_sdk: ^0.1.0
```

Then run:

```bash
dart pub get
```

## Usage

```dart
import 'package:grey_sdk/grey_sdk.dart';

void main() async {
  // Create client
  final options = GreyOptions(
    host: 'api.grey.example.com',
    port: 443,
    useTls: true,
  );
  final client = GreyClient(options);

  try {
    // Login
    final auth = await client.auth.login(
      email: 'user@example.com',
      password: 'password',
    );
    print('Logged in: ${auth.accessToken}');

    // Fetch user
    final user = await client.user.getUser();
    print('User: ${user.name}');

    // List projects
    final projectsData = await client.projects.listProjects();
    for (final project in projectsData.projects) {
      print('Project: ${project.name}');
    }
  } on GreyError catch (e) {
    print('Error: [${e.code}] ${e.message}');
  } finally {
    // Cleanup
    await client.shutdown();
  }
}
```

## Features

- **gRPC Transport**: Fast, type-safe communication
- **Async/Await**: Native Dart Futures support
- **Error Normalization**: Consistent error shape across all operations
- **Domain Clients**: Auth, User, Projects, Query, Mutation

## API

### GreyClient

Main entry point providing access to all domain clients.

### AuthClient

- `login({required String email, required String password}) -> Future<AuthData>`
- `logout() -> Future<void>`
- `refresh({String? refreshToken}) -> Future<AuthData>`

### UserClient

- `getUser({bool forceRefresh = false}) -> Future<User>`

### ProjectsClient

- `listProjects({int page = 1, int pageSize = 20}) -> Future<ProjectsData>`
- `createProject({required String name, String? description}) -> Future<Project>`

### QueryClient

- `query({required String endpoint, Map<String, String>? params, bool requireAuth = true}) -> Future<QueryData>`

### MutationClient

- `mutate({required String endpoint, String method = 'POST', Object? body}) -> Future<MutationData>`

## License

MIT
