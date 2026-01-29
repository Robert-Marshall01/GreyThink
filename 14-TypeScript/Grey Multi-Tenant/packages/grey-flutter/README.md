# @grey/flutter

Flutter Web TypeScript interop layer for Grey Multi-Tenant.

This package provides a JavaScript bridge for Flutter Web applications.

## Installation

```bash
pnpm add @grey/flutter
pnpm build
```

## Usage

### Include in Flutter Web

Add the bundled script to your Flutter web/index.html:

```html
<script src="assets/packages/grey/grey.js"></script>
```

### Dart Interop

```dart
import 'dart:js' as js;

class GreyClient {
  void init(String baseUrl) {
    js.context.callMethod('greyInit', [baseUrl]);
  }
  
  Future<bool> login(String email, String password) async {
    final result = await js.context.callMethod('greyLogin', [email, password]);
    return result as bool;
  }
  
  Future<Map<String, dynamic>> getUser() async {
    final result = await js.context.callMethod('greyGetUser', []);
    return Map<String, dynamic>.from(result);
  }
  
  Future<List<Map<String, dynamic>>> getProjects() async {
    final result = await js.context.callMethod('greyGetProjects', []);
    return List<Map<String, dynamic>>.from(result);
  }
}
```

## JavaScript API

The package exposes these global functions:

- `greyInit(baseUrl)` - Initialize client
- `greyLogin(email, password)` - Login
- `greyLogout()` - Logout
- `greyGetUser()` - Get current user
- `greyGetProjects()` - List projects
- `greyCreateProject(name, description)` - Create project
- `greyDeleteProject(id)` - Delete project
