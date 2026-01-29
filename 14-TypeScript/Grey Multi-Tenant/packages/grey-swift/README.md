# Grey Multi-Tenant SDK - Swift

Swift SDK for the Grey Multi-Tenant platform using gRPC transport.

## Requirements

- iOS 15.0+ / macOS 12.0+ / tvOS 15.0+ / watchOS 8.0+
- Swift 5.9+
- Xcode 15.0+

## Installation

### Swift Package Manager

Add the following to your `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/grey/grey-swift.git", from: "0.1.0")
]
```

Or in Xcode: File → Add Packages → Enter the repository URL.

## Usage

```swift
import GreySDK

// Create client
let options = GreyOptions(
    host: "api.grey.example.com",
    port: 443,
    useTLS: true
)
let client = try GreyClient(options: options)

// Login
do {
    let auth = try await client.auth.login(email: "user@example.com", password: "password")
    print("Logged in: \(auth.accessToken)")
} catch let error as GreyError {
    print("Login failed: \(error.message)")
}

// Fetch user
let user = try await client.user.getUser()
print("User: \(user.name ?? "Unknown")")

// List projects
let projectsData = try await client.projects.listProjects()
for project in projectsData.projects {
    print("Project: \(project.name)")
}

// Cleanup
try await client.shutdown()
```

## Features

- **gRPC Transport**: Fast, type-safe communication
- **Async/Await**: Native Swift concurrency support
- **Error Normalization**: Consistent error shape across all operations
- **Domain Clients**: Auth, User, Projects, Query, Mutation

## API

### GreyClient

Main entry point providing access to all domain clients.

### AuthClient

- `login(email: String, password: String) async throws -> AuthData`
- `logout() async throws`
- `refresh() async throws -> AuthData`

### UserClient

- `getUser() async throws -> User`

### ProjectsClient

- `listProjects(page: Int, pageSize: Int) async throws -> ProjectsData`
- `createProject(name: String, description: String?) async throws -> Project`

### QueryClient

- `query(endpoint: String, params: [String: String]?) async throws -> QueryData`

### MutationClient

- `mutate(endpoint: String, method: String, body: Any?) async throws -> MutationData`

## License

MIT
