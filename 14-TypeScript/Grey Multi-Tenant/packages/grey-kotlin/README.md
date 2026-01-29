# Grey Multi-Tenant SDK - Kotlin

Kotlin SDK for the Grey Multi-Tenant platform using gRPC transport.

## Installation

Add the dependency to your `build.gradle.kts`:

```kotlin
dependencies {
    implementation("com.grey:grey-kotlin:0.1.0")
}
```

## Usage

```kotlin
import com.grey.sdk.GreyClient
import com.grey.sdk.config.GreyOptions

// Create client
val options = GreyOptions(
    host = "api.grey.example.com",
    port = 443,
    useTls = true
)
val client = GreyClient(options)

// Login
val authResult = client.auth.login("user@example.com", "password")
if (authResult.isSuccess) {
    println("Logged in: ${authResult.data?.accessToken}")
}

// Fetch user
val userResult = client.user.getUser()
userResult.data?.let { user ->
    println("User: ${user.name}")
}

// List projects
val projectsResult = client.projects.listProjects()
projectsResult.data?.forEach { project ->
    println("Project: ${project.name}")
}

// Cleanup
client.shutdown()
```

## Features

- **gRPC Transport**: Fast, type-safe communication
- **Suspend Functions**: Native Kotlin coroutines support
- **Error Normalization**: Consistent error shape across all operations
- **Domain Clients**: Auth, User, Projects, Query, Mutation

## API

### GreyClient

Main entry point providing access to all domain clients.

### AuthClient

- `login(email: String, password: String): GreyResult<AuthData>`
- `logout(): GreyResult<Unit>`
- `refresh(refreshToken: String): GreyResult<AuthData>`

### UserClient

- `getUser(): GreyResult<User>`

### ProjectsClient

- `listProjects(page: Int = 1, pageSize: Int = 20): GreyResult<ProjectsData>`
- `createProject(name: String, description: String?): GreyResult<Project>`

### QueryClient

- `query(endpoint: String, params: Map<String, String>?): GreyResult<Any>`

### MutationClient

- `mutate(endpoint: String, method: String, body: Any?): GreyResult<Any>`

## License

MIT
