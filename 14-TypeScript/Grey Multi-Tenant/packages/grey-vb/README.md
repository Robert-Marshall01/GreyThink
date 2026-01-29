# Grey SDK for Visual Basic

Multi-tenant SDK client for Visual Basic .NET applications.

## Installation

### NuGet Package

```powershell
dotnet add package GreySdk.VisualBasic
```

### Build from Source

```powershell
dotnet build
```

## Quick Start

```vb
Imports Grey
Imports Grey.Domain

Module Program
    Sub Main()
        MainAsync().Wait()
    End Sub

    Async Function MainAsync() As Task
        ' Create a client for local development
        Using client = GreyClient.Local()
            ' Login
            Dim loginResult = Await client.Auth.LoginAsync("user@example.com", "password")
            
            If loginResult.IsSuccess Then
                Console.WriteLine($"Logged in! Token: {loginResult.Value.AccessToken}")
                
                ' Create authenticated client
                Using authClient = client.WithAuthToken(loginResult.Value.AccessToken)
                    ' Get current user
                    Dim userResult = Await authClient.Users.GetCurrentUserAsync()
                    If userResult.IsSuccess Then
                        Console.WriteLine($"Hello, {userResult.Value.DisplayName}!")
                    End If
                    
                    ' List projects
                    Dim projectsResult = Await authClient.Projects.ListProjectsAsync()
                    If projectsResult.IsSuccess Then
                        For Each project In projectsResult.Value.Items
                            Console.WriteLine($"Project: {project.Name}")
                        Next
                    End If
                End Using
            Else
                Console.WriteLine($"Login failed: {loginResult.Error.Message}")
            End If
        End Using
    End Function
End Module
```

## Features

### Domain Clients

- **Auth**: Login, logout, and token refresh
- **Users**: Get user information
- **Projects**: List, create, update, and delete projects
- **Queries**: Execute read-only queries
- **Mutations**: Execute data modifications

### Error Handling

All operations return a `Result(Of T)` type that provides safe error handling:

```vb
Dim result = Await client.Auth.LoginAsync("user", "pass")

' Pattern matching
Dim message = result.Match(
    Function(tokens) $"Success: {tokens.AccessToken}",
    Function(err) $"Error: {err.Message}"
)

' Check and access
If result.IsSuccess Then
    Dim tokens = result.Value
Else
    Dim err = result.Error
    Console.WriteLine($"[{err.Code}] {err.Message}")
End If

' Get value or default
Dim tokens = result.GetValueOrDefault(New AuthTokens())

' Get value or throw
Try
    Dim tokens = result.GetValueOrThrow()
Catch ex As GreyError
    Console.WriteLine(ex.Message)
End Try
```

### Error Codes

| Code | Description |
|------|-------------|
| `unauthorized` | Authentication required or token invalid |
| `forbidden` | Access denied |
| `not_found` | Resource not found |
| `validation_error` | Request validation failed |
| `network_error` | Network connectivity issue |
| `timeout` | Request timed out |
| `server_error` | Server returned an error |
| `unknown` | Unknown error |

### Configuration

```vb
' Local development (http://localhost:8080)
Using client = GreyClient.Local()
End Using

' Local with custom port
Using client = GreyClient.Local(3000)
End Using

' Production (https://api.example.com:443)
Using client = GreyClient.Production("api.example.com")
End Using

' Custom configuration
Dim options = New Options() With {
    .Host = "api.example.com",
    .Port = 8443,
    .UseTls = True,
    .TimeoutSeconds = 60
}
Using client = New GreyClient(options)
End Using

' Immutable configuration updates
Dim authClient = client.WithAuthToken("token")
Dim timeoutClient = client.WithTimeout(60)
Dim headerClient = client.WithHeader("X-Custom", "value")
```

### Projects

```vb
' List projects with options
Dim options = New ListProjectsOptions() With {
    .Page = 1,
    .PerPage = 20,
    .Search = "my project",
    .SortBy = "name",
    .SortDescending = False
}
Dim result = Await client.Projects.ListProjectsAsync(options)

' Create a project
Dim data = New CreateProjectData() With {
    .Name = "New Project",
    .Description = "A new project"
}
Dim result = Await client.Projects.CreateProjectAsync(data)

' Get a project
Dim result = Await client.Projects.GetProjectAsync("project-id")

' Update a project
Dim updateData = New UpdateProjectData() With {
    .Name = "Updated Name"
}
Dim result = Await client.Projects.UpdateProjectAsync("project-id", updateData)

' Delete a project
Dim result = Await client.Projects.DeleteProjectAsync("project-id")
```

### Queries

```vb
' Simple query
Dim result = Await client.Queries.QueryAsync("{ users { id name } }")

' Query with variables
Dim variables = New Dictionary(Of String, Object) From {
    {"id", "user-123"}
}
Dim result = Await client.Queries.QueryAsync(
    "query GetUser($id: ID!) { user(id: $id) { id name } }",
    variables
)

' Batch queries
Dim queries = New List(Of QueryRequest) From {
    New QueryRequest() With {.Query = "{ users { id } }"},
    New QueryRequest() With {.Query = "{ projects { id } }"}
}
Dim result = Await client.Queries.BatchQueryAsync(queries)
```

### Mutations

```vb
' Simple mutation
Dim result = Await client.Mutations.MutateAsync(
    "mutation { createUser(name: ""John"") { id } }"
)

' Mutation with variables
Dim variables = New Dictionary(Of String, Object) From {
    {"name", "John"},
    {"email", "john@example.com"}
}
Dim result = Await client.Mutations.MutateAsync(
    "mutation CreateUser($name: String!, $email: String!) { createUser(name: $name, email: $email) { id } }",
    variables
)
```

## Running Tests

```powershell
cd Tests
dotnet test
```

## API Reference

### GreyClient

The main entry point for the SDK.

| Property | Type | Description |
|----------|------|-------------|
| `Auth` | `AuthClient` | Authentication operations |
| `Users` | `UserClient` | User operations |
| `Projects` | `ProjectsClient` | Project operations |
| `Queries` | `QueryClient` | Query operations |
| `Mutations` | `MutationClient` | Mutation operations |

### Factory Methods

| Method | Description |
|--------|-------------|
| `GreyClient.Local(port)` | Create client for local development |
| `GreyClient.Production(host, port)` | Create client for production |
| `WithAuthToken(token)` | Create client with auth token |
| `WithTimeout(seconds)` | Create client with custom timeout |
| `WithHeader(name, value)` | Create client with custom header |

## License

MIT License - see LICENSE file for details.
