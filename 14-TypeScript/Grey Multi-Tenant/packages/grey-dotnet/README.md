# Grey Multi-Tenant .NET SDK

A comprehensive .NET SDK for integrating with the Grey Multi-Tenant system, providing authentication, user management, projects, and generic query/mutation capabilities.

## Packages

| Package | Description |
|---------|-------------|
| `Grey.MultiTenant.Core` | Core abstractions, interfaces, models, and exceptions |
| `Grey.MultiTenant.Http` | HTTP client implementations using HttpClientFactory |
| `Grey.MultiTenant.Blazor` | Blazor stores and components for reactive UI |
| `Grey.MultiTenant.Maui` | MAUI SecureStorage integration for mobile apps |

## Installation

```bash
# Core package (required)
dotnet add package Grey.MultiTenant.Core

# HTTP implementation
dotnet add package Grey.MultiTenant.Http

# Blazor support
dotnet add package Grey.MultiTenant.Blazor

# MAUI support (includes Blazor)
dotnet add package Grey.MultiTenant.Maui
```

## Quick Start

### ASP.NET Core / Blazor Server

```csharp
// Program.cs
builder.Services.AddGreyMultiTenant(options =>
{
    options.BaseUrl = "https://api.grey.example.com";
    options.TenantId = "your-tenant-id";
});

builder.Services.AddGreyMultiTenantBlazor();
```

### Blazor WebAssembly

```csharp
// Program.cs
builder.Services.AddGreyMultiTenantWithBlazor(options =>
{
    options.BaseUrl = "https://api.grey.example.com";
});
```

### MAUI Blazor Hybrid

```csharp
// MauiProgram.cs
builder.Services.AddGreyMultiTenantMaui(options =>
{
    options.BaseUrl = "https://api.grey.example.com";
});
```

## Usage

### Authentication

```csharp
// Using the AuthStore in Blazor
@inject AuthStore AuthStore

@if (AuthStore.IsAuthenticated)
{
    <p>Welcome, @AuthStore.User?.Name!</p>
    <button @onclick="LogoutAsync">Logout</button>
}
else
{
    <button @onclick="LoginAsync">Login</button>
}

@code {
    async Task LoginAsync()
    {
        await AuthStore.LoginAsync("user@example.com", "password");
    }

    async Task LogoutAsync()
    {
        await AuthStore.LogoutAsync();
    }
}
```

### Using Services Directly

```csharp
// Inject services
public class MyService
{
    private readonly IAuthService _authService;
    private readonly IProjectsService _projectsService;

    public MyService(IAuthService authService, IProjectsService projectsService)
    {
        _authService = authService;
        _projectsService = projectsService;
    }

    public async Task<ProjectListResult> GetProjectsAsync()
    {
        return await _projectsService.ListProjectsAsync(new ProjectListRequest
        {
            Page = 1,
            PageSize = 10
        });
    }
}
```

### Using the GreyProvider Component

```razor
@* App.razor or MainLayout.razor *@
<GreyProvider>
    <Router AppAssembly="@typeof(App).Assembly">
        @* ... *@
    </Router>
</GreyProvider>
```

### Generic Queries and Mutations

```csharp
// Query
var result = await queryService.GetAsync<MyData>("/api/custom-endpoint", new()
{
    ["filter"] = "active"
});

// Mutation
var created = await mutationService.PostAsync<MyData>("/api/items", new
{
    Name = "New Item"
});
```

## Error Handling

```csharp
try
{
    await authService.LoginAsync(request);
}
catch (AuthException ex) when (ex.ErrorType == AuthErrorType.InvalidCredentials)
{
    // Handle invalid credentials
}
catch (ValidationException ex)
{
    foreach (var error in ex.ValidationErrors)
    {
        Console.WriteLine($"{error.Field}: {error.Message}");
    }
}
catch (NetworkException ex) when (ex.IsOffline)
{
    // Handle offline state
}
catch (ApiException ex)
{
    // Handle other API errors
    Console.WriteLine($"Error {ex.StatusCode}: {ex.Message}");
}
```

## Configuration Options

```csharp
services.AddGreyMultiTenant(options =>
{
    // Required: API base URL
    options.BaseUrl = "https://api.grey.example.com";
    
    // Optional: Default tenant ID
    options.TenantId = "tenant-123";
    
    // Optional: Request timeout (default: 30s)
    options.Timeout = TimeSpan.FromSeconds(60);
    
    // Optional: Retry settings
    options.RetryCount = 3;
    options.RetryDelay = TimeSpan.FromMilliseconds(500);
    
    // Optional: Auto-refresh tokens on 401 (default: true)
    options.AutoRefreshTokens = true;
    
    // Optional: Default headers
    options.DefaultHeaders["X-Custom-Header"] = "value";
    
    // Optional: API version
    options.ApiVersion = "v1";
});
```

## Custom Token Storage

```csharp
// Implement ITokenStorage
public class CustomTokenStorage : ITokenStorage
{
    // ... implementation
}

// Register
services.AddSingleton<ITokenStorage, CustomTokenStorage>();
services.AddGreyMultiTenant(options => { ... });
```

## Building

```bash
dotnet build Grey.MultiTenant.sln
```

## Testing

```bash
dotnet test
```

## License

MIT
