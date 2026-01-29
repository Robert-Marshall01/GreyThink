# Grey Multi‑Tenant — .NET Framework Implementation

This file defines the .NET‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md
- TEMPLATE_LIBRARY.md
- The adapter core in /packages/grey-adapters

.NET code must be idiomatic, async, DI‑friendly, and framework‑agnostic.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-dotnet
  grey-dotnet.csproj
  /src
    /Services
      AuthService.cs
      UserService.cs
      ProjectsService.cs
      QueryService.cs
      MutationService.cs
    GreyClient.cs
    GreyOptions.cs
    ServiceCollectionExtensions.cs
    index.cs

============================================================
FRAMEWORK RULES
============================================================

.NET uses:

- Async/await for all domain actions
- Dependency injection via IServiceCollection
- POCO services (no static state)
- No UI dependencies (usable in any .NET host)
- All services must wrap adapter‑core functions
- All services must expose:
  - Data (public property)
  - Loading (public property)
  - Error (public property)
  - Domain actions (async methods)

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
AuthService.cs
-------------------------
- Use the SERVICE TEMPLATE (.NET variant).
- Wrap:
  - LoginCore
  - LogoutCore
  - RefreshCore
- Properties:
  - object? Data { get; private set; }
  - bool Loading { get; private set; }
  - Exception? Error { get; private set; }
- Methods:
  - Task LoginAsync(...)
  - Task LogoutAsync()
  - Task RefreshAsync()
- Normalize errors into a structured error object.

-------------------------
UserService.cs
-------------------------
- Wrap:
  - FetchUserCore
- Expose:
  - Data
  - Loading
  - Error
  - Task FetchUserAsync()

-------------------------
ProjectsService.cs
-------------------------
- Wrap:
  - ListProjectsCore
  - CreateProjectCore
- Expose:
  - Data
  - Loading
  - Error
  - Task ListProjectsAsync()
  - Task CreateProjectAsync(...)

-------------------------
QueryService.cs
-------------------------
- Wrap:
  - QueryCore
- Expose:
  - Data
  - Loading
  - Error
  - Task ExecuteQueryAsync(...)

-------------------------
MutationService.cs
-------------------------
- Wrap:
  - MutateCore
- Expose:
  - Data
  - Loading
  - Error
  - Task ExecuteMutationAsync(...)

============================================================
GreyClient.cs
============================================================

- Acts as a typed façade over all services.
- Constructor receives:
  - AuthService
  - UserService
  - ProjectsService
  - QueryService
  - MutationService
- Expose them as readonly properties.
- No logic beyond grouping.

============================================================
GreyOptions.cs
============================================================

- Holds configuration for the SDK.
- Properties:
  - string ApiUrl
  - string? ApiKey
  - TimeSpan Timeout
- Must be DI‑bindable.

============================================================
ServiceCollectionExtensions.cs
============================================================

- Adds extension:
  - IServiceCollection AddGrey(...)
- Registers:
  - GreyOptions (via configuration or delegate)
  - All services as scoped
  - GreyClient as scoped
- No side effects.

============================================================
ERROR HANDLING
============================================================

All errors must be normalized into:

public class GreyError
{
    public string Message { get; set; }
    public string? Code { get; set; }
    public int? Status { get; set; }
    public object? Raw { get; set; }
}

Services must never throw raw adapter errors.

============================================================
RUNTIME RULES
============================================================

- No static mutable state.
- No UI dependencies.
- All domain actions must be async.
- Services must be thread‑safe.

============================================================
IMPORT RULES
============================================================

Core imports:

using Grey.Adapters;

.NET imports:

using System;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;

============================================================
EXPORT RULES
============================================================

index.cs must export:

- All services
- GreyClient
- GreyOptions
- ServiceCollectionExtensions

============================================================
END OF SPEC
============================================================