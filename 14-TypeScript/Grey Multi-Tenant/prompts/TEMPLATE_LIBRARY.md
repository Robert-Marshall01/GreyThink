# Grey Multi-Tenant SDK — Template Library

You are generating code for a multi-framework, multi-runtime SDK that wraps a shared adapter core.

This file defines reusable FILE TEMPLATES that apply across ALL frameworks (React, Vue, Svelte, Angular, Solid, Qwik, Preact, Lit, Stencil, Node, Electron, React Native, Expo, Next.js, Nuxt, SvelteKit, Remix, Astro, Capacitor, .NET, etc.).

When a file includes a comment like:

// Use the HOOK TEMPLATE.
// Implement the React useAuth hook using useAuthCore.

…you must apply the corresponding template from this file, combined with:
- the global architecture rules
- the framework’s IMPLEMENTATION.md
- the file’s name and location

============================================================
HOOK TEMPLATE (JS FRAMEWORKS)
============================================================

Use this for:
- React hooks
- Vue composables
- Svelte stores/composables
- Solid signals/hooks
- Qwik hooks
- Preact hooks
- Lit/Stencil helpers
- React Native / Expo hooks
- Astro client hooks
- Capacitor client hooks

Behavior:
- Wrap the corresponding core adapter hook/function.
- Expose: data, loading, error.
- Expose domain actions (login, logout, refresh, listProjects, createProject, executeQuery, executeMutation, etc.).
- Use the idiomatic state/effect system of the framework.
- Normalize errors into a consistent shape.
- Never directly use browser APIs unless the framework is explicitly client-only.

Implementation pattern (conceptual):

- Import the core hook or core function from /packages/grey-adapters.
- Create framework-specific state (e.g., useState, ref, writable, signal).
- On mount/initialization, call the core adapter and subscribe to its state or invoke it as needed.
- Map core adapter outputs into:
  - data
  - loading
  - error
- Wrap domain actions so they:
  - call the core adapter
  - update loading
  - catch and normalize errors
- Return an object with:
  - data
  - loading
  - error
  - domain actions

============================================================
PROVIDER TEMPLATE (JS FRAMEWORKS)
============================================================

Use this for:
- React GreyProvider
- Vue provide/inject provider
- Svelte context provider
- Solid context provider
- Remix/Next/Nuxt/SvelteKit top-level providers
- Astro React island provider

Behavior:
- Provide domain APIs (auth, user, projects, query, mutation) via context or equivalent.
- Use the framework’s idiomatic provider pattern.
- No UI logic, only wiring.
- No direct browser APIs during SSR.

Implementation pattern (conceptual):

- Import the framework’s context APIs (e.g., createContext/useContext, provide/inject, setContext/getContext).
- Import the domain hooks/composables/stores (useAuth, useUser, useProjects, useQuery, useMutation).
- Create one context per domain or a single combined context object.
- In the provider component:
  - Initialize each domain hook/store.
  - Memoize the context value if appropriate.
  - Provide the value to children via context.
- Export:
  - the provider component
  - any context access helpers (e.g., useGreyAuth, useGreyUser, etc.) if appropriate.

============================================================
SERVER WRAPPER TEMPLATE (JS FRAMEWORKS)
============================================================

Use this for:
- Next.js server functions
- Nuxt server composables
- SvelteKit server utilities
- Remix loaders/actions wrappers
- Astro server utilities
- Node-only server wrappers
- Capacitor bridge/server-like wrappers

Behavior:
- Wrap core adapter functions in server-safe APIs.
- No browser APIs (no window, document, localStorage).
- Async functions only.
- Normalize errors.

Implementation pattern (conceptual):

- Import the core adapter functions (e.g., loginCore, logoutCore, fetchUserCore, listProjectsCore, queryCore, mutateCore).
- Export async functions:
  - login()
  - logout()
  - refresh()
  - fetchUser()
  - listProjects()
  - createProject()
  - query()
  - mutate()
- Each function:
  - Accepts typed parameters.
  - Calls the corresponding core function.
  - Catches errors and normalizes them into a consistent error shape.
- No framework-specific client APIs (no hooks, no components).

============================================================
INDEX EXPORT TEMPLATE (JS FRAMEWORKS)
============================================================

Use this for:
- index.ts in each framework package.

Behavior:
- Export the public API of the package.
- No logic, only exports.

Implementation pattern (conceptual):

- Export all hooks/composables/stores.
- Export the provider.
- Export server wrappers (if applicable).
- Export any bridge/token utilities (if applicable).

Example structure:

- export * from "./hooks/useAuth";
- export * from "./hooks/useUser";
- export * from "./hooks/useProjects";
- export * from "./hooks/useQuery";
- export * from "./hooks/useMutation";
- export { GreyProvider } from "./provider/GreyProvider";
- export * from "./server/auth";
- export * from "./server/user";
- export * from "./server/projects";
- export * from "./server/query";
- export * from "./server/mutation";

============================================================
TOKEN STORAGE TEMPLATE (MOBILE / HYBRID)
============================================================

Use this for:
- Expo token storage
- React Native token storage
- Capacitor token storage
- Electron secure storage (if applicable)

Behavior:
- Provide async get/set/clear token functions.
- Use platform-safe storage APIs.
- Fallback to in-memory storage if platform APIs are unavailable.

Implementation pattern (conceptual):

- Try to import and use the platform’s secure storage (e.g., SecureStore, SecureStorage, Preferences).
- Implement:
  - async getToken(): Promise<string | null>
  - async setToken(token: string): Promise<void>
  - async clearToken(): Promise<void>
- If the platform API is not available (e.g., running in web), fallback to an in-memory variable.
- Never throw on missing platform APIs; degrade gracefully.

============================================================
BRIDGE LAYER TEMPLATE (CAPACITOR / ELECTRON / HYBRID)
============================================================

Use this for:
- Capacitor bridge layer (/bridge)
- Electron preload/main bridge
- Any environment where you need a safe boundary between core logic and UI

Behavior:
- Wrap core adapter functions in a platform-safe API.
- No React or UI framework imports.
- No direct browser APIs in the bridge itself (unless explicitly allowed).
- Async functions only.

Implementation pattern (conceptual):

- Import core adapter functions.
- Export async functions:
  - login()
  - logout()
  - refresh()
  - fetchUser()
  - listProjects()
  - createProject()
  - query()
  - mutate()
- Normalize errors.
- Ensure the bridge can be called from:
  - UI components
  - background tasks
  - plugins

============================================================
. NET SERVICE INTERFACE TEMPLATE
============================================================

Use this for:
- IAuthService
- IUserService
- IProjectsService
- IQueryService
- IMutationService

Behavior:
- Define async methods that mirror the JS adapter core.
- Use Task<T> for async operations.
- Use strongly-typed request/response models.

Implementation pattern (conceptual):

- Define a public interface in the appropriate namespace.
- Methods:
  - Task<LoginResult> LoginAsync(LoginRequest request);
  - Task LogoutAsync();
  - Task RefreshAsync();
  - Task<UserModel> GetUserAsync();
  - Task<ProjectListResult> ListProjectsAsync(ProjectListRequest request);
  - Task<ProjectModel> CreateProjectAsync(CreateProjectRequest request);
  - Task<QueryResult> ExecuteQueryAsync(QueryRequest request);
  - Task<MutationResult> ExecuteMutationAsync(MutationRequest request);
- Use DTOs defined in the Models namespace.

============================================================
. NET HTTP SERVICE IMPLEMENTATION TEMPLATE
============================================================

Use this for:
- HttpAuthService
- HttpUserService
- HttpProjectsService
- HttpQueryService
- HttpMutationService

Behavior:
- Implement the service interfaces using HttpClient.
- Use HttpClientFactory.
- Use System.Text.Json.
- Normalize errors into ApiException/AuthException/etc.

Implementation pattern (conceptual):

- Inject HttpClient via constructor.
- Implement interface methods by:
  - Building the request URL from options.
  - Serializing request bodies as JSON.
  - Sending HTTP requests with HttpClient.
  - Deserializing JSON responses.
  - Throwing ApiException on non-success status codes with:
    - StatusCode
    - ErrorCode (if available)
    - Message
    - RawResponse
- Use async/await everywhere.

============================================================
. NET STORE / STATE TEMPLATE (BLAZOR / MAUI)
============================================================

Use this for:
- AuthStore
- UserStore
- ProjectsStore
- QueryStore
- MutationStore

Behavior:
- Wrap the service interfaces in observable state.
- Expose: Data, Loading, Error.
- Provide async methods that call the underlying service.
- Notify UI of changes.

Implementation pattern (conceptual):

- Implement INotifyPropertyChanged or use events.
- Properties:
  - TData? Data { get; private set; }
  - bool Loading { get; private set; }
  - Exception? Error { get; private set; }
- Methods:
  - async Task LoadAsync(...)
  - async Task ExecuteAsync(...)
- In each method:
  - Set Loading = true, Error = null.
  - Call the service.
  - Set Data on success.
  - Set Error on failure.
  - Set Loading = false.
  - Raise PropertyChanged or invoke a Changed event.

============================================================
. NET PROVIDER / DI TEMPLATE
============================================================

Use this for:
- DI extension methods:
  - AddGreyMultiTenantCore
  - AddGreyMultiTenantHttp
  - AddGreyMultiTenantBlazor
  - AddGreyMultiTenantMaui

Behavior:
- Register services, stores, and HttpClient.
- Use options pattern for configuration.

Implementation pattern (conceptual):

- Create static extension methods on IServiceCollection.
- Add options class (e.g., GreyMultiTenantOptions).
- Use services.Configure<GreyMultiTenantOptions>(configuration).
- Register:
  - core interfaces
  - HTTP implementations
  - stores
- Use AddHttpClient for Http services, configuring BaseAddress and default headers.

============================================================
USAGE RULES
============================================================

When a file includes a comment like:

// Use the HOOK TEMPLATE.
// Implement the React useAuth hook using useAuthCore.

You MUST:
1. Apply the corresponding template from this file.
2. Combine it with:
   - the global architecture rules
   - the framework’s IMPLEMENTATION.md
   - the file’s name and location
3. Generate idiomatic, production-ready code for that framework.
4. Do not generate placeholders; generate real implementations.