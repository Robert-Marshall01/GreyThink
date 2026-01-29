# Grey Adapters

Framework-agnostic adapter layer for Grey Multi-Tenant.

## Overview

This package provides the shared business logic that all framework-specific
adapters use. It includes controllers for:

- **AuthController** - Authentication state management
- **UserController** - Current user management
- **ProjectsController** - Project CRUD operations
- **QueryController** - Generic async data fetching
- **MutationController** - Generic async mutations

## Installation

```bash
pnpm add @grey/adapters
```

## Usage

```typescript
import { AuthController, BrowserTokenStorage } from '@grey/adapters';

const auth = new AuthController({
  apiBaseUrl: 'http://localhost:8080/api/v1',
  storage: new BrowserTokenStorage(),
  onLogout: () => {
    // Redirect to login
  },
});

// Subscribe to state changes
auth.subscribe((state) => {
  console.log('Auth state:', state);
});

// Login
await auth.login('user@example.com', 'password');

// Logout
auth.logout();
```

## Framework Adapters

Each framework has a dedicated adapter package that wraps this core:

- `@grey/react` - React hooks
- `@grey/vue` - Vue 3 composables
- `@grey/svelte` - Svelte stores
- `@grey/angular` - Angular services

## Architecture

```
@grey/adapters (this package)
    ├── AuthController
    ├── UserController
    ├── ProjectsController
    └── Query/MutationController
          │
          ├── @grey/react (useAuth, useUser, useProjects)
          ├── @grey/vue (useAuth, useUser, useProjects)
          ├── @grey/svelte (auth, user, projects stores)
          └── @grey/angular (AuthService, UserService, ProjectsService)
```
