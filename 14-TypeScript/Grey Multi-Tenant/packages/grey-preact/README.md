# @grey/preact

Preact bindings for Grey Multi-Tenant.

## Installation

```bash
pnpm add @grey/preact preact
```

## Usage

```tsx
import { GreyProvider, useAuth, useUser, useProjects } from '@grey/preact';

function App() {
  return (
    <GreyProvider baseUrl="https://api.grey.example.com">
      <MyApp />
    </GreyProvider>
  );
}

function MyApp() {
  const { isAuthenticated } = useAuth();
  const { user } = useUser();
  const { projects } = useProjects();
  
  return (
    <div>
      {isAuthenticated ? (
        <p>Welcome, {user?.name}</p>
      ) : (
        <p>Please log in</p>
      )}
    </div>
  );
}
```

## API

- `GreyProvider` - Context provider for Grey client
- `useAuth()` - Auth state and methods
- `useUser()` - User state
- `useProjects()` - Projects state and CRUD methods
- `useQuery()` - Generic query hook
- `useMutation()` - Generic mutation hook
