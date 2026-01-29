# @grey/solid

SolidJS bindings for Grey Multi-Tenant.

## Installation

```bash
pnpm add @grey/solid solid-js
```

## Usage

```tsx
import { GreyProvider, createAuth, createUser, createProjects } from '@grey/solid';

function App() {
  return (
    <GreyProvider baseUrl="https://api.grey.example.com">
      <MyApp />
    </GreyProvider>
  );
}

function MyApp() {
  const auth = createAuth();
  const user = createUser();
  const projects = createProjects();
  
  return <div>...</div>;
}
```

## API

- `GreyProvider` - Context provider for Grey client
- `createAuth()` - Reactive auth state and methods
- `createUser()` - Reactive user state and methods
- `createProjects()` - Reactive projects state and methods
- `createQuery()` - Generic query primitive
- `createMutation()` - Generic mutation primitive
