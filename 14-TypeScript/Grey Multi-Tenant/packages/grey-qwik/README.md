# @grey/qwik

Qwik bindings for Grey Multi-Tenant.

## Installation

```bash
pnpm add @grey/qwik @builder.io/qwik
```

## Usage

```tsx
import { GreyProvider, useAuth$, useUser$, useProjects$ } from '@grey/qwik';

export default component$(() => {
  return (
    <GreyProvider baseUrl="https://api.grey.example.com">
      <MyApp />
    </GreyProvider>
  );
});

const MyApp = component$(() => {
  const auth = useAuth$();
  const user = useUser$();
  const projects = useProjects$();
  
  return <div>...</div>;
});
```

## API

- `GreyProvider` - Context provider for Grey client
- `useAuth$()` - Reactive auth state and methods
- `useUser$()` - Reactive user state and methods
- `useProjects$()` - Reactive projects state and methods
- `useQuery$()` - Generic query hook
- `useMutation$()` - Generic mutation hook
