# @grey/astro

Astro bindings for Grey Multi-Tenant.

## Installation

```bash
pnpm add @grey/astro
```

## Usage

### Astro Pages (SSG/SSR)

```astro
---
import { createServerClient } from '@grey/astro/server';

const client = createServerClient(Astro);
const projects = await client.projects.list();
---

<ul>
  {projects.data?.map(p => <li>{p.name}</li>)}
</ul>
```

### Middleware

```ts
// src/middleware.ts
import { createGreyMiddleware } from '@grey/astro/server';

export const onRequest = createGreyMiddleware({
  baseUrl: import.meta.env.GREY_API_URL,
  protectedPaths: ['/dashboard'],
});
```

### React Islands

```tsx
import { GreyProvider, useAuth } from '@grey/astro/react';

export function AuthIsland({ baseUrl }: { baseUrl: string }) {
  return (
    <GreyProvider baseUrl={baseUrl}>
      <AuthContent />
    </GreyProvider>
  );
}
```

## API

### Server
- `createServerClient()` - Create client for Astro pages
- `createGreyMiddleware()` - Auth middleware

### React Islands
- `GreyProvider` - Context provider
- `useAuth`, `useUser`, `useProjects` - Hooks
