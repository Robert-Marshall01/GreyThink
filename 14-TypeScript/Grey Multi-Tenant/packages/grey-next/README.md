# @grey/next

Next.js bindings for Grey Multi-Tenant with SSR support.

## Installation

```bash
pnpm add @grey/next next react
```

## Usage

### Client Components

```tsx
'use client';

import { GreyProvider, useAuth, useUser, useProjects } from '@grey/next';

export function Providers({ children }: { children: React.ReactNode }) {
  return (
    <GreyProvider baseUrl="https://api.grey.example.com">
      {children}
    </GreyProvider>
  );
}
```

### Server Components

```tsx
import { createServerClient } from '@grey/next/server';
import { cookies } from 'next/headers';

export default async function Page() {
  const client = createServerClient({
    baseUrl: process.env.API_URL!,
    getToken: async () => cookies().get('token')?.value,
  });
  
  const user = await client.users.me();
  return <div>Welcome, {user?.name}</div>;
}
```

## API

### Client
- `GreyProvider` - Context provider (re-exported from @grey/react)
- `useAuth`, `useUser`, `useProjects` - React hooks
- `useQuery`, `useMutation` - Generic hooks

### Server
- `createServerClient()` - Create client for Server Components
- `withGreyAuth()` - Middleware for auth
