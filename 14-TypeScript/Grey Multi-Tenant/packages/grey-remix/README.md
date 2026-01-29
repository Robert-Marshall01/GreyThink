# @grey/remix

Remix bindings for Grey Multi-Tenant with SSR support.

## Installation

```bash
pnpm add @grey/remix
```

## Usage

### Client Components

```tsx
import { GreyProvider, useAuth, useUser, useProjects } from '@grey/remix';

export default function App() {
  return (
    <GreyProvider baseUrl="https://api.grey.example.com">
      <Outlet />
    </GreyProvider>
  );
}
```

### Loaders

```tsx
import { createServerClient } from '@grey/remix/server';
import type { LoaderFunctionArgs } from '@remix-run/node';

export async function loader({ request }: LoaderFunctionArgs) {
  const client = createServerClient(request);
  const projects = await client.projects.list();
  return json({ projects: projects.data });
}
```

### Actions

```tsx
import { createLoginAction } from '@grey/remix/server';
import type { ActionFunctionArgs } from '@remix-run/node';

export const action = async ({ request }: ActionFunctionArgs) => {
  return createLoginAction(request, {
    baseUrl: process.env.API_URL!,
    onSuccess: '/dashboard',
  });
};
```

## API

### Client
- `GreyProvider` - Context provider
- `useAuth`, `useUser`, `useProjects` - React hooks

### Server
- `createServerClient()` - Create client for loaders
- `createLoginAction()` - Handle login forms
- `createLogoutAction()` - Handle logout
- `requireAuth()` - Protect routes
