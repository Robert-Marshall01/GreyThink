# @grey/nuxt

Nuxt bindings for Grey Multi-Tenant with SSR support.

## Installation

```bash
pnpm add @grey/nuxt
```

## Setup

Add to your `nuxt.config.ts`:

```ts
export default defineNuxtConfig({
  runtimeConfig: {
    greyApiUrl: process.env.GREY_API_URL,
    public: {
      greyApiUrl: process.env.GREY_PUBLIC_API_URL,
    },
  },
});
```

## Usage

### Composables

```vue
<script setup lang="ts">
import { useAuth, useUser, useProjects } from '@grey/nuxt';

const { session, login, logout } = useAuth();
const { user } = useUser();
const { projects, create } = useProjects();
</script>
```

### Server Routes

```ts
// server/api/projects.get.ts
import { createServerClient } from '@grey/nuxt/server';

export default defineEventHandler(async (event) => {
  const client = createServerClient(event);
  const projects = await client.projects.list();
  return projects.data;
});
```

## API

### Client
- `useAuth()` - Auth composable
- `useUser()` - User composable  
- `useProjects()` - Projects composable
- `useGrey()` - Access Grey client

### Server
- `createServerClient()` - Create client for server routes
- `useGreyAuth()` - Server-side auth validation
