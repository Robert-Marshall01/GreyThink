# @grey/sveltekit

SvelteKit bindings for Grey Multi-Tenant with SSR support.

## Installation

```bash
pnpm add @grey/sveltekit
```

## Setup

### hooks.server.ts

```ts
import { createGreyHandle } from '@grey/sveltekit/server';

export const handle = createGreyHandle({
  baseUrl: process.env.GREY_API_URL,
});
```

## Usage

### Client-side Stores

```svelte
<script>
import { auth, projects } from '@grey/sveltekit';

$: if ($auth.isAuthenticated) {
  console.log('User logged in');
}
</script>

{#each $projects.items as project}
  <p>{project.name}</p>
{/each}
```

### Server Load Functions

```ts
// +page.server.ts
import { createServerClient } from '@grey/sveltekit/server';
import type { PageServerLoad } from './$types';

export const load: PageServerLoad = async ({ cookies }) => {
  const client = createServerClient({ cookies });
  const projects = await client.projects.list();
  return { projects: projects.data };
};
```

## API

### Client
- `auth` - Auth store
- `user` - User store
- `projects` - Projects store

### Server
- `createServerClient()` - Create client for load functions
- `createGreyHandle()` - Create SvelteKit handle hook
