# Grey Svelte

Svelte stores for the Grey Multi-Tenant platform.

## Installation

```bash
npm install @grey/svelte @grey/core-client
```

## Quick Start

```svelte
<!-- +layout.svelte -->
<script lang="ts">
  import { initGrey } from '@grey/svelte';
  import { onMount } from 'svelte';

  onMount(() => {
    initGrey({ apiBaseUrl: 'http://localhost:8080/api/v1' });
  });
</script>

<slot />
```

```svelte
<!-- Dashboard.svelte -->
<script lang="ts">
  import { createAuthStore, createProjectsStore } from '@grey/svelte';

  const auth = createAuthStore();
  const projects = createProjectsStore();

  $: isAuthenticated = $auth.isAuthenticated;
  $: user = $auth.user;

  function logout() {
    auth.logout();
  }
</script>

{#if !isAuthenticated}
  <Login on:submit={auth.login} />
{:else}
  <h1>Welcome, {user?.name}</h1>
  <button on:click={logout}>Logout</button>
  <ProjectList projects={$projects.projects} />
{/if}
```

## API

### Initialization

```typescript
import { initGrey } from '@grey/svelte';

initGrey({
  apiBaseUrl: 'http://localhost:8080/api/v1',
  storage: new BrowserTokenStorage(), // optional
  onAuthChange: (state) => console.log('Auth changed:', state),
  onLogout: () => goto('/login'),
});
```

### Auth Store

```typescript
import { createAuthStore, isAuthenticated } from '@grey/svelte';

const auth = createAuthStore();

// Subscribe to state
$auth.user;           // User | null
$auth.isAuthenticated // boolean
$auth.isLoading       // boolean
$auth.error           // string | null

// Actions
auth.login(email, password);   // Promise<boolean>
auth.logout();                 // void
auth.restoreSession();         // Promise<boolean>

// Derived store
const authenticated = isAuthenticated(auth);
$authenticated // boolean
```

### User Store

```typescript
import { createUserStore } from '@grey/svelte';

const user = createUserStore();

// Subscribe to state
$user.user;      // User | null
$user.isLoading; // boolean
$user.error;     // string | null

// Actions
user.refresh(); // Promise<User | null>
```

### Projects Store

```typescript
import { createProjectsStore } from '@grey/svelte';

const projects = createProjectsStore();

// Subscribe to state
$projects.projects;   // Project[]
$projects.pagination; // Pagination | null
$projects.isLoading;  // boolean
$projects.error;      // string | null

// Actions
projects.load(page, pageSize);  // Promise<void>
projects.create({ name, description }); // Promise<Project | null>
```

### Project Store

```typescript
import { createProjectStore } from '@grey/svelte';

const project = createProjectStore('project-id');

// Subscribe to state
$project.project;   // Project | null
$project.isLoading; // boolean
$project.error;     // string | null

// Actions
project.load(); // Promise<Project | null>
```

### Query Store

```typescript
import { createQueryStore, getClient } from '@grey/svelte';

const client = getClient();
const orgQuery = createQueryStore({
  queryFn: () => client.organizations.get('org-id'),
  enabled: true,
  onSuccess: (data) => console.log('Loaded:', data),
});

// Subscribe to state
$orgQuery.data;      // T | null
$orgQuery.isLoading; // boolean
$orgQuery.isError;   // boolean
$orgQuery.error;     // Error | null
$orgQuery.isSuccess; // boolean

// Actions
orgQuery.refetch(); // Promise<T | null>
```

### Mutation Store

```typescript
import { createMutationStore, getClient } from '@grey/svelte';

const client = getClient();
const createMutation = createMutationStore({
  mutationFn: (data) => client.projects.create(data),
  onSuccess: (data) => {
    console.log('Created:', data);
    goto(`/projects/${data.id}`);
  },
});

// Subscribe to state
$createMutation.data;      // TData | null
$createMutation.isLoading; // boolean
$createMutation.isError;   // boolean
$createMutation.error;     // Error | null
$createMutation.isSuccess; // boolean

// Actions
createMutation.mutate({ name: 'New Project' });
createMutation.mutateAsync({ name: 'New Project' }); // same as mutate
createMutation.reset();
```

### Get Client

```typescript
import { getClient } from '@grey/svelte';

const client = getClient();
const result = await client.organizations.get('org-id');
```

## Derived Stores

```typescript
import { derived } from 'svelte/store';
import { createAuthStore, isAuthenticated, isLoading } from '@grey/svelte';

const auth = createAuthStore();

// Built-in derived stores
const authenticated = isAuthenticated(auth);
const loading = isLoading(auth);

// Custom derived stores
const userName = derived(auth, ($auth) => $auth.user?.name ?? 'Guest');
```

## SvelteKit Integration

```typescript
// hooks.server.ts
import type { Handle } from '@sveltejs/kit';

export const handle: Handle = async ({ event, resolve }) => {
  // Server-side token handling if needed
  return resolve(event);
};
```

```svelte
<!-- +layout.svelte -->
<script lang="ts">
  import { browser } from '$app/environment';
  import { goto } from '$app/navigation';
  import { initGrey, createAuthStore } from '@grey/svelte';
  import { onMount } from 'svelte';

  let initialized = false;

  onMount(async () => {
    if (browser) {
      initGrey({
        apiBaseUrl: 'http://localhost:8080/api/v1',
        onLogout: () => goto('/login'),
      });

      const auth = createAuthStore();
      await auth.restoreSession();
      initialized = true;
    }
  });
</script>

{#if initialized}
  <slot />
{:else}
  <div>Loading...</div>
{/if}
```

## License

MIT
