# Grey Vue

Vue 3 composables for the Grey Multi-Tenant platform.

## Installation

```bash
pnpm add @grey/vue @grey/core-client
```

## Quick Start

```vue
<!-- App.vue -->
<script setup lang="ts">
import { provideGrey } from '@grey/vue';

provideGrey({ apiBaseUrl: 'http://localhost:8080/api/v1' });
</script>

<template>
  <RouterView />
</template>
```

```vue
<!-- Dashboard.vue -->
<script setup lang="ts">
import { useAuth, useProjects } from '@grey/vue';

const { isAuthenticated, user, login, logout } = useAuth();
const { projects, isLoading, load } = useProjects();

onMounted(() => {
  load();
});
</script>

<template>
  <div v-if="!isAuthenticated.value">
    <LoginForm @submit="login" />
  </div>
  <div v-else>
    <h1>Welcome, {{ user.value?.name }}</h1>
    <button @click="logout">Logout</button>
    <ProjectList :projects="projects.value" :loading="isLoading.value" />
  </div>
</template>
```

## Composables

### useAuth()

```ts
const {
  state,           // Readonly<Ref<AuthState>>
  isAuthenticated, // ComputedRef<boolean>
  isLoading,       // ComputedRef<boolean>
  error,           // ComputedRef<string | null>
  login,           // (email, password) => Promise<boolean>
  logout,          // () => void
  restoreSession,  // () => Promise<boolean>
} = useAuth();
```

### useUser()

```ts
const {
  user,       // ComputedRef<User | null>
  isLoading,  // ComputedRef<boolean>
  error,      // ComputedRef<string | null>
  refresh,    // () => Promise<User | null>
} = useUser();
```

### useProjects()

```ts
const {
  projects,    // ComputedRef<Project[]>
  pagination,  // ComputedRef<Pagination | null>
  isLoading,   // ComputedRef<boolean>
  error,       // ComputedRef<string | null>
  load,        // (page?, pageSize?) => Promise<void>
  create,      // (input) => Promise<Project | null>
} = useProjects();
```

### useProject(id)

```ts
const {
  project,    // ComputedRef<Project | null>
  isLoading,  // ComputedRef<boolean>
  error,      // ComputedRef<string | null>
  load,       // () => Promise<Project | null>
} = useProject('project-id');
// or with reactive id:
} = useProject(projectIdRef);
```

### useQuery()

```ts
const { data, isLoading, isError, error, refetch } = useQuery({
  queryFn: () => client.organizations.get('org-id'),
  enabled: true,
  onSuccess: (data) => console.log('Loaded:', data),
  onError: (error) => console.error('Failed:', error),
});
```

### useMutation()

```ts
const { mutate, isLoading, isSuccess, error, reset } = useMutation({
  mutationFn: (data) => client.projects.create(data),
  onSuccess: (data) => console.log('Created:', data),
});

// Use it
mutate({ name: 'New Project' });
```

### useClient()

```ts
const client = useClient();
const result = await client.organizations.get('org-id');
```

## Provider Setup

```ts
// In your root component or main.ts
import { provideGrey, BrowserTokenStorage } from '@grey/vue';

provideGrey({
  apiBaseUrl: 'http://localhost:8080/api/v1',
  storage: new BrowserTokenStorage(),
  onAuthChange: (state) => console.log('Auth changed:', state),
  onLogout: () => router.push('/login'),
});
```

## License

MIT
