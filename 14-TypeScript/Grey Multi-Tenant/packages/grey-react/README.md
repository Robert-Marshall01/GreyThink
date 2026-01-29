# Grey React

React hooks for the Grey Multi-Tenant platform.

## Installation

```bash
pnpm add @grey/react @grey/core-client
```

## Quick Start

```tsx
import { GreyProvider, useAuth, useProjects } from '@grey/react';

function App() {
  return (
    <GreyProvider config={{ apiBaseUrl: 'http://localhost:8080/api/v1' }}>
      <MyApp />
    </GreyProvider>
  );
}

function MyApp() {
  const { isAuthenticated, user, login, logout } = useAuth();
  const { projects, isLoading, load } = useProjects();

  if (!isAuthenticated) {
    return <LoginForm onSubmit={login} />;
  }

  return (
    <div>
      <h1>Welcome, {user?.name}</h1>
      <button onClick={logout}>Logout</button>
      <ProjectList projects={projects} />
    </div>
  );
}
```

## Hooks

### useAuth()

Authentication state and methods.

```tsx
const {
  user,              // Current user or null
  isAuthenticated,   // boolean
  isLoading,         // boolean
  error,             // string or null
  login,             // (email, password) => Promise<boolean>
  logout,            // () => void
  restoreSession,    // () => Promise<boolean>
} = useAuth();
```

### useUser()

Current user data.

```tsx
const {
  user,       // User or null
  isLoading,  // boolean
  error,      // string or null
  refresh,    // () => Promise<User | null>
} = useUser();
```

### useProjects()

Projects list with pagination.

```tsx
const {
  projects,    // Project[]
  pagination,  // { page, page_size, total } or null
  isLoading,   // boolean
  error,       // string or null
  load,        // (page?, pageSize?) => Promise<void>
  create,      // (input) => Promise<Project | null>
} = useProjects();
```

### useProject(id)

Single project by ID.

```tsx
const {
  project,    // Project or null
  isLoading,  // boolean
  error,      // string or null
  load,       // () => Promise<Project | null>
} = useProject('project-id');
```

### useQuery()

Generic data fetching hook.

```tsx
const { data, isLoading, isError, error, refetch } = useQuery({
  queryFn: () => client.organizations.get('org-id'),
  enabled: true,
  onSuccess: (data) => console.log('Loaded:', data),
  onError: (error) => console.error('Failed:', error),
});
```

### useMutation()

Generic mutation hook.

```tsx
const { mutate, isLoading, isSuccess, error, reset } = useMutation({
  mutationFn: (data) => client.projects.create(data),
  onSuccess: (data) => console.log('Created:', data),
  onError: (error) => console.error('Failed:', error),
});

// Use it
mutate({ name: 'New Project' });
```

### useClient()

Access the raw Grey API client.

```tsx
const client = useClient();
const result = await client.organizations.get('org-id');
```

## Provider Configuration

```tsx
<GreyProvider
  config={{
    apiBaseUrl: 'http://localhost:8080/api/v1',
    storage: new BrowserTokenStorage(), // optional, uses localStorage
    onAuthChange: (state) => console.log('Auth changed:', state),
    onLogout: () => router.push('/login'),
  }}
>
  {children}
</GreyProvider>
```

## License

MIT
