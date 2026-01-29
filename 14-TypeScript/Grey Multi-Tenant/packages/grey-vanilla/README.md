# @grey/vanilla

Vanilla JavaScript bindings for Grey Multi-Tenant.

No framework dependencies. Works with any JavaScript project.

## Installation

```bash
pnpm add @grey/vanilla
```

## Usage

### Basic Usage

```ts
import { createGrey } from '@grey/vanilla';

const grey = createGrey({ baseUrl: 'https://api.example.com' });

// Login
await grey.auth.login('email@example.com', 'password');
console.log(grey.auth.state.isAuthenticated);

// Get user
await grey.user.fetch();
console.log(grey.user.state.data?.name);

// List projects
await grey.projects.fetch();
console.log(grey.projects.state.items);

// Create project
await grey.projects.create({ name: 'New Project' });
```

### Observable State

```ts
// Subscribe to state changes
grey.auth.subscribe((state) => {
  console.log('Auth changed:', state.isAuthenticated);
});

// Works with any UI update mechanism
grey.projects.subscribe((state) => {
  document.getElementById('project-count')!.textContent = 
    `${state.items.length} projects`;
});
```

### With Custom Fetch

```ts
const grey = createGrey({
  baseUrl: 'https://api.example.com',
  fetchFn: (url, init) => fetch(url, { ...init, credentials: 'include' }),
});
```

## API

- `createGrey(options)` - Create Grey instance
- `grey.auth` - Auth state and methods
- `grey.user` - User state and methods
- `grey.projects` - Projects state and CRUD methods
- `grey.query(fn)` - Create custom query
- `grey.mutation(fn)` - Create custom mutation
