# @grey/stencil

Stencil bindings for Grey Multi-Tenant.

## Installation

```bash
pnpm add @grey/stencil
```

## Usage

### Store-based State Management

```tsx
import { Component, h } from '@stencil/core';
import { authStore, projectsStore } from '@grey/stencil';

@Component({
  tag: 'my-app',
})
export class MyApp {
  componentWillLoad() {
    initGrey({ baseUrl: 'https://api.example.com' });
  }

  render() {
    return (
      <div>
        {authStore.state.isAuthenticated ? (
          <project-list />
        ) : (
          <login-form />
        )}
      </div>
    );
  }
}
```

### Services

```tsx
import { authService } from '@grey/stencil';

async function handleLogin() {
  await authService.login('email@example.com', 'password');
}
```

## API

### Stores
- `authStore` - Auth state store
- `userStore` - User state store  
- `projectsStore` - Projects state store

### Services
- `authService` - Auth methods
- `userService` - User methods
- `projectsService` - Projects CRUD
