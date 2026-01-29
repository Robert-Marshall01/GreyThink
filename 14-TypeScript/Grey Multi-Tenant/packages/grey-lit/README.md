# @grey/lit

Lit bindings for Grey Multi-Tenant using Lit's reactive controllers.

## Installation

```bash
pnpm add @grey/lit lit
```

## Usage

```ts
import { LitElement, html } from 'lit';
import { customElement } from 'lit/decorators.js';
import { AuthController, ProjectsController } from '@grey/lit';

@customElement('my-app')
export class MyApp extends LitElement {
  private auth = new AuthController(this, {
    baseUrl: 'https://api.example.com',
  });
  
  private projects = new ProjectsController(this);

  render() {
    if (!this.auth.isAuthenticated) {
      return html`<button @click=${this.login}>Login</button>`;
    }
    
    return html`
      <ul>
        ${this.projects.items.map(p => html`<li>${p.name}</li>`)}
      </ul>
    `;
  }
  
  private async login() {
    await this.auth.login('email@example.com', 'password');
  }
}
```

## API

### Controllers
- `AuthController` - Authentication reactive controller
- `UserController` - User data reactive controller
- `ProjectsController` - Projects CRUD reactive controller
- `QueryController` - Generic query reactive controller
- `MutationController` - Generic mutation reactive controller

### Mixins
- `GreyMixin` - Mixin that adds Grey client to any LitElement
