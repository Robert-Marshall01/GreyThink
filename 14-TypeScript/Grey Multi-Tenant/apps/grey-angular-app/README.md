# Grey Angular App

Angular implementation of Grey Multi-Tenant using @grey/angular.

## Status

**Placeholder** - Not yet implemented.

## Setup

```bash
pnpm install
pnpm dev
```

## Stack

- Angular 17+
- Angular Router
- @grey/angular services
- @grey/core-client SDK

## Pages

- `/login` - Authentication
- `/projects` - Project list
- `/projects/:id` - Project details
- `/projects/new` - Create project

## Implementation Notes

Uses Angular with:
- Standalone components
- AuthService for authentication
- UserService for current user
- ProjectsService for project operations
- RxJS observables for state management
