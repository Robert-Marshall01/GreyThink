# Grey Vue App

Vue 3 implementation of Grey Multi-Tenant using @grey/vue.

## Status

**Placeholder** - Not yet implemented.

## Setup

```bash
pnpm install
pnpm dev
```

## Stack

- Vue 3 (Composition API)
- Vue Router
- @grey/vue composables
- @grey/core-client SDK

## Pages

- `/login` - Authentication
- `/projects` - Project list
- `/projects/:id` - Project details
- `/projects/new` - Create project

## Implementation Notes

Uses Vue 3 Composition API with:
- `useAuth()` composable
- `useUser()` composable
- `useProjects()` composable
- `useProject(id)` composable
