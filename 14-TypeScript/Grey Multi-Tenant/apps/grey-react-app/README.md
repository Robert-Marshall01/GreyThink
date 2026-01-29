# Grey React App

React-based implementation of Grey Multi-Tenant using @grey/react.

## Status

**Placeholder** - Not yet implemented.

## Setup

```bash
pnpm install
pnpm dev
```

## Stack

- React 18
- React Router
- @grey/react hooks
- @grey/core-client SDK

## Pages

- `/login` - Authentication
- `/projects` - Project list
- `/projects/:id` - Project details
- `/projects/new` - Create project

## Implementation Notes

This app will use the same patterns as the reference SvelteKit app,
adapted for React idioms using:

- `useAuth()` for authentication
- `useUser()` for current user
- `useProjects()` for project list
- `useProject(id)` for single project
