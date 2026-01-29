# Grey Astro App

Astro implementation of Grey Multi-Tenant.

## Status

**Placeholder** - Not yet implemented.

## Setup

```bash
pnpm install
pnpm dev
```

## Stack

- Astro
- React/Vue/Svelte islands (TBD)
- @grey/core-client SDK

## Pages

- `/login` - Authentication
- `/projects` - Project list
- `/projects/[id]` - Project details
- `/projects/new` - Create project

## Implementation Notes

Uses Astro with:
- Islands architecture
- Choice of UI framework for interactive components
- Static generation where possible
- Server endpoints for auth
