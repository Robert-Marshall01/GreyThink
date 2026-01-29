# Grey Solid App

SolidJS implementation of Grey Multi-Tenant.

## Status

**Placeholder** - Not yet implemented.

## Setup

```bash
pnpm install
pnpm dev
```

## Stack

- SolidJS
- Solid Router
- @grey/adapters (custom Solid integration)
- @grey/core-client SDK

## Pages

- `/login` - Authentication
- `/projects` - Project list
- `/projects/:id` - Project details
- `/projects/new` - Create project

## Implementation Notes

Uses SolidJS with:
- Signals for reactive state
- Custom hooks wrapping @grey/adapters
- Fine-grained reactivity
