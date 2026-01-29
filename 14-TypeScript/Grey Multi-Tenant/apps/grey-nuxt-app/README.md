# Grey Nuxt App

Nuxt 3 implementation of Grey Multi-Tenant using @grey/vue.

## Status

**Placeholder** - Not yet implemented.

## Setup

```bash
pnpm install
pnpm dev
```

## Stack

- Nuxt 3
- @grey/vue composables
- @grey/core-client SDK

## Pages

- `/login` - Authentication
- `/projects` - Project list
- `/projects/[id]` - Project details
- `/projects/new` - Create project

## Implementation Notes

Uses Nuxt 3 with:
- Auto-imported composables
- File-based routing
- Server routes for BFF patterns (optional)
