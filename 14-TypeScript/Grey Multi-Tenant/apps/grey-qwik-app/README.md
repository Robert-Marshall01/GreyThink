# Grey Qwik App

Qwik implementation of Grey Multi-Tenant.

## Status

**Placeholder** - Not yet implemented.

## Setup

```bash
pnpm install
pnpm dev
```

## Stack

- Qwik
- Qwik City
- @grey/adapters (custom Qwik integration)
- @grey/core-client SDK

## Pages

- `/login` - Authentication
- `/projects` - Project list
- `/projects/[id]` - Project details
- `/projects/new` - Create project

## Implementation Notes

Uses Qwik with:
- Resumability for instant loading
- useSignal/useStore for state
- Custom hooks wrapping @grey/adapters
