# Grey Next.js App

Next.js implementation of Grey Multi-Tenant using @grey/react.

## Status

**Placeholder** - Not yet implemented.

## Setup

```bash
pnpm install
pnpm dev
```

## Stack

- Next.js 14 (App Router)
- @grey/react hooks
- @grey/core-client SDK

## Pages

- `/login` - Authentication
- `/projects` - Project list
- `/projects/[id]` - Project details
- `/projects/new` - Create project

## Implementation Notes

Uses Next.js App Router with:
- Server Components where possible
- Client Components for interactive features
- Route handlers for BFF patterns (optional)
