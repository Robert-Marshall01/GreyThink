# Grey Multi‑Tenant — Next.js Framework Implementation

This file defines the Next.js‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.  
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md  
- TEMPLATE_LIBRARY.md  
- The adapter core in /packages/grey-adapters  

Next.js code must be idiomatic, App‑Router‑based, server‑first, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-next
  package.json
  tsconfig.json
  /src
    /hooks
      useAuth.ts
      useUser.ts
      useProjects.ts
      useQuery.ts
      useMutation.ts
    /server
      auth.ts
      user.ts
      projects.ts
      query.ts
      mutation.ts
    /provider
      GreyProvider.tsx
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Next.js uses:

- React Server Components (default)
- Client Components only when needed (hooks)
- Server Actions for server‑side domain operations
- No browser APIs in server files
- All hooks must be `"use client"`
- All server wrappers must be `"use server"`
- All hooks must expose:
  - data
  - loading
  - error
  - domain actions (client‑side wrappers calling server actions)

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
HOOKS (client components)
-------------------------

All hooks must:

- Begin with `"use client"`
- Use the HOOK TEMPLATE
- Wrap server actions (not core functions directly)
- Use React state:
  - const [data, setData] = useState(null)
  - const [loading, setLoading] = useState(false)
  - const [error, setError] = useState(null)
- Normalize errors

-------------------------
useAuth.ts
-------------------------
- Wrap server actions from /server/auth.ts
- Expose:
  - data
  - loading
  - error
  - login()
  - logout()
  - refresh()

-------------------------
useUser.ts
-------------------------
- Wrap server actions from /server/user.ts
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
useProjects.ts
-------------------------
- Wrap server actions from /server/projects.ts
- Expose:
  - data
  - loading
  - error
  - listProjects()
  - createProject()

-------------------------
useQuery.ts
-------------------------
- Wrap server actions from /server/query.ts
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
useMutation.ts
-------------------------
- Wrap server actions from /server/mutation.ts
- Expose:
  - data
  - loading
  - error
  - executeMutation()

============================================================
SERVER ACTIONS (server components)
============================================================

All server files must:

- Begin with `"use server"`
- Use the SERVER WRAPPER TEMPLATE
- Wrap adapter‑core functions directly
- Export async functions:
  - login
  - logout
  - refresh
  - fetchUser
  - listProjects
  - createProject
  - executeQuery
  - executeMutation
- Normalize errors
- Never import React
- Never use browser APIs

-------------------------
auth.ts
-------------------------
- Wrap:
  - loginCore
  - logoutCore
  - refreshCore

-------------------------
user.ts
-------------------------
- Wrap:
  - fetchUserCore

-------------------------
projects.ts
-------------------------
- Wrap:
  - listProjectsCore
  - createProjectCore

-------------------------
query.ts
-------------------------
- Wrap:
  - queryCore

-------------------------
mutation.ts
-------------------------
- Wrap:
  - mutateCore

============================================================
PROVIDER (client component)
============================================================

GreyProvider.tsx must:

- Begin with `"use client"`
- Use the PROVIDER TEMPLATE
- Create React contexts for:
  - auth
  - user
  - projects
  - query
  - mutation
- Initialize hooks inside the provider
- Memoize the context value
- Render children directly
- No UI logic

============================================================
SSR RULES
============================================================

- Hooks must not access window/document during SSR.
- Server actions must not import client code.
- Providers must be client components.

============================================================
ERROR HANDLING
============================================================

All errors must be normalized into:

{
  message: string
  code?: string
  status?: number
  raw?: any
}

============================================================
IMPORT RULES
============================================================

Core imports (server only):

import {
  loginCore,
  logoutCore,
  refreshCore,
  fetchUserCore,
  listProjectsCore,
  createProjectCore,
  queryCore,
  mutateCore
} from "@grey/adapters";

Client imports:

import { useState } from "react";

============================================================
EXPORT RULES
============================================================

index.ts must export:

- all hooks
- all server actions
- GreyProvider

============================================================
END OF SPEC
============================================================