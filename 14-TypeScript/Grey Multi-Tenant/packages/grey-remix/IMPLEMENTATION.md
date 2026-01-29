# Grey Multi‑Tenant — Remix Framework Implementation

This file defines the Remix‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md
- TEMPLATE_LIBRARY.md
- The adapter core in /packages/grey-adapters

Remix code must be server‑first, loader/action‑driven, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-remix
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
      auth.server.ts
      user.server.ts
      projects.server.ts
      query.server.ts
      mutation.server.ts
    /provider
      GreyProvider.tsx
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Remix uses:

- Loaders for data fetching
- Actions for mutations
- React hooks for client state
- Server modules for backend logic
- No browser APIs in server files
- All hooks must wrap loaders/actions (not core functions directly)
- All hooks must expose:
  - data
  - loading
  - error
  - domain actions

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
HOOKS (client)
-------------------------

All hooks must:

- Use the HOOK TEMPLATE (React variant)
- Use React state:
  - const [data, setData] = useState(null)
  - const [loading, setLoading] = useState(false)
  - const [error, setError] = useState(null)
- Call Remix loaders/actions via fetcher or fetch()
- Normalize errors
- Never import server modules directly

-------------------------
useAuth.ts
-------------------------
- Wrap server functions from auth.server.ts
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
- Wrap server functions from user.server.ts
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
useProjects.ts
-------------------------
- Wrap server functions from projects.server.ts
- Expose:
  - data
  - loading
  - error
  - listProjects()
  - createProject()

-------------------------
useQuery.ts
-------------------------
- Wrap server functions from query.server.ts
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
useMutation.ts
-------------------------
- Wrap server functions from mutation.server.ts
- Expose:
  - data
  - loading
  - error
  - executeMutation()

============================================================
SERVER MODULES (server‑only)
============================================================

All server files must:

- Use the SERVER WRAPPER TEMPLATE
- Export functions for loaders/actions
- Wrap adapter‑core functions directly
- Normalize errors
- Never import React or client code
- Never use browser APIs

-------------------------
auth.server.ts
-------------------------
- Wrap:
  - loginCore
  - logoutCore
  - refreshCore

-------------------------
user.server.ts
-------------------------
- Wrap:
  - fetchUserCore

-------------------------
projects.server.ts
-------------------------
- Wrap:
  - listProjectsCore
  - createProjectCore

-------------------------
query.server.ts
-------------------------
- Wrap:
  - queryCore

-------------------------
mutation.server.ts
-------------------------
- Wrap:
  - mutateCore

============================================================
PROVIDER (client)
============================================================

GreyProvider.tsx must:

- Use the PROVIDER TEMPLATE (React variant)
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
- Server modules must not import client code.
- Providers must be client‑side only.

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
import { useFetcher } from "@remix-run/react";

============================================================
EXPORT RULES
============================================================

index.ts must export:

- all hooks
- all server modules
- GreyProvider

============================================================
END OF SPEC
============================================================