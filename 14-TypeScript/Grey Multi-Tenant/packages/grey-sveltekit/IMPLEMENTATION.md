# Grey Multi‑Tenant — SvelteKit Framework Implementation

This file defines the SvelteKit‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md
- TEMPLATE_LIBRARY.md
- The adapter core in /packages/grey-adapters

SvelteKit code must be idiomatic, SSR‑first, store‑based, and server‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-sveltekit
  package.json
  tsconfig.json
  /src
    /stores
      auth.ts
      user.ts
      projects.ts
      query.ts
      mutation.ts
    /server
      auth.ts
      user.ts
      projects.ts
      query.ts
      mutation.ts
    /provider
      GreyProvider.svelte
    index.ts

============================================================
FRAMEWORK RULES
============================================================

SvelteKit uses:

- writable() stores for client state
- SSR‑safe modules (no browser APIs in server code)
- Server routes for backend logic
- Server actions for mutations
- Universal modules when possible
- No direct browser APIs in load functions or server modules
- All stores must wrap server routes (not core functions directly)
- All stores must expose:
  - data
  - loading
  - error
  - domain actions

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
STORES (client)
-------------------------

All stores must:

- Use the HOOK TEMPLATE (Svelte store variant)
- Use writable() for:
  - data
  - loading
  - error
- Call server routes under /server/
- Normalize errors
- Never import browser‑only APIs

-------------------------
auth.ts
-------------------------
- Wrap server routes from /server/auth.ts
- Expose:
  - data
  - loading
  - error
  - login()
  - logout()
  - refresh()

-------------------------
user.ts
-------------------------
- Wrap server routes from /server/user.ts
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
projects.ts
-------------------------
- Wrap server routes from /server/projects.ts
- Expose:
  - data
  - loading
  - error
  - listProjects()
  - createProject()

-------------------------
query.ts
-------------------------
- Wrap server routes from /server/query.ts
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
mutation.ts
-------------------------
- Wrap server routes from /server/mutation.ts
- Expose:
  - data
  - loading
  - error
  - executeMutation()

============================================================
SERVER ROUTES (SvelteKit server modules)
============================================================

All server files must:

- Use the SERVER WRAPPER TEMPLATE
- Export functions via:
  - export async function POST()
  - export async function GET()
  - or named exports for server actions
- Wrap adapter‑core functions directly
- Normalize errors
- Never import Svelte or client code
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
PROVIDER (client)
============================================================

GreyProvider.svelte must:

- Use the PROVIDER TEMPLATE (Svelte variant)
- Initialize stores inside <script>
- Provide them via setContext()
- Render <slot />
- Contain no UI logic
- Be SSR‑safe

============================================================
SSR RULES
============================================================

- Stores must not access window/document.
- Server routes must not import client code.
- Providers must be SSR‑safe.
- All domain actions must be async.

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

import { writable } from "svelte/store";
import { setContext } from "svelte";

============================================================
EXPORT RULES
============================================================

index.ts must export:

- all stores
- all server routes
- GreyProvider

============================================================
END OF SPEC
============================================================