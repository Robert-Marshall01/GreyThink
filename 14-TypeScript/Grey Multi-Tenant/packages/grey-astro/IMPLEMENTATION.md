# Grey Multi‑Tenant — Astro Framework Implementation

This file defines the Astro‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md
- TEMPLATE_LIBRARY.md
- The adapter core in /packages/grey-adapters

Astro code must be server‑first, island‑compatible, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-astro
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
      GreyProvider.astro
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Astro uses:

- Server‑first execution
- Client islands for interactive components
- No browser APIs in .astro frontmatter unless inside a client island
- Server endpoints for backend logic
- Stores for client‑side state (Svelte/React/Solid/Vue optional)
- All stores must wrap server endpoints (not core functions directly)
- All stores must expose:
  - data
  - loading
  - error
  - domain actions

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
STORES (client islands)
-------------------------

All stores must:

- Use the HOOK TEMPLATE (Svelte/React/Solid/Vue variant depending on chosen island)
- Use writable()/useState()/signals depending on island
- Call server endpoints under /server/
- Normalize errors
- Never import Astro server code directly

-------------------------
auth.ts
-------------------------
- Wrap server endpoints from /server/auth.ts
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
- Wrap server endpoints from /server/user.ts
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
projects.ts
-------------------------
- Wrap server endpoints from /server/projects.ts
- Expose:
  - data
  - loading
  - error
  - listProjects()
  - createProject()

-------------------------
query.ts
-------------------------
- Wrap server endpoints from /server/query.ts
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
mutation.ts
-------------------------
- Wrap server endpoints from /server/mutation.ts
- Expose:
  - data
  - loading
  - error
  - executeMutation()

============================================================
SERVER ENDPOINTS (Astro server routes)
============================================================

All server files must:

- Use the SERVER WRAPPER TEMPLATE
- Export functions via:
  - export async function GET()
  - export async function POST()
  - or named exports
- Wrap adapter‑core functions directly
- Normalize errors
- Never import client code
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
PROVIDER (Astro component)
============================================================

GreyProvider.astro must:

- Use the PROVIDER TEMPLATE (Astro variant)
- Provide stores to client islands via props or context
- Render <slot />
- Contain no UI logic
- Be SSR‑safe

Example structure:

---
import { authStore, userStore, ... } from "../stores";
---

<slot />

============================================================
SSR RULES
============================================================

- Stores must not access window/document unless inside a client island.
- Server endpoints must not import client code.
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

Client imports depend on chosen island framework.

============================================================
EXPORT RULES
============================================================

index.ts must export:

- all stores
- all server endpoints
- GreyProvider

============================================================
END OF SPEC
============================================================