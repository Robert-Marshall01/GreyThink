# Grey Multi‑Tenant — Nuxt Framework Implementation

This file defines the Nuxt‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md
- TEMPLATE_LIBRARY.md
- The adapter core in /packages/grey-adapters

Nuxt code must be idiomatic, SSR‑first, Composition‑API‑based, and Nitro‑compatible.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-nuxt
  package.json
  tsconfig.json
  /src
    /composables
      useAuth.ts
      useUser.ts
      useProjects.ts
      useQuery.ts
      useMutation.ts
    /server
      /auth.ts
      /user.ts
      /projects.ts
      /query.ts
      /mutation.ts
    /provider
      GreyProvider.vue
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Nuxt uses:

- Vue 3 Composition API
- defineNuxtPlugin for providers
- useState() for SSR‑safe state
- $fetch / server routes for server operations
- Nitro server functions for backend logic
- No browser APIs during SSR
- All composables must wrap server routes (not core functions directly)
- All composables must expose:
  - data
  - loading
  - error
  - domain actions

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
COMPOSABLES (client + SSR)
-------------------------

All composables must:

- Use the HOOK TEMPLATE (Vue variant)
- Use useState() for SSR‑safe state:
  - const data = useState(...)
  - const loading = useState(...)
  - const error = useState(...)
- Call server routes under /server/
- Normalize errors
- Never import browser‑only APIs

-------------------------
useAuth.ts
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
useUser.ts
-------------------------
- Wrap server routes from /server/user.ts
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
useProjects.ts
-------------------------
- Wrap server routes from /server/projects.ts
- Expose:
  - data
  - loading
  - error
  - listProjects()
  - createProject()

-------------------------
useQuery.ts
-------------------------
- Wrap server routes from /server/query.ts
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
useMutation.ts
-------------------------
- Wrap server routes from /server/mutation.ts
- Expose:
  - data
  - loading
  - error
  - executeMutation()

============================================================
SERVER ROUTES (Nitro server)
============================================================

All server files must:

- Use the SERVER WRAPPER TEMPLATE
- Export event handlers via defineEventHandler()
- Wrap adapter‑core functions directly
- Normalize errors
- Never import Vue or client code
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
PROVIDER (Nuxt plugin)
============================================================

GreyProvider.vue must:

- Use the PROVIDER TEMPLATE (Vue variant)
- Provide domain APIs via provide()
- Initialize composables inside setup()
- Render <slot />
- Contain no UI logic

Additionally, create a Nuxt plugin:

/provider/grey-provider.ts

- export default defineNuxtPlugin(...)
- inject:
  - auth
  - user
  - projects
  - query
  - mutation

============================================================
SSR RULES
============================================================

- Composables must not access window/document.
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

import { ref, computed, provide } from "vue";

Nuxt imports:

import { useState, defineNuxtPlugin } from "#app";

============================================================
EXPORT RULES
============================================================

index.ts must export:

- all composables
- all server routes
- GreyProvider
- Nuxt plugin

============================================================
END OF SPEC
============================================================