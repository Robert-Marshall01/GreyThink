# Grey Multi‑Tenant — Svelte Framework Implementation

This file defines the Svelte‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.  
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md  
- TEMPLATE_LIBRARY.md  
- The adapter core in /packages/grey-adapters  

Svelte code must be idiomatic, typed, store‑based, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-svelte
  package.json
  tsconfig.json
  /src
    /stores
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

Svelte uses:

- writable, readable, derived stores
- setContext/getContext for provider behavior
- No browser APIs during SSR
- All stores must wrap the adapter core functions
- All stores must expose:
  - data
  - loading
  - error
  - domain actions

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
auth.ts
-------------------------
- Use the HOOK TEMPLATE (Svelte store variant).
- Wrap useAuthCore.
- Use writable() for:
  - data
  - loading
  - error
- Expose async actions:
  - login
  - logout
  - refresh
- Normalize errors.

-------------------------
user.ts
-------------------------
- Use the HOOK TEMPLATE (Svelte store variant).
- Wrap useUserCore.
- Expose:
  - data
  - loading
  - error
  - fetchUser

-------------------------
projects.ts
-------------------------
- Use the HOOK TEMPLATE (Svelte store variant).
- Wrap useProjectsCore.
- Expose:
  - data
  - loading
  - error
  - listProjects
  - createProject

-------------------------
query.ts
-------------------------
- Use the HOOK TEMPLATE (Svelte store variant).
- Wrap useQueryCore.
- Expose:
  - data
  - loading
  - error
  - executeQuery

-------------------------
mutation.ts
-------------------------
- Use the HOOK TEMPLATE (Svelte store variant).
- Wrap useMutationCore.
- Expose:
  - data
  - loading
  - error
  - executeMutation

-------------------------
GreyProvider.svelte
-------------------------
- Use the PROVIDER TEMPLATE.
- In <script>:
  - Initialize each store.
  - Provide them via setContext().
- In markup:
  - <slot /> only.
- No UI logic.
- Must be SSR‑safe.

-------------------------
index.ts
-------------------------
- Use the INDEX EXPORT TEMPLATE.
- Export:
  - all stores
  - GreyProvider
- No logic, only exports.

============================================================
SSR RULES
============================================================

- Stores must not access window/document.
- Provider must not assume browser environment.
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

Core imports come from:

import {
  useAuthCore,
  useUserCore,
  useProjectsCore,
  useQueryCore,
  useMutationCore
} from "@grey/adapters";

============================================================
EXPORT RULES
============================================================

index.ts must export:

- auth store
- user store
- projects store
- query store
- mutation store
- GreyProvider

============================================================
END OF SPEC
============================================================