# Grey Multi‑Tenant — Node Framework Implementation

This file defines the Node‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md
- TEMPLATE_LIBRARY.md
- The adapter core in /packages/grey-adapters

Node code must be idiomatic, ESM‑based, server‑only, and dependency‑injection‑friendly.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-node
  package.json
  tsconfig.json
  /src
    /services
      auth.service.ts
      user.service.ts
      projects.service.ts
      query.service.ts
      mutation.service.ts
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Node uses:

- Pure ESM modules
- No browser APIs
- Service classes instead of hooks
- Direct calls to adapter‑core functions
- No global state
- All services must expose:
  - data (internal field)
  - loading (internal field)
  - error (internal field)
  - domain actions (async methods)

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
auth.service.ts
-------------------------
- Use the SERVICE TEMPLATE (Node variant).
- Wrap useAuthCore.
- Internal fields:
  - this.data = null
  - this.loading = false
  - this.error = null
- Expose async methods:
  - login()
  - logout()
  - refresh()
- Normalize errors.
- No side effects outside the class.

-------------------------
user.service.ts
-------------------------
- Use the SERVICE TEMPLATE (Node variant).
- Wrap useUserCore.
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
projects.service.ts
-------------------------
- Use the SERVICE TEMPLATE (Node variant).
- Wrap useProjectsCore.
- Expose:
  - data
  - loading
  - error
  - listProjects()
  - createProject()

-------------------------
query.service.ts
-------------------------
- Use the SERVICE TEMPLATE (Node variant).
- Wrap useQueryCore.
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
mutation.service.ts
-------------------------
- Use the SERVICE TEMPLATE (Node variant).
- Wrap useMutationCore.
- Expose:
  - data
  - loading
  - error
  - executeMutation()

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

Services must never throw raw adapter errors.

============================================================
SSR / RUNTIME RULES
============================================================

- Node services must not import window/document.
- No top‑level async except for initialization.
- All domain actions must be async.
- No global mutable state.

============================================================
IMPORT RULES
============================================================

Core imports:

import {
  useAuthCore,
  useUserCore,
  useProjectsCore,
  useQueryCore,
  useMutationCore
} from "@grey/adapters";

Node imports:

- Standard ESM only
- No CommonJS require()

============================================================
EXPORT RULES
============================================================

index.ts must export:

- AuthService
- UserService
- ProjectsService
- QueryService
- MutationService

============================================================
END OF SPEC
============================================================