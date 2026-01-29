# Grey Multi‑Tenant — Stencil Framework Implementation

This file defines the Stencil‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.  
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md  
- TEMPLATE_LIBRARY.md  
- The adapter core in /packages/grey-adapters  

Stencil code must be idiomatic, reactive, Web‑Component‑based, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-stencil
  package.json
  tsconfig.json
  stencil.config.ts
  /src
    /controllers
      auth-controller.ts
      user-controller.ts
      projects-controller.ts
      query-controller.ts
      mutation-controller.ts
    /provider
      grey-provider.tsx
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Stencil uses:

- @State() for reactive fields
- @Method() for public async actions
- @Element() for host reference
- Context via custom DI or @stencil/store
- No browser APIs during SSR
- All controllers must wrap the adapter core functions
- All controllers must expose:
  - data (@State)
  - loading (@State)
  - error (@State)
  - domain actions (@Method)

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
auth-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Stencil controller variant).
- Wrap useAuthCore.
- Use:
  @State() data = null
  @State() loading = false
  @State() error = null
- Expose async actions via @Method():
  - login()
  - logout()
  - refresh()
- Normalize errors.
- Must call forceUpdate() or rely on @State() reactivity.

-------------------------
user-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Stencil controller variant).
- Wrap useUserCore.
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
projects-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Stencil controller variant).
- Wrap useProjectsCore.
- Expose:
  - data
  - loading
  - error
  - listProjects()
  - createProject()

-------------------------
query-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Stencil controller variant).
- Wrap useQueryCore.
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
mutation-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Stencil controller variant).
- Wrap useMutationCore.
- Expose:
  - data
  - loading
  - error
  - executeMutation()

-------------------------
grey-provider.tsx
-------------------------
- Use the PROVIDER TEMPLATE.
- Implement as a Stencil component:
  @Component({ tag: 'grey-provider', shadow: true })
- Provide controllers via:
  - custom DI
  - or @stencil/store
- Render <slot></slot>.
- No UI logic.
- Must be SSR‑safe.

-------------------------
index.ts
-------------------------
- Use the INDEX EXPORT TEMPLATE.
- Export:
  - all controllers
  - GreyProvider component
- No logic, only exports.

============================================================
SSR RULES
============================================================

- Controllers must not access window/document.
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

Stencil imports:

import { Component, State, Method, Element, h } from "@stencil/core";

============================================================
EXPORT RULES
============================================================

index.ts must export:

- AuthController
- UserController
- ProjectsController
- QueryController
- MutationController
- GreyProvider

============================================================
END OF SPEC
============================================================