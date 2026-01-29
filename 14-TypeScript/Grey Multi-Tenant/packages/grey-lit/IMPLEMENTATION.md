# Grey Multi‑Tenant — Lit Framework Implementation

This file defines the Lit‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.  
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md  
- TEMPLATE_LIBRARY.md  
- The adapter core in /packages/grey-adapters  

Lit code must be idiomatic, controller‑based, reactive, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-lit
  package.json
  tsconfig.json
  /src
    /controllers
      auth-controller.ts
      user-controller.ts
      projects-controller.ts
      query-controller.ts
      mutation-controller.ts
    /provider
      grey-provider.ts
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Lit uses:

- Reactive controllers for logic (equivalent to hooks)
- @lit-labs/context for dependency injection
- Custom elements for providers
- No browser APIs during SSR
- All controllers must wrap the adapter core functions
- All controllers must expose:
  - data (reactive)
  - loading (reactive)
  - error (reactive)
  - domain actions (async methods)

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
auth-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Lit controller variant).
- Wrap useAuthCore.
- Use ReactiveController + host.requestUpdate().
- Internal state:
  - data
  - loading
  - error
- Expose async actions:
  - login
  - logout
  - refresh
- Normalize errors.
- Must call host.requestUpdate() after state changes.

-------------------------
user-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Lit controller variant).
- Wrap useUserCore.
- Expose:
  - data
  - loading
  - error
  - fetchUser

-------------------------
projects-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Lit controller variant).
- Wrap useProjectsCore.
- Expose:
  - data
  - loading
  - error
  - listProjects
  - createProject

-------------------------
query-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Lit controller variant).
- Wrap useQueryCore.
- Expose:
  - data
  - loading
  - error
  - executeQuery

-------------------------
mutation-controller.ts
-------------------------
- Use the HOOK TEMPLATE (Lit controller variant).
- Wrap useMutationCore.
- Expose:
  - data
  - loading
  - error
  - executeMutation

-------------------------
grey-provider.ts
-------------------------
- Use the PROVIDER TEMPLATE.
- Use @lit-labs/context to create Contexts for:
  - auth
  - user
  - projects
  - query
  - mutation
- Provide controllers via:
  provide(context, value)
- Implement as a custom element:
  class GreyProvider extends LitElement { ... }
- Render <slot></slot>.
- No UI logic.

-------------------------
index.ts
-------------------------
- Use the INDEX EXPORT TEMPLATE.
- Export:
  - all controllers
  - GreyProvider
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

Lit imports:

import { ReactiveController, ReactiveControllerHost } from "lit";
import { createContext, provide } from "@lit-labs/context";

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