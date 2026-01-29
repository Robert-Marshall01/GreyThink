# Grey Multi‑Tenant — Qwik Framework Implementation

This file defines the Qwik‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.  
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md  
- TEMPLATE_LIBRARY.md  
- The adapter core in /packages/grey-adapters  

Qwik code must be resumable, signal‑based, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-qwik
  package.json
  tsconfig.json
  /src
    /hooks
      useAuth.ts
      useUser.ts
      useProjects.ts
      useQuery.ts
      useMutation.ts
    /provider
      GreyProvider.tsx
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Qwik uses:

- useSignal() for reactive state
- useTask$ for reactive effects
- ContextId + useContextProvider/useContext for providers
- No browser APIs during SSR
- All hooks must wrap the adapter core hooks
- All hooks must expose:
  - data (signal)
  - loading (signal)
  - error (signal)
  - domain actions (async functions wrapped in $())

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
useAuth.ts
-------------------------
- Use the HOOK TEMPLATE (Qwik variant).
- Wrap useAuthCore.
- Use:
  - const data = useSignal(null)
  - const loading = useSignal(false)
  - const error = useSignal(null)
- Domain actions must be wrapped in $():
  - const login = $(async () => { ... })
  - const logout = $(async () => { ... })
  - const refresh = $(async () => { ... })
- Normalize errors.
- No direct browser APIs.

-------------------------
useUser.ts
-------------------------
- Use the HOOK TEMPLATE (Qwik variant).
- Wrap useUserCore.
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
useProjects.ts
-------------------------
- Use the HOOK TEMPLATE (Qwik variant).
- Wrap useProjectsCore.
- Expose:
  - data
  - loading
  - error
  - listProjects()
  - createProject()

-------------------------
useQuery.ts
-------------------------
- Use the HOOK TEMPLATE (Qwik variant).
- Wrap useQueryCore.
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
useMutation.ts
-------------------------
- Use the HOOK TEMPLATE (Qwik variant).
- Wrap useMutationCore.
- Expose:
  - data
  - loading
  - error
  - executeMutation()

-------------------------
GreyProvider.tsx
-------------------------
- Use the PROVIDER TEMPLATE.
- Create ContextIds for:
  - auth
  - user
  - projects
  - query
  - mutation
- Initialize each hook inside the provider component.
- Provide values via useContextProvider().
- Render children directly.
- No UI logic.

-------------------------
index.ts
-------------------------
- Use the INDEX EXPORT TEMPLATE.
- Export:
  - all hooks
  - GreyProvider
- No logic, only exports.

============================================================
SSR RULES
============================================================

- Hooks must not access window/document.
- Provider must not assume browser environment.
- All domain actions must be async and wrapped in $().

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

Qwik imports:

import { useSignal, $, useTask$, createContextId, useContextProvider } from "@builder.io/qwik";

============================================================
EXPORT RULES
============================================================

index.ts must export:

- useAuth
- useUser
- useProjects
- useQuery
- useMutation
- GreyProvider

============================================================
END OF SPEC
============================================================