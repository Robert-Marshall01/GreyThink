# Grey Multi‑Tenant — Solid Framework Implementation

This file defines the Solid‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.  
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md  
- TEMPLATE_LIBRARY.md  
- The adapter core in /packages/grey-adapters  

Solid code must be idiomatic, signal‑based, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-solid
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

Solid uses:

- createSignal for reactive state
- createContext / useContext for providers
- createMemo for derived values
- No browser APIs during SSR
- All hooks must wrap the adapter core hooks
- All hooks must expose:
  - data (signal)
  - loading (signal)
  - error (signal)
  - domain actions (async functions)

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
useAuth.ts
-------------------------
- Use the HOOK TEMPLATE (Solid variant).
- Wrap useAuthCore.
- Use createSignal() for:
  - data
  - loading
  - error
- Expose async actions:
  - login
  - logout
  - refresh
- Normalize errors.
- Return an object of getters + actions.

-------------------------
useUser.ts
-------------------------
- Use the HOOK TEMPLATE (Solid variant).
- Wrap useUserCore.
- Expose:
  - data
  - loading
  - error
  - fetchUser

-------------------------
useProjects.ts
-------------------------
- Use the HOOK TEMPLATE (Solid variant).
- Wrap useProjectsCore.
- Expose:
  - data
  - loading
  - error
  - listProjects
  - createProject

-------------------------
useQuery.ts
-------------------------
- Use the HOOK TEMPLATE (Solid variant).
- Wrap useQueryCore.
- Expose:
  - data
  - loading
  - error
  - executeQuery

-------------------------
useMutation.ts
-------------------------
- Use the HOOK TEMPLATE (Solid variant).
- Wrap useMutationCore.
- Expose:
  - data
  - loading
  - error
  - executeMutation

-------------------------
GreyProvider.tsx
-------------------------
- Use the PROVIDER TEMPLATE.
- Create contexts for:
  - auth
  - user
  - projects
  - query
  - mutation
- Initialize each hook inside the provider.
- Provide via <Context.Provider value={...}>.
- Children must be rendered directly.
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

Solid imports:

import { createSignal, createContext, useContext } from "solid-js";

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