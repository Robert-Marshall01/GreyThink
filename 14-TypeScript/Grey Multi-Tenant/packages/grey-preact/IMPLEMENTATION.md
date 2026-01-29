# Grey Multi‑Tenant — Preact Framework Implementation

This file defines the Preact‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.  
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md  
- TEMPLATE_LIBRARY.md  
- The adapter core in /packages/grey-adapters  

Preact code must be idiomatic, hook‑based, lightweight, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-preact
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

Preact uses:

- Hooks identical to React:
  - useState
  - useEffect
  - useMemo
  - useCallback
- createContext / useContext for providers
- No browser APIs during SSR
- All hooks must wrap the adapter core hooks
- All hooks must expose:
  - data
  - loading
  - error
  - domain actions

Differences from React:

- Imports come from "preact/hooks" and "preact"
- Components must be lightweight
- No React.FC typing
- No synthetic events or React‑specific APIs

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
useAuth.ts
-------------------------
- Use the HOOK TEMPLATE.
- Wrap useAuthCore.
- Use:
  - const [data, setData] = useState(null)
  - const [loading, setLoading] = useState(false)
  - const [error, setError] = useState(null)
- Expose async actions:
  - login
  - logout
  - refresh
- Normalize errors.
- Must be fully typed.

-------------------------
useUser.ts
-------------------------
- Use the HOOK TEMPLATE.
- Wrap useUserCore.
- Expose:
  - data
  - loading
  - error
  - fetchUser

-------------------------
useProjects.ts
-------------------------
- Use the HOOK TEMPLATE.
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
- Use the HOOK TEMPLATE.
- Wrap useQueryCore.
- Expose:
  - data
  - loading
  - error
  - executeQuery

-------------------------
useMutation.ts
-------------------------
- Use the HOOK TEMPLATE.
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
- Provide via:
  <Context.Provider value={...}>{children}</Context.Provider>
- No UI logic.
- Must be SSR‑safe.

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

Preact imports:

import { h } from "preact";
import { useState, useEffect, useMemo, useCallback } from "preact/hooks";
import { createContext } from "preact";

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