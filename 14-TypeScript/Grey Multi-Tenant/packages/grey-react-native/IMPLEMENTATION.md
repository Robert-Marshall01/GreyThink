# Grey Multi‑Tenant — React Native Framework Implementation

This file defines the React‑Native‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md
- TEMPLATE_LIBRARY.md
- The adapter core in /packages/grey-adapters

React Native code must be idiomatic, mobile‑safe, hook‑based, and free of browser/DOM APIs.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-react-native
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

React Native uses:

- React hooks (useState, useEffect, useCallback, useMemo)
- Context for dependency injection
- No DOM APIs (no window, document, localStorage)
- Optional: AsyncStorage for persistence (only if explicitly requested)
- All hooks must wrap adapter‑core hooks
- All hooks must expose:
  - data
  - loading
  - error
  - domain actions

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
useAuth.ts
-------------------------
- Use the HOOK TEMPLATE (React variant).
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
- Must be mobile‑safe (no browser APIs).

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

============================================================
PROVIDER
============================================================

GreyProvider.tsx must:

- Use the PROVIDER TEMPLATE (React variant)
- Create contexts for:
  - auth
  - user
  - projects
  - query
  - mutation
- Initialize hooks inside the provider
- Memoize the context value
- Render children directly
- Contain no UI logic
- Be mobile‑safe (no DOM APIs)

============================================================
SSR / RUNTIME RULES
============================================================

- React Native has no SSR; ignore SSR‑specific rules.
- Hooks must not access browser APIs.
- Providers must not assume DOM availability.
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

Core imports:

import {
  useAuthCore,
  useUserCore,
  useProjectsCore,
  useQueryCore,
  useMutationCore
} from "@grey/adapters";

React Native imports:

import { useState, useEffect, useCallback } from "react";

No DOM imports.

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