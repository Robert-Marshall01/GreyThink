# Grey Multi‑Tenant — Capacitor Framework Implementation

This file defines the Capacitor‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md
- TEMPLATE_LIBRARY.md
- The adapter core in /packages/grey-adapters

Capacitor code must be web‑safe, mobile‑safe, plugin‑safe, and free of Node APIs.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-capacitor
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
    /plugins
      storage.ts   (optional)
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Capacitor uses:

- React hooks (same as React Native)
- WebView execution (browser APIs allowed)
- Native plugins for persistence or secure storage
- No Node APIs (no fs, path, crypto unless via plugin)
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
- Must be Capacitor‑safe (no Node APIs).

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
OPTIONAL NATIVE PERSISTENCE
============================================================

If persistence is needed:

/plugins/storage.ts must:

- Wrap Capacitor Preferences or Secure Storage plugin
- Expose:
  - getItem(key)
  - setItem(key, value)
  - removeItem(key)
- Never store raw tokens unless encrypted

Hooks may optionally use this plugin, but only if explicitly requested.

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
- Be Capacitor‑safe (no Node APIs)

============================================================
RUNTIME RULES
============================================================

- Capacitor runs in a WebView; browser APIs are allowed.
- Node APIs are not allowed.
- Plugins must be called through Capacitor bridge only.
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

Capacitor imports (optional):

import { Preferences } from "@capacitor/preferences";

React imports:

import { useState, useEffect, useCallback } from "react";

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
- storage plugin (optional)

============================================================
END OF SPEC
============================================================