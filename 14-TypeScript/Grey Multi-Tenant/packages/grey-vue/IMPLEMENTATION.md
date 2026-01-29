# Grey Multi‑Tenant — Vue Framework Implementation

This file defines the Vue‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.  
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md  
- TEMPLATE_LIBRARY.md  
- The adapter core in /packages/grey-adapters  

Vue code must be idiomatic, typed, Composition‑API‑based, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-vue
  package.json
  tsconfig.json
  /src
    /composables
      useAuth.ts
      useUser.ts
      useProjects.ts
      useQuery.ts
      useMutation.ts
    /provider
      GreyProvider.vue
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Vue uses:

- Composition API
- ref, reactive, computed
- provide/inject for context
- No browser APIs during SSR
- All composables must wrap the adapter core composables
- All composables must expose:
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
- Use the HOOK TEMPLATE.
- Wrap useAuthCore.
- Use:
  - ref() for data, loading, error
  - async functions for domain actions
- Expose:
  - data
  - loading
  - error
  - login
  - logout
  - refresh
- Normalize errors.

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
GreyProvider.vue
-------------------------
- Use the PROVIDER TEMPLATE.
- Provide domain APIs via provide().
- Initialize each composable inside setup().
- Provide:
  - auth
  - user
  - projects
  - query
  - mutation
- No UI logic.
- Template should be a simple <slot /> passthrough.

-------------------------
index.ts
-------------------------
- Use the INDEX EXPORT TEMPLATE.
- Export:
  - all composables
  - GreyProvider
- No logic, only exports.

============================================================
SSR RULES
============================================================

- Composables must not access window/document.
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

- useAuth
- useUser
- useProjects
- useQuery
- useMutation
- GreyProvider

============================================================
END OF SPEC
============================================================