# Grey Multi‑Tenant — Angular Framework Implementation

This file defines the Angular‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.  
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md  
- TEMPLATE_LIBRARY.md  
- The adapter core in /packages/grey-adapters  

Angular code must be idiomatic, typed, DI‑driven, and SSR‑safe.

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-angular
  package.json
  tsconfig.json
  /src
    /services
      auth.service.ts
      user.service.ts
      projects.service.ts
      query.service.ts
      mutation.service.ts
    /provider
      grey-provider.ts
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Angular uses:

- Injectable services
- Signals (preferred) or BehaviorSubjects
- Dependency Injection
- No browser APIs during SSR
- All services must wrap the adapter core functions
- All services must expose:
  - data (signal)
  - loading (signal)
  - error (signal)
  - domain actions (async methods)

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

-------------------------
auth.service.ts
-------------------------
- Use the HOOK TEMPLATE (Angular service variant).
- Wrap useAuthCore.
- Use Angular signals:
  - data = signal(null)
  - loading = signal(false)
  - error = signal(null)
- Expose async methods:
  - login()
  - logout()
  - refresh()
- Normalize errors.
- Mark service as:
  @Injectable({ providedIn: 'root' })

-------------------------
user.service.ts
-------------------------
- Use the HOOK TEMPLATE (Angular service variant).
- Wrap useUserCore.
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
projects.service.ts
-------------------------
- Use the HOOK TEMPLATE (Angular service variant).
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
- Use the HOOK TEMPLATE (Angular service variant).
- Wrap useQueryCore.
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
mutation.service.ts
-------------------------
- Use the HOOK TEMPLATE (Angular service variant).
- Wrap useMutationCore.
- Expose:
  - data
  - loading
  - error
  - executeMutation()

-------------------------
grey-provider.ts
-------------------------
- Use the PROVIDER TEMPLATE.
- Provide all services using Angular’s DI system.
- Export a function:
  export function provideGrey() { return [AuthService, UserService, ...]; }
- No UI logic.
- Must be SSR‑safe.

-------------------------
index.ts
-------------------------
- Use the INDEX EXPORT TEMPLATE.
- Export:
  - all services
  - provideGrey()
- No logic, only exports.

============================================================
SSR RULES
============================================================

- Services must not access window/document.
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

Angular imports:

import { Injectable, signal } from '@angular/core';

============================================================
EXPORT RULES
============================================================

index.ts must export:

- AuthService
- UserService
- ProjectsService
- QueryService
- MutationService
- provideGrey

============================================================
END OF SPEC
============================================================