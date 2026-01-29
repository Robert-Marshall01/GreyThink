# Grey Multi‑Tenant — Electron Framework Implementation

This file defines the Electron‑specific rules, folder structure, and file responsibilities for the Grey Multi‑Tenant SDK.
All files in this package must follow:

- COPILOT_GLOBAL_RULES.md
- TEMPLATE_LIBRARY.md
- The adapter core in /packages/grey-adapters

Electron code must be secure, IPC‑based, and split cleanly between:
- Main process (trusted)
- Preload (bridge)
- Renderer (untrusted)

============================================================
FOLDER STRUCTURE
============================================================

/packages/grey-electron
  package.json
  tsconfig.json
  /src
    /main
      auth.service.ts
      user.service.ts
      projects.service.ts
      query.service.ts
      mutation.service.ts
      ipc.ts
    /preload
      bridge.ts
    /renderer
      useAuth.ts
      useUser.ts
      useProjects.ts
      useQuery.ts
      useMutation.ts
      GreyProvider.tsx
    index.ts

============================================================
FRAMEWORK RULES
============================================================

Electron uses:

- Main process for all privileged logic
- Preload scripts for secure IPC bridging
- Renderer for UI logic only
- No adapter‑core imports in the renderer
- All adapter‑core calls must happen in the main process
- All renderer hooks must call preload bridge functions
- All IPC must be:
  - typed
  - validated
  - one‑directional (renderer → preload → main)

============================================================
FILE‑LEVEL SPECIFICATIONS
============================================================

============================================================
MAIN PROCESS SERVICES
============================================================

All main services must:

- Use the SERVICE TEMPLATE (Node variant)
- Wrap adapter‑core functions directly
- Never expose raw adapter errors
- Register IPC handlers in ipc.ts

-------------------------
auth.service.ts
-------------------------
- Wrap:
  - loginCore
  - logoutCore
  - refreshCore
- Expose:
  - async login()
  - async logout()
  - async refresh()

-------------------------
user.service.ts
-------------------------
- Wrap:
  - fetchUserCore
- Expose:
  - async fetchUser()

-------------------------
projects.service.ts
-------------------------
- Wrap:
  - listProjectsCore
  - createProjectCore
- Expose:
  - async listProjects()
  - async createProject()

-------------------------
query.service.ts
-------------------------
- Wrap:
  - queryCore
- Expose:
  - async executeQuery()

-------------------------
mutation.service.ts
-------------------------
- Wrap:
  - mutateCore
- Expose:
  - async executeMutation()

-------------------------
ipc.ts
-------------------------
- Register IPC channels for:
  - auth:login
  - auth:logout
  - auth:refresh
  - user:fetch
  - projects:list
  - projects:create
  - query:execute
  - mutation:execute
- Validate inputs
- Normalize errors
- Return structured responses

============================================================
PRELOAD BRIDGE
============================================================

bridge.ts must:

- Use contextBridge.exposeInMainWorld()
- Expose a typed API:
  - auth.login()
  - auth.logout()
  - auth.refresh()
  - user.fetchUser()
  - projects.listProjects()
  - projects.createProject()
  - query.executeQuery()
  - mutation.executeMutation()
- Never expose ipcRenderer directly
- Validate all arguments

============================================================
RENDERER HOOKS
============================================================

All renderer hooks must:

- Use the HOOK TEMPLATE (React variant)
- Use:
  - useState()
  - useEffect()
  - useCallback()
- Call preload bridge functions (never IPC directly)
- Normalize errors

-------------------------
useAuth.ts
-------------------------
- Wrap preload.auth.*
- Expose:
  - data
  - loading
  - error
  - login()
  - logout()
  - refresh()

-------------------------
useUser.ts
-------------------------
- Wrap preload.user.fetchUser
- Expose:
  - data
  - loading
  - error
  - fetchUser()

-------------------------
useProjects.ts
-------------------------
- Wrap preload.projects.*
- Expose:
  - data
  - loading
  - error
  - listProjects()
  - createProject()

-------------------------
useQuery.ts
-------------------------
- Wrap preload.query.executeQuery
- Expose:
  - data
  - loading
  - error
  - executeQuery()

-------------------------
useMutation.ts
-------------------------
- Wrap preload.mutation.executeMutation
- Expose:
  - data
  - loading
  - error
  - executeMutation()

============================================================
PROVIDER (renderer)
============================================================

GreyProvider.tsx must:

- Use the PROVIDER TEMPLATE (React variant)
- Initialize hooks inside the provider
- Memoize context values
- Render children directly
- Contain no UI logic

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

Renderer must never receive raw adapter errors.

============================================================
SECURITY RULES
============================================================

- No adapter‑core imports in renderer or preload
- No ipcRenderer exposed globally
- All IPC channels must be explicitly whitelisted
- All inputs must be validated in main
- No eval, Function(), or dynamic require()

============================================================
IMPORT RULES
============================================================

Core imports (main only):

import {
  loginCore,
  logoutCore,
  refreshCore,
  fetchUserCore,
  listProjectsCore,
  createProjectCore,
  queryCore,
  mutateCore
} from "@grey/adapters";

Renderer imports:

import { useState, useCallback } from "react";

Preload imports:

import { contextBridge, ipcRenderer } from "electron";

============================================================
EXPORT RULES
============================================================

index.ts must export:

- all renderer hooks
- GreyProvider
- preload bridge types
- main services (optional)

============================================================
END OF SPEC
============================================================