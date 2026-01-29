# GREY MULTI‑TENANT — MONOREPO MAP

This file gives Copilot a complete map of the Grey Multi‑Tenant monorepo.
Copilot must use this map to understand:
- where each package lives
- what type of framework it is
- which IMPLEMENTATION.md governs it
- which templates to use
- which files must be generated

=====================================================================
ROOT STRUCTURE
=====================================================================

/packages
  /grey-adapters
  /grey-react
  /grey-vue
  /grey-svelte
  /grey-qwik
  /grey-solid
  /grey-angular
  /grey-preact
  /grey-lit
  /grey-stencil
  /grey-next
  /grey-nuxt
  /grey-sveltekit
  /grey-remix
  /grey-astro
  /grey-node
  /grey-electron
  /grey-react-native
  /grey-expo
  /grey-capacitor
  /grey-dotnet

Each package contains:
- IMPLEMENTATION.md (framework‑specific rules)
- src/ (hooks, controllers, stores, services, providers)
- index.ts (or index.cs for .NET)

=====================================================================
PACKAGE TYPES
=====================================================================

Copilot must treat each package according to its framework type:

React-like (hooks + provider):
  - grey-react
  - grey-preact
  - grey-next (client hooks + server actions)
  - grey-remix
  - grey-react-native
  - grey-expo
  - grey-capacitor

Vue-like (composables + provider):
  - grey-vue
  - grey-nuxt

Svelte-like (stores + provider):
  - grey-svelte
  - grey-sveltekit
  - grey-astro

Qwik-like (signals + context provider):
  - grey-qwik

Solid-like (signals + context provider):
  - grey-solid

Angular-like (services + DI provider):
  - grey-angular

Web Component frameworks:
  - grey-lit (controllers + context)
  - grey-stencil (controllers + components)

Server-only frameworks:
  - grey-node (services)
  - grey-dotnet (services + DI)

Hybrid desktop:
  - grey-electron (main services + preload bridge + renderer hooks)

=====================================================================
IMPLEMENTATION.md LOCATIONS
=====================================================================

Copilot must always read the IMPLEMENTATION.md inside the package it is generating code for:

/packages/<framework>/IMPLEMENTATION.md

Examples:
- /packages/grey-react/IMPLEMENTATION.md
- /packages/grey-sveltekit/IMPLEMENTATION.md
- /packages/grey-dotnet/IMPLEMENTATION.md

=====================================================================
TEMPLATE LIBRARY USAGE
=====================================================================

Copilot must use the correct template based on the package type:

HOOK TEMPLATE:
  - React, Preact, Solid, Qwik, Vue (composables), React Native, Expo, Capacitor

STORE TEMPLATE:
  - Svelte, SvelteKit, Astro

CONTROLLER TEMPLATE:
  - Lit, Stencil

SERVICE TEMPLATE:
  - Node, .NET, Electron main

PROVIDER TEMPLATE:
  - React, Vue, Svelte, Qwik, Solid, Preact, Astro

SERVER WRAPPER TEMPLATE:
  - Next.js, Nuxt, SvelteKit, Remix, Astro

INDEX EXPORT TEMPLATE:
  - All frameworks

=====================================================================
ADAPTER CORE LOCATION
=====================================================================

All framework packages wrap the adapter core:

/packages/grey-adapters

Exports include:
- useAuthCore
- useUserCore
- useProjectsCore
- useQueryCore
- useMutationCore

And server variants:
- loginCore
- logoutCore
- refreshCore
- fetchUserCore
- listProjectsCore
- createProjectCore
- queryCore
- mutateCore

=====================================================================
FILES EACH PACKAGE MUST IMPLEMENT
=====================================================================

Every framework package must implement:

src/hooks or src/controllers or src/stores or src/services:
  - Auth
  - User
  - Projects
  - Query
  - Mutation

src/provider:
  - GreyProvider (if applicable)

index.ts (or index.cs):
  - Pure barrel file exporting exactly what IMPLEMENTATION.md specifies

=====================================================================
RULES FOR COPILOT
=====================================================================

When generating code for any package:

1. Read that package’s IMPLEMENTATION.md.
2. Use the correct template from TEMPLATE_LIBRARY.md.
3. Follow COPILOT_GLOBAL_RULES.md.
4. Generate only the files listed in IMPLEMENTATION.md.
5. Use only the imports allowed for that framework.
6. Never mix patterns between frameworks.
7. Never invent APIs, imports, or file names.
8. Normalize all errors into the shared error shape.
9. Keep index.ts files logic‑free.
10. Keep providers UI‑free.

=====================================================================
END OF MONOREPO MAP
=====================================================================