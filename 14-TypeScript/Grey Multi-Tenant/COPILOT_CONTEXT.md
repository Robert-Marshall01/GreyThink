# GREY MULTI‑TENANT — GLOBAL COPILOT CONTEXT

This file defines the global architectural rules for the entire Grey Multi‑Tenant monorepo.
Copilot must treat this file as the highest‑priority context when generating code.

=====================================================================
ARCHITECTURE OVERVIEW
=====================================================================

Grey Multi‑Tenant is a multi‑framework SDK built on a single shared core:

1. @grey/adapters (the adapter core)
2. Framework packages (React, Vue, Svelte, Qwik, Solid, Angular, Lit, Stencil, Next.js, Nuxt, SvelteKit, Remix, Astro, Node, Electron, React Native, Expo, Capacitor, .NET, etc.)
3. Each framework package has:
   - IMPLEMENTATION.md (framework‑specific rules)
   - A set of hooks/controllers/stores/services
   - A Provider component (if applicable)
   - A pure index.ts barrel file

All framework packages must wrap the same domain surface:

- Auth
- User
- Projects
- Query
- Mutation

All domain actions must be async.

=====================================================================
GLOBAL RULES FOR ALL FRAMEWORKS
=====================================================================

Copilot must obey these rules for every file in every package:

1. Follow COPILOT_GLOBAL_RULES.md exactly.
2. Follow TEMPLATE_LIBRARY.md exactly.
3. Follow the package’s IMPLEMENTATION.md exactly.
4. Never invent APIs, imports, or behaviors not defined in the above.
5. Never add UI logic to providers.
6. Never add side effects outside of allowed patterns.
7. Normalize all errors into the shared error shape:

{
  message: string
  code?: string
  status?: number
  raw?: any
}

8. All hooks/controllers/stores/services must expose:
   - data
   - loading
   - error
   - domain actions

9. All domain actions must:
   - be async
   - wrap adapter‑core functions (or server actions / server routes / IPC / etc. depending on framework)
   - normalize errors
   - update state consistently

10. All index.ts files must:
    - export exactly the symbols listed in IMPLEMENTATION.md
    - contain no logic

=====================================================================
TEMPLATE LIBRARY SUMMARY
=====================================================================

Copilot must use the templates from TEMPLATE_LIBRARY.md:

- HOOK TEMPLATE (React, Vue, Svelte, Qwik, Solid, Preact, etc.)
- CONTROLLER TEMPLATE (Lit, Stencil)
- STORE TEMPLATE (SvelteKit, Astro)
- SERVICE TEMPLATE (Node, .NET, Electron main)
- PROVIDER TEMPLATE (React, Vue, Svelte, Qwik, Solid, Preact, Astro)
- SERVER WRAPPER TEMPLATE (Next.js, Nuxt, SvelteKit, Remix, Astro)
- INDEX EXPORT TEMPLATE (all frameworks)

Templates define:
- File structure
- Imports
- State shape
- Error handling
- Domain action patterns
- Export rules

Copilot must follow templates exactly.

=====================================================================
ADAPTER CORE CONTRACT
=====================================================================

Framework packages must wrap these core functions:

useAuthCore
useUserCore
useProjectsCore
useQueryCore
useMutationCore

Or, in server environments:

loginCore
logoutCore
refreshCore
fetchUserCore
listProjectsCore
createProjectCore
queryCore
mutateCore

No framework package may call any other core function.

=====================================================================
FRAMEWORK IMPLEMENTATION CONTRACT
=====================================================================

Each framework package contains an IMPLEMENTATION.md that defines:

- Folder structure
- File responsibilities
- Import rules
- SSR rules
- Runtime constraints
- Error handling rules
- Export rules

Copilot must treat IMPLEMENTATION.md as the authoritative specification for that package.

=====================================================================
COPILOT WORKFLOW RULES
=====================================================================

When generating code:

1. Read IMPLEMENTATION.md for the current package.
2. Read TEMPLATE_LIBRARY.md.
3. Apply COPILOT_GLOBAL_RULES.md.
4. Generate the file using the correct template.
5. Ensure imports match the framework’s import rules.
6. Ensure exports match the framework’s export rules.
7. Ensure no logic appears in index.ts.
8. Ensure providers contain no UI logic.
9. Ensure all domain actions are async and normalized.
10. Ensure no browser APIs appear in SSR/server‑only files.
11. Ensure no Node APIs appear in client‑only files.

=====================================================================
VALIDATION RULES
=====================================================================

Copilot must validate its output against:

- IMPLEMENTATION.md
- TEMPLATE_LIBRARY.md
- COPILOT_GLOBAL_RULES.md

If anything conflicts, the order of authority is:

1. IMPLEMENTATION.md (framework‑specific)
2. TEMPLATE_LIBRARY.md (shared templates)
3. COPILOT_GLOBAL_RULES.md (global rules)

=====================================================================
END OF GLOBAL CONTEXT
=====================================================================