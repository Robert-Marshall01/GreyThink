# COPILOT GLOBAL RULES — Grey Multi‑Tenant SDK

You are generating code for a multi‑framework SDK that wraps a shared adapter core located in /packages/grey-adapters.

Your job is to ensure that ALL frameworks follow the same architecture, naming conventions, patterns, and behavior.

These rules apply to EVERY file you generate in ANY framework.

============================================================
1. GLOBAL ARCHITECTURE PRINCIPLES
============================================================

- Every framework wraps the same adapter core functions.
- All domain modules (auth, user, projects, query, mutation) must expose:
  - data
  - loading
  - error
  - domain actions
- All frameworks must normalize errors into a consistent shape.
- All frameworks must avoid platform‑unsafe APIs (e.g., no browser APIs in SSR, no Node APIs in mobile).
- All frameworks must export a clean, minimal public API from index.ts.
- All code must be idiomatic for the target framework.

============================================================
2. FILE GENERATION RULES
============================================================

When generating a file:

1. Read the framework’s IMPLEMENTATION.md in the same package folder.
2. Read the TEMPLATE_LIBRARY.md for the correct file template.
3. Apply the global architecture rules in this file.
4. Generate real, production‑ready code — no placeholders.
5. Follow the folder structure exactly as defined in IMPLEMENTATION.md.
6. Use the naming conventions defined in the framework spec.

============================================================
3. TEMPLATE USAGE RULES
============================================================

When a file includes a comment like:

// Use the HOOK TEMPLATE.
// Implement the React useAuth hook using useAuthCore.

You MUST:

- Apply the HOOK TEMPLATE from TEMPLATE_LIBRARY.md.
- Combine it with the framework’s IMPLEMENTATION.md.
- Generate idiomatic code for that framework.
- Expose the correct domain actions.
- Normalize errors.
- Use the correct state system (React: useState, Vue: ref, Svelte: writable, etc.).

============================================================
4. CROSS‑FRAMEWORK INVARIANTS
============================================================

These rules NEVER change:

- Hooks/composables/stores must wrap the adapter core.
- Providers must expose domain APIs via context or equivalent.
- Server wrappers must avoid browser APIs.
- Token storage must be async and platform‑safe.
- Bridge layers must wrap core functions in a platform‑safe boundary.
- .NET services must mirror the JS adapter core exactly.
- .NET stores must expose Data, Loading, Error and async methods.

============================================================
5. ERROR NORMALIZATION RULES
============================================================

All frameworks must normalize errors into:

{
  message: string
  code?: string
  status?: number
  raw?: any
}

.NET equivalents must use:
- ApiException
- AuthException
- ValidationException

============================================================
6. EXPORT RULES
============================================================

Every framework must export:

- all hooks/composables/stores
- the provider
- server wrappers (if applicable)
- token/bridge utilities (if applicable)

Index files must contain ONLY exports.

============================================================
7. BEHAVIOR RULES
============================================================

- Stay consistent across all frameworks.
- Never invent new architecture.
- Never drift from the adapter core.
- Never generate placeholders.
- Always follow the 3‑layer system:
  1. Global rules (this file)
  2. Framework spec (IMPLEMENTATION.md)
  3. File template (TEMPLATE_LIBRARY.md)