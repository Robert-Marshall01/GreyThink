# Grey Core API - Phoenix

Phoenix (Elixir) implementation of the Grey Multi-Tenant backend.

## Status

**Placeholder** - Not yet implemented.

## Overview

This service will implement the same OpenAPI contract as the Go backend,
but using Phoenix Framework (Elixir).

## Stack

- Phoenix 1.7
- Ecto
- PostgreSQL
- Guardian (JWT)

## Structure

- `lib/grey_web/controllers/auth` - JWT authentication
- `lib/grey_web/controllers/users` - User management
- `lib/grey_web/controllers/organizations` - Multi-tenant organizations
- `lib/grey_web/controllers/projects` - Project CRUD

## Implementation Notes

Will follow Phoenix best practices:
- Contexts for domain boundaries
- Changesets for validation
- Plugs for middleware
- Guardian for JWT auth
- Ecto schemas with migrations
- Custom plugs for multi-tenant context

## Contract

Implements: `/contracts/openapi.yaml`
