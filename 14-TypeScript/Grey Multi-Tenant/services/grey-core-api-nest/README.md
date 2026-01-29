# Grey Core API - NestJS

NestJS implementation of the Grey Multi-Tenant backend.

## Status

**Placeholder** - Not yet implemented.

## Overview

This service will implement the same OpenAPI contract as the Go backend,
but using NestJS (Node.js/TypeScript).

## Stack

- NestJS
- TypeORM
- PostgreSQL
- Passport.js (JWT)

## Modules

- `auth` - JWT authentication with refresh tokens
- `users` - User management
- `organizations` - Multi-tenant organizations
- `projects` - Project CRUD

## Implementation Notes

Will follow NestJS best practices:
- Module-based architecture
- Guards for authentication
- Pipes for validation
- Interceptors for logging/transformation
- Decorators for multi-tenant context

## Contract

Implements: `/contracts/openapi.yaml`
