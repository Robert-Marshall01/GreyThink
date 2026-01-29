# Grey Core API - FastAPI

FastAPI implementation of the Grey Multi-Tenant backend.

## Status

**Placeholder** - Not yet implemented.

## Overview

This service will implement the same OpenAPI contract as the Go backend,
but using FastAPI (Python).

## Stack

- FastAPI
- SQLAlchemy
- PostgreSQL
- python-jose (JWT)

## Modules

- `auth` - JWT authentication with refresh tokens
- `users` - User management
- `organizations` - Multi-tenant organizations
- `projects` - Project CRUD

## Implementation Notes

Will follow FastAPI best practices:
- Pydantic models for validation
- Dependency injection for services
- Middleware for multi-tenant context
- Alembic for migrations
- Async handlers where beneficial

## Contract

Implements: `/contracts/openapi.yaml`
