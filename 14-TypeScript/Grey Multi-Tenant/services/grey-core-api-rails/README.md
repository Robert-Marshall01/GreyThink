# Grey Core API - Rails

Ruby on Rails implementation of the Grey Multi-Tenant backend.

## Status

**Placeholder** - Not yet implemented.

## Overview

This service will implement the same OpenAPI contract as the Go backend,
but using Ruby on Rails (API mode).

## Stack

- Rails 7 (API mode)
- PostgreSQL
- jwt gem

## Structure

- `app/controllers/auth` - JWT authentication
- `app/controllers/users` - User management
- `app/controllers/organizations` - Multi-tenant organizations
- `app/controllers/projects` - Project CRUD

## Implementation Notes

Will follow Rails best practices:
- API-only mode
- Active Record for ORM
- Serializers for JSON responses
- Pundit for authorization
- Concerns for multi-tenant scoping

## Contract

Implements: `/contracts/openapi.yaml`
