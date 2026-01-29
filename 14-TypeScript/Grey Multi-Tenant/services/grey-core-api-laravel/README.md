# Grey Core API - Laravel

Laravel implementation of the Grey Multi-Tenant backend.

## Status

**Placeholder** - Not yet implemented.

## Overview

This service will implement the same OpenAPI contract as the Go backend,
but using Laravel (PHP).

## Stack

- Laravel 11
- PostgreSQL
- Laravel Sanctum or JWT

## Structure

- `app/Http/Controllers/Auth` - JWT authentication
- `app/Http/Controllers/Users` - User management
- `app/Http/Controllers/Organizations` - Multi-tenant organizations
- `app/Http/Controllers/Projects` - Project CRUD

## Implementation Notes

Will follow Laravel best practices:
- Eloquent ORM
- Form Requests for validation
- API Resources for transformation
- Policies for authorization
- Middleware for multi-tenant context
- Migrations for database schema

## Contract

Implements: `/contracts/openapi.yaml`
