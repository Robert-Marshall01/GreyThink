# Grey Core API - Django

Django/Django REST Framework implementation of the Grey Multi-Tenant backend.

## Status

**Placeholder** - Not yet implemented.

## Overview

This service will implement the same OpenAPI contract as the Go backend,
but using Django with Django REST Framework.

## Stack

- Django 5
- Django REST Framework
- PostgreSQL
- Simple JWT

## Apps

- `auth` - JWT authentication with refresh tokens
- `users` - User management
- `organizations` - Multi-tenant organizations
- `projects` - Project CRUD

## Implementation Notes

Will follow Django best practices:
- App-based architecture
- DRF serializers for validation
- Custom permissions for multi-tenant access
- Middleware for request context
- Django ORM with migrations

## Contract

Implements: `/contracts/openapi.yaml`
