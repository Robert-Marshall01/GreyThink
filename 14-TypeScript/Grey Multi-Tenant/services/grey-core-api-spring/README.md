# Grey Core API - Spring Boot

Spring Boot implementation of the Grey Multi-Tenant backend.

## Status

**Placeholder** - Not yet implemented.

## Overview

This service will implement the same OpenAPI contract as the Go backend,
but using Spring Boot (Java/Kotlin).

## Stack

- Spring Boot 3
- Spring Security (JWT)
- Spring Data JPA
- PostgreSQL
- Flyway

## Structure

- `auth` - JWT authentication with refresh tokens
- `users` - User management
- `organizations` - Multi-tenant organizations
- `projects` - Project CRUD

## Implementation Notes

Will follow Spring best practices:
- Layered architecture (Controller → Service → Repository)
- Spring Security for authentication
- JPA entities with auditing
- DTOs for API contracts
- Custom filters for multi-tenant context
- Flyway for migrations

## Contract

Implements: `/contracts/openapi.yaml`
