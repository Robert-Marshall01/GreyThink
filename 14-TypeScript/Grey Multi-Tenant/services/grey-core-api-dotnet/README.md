# Grey Core API - .NET

ASP.NET Core implementation of the Grey Multi-Tenant backend.

## Status

**Placeholder** - Not yet implemented.

## Overview

This service will implement the same OpenAPI contract as the Go backend,
but using ASP.NET Core (C#).

## Stack

- ASP.NET Core 8
- Entity Framework Core
- PostgreSQL (Npgsql)
- JWT Bearer Authentication

## Structure

- `Controllers/` - API controllers
- `Services/` - Business logic
- `Repositories/` - Data access
- `Models/` - Domain entities
- `DTOs/` - Data transfer objects

## Implementation Notes

Will follow .NET best practices:
- Clean architecture
- Dependency injection
- Middleware pipeline
- EF Core with migrations
- ASP.NET Core Identity (customized)
- Custom authorization handlers for multi-tenant

## Contract

Implements: `/contracts/openapi.yaml`
