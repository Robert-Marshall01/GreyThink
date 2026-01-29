# Grey Core Client

TypeScript SDK for the Grey Multi-Tenant platform API.

## Installation

```bash
pnpm add @grey/core-client
```

## Quick Start

```typescript
import { createGreyClient } from "@grey/core-client";

const client = createGreyClient({
  baseUrl: "http://localhost:8080/api/v1",
});

// Login
const { data } = await client.auth.login({
  email: "user@example.com",
  password: "password123",
});

// Get current user
const { data: user } = await client.users.me();
console.log("Hello,", user?.data.name);
```

## API Reference

### Creating a Client

```typescript
import { createGreyClient } from "@grey/core-client";

const client = createGreyClient({
  baseUrl: "http://localhost:8080/api/v1",
  
  // Optional: callback when tokens are refreshed
  onTokenRefresh: (session) => {
    console.log("Tokens refreshed, expires in:", session.expires_in, "seconds");
  },
  
  // Optional: callback when authentication fails
  onAuthError: (error) => {
    console.error("Auth error:", error.message);
    // Redirect to login page
  },
});
```

### Authentication

```typescript
// Login with email/password
const { data, error } = await client.auth.login({
  email: "user@example.com",
  password: "password123",
});

if (error) {
  console.error("Login failed:", error);
} else {
  console.log("Logged in! Token expires in:", data.data.expires_in, "seconds");
}

// Manually refresh tokens
await client.auth.refresh();

// Check if authenticated
if (client.isAuthenticated()) {
  console.log("User is authenticated");
}

// Logout
client.auth.logout();
```

### Users

```typescript
// Create a new user
const { data: newUser } = await client.users.create({
  email: "newuser@example.com",
  password: "securepassword",
  name: "New User",
});

// Get current authenticated user
const { data: me } = await client.users.me();
console.log("Current user:", me?.data.name);
console.log("Organization:", me?.data.organization_id);
```

### Organizations

```typescript
// Create an organization
const { data: org } = await client.organizations.create({
  name: "My Company",
});

// Get an organization by ID
const { data: organization } = await client.organizations.get("org-uuid");
console.log("Organization:", organization?.data.name);
```

### Projects

```typescript
// Create a project
const { data: project } = await client.projects.create({
  name: "My Project",
  description: "A great project",
});

// List projects with pagination
const { data: response } = await client.projects.list({
  page: 1,
  pageSize: 20,
});
console.log("Projects:", response?.data);
console.log("Total:", response?.pagination.total);

// Get a specific project
const { data: proj } = await client.projects.get("project-uuid");
```

### Error Handling

```typescript
import {
  GreyApiError,
  isUnauthorized,
  isNotFound,
  getErrorMessage,
} from "@grey/core-client";

const { data, error } = await client.projects.get("non-existent-id");

if (error) {
  const apiError = new GreyApiError(
    error.message || "Unknown error",
    404,
    error.error || "not_found"
  );
  
  if (apiError.isNotFound()) {
    console.log("Project not found");
  } else if (apiError.isUnauthorized()) {
    console.log("Please log in again");
  } else if (apiError.isValidationError()) {
    console.log("Invalid request");
  }
  
  console.error(getErrorMessage(apiError));
}
```

### Token Utilities

```typescript
import {
  getTokenClaims,
  getUserIdFromToken,
  getOrganizationIdFromToken,
  isTokenExpiredFromJwt,
} from "@grey/core-client";

const token = client.getAccessToken();

if (token) {
  // Get all claims
  const claims = getTokenClaims(token);
  console.log("User ID:", claims?.user_id);
  console.log("Organization:", claims?.organization_id);
  console.log("Expires:", new Date(claims!.exp * 1000));
  
  // Or use helper functions
  const userId = getUserIdFromToken(token);
  const orgId = getOrganizationIdFromToken(token);
  
  // Check if token is expired (with 60 second buffer)
  if (isTokenExpiredFromJwt(token, 60)) {
    await client.auth.refresh();
  }
}
```

### Browser Storage

```typescript
import { browserAuthStorage, createGreyClient } from "@grey/core-client";

// Check for existing session
if (browserAuthStorage.hasValidTokens()) {
  console.log("Found valid session");
}

// Create client with localStorage persistence
const client = createGreyClient({
  baseUrl: "http://localhost:8080/api/v1",
  onTokenRefresh: (session) => {
    // Persist tokens to localStorage
    browserAuthStorage.setTokens(session);
  },
  onAuthError: () => {
    // Clear tokens on auth error
    browserAuthStorage.clearTokens();
    window.location.href = "/login";
  },
});

// On app load, restore tokens
const existingSession = browserAuthStorage.getSession();
if (existingSession) {
  client.tokens.setTokens(existingSession);
}
```

### Types

```typescript
import type {
  User,
  Organization,
  Project,
  AuthSession,
  ErrorResponse,
  Pagination,
  CreateUserRequest,
  CreateProjectRequest,
} from "@grey/core-client";

// All types are generated from the OpenAPI specification
const user: User = {
  id: "uuid",
  email: "user@example.com",
  name: "John Doe",
  organization_id: "org-uuid",
  created_at: "2026-01-01T00:00:00Z",
  updated_at: "2026-01-01T00:00:00Z",
};
```

## Development

### Regenerate Types from OpenAPI

```bash
pnpm run generate
```

This regenerates TypeScript types from `/contracts/openapi.yaml`.

### Build

```bash
pnpm run build
```

### Watch Mode

```bash
pnpm run dev
```

### Test

```bash
pnpm test
```

## Package Structure

```
packages/grey-core-client/
├── src/
│   ├── generated/
│   │   └── schema.ts      # Generated from OpenAPI
│   ├── client.ts          # API client with typed methods
│   ├── auth.ts            # Authentication utilities
│   ├── errors.ts          # Error handling utilities
│   └── index.ts           # Main exports
├── package.json
├── tsconfig.json
└── README.md
```

## License

MIT
