# Grey Reference App

The reference implementation of a Grey Multi-Tenant frontend using React.

Uses:
- **@grey/react** - React bindings (GreyProvider, hooks)
- **@grey/core-client** - TypeScript SDK
- **@grey/adapters** - Core adapters (future)
- **React Router** - Client-side routing

## Features

- Login flow with JWT authentication
- Token storage and automatic refresh
- Dashboard with user info
- Organization management (multi-tenancy)
- Project list with pagination
- Project creation and detail views
- Protected routes

## Getting Started

```bash
# Install dependencies
pnpm install

# Start development server
pnpm dev

# Build for production
pnpm build

# Preview production build
pnpm preview
```

## Environment Variables

Create a `.env` file:

```
VITE_API_URL=http://localhost:8080/api/v1
```

## Project Structure

```
src/
├── main.tsx               # Entry point with GreyProvider
├── App.tsx                # Routing configuration
├── vite-env.d.ts          # Vite TypeScript declarations
├── components/
│   ├── Layout.tsx         # Main layout with navigation
│   └── ProtectedRoute.tsx # Auth guard component
├── context/
│   └── OrganizationContext.tsx  # Active organization state
├── pages/
│   ├── Login.tsx          # Login form
│   ├── Dashboard.tsx      # Main dashboard
│   ├── Organizations.tsx  # Organization management
│   ├── Projects.tsx       # Project list
│   └── ProjectDetail.tsx  # Project details
└── styles/
    └── global.css         # Base styles
```

## Key Patterns

### GreyProvider Setup

```tsx
import { GreyProvider } from '@grey/react';

function Root() {
  return (
    <GreyProvider baseUrl="http://localhost:8080/api/v1">
      <App />
    </GreyProvider>
  );
}
```

### Using Hooks

```tsx
import { useAuth, useUser, useProjects } from '@grey/react';

function MyComponent() {
  const { isAuthenticated, login, logout } = useAuth();
  const { user } = useUser();
  const { projects, list, create } = useProjects();
  // ...
}
```

### Protected Routes

```tsx
import { ProtectedRoute } from './components/ProtectedRoute';

<Route element={<ProtectedRoute><Layout /></ProtectedRoute>}>
  <Route path="/dashboard" element={<Dashboard />} />
</Route>
```

## Usage as Template

This app serves as the reference for implementing Grey Multi-Tenant frontends.
The patterns shown here should be replicated in other framework implementations:

1. **Provider Setup** - Wrap app in GreyProvider with API URL
2. **Auth Flow** - Use useAuth() for login/logout, isAuthenticated check
3. **Protected Routes** - Guard routes with authentication checks
4. **Data Fetching** - Use useUser(), useProjects(), useQuery()
5. **Mutations** - Use useMutation() for create/update/delete
6. **Multi-Tenancy** - Track active organization in context
7. **Error Handling** - Display loading and error states

See `/packages/grey-react` for the React bindings source.

