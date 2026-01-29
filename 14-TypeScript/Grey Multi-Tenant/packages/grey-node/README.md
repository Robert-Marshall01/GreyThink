# @grey/node

Node.js server-side utilities for Grey Multi-Tenant.

Pure server-side package without any reactivity layer.

## Installation

```bash
pnpm add @grey/node
```

## Usage

### Basic Client Usage

```ts
import { createClient } from '@grey/node';

const client = createClient({
  baseUrl: 'https://api.example.com',
  token: process.env.API_TOKEN,
});

// Get user
const user = await client.getUser();
console.log('User:', user?.name);

// List projects
const projects = await client.listProjects();
projects.forEach(p => console.log(p.name));
```

### With Service Account

```ts
import { createServiceClient } from '@grey/node';

const client = await createServiceClient({
  baseUrl: 'https://api.example.com',
  clientId: process.env.CLIENT_ID!,
  clientSecret: process.env.CLIENT_SECRET!,
});

// Client automatically handles authentication
const projects = await client.listProjects();
```

### Express Middleware

```ts
import express from 'express';
import { createAuthMiddleware, createClient } from '@grey/node';

const app = express();

app.use(createAuthMiddleware({
  baseUrl: process.env.API_URL!,
  headerName: 'Authorization',
}));

app.get('/api/me', (req, res) => {
  res.json(req.greyUser);
});
```

## API

- `createClient()` - Create basic client
- `createServiceClient()` - Create client with service account auth
- `createAuthMiddleware()` - Express auth middleware
- `verifyToken()` - JWT token verification
