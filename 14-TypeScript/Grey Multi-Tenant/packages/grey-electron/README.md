# @grey/electron

Electron bindings for Grey Multi-Tenant.

## Installation

```bash
pnpm add @grey/electron
```

## Usage

### Main Process

```ts
// main.ts
import { initMainProcess } from '@grey/electron/main';

initMainProcess({
  baseUrl: 'https://api.example.com',
  secureStorage: true,
});
```

### Preload Script

```ts
// preload.ts
import { exposeGreyAPI } from '@grey/electron/preload';

exposeGreyAPI();
```

### Renderer Process (React)

```tsx
import { GreyProvider, useAuth, useUser } from '@grey/electron';

function App() {
  return (
    <GreyProvider>
      <MainWindow />
    </GreyProvider>
  );
}

function MainWindow() {
  const { user } = useUser();
  const { logout } = useAuth();
  
  return (
    <div>
      <h1>Welcome, {user?.name}</h1>
      <button onClick={logout}>Logout</button>
    </div>
  );
}
```

## Features

- Secure credential storage (via keytar or safeStorage)
- IPC communication between main and renderer
- Deep linking support
- Auto-update integration

## API

### Main Process (`@grey/electron/main`)
- `initMainProcess()` - Initialize in main
- `handleDeepLink()` - Handle deep links

### Preload (`@grey/electron/preload`)
- `exposeGreyAPI()` - Expose API to renderer

### Renderer
- `GreyProvider` - Context provider
- `useAuth`, `useUser`, `useProjects` - React hooks
