# Grey Multi-Tenant GUI

Cross-platform desktop application for the Grey Multi-Tenant Platform.

## Features

- **Cross-Platform**: Runs on Windows, macOS, and Linux
- **Service Status Monitoring**: Real-time status indicators for all backend services
- **User Authentication**: Secure login/logout with token management
- **Project Management**: Create and manage multi-tenant projects
- **Query Console**: Execute read-only queries against the adapter core
- **Mutation Console**: Execute write operations with confirmation prompts
- **Settings**: Configure endpoints, ports, and application behavior
- **Activity Logging**: Visible log panel for monitoring all operations

## Prerequisites

- Node.js 18+ and npm/pnpm
- Running backend services (adapter-core, HTTP gateway, gRPC service)
- PostgreSQL database

## Installation

```bash
# Navigate to the gui directory
cd gui

# Install dependencies
npm install
# or
pnpm install
```

## Development

```bash
# Start development server with hot reload
npm run dev
# or
pnpm dev
```

This will launch the Electron app in development mode with DevTools enabled.

## Building

### Build for All Platforms

```bash
npm run build
npm run package:all
```

### Build for Specific Platform

```bash
# Windows
npm run package:win

# macOS
npm run package:mac

# Linux
npm run package:linux
```

Build outputs are placed in the `release/` directory.

## Project Structure

```
gui/
├── package.json              # Dependencies and scripts
├── electron.vite.config.ts   # Vite configuration for Electron
├── tsconfig.json             # TypeScript configuration
├── resources/                # Application icons
│   ├── icon.svg              # Source icon
│   ├── icon.ico              # Windows icon
│   ├── icon.icns             # macOS icon
│   └── icons/                # Linux icons
└── src/
    ├── main/                 # Main process (Node.js)
    │   ├── index.ts          # Application entry point
    │   ├── ipcHandlers.ts    # IPC communication handlers
    │   └── serviceManager.ts # Service health checking
    ├── preload/              # Preload scripts (secure bridge)
    │   └── index.ts          # Context bridge API
    └── renderer/             # Renderer process (React)
        ├── index.html        # HTML entry
        └── src/
            ├── main.tsx      # React entry point
            ├── App.tsx       # Main app with routing
            ├── components/   # Reusable UI components
            ├── screens/      # Application screens
            ├── services/     # API client wrappers
            ├── stores/       # Zustand state stores
            ├── styles/       # Global styles
            └── types/        # TypeScript definitions
```

## Screens

| Screen | Description |
|--------|-------------|
| **Login** | User authentication with service status |
| **Dashboard** | User info, service status, projects overview |
| **Projects** | List, create, and manage projects |
| **Query Console** | Execute read-only queries |
| **Mutation Console** | Execute write operations |
| **Service Health** | Monitor all backend services with start/stop controls |
| **Settings** | Configure endpoints and behavior |

## API Surface

The GUI communicates with backend services using only the defined domain API:

### Auth
- `login(email, password)` - Authenticate user
- `logout()` - End session
- `refresh()` - Refresh auth token

### User
- `getUser()` - Get current user info

### Projects
- `listProjects()` - List all projects
- `createProject(name, description)` - Create new project

### Query
- `query(queryString, variables)` - Execute read-only query

### Mutation
- `mutate(mutation, variables)` - Execute write operation

## Configuration

Settings are persisted using `electron-store` and include:

| Setting | Default | Description |
|---------|---------|-------------|
| `httpEndpoint` | `http://localhost:8080` | HTTP API URL |
| `grpcEndpoint` | `localhost:50051` | gRPC service address |
| `autoStartServices` | `true` | Auto-start on launch |
| `theme` | `system` | UI theme preference |

## Service Detection

The GUI automatically detects backend service status:

1. **Adapter Core** (port 8080) - Main API service
2. **HTTP Gateway** (port 8080) - JSON REST interface
3. **gRPC Service** (port 50051) - Protocol buffer interface

If services are not running, clear error messages are displayed with suggested actions.

## Icon Generation

To generate platform-specific icons from the SVG source:

```bash
cd resources
chmod +x generate-icons.sh
./generate-icons.sh
```

Requires ImageMagick. For macOS icons, run on macOS with Xcode installed.

## Troubleshooting

### App won't start
1. Check if backend services are running
2. Verify PostgreSQL is accessible
3. Check the application logs in Settings

### Services show as "Stopped"
1. Start Docker and run `docker compose up -d`
2. Or start services manually with the system service manager

### Login fails
1. Ensure the HTTP endpoint is correct in Settings
2. Verify the auth service is responding

## License

MIT
