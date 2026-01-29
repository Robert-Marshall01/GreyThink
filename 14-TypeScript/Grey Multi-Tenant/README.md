# Grey Multi-Tenant Platform

⚠️ WARNING: contains known bugs, and may be unstable! Verify stability before integrating it into a high-stakes environment!

---

A multi-tenant operational platform with polyglot backend support, OpenAPI contract-first design, and SDK packages for 43 programming ecosystems. The platform exposes services through both HTTP JSON and gRPC transports via a unified adapter-core.

---

## 🚀 How to Run the App

This section explains how to start the Grey Multi-Tenant platform locally.

### What You Need to Run

| Component | Description | Port |
|-----------|-------------|------|
| **PostgreSQL** | Database | 5432 |
| **Adapter-Core (Go API)** | Backend server with HTTP + gRPC | 8080, 50051 |
| **Frontend App** (optional) | Reference implementation | 3000 |

### Prerequisites

| Requirement | Version | Check Command |
|-------------|---------|---------------|
| Node.js | 20+ | `node --version` |
| pnpm | 9+ | `pnpm --version` |
| Go | 1.22+ | `go version` |
| Docker | Latest | `docker --version` |
| Docker Compose | Latest | `docker compose version` |

### Step 1: Install Dependencies

```bash
make install
```

Or manually:

```bash
cd services/grey-core-api && go mod download
pnpm install
```

### Step 2: Set Up Environment

```bash
cp .env.example .env
```

Required variables in `.env`:

| Variable | Example Value |
|----------|---------------|
| `DATABASE_URL` | `postgres://grey:grey_password@localhost:5432/grey_multitenant?sslmode=disable` |
| `JWT_SECRET` | `your-secret-key-here` |

### Step 3: Start the Database

```bash
make docker-db
```

Or:

```bash
docker compose up -d postgres
```

### Step 4: Run Migrations

```bash
export DATABASE_URL="postgres://grey:grey_password@localhost:5432/grey_multitenant?sslmode=disable"
make migrate
```

### Step 5: Start the Adapter-Core

```bash
make run
```

This starts the Go backend which serves both transports:

| Transport | URL |
|-----------|-----|
| **HTTP JSON Gateway** | `http://localhost:8080/api/v1` |
| **gRPC Service** | `localhost:50051` |

### Step 6: Verify It Works

```bash
curl http://localhost:8080/health
```

Expected response:

```json
{"data":{"status":"healthy"}}
```

### Step 7: Run a Sample SDK Call (Optional)

Start the reference frontend app:

```bash
pnpm run dev:app
```

Or run both API and app together:

```bash
pnpm run dev:local
```

The frontend runs at `http://localhost:3000`.

---

## 🛠️ Installation (Windows, macOS, Linux)

The unified installer installs **both** the backend service and GUI desktop application with a single command.

### Windows

**Installer:** `installers/windows/install.ps1`

```powershell
.\installers\windows\install.ps1
```

The installer creates:
- ✅ Desktop shortcut: "Grey Multi-Tenant" (backend launcher)
- ✅ Desktop shortcut: "Grey Multi-Tenant GUI" (desktop app)
- ✅ Start Menu entries
- ✅ Windows Service: GreyMultiTenant
- ✅ Configuration: `C:\Program Files\GreyMultiTenant\config\.env`

#### Options

| Flag | Description |
|------|-------------|
| `-SkipBackend` | Skip backend installation, GUI only |
| `-SkipGUI` | Skip GUI installation, backend only |
| `-SkipService` | Install backend without Windows service |

---

### macOS

**Installer:** `installers/macos/install.sh`

```bash
sudo ./installers/macos/install.sh
```

The installer creates:
- ✅ App: `/Applications/GreyMultiTenant.app` (backend)
- ✅ App: `/Applications/Grey Multi-Tenant GUI.app` (desktop app)
- ✅ Commands: `grey-core-api`, `grey-gui`, `grey-launcher`
- ✅ launchd service: `com.grey.multitenant`
- ✅ Configuration: `/etc/grey-multitenant/.env`

#### Options

| Flag | Description |
|------|-------------|
| `--skip-backend` | Skip backend installation |
| `--skip-gui` | Skip GUI installation |
| `--skip-service` | Skip launchd service |
| `--user-install` | Install for current user only |

---

### Linux

**Installer:** `installers/linux/install.sh`

```bash
sudo ./installers/linux/install.sh
```

The installer creates:
- ✅ Desktop entries: "Grey Multi-Tenant", "Grey Multi-Tenant GUI"
- ✅ Commands: `grey-core-api`, `grey-gui`, `grey-launcher`
- ✅ systemd service: `grey-multitenant`
- ✅ Configuration: `/etc/grey-multitenant/.env`

#### Options

| Flag | Description |
|------|-------------|
| `--skip-backend` | Skip backend installation |
| `--skip-gui` | Skip GUI installation |
| `--skip-service` | Skip systemd service |
| `--user-install` | Install for current user only |
| `--prefix=/path` | Custom prefix (default: `/usr/local`) |

---

## 🧹 Uninstallation

The unified uninstaller removes both backend and GUI installations.

### Windows

```powershell
.\installers\windows\uninstall.ps1
```

| Flag | Description |
|------|-------------|
| `-KeepData` | Preserve configuration and logs |
| `-BackendOnly` | Only remove backend |
| `-GUIOnly` | Only remove GUI |

---

### macOS

```bash
sudo ./installers/macos/uninstall.sh
```

| Flag | Description |
|------|-------------|
| `--keep-data` | Preserve configuration and logs |
| `--backend-only` | Only remove backend |
| `--gui-only` | Only remove GUI |
| `--user-install` | Uninstall user-level installation |

---

### Linux

```bash
sudo ./installers/linux/uninstall.sh
```

| Flag | Description |
|------|-------------|
| `--keep-data` | Preserve configuration and logs |
| `--backend-only` | Only remove backend |
| `--gui-only` | Only remove GUI |
| `--user-install` | Uninstall user-level installation |

---

## 🖥️ Grey Multi-Tenant GUI

A standalone desktop application for managing and monitoring the Grey Multi-Tenant platform. Built with Electron, React, and TypeScript.

### Features

| Screen | Description |
|--------|-------------|
| Dashboard | Overview of tenant metrics and activity |
| Tenants | Create, view, and manage tenants |
| Users | User management per tenant |
| Settings | Application configuration |
| Service Health | Monitor backend service status (HTTP, gRPC, Database) |

### Installing the GUI

The GUI is installed automatically by the unified installer. To install GUI only:

| Platform | Command |
|----------|---------|
| Windows | `.\installers\windows\install.ps1 -SkipBackend` |
| macOS | `./installers/macos/install.sh --skip-backend` |
| Linux | `./installers/linux/install.sh --skip-backend --user-install` |

### Running the GUI (Development)

```bash
cd gui
pnpm install
pnpm dev
```

### Building the GUI

```bash
cd gui
pnpm build
```

Build outputs by platform:

| Platform | Output |
|----------|--------|
| Windows | `release/Grey-Multi-Tenant-GUI-Setup.exe` |
| macOS | `release/Grey Multi-Tenant GUI.dmg` |
| Linux | `release/Grey-Multi-Tenant-GUI.AppImage` |

### GUI vs Backend

| Component | Purpose | Shortcut Name |
|-----------|---------|---------------|
| Backend Service | API server (HTTP + gRPC) | "Grey Multi-Tenant" |
| GUI Application | Desktop management interface | "Grey Multi-Tenant GUI" |

The GUI connects to backend services but runs independently. Start the backend first, then launch the GUI.

---

## Table of Contents

- [How to Run the App](#-how-to-run-the-app)
- [Installation (Windows, macOS, Linux)](#️-installation-windows-macos-linux)
- [Grey Multi-Tenant GUI](#️-grey-multi-tenant-gui)
- [Uninstallation](#-uninstallation)
- [Project Overview](#project-overview)
- [Architecture](#architecture)
- [Supported Ecosystems](#supported-ecosystems)
- [Folder Structure](#folder-structure)
- [Running Tests](#running-tests)
- [Regenerating SDK Packages](#regenerating-sdk-packages)
- [Contributing](#contributing)
- [License](#license)

---

## Project Overview

Grey Multi-Tenant is a platform designed to support organization-based multi-tenancy. Each user belongs to an organization, and all resources are scoped accordingly. The platform provides:

- A contract-first API defined via OpenAPI 3.1
- A primary Go backend with variant implementations in 9 frameworks
- A unified adapter-core that normalizes domain operations
- HTTP JSON and gRPC transport layers
- SDK packages for 44 languages and frameworks
- Frontend reference applications for 10 JavaScript frameworks

---

## Architecture

### System Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           Frontend Applications                         │
│  (React, Vue, Angular, Svelte, Solid, Qwik, Next.js, Nuxt, Remix, etc.) │
└─────────────────────────────────────────────────────────────────────────┘
                                     │
                    ┌────────────────┴────────────────┐
                    ▼                                 ▼
          ┌─────────────────┐               ┌─────────────────┐
          │  HTTP Gateway   │               │  gRPC Service   │
          │  (JSON REST)    │               │  (Proto3)       │
          └────────┬────────┘               └────────┬────────┘
                   │                                  │
                   └────────────┬─────────────────────┘
                                ▼
                   ┌─────────────────────────┐
                   │      Adapter-Core       │
                   │  (Domain Operations)    │
                   └────────────┬────────────┘
                                ▼
                   ┌─────────────────────────┐
                   │     Backend Services    │
                   │  (Go, NestJS, Django,   │
                   │   FastAPI, Rails, etc.) │
                   └─────────────────────────┘
                                ▼
                   ┌─────────────────────────┐
                   │       PostgreSQL        │
                   └─────────────────────────┘
```

### Adapter-Core

Located at `packages/grey-adapters/`, the adapter-core is the single source of truth for all domain operations.

**Domain Surface:**

| Domain | Operations |
|--------|------------|
| Auth | login, logout, refresh |
| User | getCurrentUser, getUser |
| Projects | listProjects, createProject |
| Query | query |
| Mutation | mutate |

**Unified Error Shape:**

| Field | Type | Description |
|-------|------|-------------|
| code | string | unauthorized, forbidden, not_found, validation_error, network_error, timeout, server_error, unknown |
| message | string | Human-readable error message |
| details | string | Additional context |

**Core Files:**

| File | Purpose |
|------|---------|
| `src/adapter_core.ts` | Domain interface definitions |
| `src/error_normalizer.ts` | Error normalization utilities |
| `src/http_gateway/` | HTTP JSON transport layer |
| `src/grpc_service/` | gRPC transport layer |
| `src/integration_manifest.json` | SDK ecosystem registry |

### HTTP JSON Gateway

REST endpoints at `/api/v1`:

| Method | Path | Operation |
|--------|------|-----------|
| POST | /auth/login | Auth.login |
| POST | /auth/logout | Auth.logout |
| POST | /auth/refresh | Auth.refresh |
| GET | /users/me | User.getCurrentUser |
| GET | /users/:userId | User.getUser |
| GET | /projects | Projects.listProjects |
| POST | /projects | Projects.createProject |
| POST | /query | Query.query |
| POST | /mutate | Mutation.mutate |

### gRPC Service

Proto3 definitions in `packages/grey-adapters/src/grpc_service/proto/`:

| Package | Service | Methods |
|---------|---------|---------|
| grey.auth.v1 | AuthService | Login, Logout, Refresh |
| grey.user.v1 | UserService | GetCurrentUser, GetUser |
| grey.projects.v1 | ProjectsService | ListProjects, CreateProject, GetProject |
| grey.query.v1 | QueryService | Query |
| grey.mutation.v1 | MutationService | Mutate |

---

## Supported Ecosystems

44 SDK packages across 30+ programming languages.

### JavaScript/TypeScript Frameworks

| Package | Framework | Transport |
|---------|-----------|-----------|
| grey-angular | Angular | HTTP JSON |
| grey-react | React | HTTP JSON |
| grey-vue | Vue 3 | HTTP JSON |
| grey-svelte | Svelte | HTTP JSON |
| grey-sveltekit | SvelteKit | HTTP JSON |
| grey-solid | SolidJS | HTTP JSON |
| grey-qwik | Qwik | HTTP JSON |
| grey-preact | Preact | HTTP JSON |
| grey-lit | Lit | HTTP JSON |
| grey-stencil | Stencil | HTTP JSON |
| grey-vanilla | Vanilla JS/TS | HTTP JSON |
| grey-next | Next.js | HTTP JSON |
| grey-nuxt | Nuxt | HTTP JSON |
| grey-remix | Remix | HTTP JSON |
| grey-astro | Astro | HTTP JSON |
| grey-node | Node.js | gRPC |

### Mobile & Desktop

| Package | Platform | Transport |
|---------|----------|-----------|
| grey-react-native | React Native | HTTP JSON |
| grey-expo | Expo | HTTP JSON |
| grey-flutter | Flutter | gRPC |
| grey-swift | iOS/macOS | gRPC |
| grey-kotlin | Android/JVM | gRPC |
| grey-capacitor | Capacitor | HTTP JSON |
| grey-electron | Electron | gRPC |

### Backend Languages

| Package | Language | Transport |
|---------|----------|-----------|
| grey-go | Go | gRPC |
| grey-rust | Rust | gRPC |
| grey-python | Python | gRPC |
| grey-ruby | Ruby | gRPC |
| grey-php | PHP | HTTP JSON |
| grey-java | Java | gRPC |
| grey-scala | Scala | gRPC |
| grey-clojure | Clojure | gRPC |
| grey-elixir | Elixir | gRPC |
| grey-erlang | Erlang | gRPC |
| grey-haskell | Haskell | gRPC |
| grey-dotnet | C#/.NET | gRPC |

### Systems & Specialized Languages

| Package | Language | Transport |
|---------|----------|-----------|
| grey-c | C | HTTP JSON |
| grey-cpp | C++ | gRPC |
| grey-zig | Zig | gRPC |
| grey-mojo | Mojo | gRPC |
| grey-matlab | MATLAB | HTTP JSON |
| grey-r | R | HTTP JSON |
| grey-julia | Julia | gRPC |
| grey-cobol | COBOL | HTTP JSON |

---

## Folder Structure

```
grey-multi-tenant/
├── apps/                           # Frontend applications
│   ├── grey-reference-app/         # Reference Vite implementation
│   ├── grey-react-app/             # React (Vite)
│   ├── grey-nextjs-app/            # Next.js
│   ├── grey-vue-app/               # Vue 3 (Vite)
│   ├── grey-nuxt-app/              # Nuxt 3
│   ├── grey-angular-app/           # Angular
│   ├── grey-remix-app/             # Remix
│   ├── grey-astro-app/             # Astro
│   ├── grey-solid-app/             # SolidJS
│   └── grey-qwik-app/              # Qwik
├── contracts/                      # API contracts
│   └── openapi.yaml                # OpenAPI 3.1 specification
├── infrastructure/                 # Infrastructure configurations
├── packages/                       # SDK packages (44 ecosystems)
│   ├── grey-adapters/              # Adapter-core + transports
│   │   └── src/
│   │       ├── adapter_core.ts
│   │       ├── error_normalizer.ts
│   │       ├── http_gateway/
│   │       ├── grpc_service/
│   │       └── integration_manifest.json
│   ├── grey-core-client/           # TypeScript SDK (generated)
│   ├── grey-react/                 # React hooks
│   ├── grey-vue/                   # Vue composables
│   └── ...                         # 40+ additional SDK packages
├── services/                       # Backend services
│   ├── grey-core-api/              # Go API (primary)
│   ├── grey-core-api-nest/         # NestJS variant
│   ├── grey-core-api-django/       # Django REST variant
│   ├── grey-core-api-fastapi/      # FastAPI variant
│   ├── grey-core-api-rails/        # Rails API variant
│   ├── grey-core-api-laravel/      # Laravel variant
│   ├── grey-core-api-spring/       # Spring Boot variant
│   ├── grey-core-api-dotnet/       # ASP.NET Core variant
│   └── grey-core-api-phoenix/      # Phoenix variant
├── prompts/                        # Copilot prompt templates
│   └── TEMPLATE_LIBRARY.md
├── docker-compose.yml
├── Makefile
├── package.json
├── pnpm-workspace.yaml
└── turbo.json
```

---

## Running Tests

### All Tests

```bash
make test
```

### Go Tests

```bash
make test-go
```

### Go Tests with Coverage

```bash
make test-go-coverage
```

### TypeScript Tests

```bash
make test-ts
```

Or:

```bash
pnpm run test
```

### Linting

```bash
make lint
```

---

## Regenerating SDK Packages

SDK packages can be regenerated using GitHub Copilot with templates in `prompts/TEMPLATE_LIBRARY.md`.

### Using Copilot Chat

1. Open the Copilot Chat panel in VS Code
2. Reference the prompt template
3. Specify the target ecosystem

**Example prompt:**

```
Generate the full Grey Multi-Tenant SDK package for [LANGUAGE].

Follow these rules exactly:
1. The SDK must use [TRANSPORT] transport
2. Domain surface: Auth (login, logout, refresh), User (getUser), Projects (listProjects, createProject), Query (query), Mutation (mutate)
3. Unified error shape: { code, message, details }
4. Use idiomatic patterns for [LANGUAGE]
```

### Regenerating TypeScript Types from OpenAPI

```bash
make generate
```

Or:

```bash
cd packages/grey-core-client && pnpm run generate
```

---

## Contributing

### Development Workflow

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Make changes following the existing code style
4. Run tests: `make test`
5. Run linting: `make lint`
6. Commit with conventional commits: `git commit -m "feat: add feature"`
7. Push and open a pull request

### Code Style

| Language | Standard |
|----------|----------|
| Go | `gofmt` + `golangci-lint` |
| TypeScript | ESLint configuration |
| Commits | Conventional commits |

### Adding a New SDK Package

1. Create directory: `packages/grey-[ecosystem]/`
2. Implement domain clients: auth, user, projects, query, mutation
3. Follow transport pattern (HTTP JSON or gRPC)
4. Add entry to `packages/grey-adapters/src/integration_manifest.json`
5. Add tests
6. Update documentation

### Adding a New Backend Variant

1. Create directory: `services/grey-core-api-[framework]/`
2. Implement all endpoints from `contracts/openapi.yaml`
3. Follow the same request/response schemas
4. Add Dockerfile
5. Update documentation

---

## License

MIT
