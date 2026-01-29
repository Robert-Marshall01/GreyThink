# Grey Clojure SDK Implementation

## Overview

The Grey Clojure SDK provides a functional, idiomatic Clojure client for the Grey Multi-Tenant API using gRPC transport. The implementation emphasizes immutability, composable result types, and clean namespace organization.

## Architecture

```
grey.sdk.core (façade)
    ├── grey.sdk.domain.auth
    ├── grey.sdk.domain.user
    ├── grey.sdk.domain.projects
    ├── grey.sdk.domain.query
    └── grey.sdk.domain.mutation
            └── grey.sdk.grpc.* (gRPC wrappers)
                    └── grey.sdk.grpc.channel (connection management)
```

## Namespace Structure

### Core Namespaces

| Namespace | Description |
|-----------|-------------|
| `grey.sdk.core` | Main façade with all public API functions |
| `grey.sdk.options` | Configuration options with spec validation |
| `grey.sdk.error` | Normalized error handling |
| `grey.sdk.result` | Result monad utilities (`ok`/`err`) |

### gRPC Namespaces

| Namespace | Description |
|-----------|-------------|
| `grey.sdk.grpc.channel` | gRPC channel and auth state management |
| `grey.sdk.grpc.auth` | Auth service wrapper |
| `grey.sdk.grpc.user` | User service wrapper |
| `grey.sdk.grpc.projects` | Projects service wrapper |
| `grey.sdk.grpc.query` | Query service wrapper |
| `grey.sdk.grpc.mutation` | Mutation service wrapper |

### Domain Namespaces

| Namespace | Operations |
|-----------|------------|
| `grey.sdk.domain.auth` | `login`, `logout`, `refresh` |
| `grey.sdk.domain.user` | `get-user` |
| `grey.sdk.domain.projects` | `list-projects`, `create-project` |
| `grey.sdk.domain.query` | `query` |
| `grey.sdk.domain.mutation` | `mutate` |

## Result Pattern

All operations return a result map following the pattern:

```clojure
;; Success
{:ok true :data {...}}

;; Error  
{:ok false :error {:code :error-code
                   :message "Human readable message"
                   :details {...}}}
```

### Result Utilities

```clojure
(require '[grey.sdk.result :as r])

;; Create results
(r/ok {:id 1})
(r/err (error/unauthorized))

;; Check results
(r/ok? result)
(r/err? result)

;; Chain operations (monadic bind)
(-> (r/ok user)
    (r/then (fn [u] (r/ok (update u :name str/upper-case))))
    (r/catch-err (fn [e] (log/error e))))

;; Map data (functor map)
(r/map-data result (fn [data] (assoc data :processed true)))

;; Unwrap
(r/unwrap result)           ;; throws if err
(r/unwrap-or result default) ;; returns default if err

;; Combine multiple results
(r/combine result1 result2 result3) ;; => {:ok true :data [d1 d2 d3]}
```

## Error Handling

Error codes map to keywords:

| Code | Description |
|------|-------------|
| `:unauthorized` | Authentication required |
| `:forbidden` | Permission denied |
| `:not-found` | Resource not found |
| `:validation-error` | Input validation failed |
| `:network-error` | Network failure |
| `:timeout` | Request timeout |
| `:server-error` | Server-side error |
| `:unknown` | Unclassified error |

### Error Constructors

```clojure
(require '[grey.sdk.error :as error])

(error/grey-error :not-found "User not found" {:id user-id})
(error/unauthorized)
(error/unauthorized "Custom message")
(error/validation-error "Email is required")
(error/from-exception ex)
(error/from-grpc-status 16 "Unauthenticated")
```

## Configuration

```clojure
(require '[grey.sdk.options :as opts])

;; Local development
(opts/local)        ;; localhost:8080, no TLS
(opts/local 9000)   ;; localhost:9000, no TLS

;; Production
(opts/production "api.grey.com")     ;; TLS on port 443
(opts/production "api.grey.com" 8443) ;; TLS on custom port

;; Full control
(opts/options "host" 8080 true 5000 {:custom "metadata"})

;; Modifiers
(-> (opts/local)
    (opts/with-timeout 10000)
    (opts/with-metadata {:tenant-id "123"}))
```

## Usage Examples

### Basic Usage

```clojure
(require '[grey.sdk.core :as grey]
         '[grey.sdk.options :as opts]
         '[grey.sdk.result :as r])

;; Create client
(def client (grey/create-client (opts/local 8080)))

;; Login
(let [{:keys [ok data error]} (grey/login client "user@example.com" "password")]
  (if ok
    (println "Access token:" (:access-token data))
    (println "Login failed:" (:message error))))

;; Get user
(when (grey/authenticated? client)
  (let [{:keys [ok data]} (grey/get-user client)]
    (when ok
      (println "Hello," (:name data)))))

;; Cleanup
(grey/close client)
```

### With Client Macro

```clojure
(grey/with-client [client (opts/local 8080)]
  (grey/login client "user@example.com" "password")
  (grey/get-user client))
;; client is automatically closed
```

### Functional Chaining

```clojure
(-> (grey/login client email password)
    (r/then (fn [_] (grey/get-user client)))
    (r/map-data :name)
    (r/catch-err (fn [e] (r/ok "Anonymous"))))
```

### Parallel Operations

```clojure
(let [user-future (future (grey/get-user client))
      projects-future (future (grey/list-projects client))]
  (r/combine @user-future @projects-future))
```

## Dependencies

- `org.clojure/clojure` 1.11.1
- `io.grpc/grpc-netty-shaded` 1.60.0
- `io.grpc/grpc-protobuf` 1.60.0
- `io.grpc/grpc-stub` 1.60.0
- `cheshire/cheshire` 5.12.0 (JSON)

## Building

```bash
# Run tests
clj -M:test

# Build jar
clj -T:build jar

# REPL development
clj -M:dev
```

## Checklist

- [x] gRPC transport
- [x] Error normalization
- [x] Result monad pattern
- [x] Auth: login, logout, refresh
- [x] User: get-user
- [x] Projects: list-projects, create-project
- [x] Query: query
- [x] Mutation: mutate
- [x] Functional composition utilities
- [x] Spec validation for options
- [x] with-client macro
- [ ] Protobuf code generation (stub implementation)
- [ ] Async/core.async support
- [ ] Connection pooling
- [ ] Retry logic
