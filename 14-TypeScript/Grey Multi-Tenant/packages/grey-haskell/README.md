# Grey SDK for Haskell

Multi-tenant SDK client for Haskell applications using gRPC transport.

## Installation

Add to your `package.yaml` or `cabal` file:

```yaml
dependencies:
  - grey-sdk
```

Or with cabal:

```bash
cabal install grey-sdk
```

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Grey
import qualified Grey.Domain.AuthClient as Auth
import qualified Grey.Domain.ProjectsClient as Projects

main :: IO ()
main = do
  -- Create a local client
  client <- localClient Nothing
  
  -- Login
  let creds = Auth.LoginCredentials
        { Auth.username = "user@example.com"
        , Auth.password = "password"
        }
  loginResult <- Auth.login (grpcClient client) creds
  
  case loginResult of
    Ok tokens -> do
      putStrLn $ "Logged in! Token: " ++ show (Auth.accessToken tokens)
      
      -- Create authenticated client
      authClient <- withAuthToken (Auth.accessToken tokens) client
      
      -- List projects
      projectsResult <- Projects.listProjects (grpcClient authClient) Projects.defaultListOptions
      case projectsResult of
        Ok projects -> mapM_ (print . Projects.projectName) (Projects.plItems projects)
        Err e -> putStrLn $ "Error: " ++ show e
    
    Err e -> putStrLn $ "Login failed: " ++ show e
  
  -- Cleanup
  closeClient client
```

## Features

### Pure Functional Design

All operations return a `Result` type for explicit error handling:

```haskell
data Result a
  = Ok a           -- Success with value
  | Err GreyError  -- Failure with error

-- Use pattern matching
case result of
  Ok value -> doSomething value
  Err e -> handleError e

-- Or use combinators
fromResult handleError handleSuccess result
getOr defaultValue result
mapResult transform result
flatMap chainOperation result
```

### Domain Clients

- **AuthClient**: `login`, `logout`, `refresh`
- **UserClient**: `getUser`, `getCurrentUser`
- **ProjectsClient**: `listProjects`, `getProject`, `createProject`, `updateProject`, `deleteProject`
- **QueryClient**: `query`, `querySimple`, `batchQuery`
- **MutationClient**: `mutate`, `mutateSimple`, `batchMutate`

### Error Handling

Normalized errors with codes:

```haskell
data GreyError = GreyError
  { errorCode    :: ErrorCode
  , errorMessage :: Text
  , errorDetails :: Maybe Text
  }

data ErrorCode
  = Unauthorized
  | Forbidden
  | NotFound
  | ValidationError
  | NetworkError
  | Timeout
  | ServerError
  | Unknown

-- Check if retryable
isRetryable :: GreyError -> Bool
```

### Configuration

```haskell
-- Local development (localhost:50051)
client <- localClient Nothing

-- Local with custom port
client <- localClient (Just 9000)

-- Production
client <- productionClient "api.example.com" Nothing

-- Custom configuration
client <- newClient $ defaultOptions
  & withHost "api.example.com"
  & withPort 443
  & withTls True
  & withTimeout 60
  & withAuthToken "token"
  & withHeader "X-Custom" "value"
```

### Input Validation

All clients validate inputs before making requests:

```haskell
-- Validation is automatic
result <- Auth.login client creds
case result of
  Err e | errorCode e == ValidationError -> 
    putStrLn $ "Validation failed: " ++ show (errorMessage e)
  _ -> ...

-- Or validate explicitly
case Auth.validateCredentials creds of
  Left err -> putStrLn "Invalid credentials"
  Right validCreds -> ...
```

## Module Structure

```
Grey
├── Config
│   └── Options          -- Configuration
├── Error
│   ├── Codes           -- Error codes
│   ├── GreyError       -- Error type
│   └── Result          -- Result monad
├── GRPC
│   ├── Client          -- gRPC client
│   ├── AuthService     -- Auth gRPC stubs
│   ├── UserService     -- User gRPC stubs
│   ├── ProjectsService -- Projects gRPC stubs
│   ├── QueryService    -- Query gRPC stubs
│   └── MutationService -- Mutation gRPC stubs
├── Domain
│   ├── AuthClient      -- Auth domain client
│   ├── UserClient      -- User domain client
│   ├── ProjectsClient  -- Projects domain client
│   ├── QueryClient     -- Query domain client
│   └── MutationClient  -- Mutation domain client
└── Client              -- Main facade
```

## Running Tests

```bash
# With Stack
stack test

# With Cabal
cabal test
```

## Building

```bash
# With Stack
stack build

# With Cabal
cabal build
```

## API Reference

### Result Combinators

| Function | Type | Description |
|----------|------|-------------|
| `ok` | `a -> Result a` | Create success |
| `err` | `GreyError -> Result a` | Create failure |
| `isOk` | `Result a -> Bool` | Check for success |
| `isErr` | `Result a -> Bool` | Check for failure |
| `fromResult` | `(GreyError -> b) -> (a -> b) -> Result a -> b` | Pattern match |
| `getOr` | `a -> Result a -> a` | Get value or default |
| `mapResult` | `(a -> b) -> Result a -> Result b` | Transform success |
| `mapError` | `(GreyError -> GreyError) -> Result a -> Result a` | Transform error |
| `flatMap` | `(a -> Result b) -> Result a -> Result b` | Chain operations |
| `partition` | `[Result a] -> ([GreyError], [a])` | Separate results |

### Error Constructors

| Function | Description |
|----------|-------------|
| `unauthorized` | Create unauthorized error |
| `forbidden` | Create forbidden error |
| `notFound` | Create not found error |
| `validationError` | Create validation error with message |
| `networkError` | Create network error |
| `timeout` | Create timeout error |
| `serverError` | Create server error |
| `fromGrpcStatus` | Create from gRPC status code |
| `fromHttpStatus` | Create from HTTP status code |

## gRPC Integration

This SDK provides gRPC stubs that can be integrated with `grpc-haskell` or `proto-lens`.

To enable actual gRPC calls, uncomment the gRPC dependencies in `package.yaml` and implement the `runGrpc` function in `Grey.GRPC.Client`.

## License

MIT License - see LICENSE file for details.
