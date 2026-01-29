# Grey PHP SDK Implementation

## Overview

The Grey PHP SDK provides an HTTP JSON client for the Grey Multi-Tenant API. The implementation follows modern PHP 8.1+ conventions with strict typing, enums, readonly properties, and PSR standards.

## Architecture

```
GreyClient (façade)
    ├── AuthClient
    ├── UserClient
    ├── ProjectsClient
    ├── QueryClient
    └── MutationClient
            └── HttpClient (shared)
```

## Class Structure

### Core Classes

| Class | Namespace | Description |
|-------|-----------|-------------|
| `GreyClient` | `Grey\Sdk` | Main façade providing access to domain clients |
| `GreyOptions` | `Grey\Sdk\Config` | Configuration options |
| `HttpClient` | `Grey\Sdk\Http` | HTTP client with GET/POST/PUT/PATCH/DELETE |
| `GreyError` | `Grey\Sdk\Error` | Normalized error representation |
| `ErrorCode` | `Grey\Sdk\Error` | Error code enum |
| `Result` | `Grey\Sdk\Error` | Result type for success/failure |

### Domain Clients

| Class | Operations |
|-------|------------|
| `AuthClient` | `login()`, `logout()`, `refresh()` |
| `UserClient` | `getUser()` |
| `ProjectsClient` | `listProjects()`, `createProject()` |
| `QueryClient` | `query()` |
| `MutationClient` | `mutate()` |

## Result Pattern

All operations return a `Result<T>` object:

```php
// Success
$result = Result::ok(['id' => 1, 'name' => 'Test']);
$result->isOk();    // true
$result->getData(); // ['id' => 1, 'name' => 'Test']

// Error
$result = Result::err(GreyError::unauthorized());
$result->isErr();     // true
$result->getError();  // GreyError instance
```

### Result Methods

| Method | Description |
|--------|-------------|
| `isOk()` | Returns true if successful |
| `isErr()` | Returns true if failed |
| `getData()` | Gets data (null if error) |
| `getError()` | Gets error (null if success) |
| `unwrap()` | Gets data or throws |
| `unwrapOr($default)` | Gets data or returns default |
| `map(callable)` | Maps data if successful |
| `then(callable)` | Chains another Result operation |
| `catch(callable)` | Handles error if failed |
| `toArray()` | Converts to array |

## Error Handling

### Error Codes

| Code | HTTP Status | Description |
|------|-------------|-------------|
| `unauthorized` | 401 | Authentication required |
| `forbidden` | 403 | Permission denied |
| `not_found` | 404 | Resource not found |
| `validation_error` | 400, 422 | Input validation failed |
| `timeout` | 408, 504 | Request timeout |
| `server_error` | 5xx | Server-side error |
| `network_error` | - | Connection failure |
| `unknown` | - | Unclassified error |

### Error Factories

```php
GreyError::unauthorized('Custom message');
GreyError::validation('Email is required', ['field' => 'email']);
GreyError::fromHttpResponse(404, ['message' => 'Not found']);
GreyError::fromException($exception);
```

## Configuration

```php
use Grey\Sdk\Config\GreyOptions;

// Local development
$options = GreyOptions::local();        // localhost:8080, HTTP
$options = GreyOptions::local(9000);    // localhost:9000, HTTP

// Production
$options = GreyOptions::production('api.grey.com');     // port 443, HTTPS
$options = GreyOptions::production('api.grey.com', 8443);

// Full control
$options = new GreyOptions(
    host: 'api.grey.com',
    port: 443,
    useTls: true,
    timeoutMs: 30000,
    metadata: ['X-Tenant-Id' => 'abc123'],
);

// Modifiers (immutable)
$options = $options->withTimeout(5000);
$options = $options->withMetadata(['X-Custom' => 'value']);
```

## Usage Examples

### Basic Usage

```php
use Grey\Sdk\GreyClient;
use Grey\Sdk\Config\GreyOptions;

$client = new GreyClient(GreyOptions::local(8080));

// Login
$result = $client->auth()->login('user@example.com', 'password');
if ($result->isOk()) {
    echo "Logged in!\n";
}

// Get user
$result = $client->user()->getUser();
if ($result->isOk()) {
    $user = $result->getData();
    echo "Hello, {$user['name']}!\n";
}

// List projects with pagination
$result = $client->projects()->listProjects([
    'page' => 1,
    'pageSize' => 20,
]);

// Create project
$result = $client->projects()->createProject('My Project', [
    'description' => 'A new project',
]);

// Query
$result = $client->query()->query('/api/data', [
    'params' => ['filter' => 'active'],
    'requireAuth' => true,
]);

// Mutate
$result = $client->mutation()->mutate('/api/update', [
    'method' => 'PUT',
    'body' => ['name' => 'Updated Name'],
]);

// Logout
$client->auth()->logout();
```

### Chaining with Result

```php
$result = $client->auth()->login($email, $password)
    ->then(fn() => $client->user()->getUser())
    ->map(fn($user) => $user['name'])
    ->catch(fn($error) => Result::ok('Anonymous'));

echo "User: " . $result->unwrap() . "\n";
```

### Error Handling

```php
$result = $client->auth()->login($email, $password);

if ($result->isErr()) {
    $error = $result->getError();
    
    match ($error->getCode()) {
        ErrorCode::UNAUTHORIZED => handleUnauthorized(),
        ErrorCode::VALIDATION_ERROR => handleValidation($error->getDetails()),
        ErrorCode::NETWORK_ERROR => handleNetworkError(),
        default => handleGenericError($error->getMessage()),
    };
}
```

## Requirements

- PHP 8.1+
- ext-json
- guzzlehttp/guzzle ^7.8

## Installation

```bash
composer require grey/sdk
```

## Testing

```bash
composer test
```

## Code Quality

```bash
# Static analysis
composer analyse

# Code style check
composer cs

# Code style fix
composer cs-fix
```

## Checklist

- [x] HTTP JSON transport
- [x] Shared HttpClient with GET/POST/PUT/PATCH/DELETE
- [x] Error normalization with { code, message, details }
- [x] Auth: login, logout, refresh
- [x] User: getUser
- [x] Projects: listProjects, createProject
- [x] Query: query
- [x] Mutation: mutate
- [x] Result monad pattern
- [x] PHP 8.1+ features (enums, readonly, named args)
- [x] PSR-4 autoloading
- [x] PHPUnit tests
- [x] PHPStan configuration
