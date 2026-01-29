# Grey PHP SDK

PHP client for the Grey Multi-Tenant API using HTTP JSON transport.

## Requirements

- PHP 8.1+
- Composer

## Installation

```bash
composer require grey/sdk
```

## Quick Start

```php
<?php

use Grey\Sdk\GreyClient;
use Grey\Sdk\Config\GreyOptions;

// Create client
$options = GreyOptions::local(8080);
// Or for production:
// $options = GreyOptions::production('api.grey.com');

$client = new GreyClient($options);

// Login
$result = $client->auth()->login('user@example.com', 'password');
if ($result->isOk()) {
    $authData = $result->getData();
    echo "Access token: " . $authData['accessToken'] . "\n";
} else {
    $error = $result->getError();
    echo "Login failed: " . $error->getMessage() . "\n";
}

// Get current user
$result = $client->user()->getUser();
if ($result->isOk()) {
    $user = $result->getData();
    echo "Hello, " . $user['name'] . "\n";
}

// List projects
$result = $client->projects()->listProjects(['page' => 1, 'pageSize' => 10]);
if ($result->isOk()) {
    $data = $result->getData();
    foreach ($data['projects'] as $project) {
        echo "Project: " . $project['name'] . "\n";
    }
}

// Create project
$result = $client->projects()->createProject('My Project', [
    'description' => 'A new project'
]);

// Query
$result = $client->query()->query('/api/data', [
    'params' => ['filter' => 'active']
]);

// Mutate
$result = $client->mutation()->mutate('/api/update', [
    'method' => 'PUT',
    'body' => ['name' => 'Updated']
]);

// Logout
$client->auth()->logout();
```

## Error Handling

All operations return a `Result` object:

```php
$result = $client->auth()->login($email, $password);

if ($result->isOk()) {
    $data = $result->getData();
    // Success
} else {
    $error = $result->getError();
    echo $error->getCode();    // e.g., 'unauthorized'
    echo $error->getMessage(); // Human-readable message
    echo $error->getDetails(); // Additional details (array or null)
}
```

Error codes:
- `unauthorized` - Authentication required
- `forbidden` - Permission denied
- `not_found` - Resource not found
- `validation_error` - Input validation failed
- `network_error` - Network failure
- `timeout` - Request timeout
- `server_error` - Server-side error
- `unknown` - Unclassified error

## Configuration

```php
use Grey\Sdk\Config\GreyOptions;

// Local development
$options = GreyOptions::local();       // localhost:8080
$options = GreyOptions::local(9000);   // localhost:9000

// Production with HTTPS
$options = GreyOptions::production('api.grey.com');

// Full control
$options = new GreyOptions(
    host: 'api.grey.com',
    port: 443,
    useTls: true,
    timeoutMs: 30000,
    metadata: ['X-Custom-Header' => 'value']
);
```

## Testing

```bash
composer test
```

## License

MIT
