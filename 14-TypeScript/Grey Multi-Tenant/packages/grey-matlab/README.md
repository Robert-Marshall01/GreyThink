# Grey SDK for MATLAB

A MATLAB SDK for the Grey Multi-Tenant platform using HTTP JSON transport.

## Requirements

- MATLAB R2016b or later
- Web services toolbox (for `webread`/`webwrite`)

## Installation

Add the `grey-matlab` folder to your MATLAB path:

```matlab
addpath('/path/to/grey-matlab');
```

Or use the startup script:

```matlab
run('/path/to/grey-matlab/startup.m');
```

## Quick Start

```matlab
% Import the package
import grey.*

% Create client for local development
client = GreyClient(grey.config.Options.local(8080));

% Create client for production
client = GreyClient(grey.config.Options.production('api.grey.com'));

% Login
result = client.auth.login('username', 'password');
if result.isOk()
    disp('Logged in successfully');
    tokens = result.unwrap();
end

% List projects
result = client.projects.list();
if result.isOk()
    projects = result.unwrap();
    for i = 1:length(projects)
        disp(projects(i).name);
    end
end

% Create a project
result = client.projects.create('My Project', 'Description');

% Execute a query
result = client.query.execute('{ users { id name } }');

% Execute a mutation
result = client.mutation.execute('mutation { createUser(name: "John") { id } }');

% Logout
client.auth.logout();
```

## API Reference

### GreyClient

Main client class that provides access to all domain clients.

#### Constructor
- `GreyClient(options)` - Create client with Options

#### Properties
- `auth` - AuthClient for authentication
- `user` - UserClient for user operations
- `projects` - ProjectsClient for project operations
- `query` - QueryClient for queries
- `mutation` - MutationClient for mutations

### Options

```matlab
% Local development
opts = grey.config.Options.local(8080);

% Production
opts = grey.config.Options.production('api.grey.com');

% Custom
opts = grey.config.Options('api.grey.com', 443, true, 30);
```

### Result Type

All operations return a Result object:

```matlab
result = client.projects.list();

% Check status
if result.isOk()
    data = result.unwrap();
elseif result.isErr()
    err = result.error();
    disp(err.message);
end

% With default value
data = result.unwrapOr(defaultValue);
```

### Error Handling

Errors are normalized to:
- `code` - Error code string
- `message` - Human-readable message
- `details` - Additional error details (struct or empty)

## Domain Clients

### AuthClient
- `login(username, password)` - Authenticate
- `logout()` - End session
- `refresh(refreshToken)` - Refresh access token

### UserClient
- `get(userId)` - Get user by ID
- `getCurrent()` - Get current user

### ProjectsClient
- `list(options)` - List projects with pagination
- `get(projectId)` - Get project by ID
- `create(name, description, metadata)` - Create project
- `update(projectId, updates)` - Update project
- `delete(projectId)` - Delete project

### QueryClient
- `execute(queryString, variables, operationName)` - Execute query
- `batch(queries)` - Execute multiple queries

### MutationClient
- `execute(mutationString, variables, operationName)` - Execute mutation
- `batch(mutations)` - Execute multiple mutations

## License

MIT License
