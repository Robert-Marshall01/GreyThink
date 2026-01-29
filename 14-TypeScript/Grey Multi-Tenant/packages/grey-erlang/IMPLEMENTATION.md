# Grey Erlang SDK Implementation

## Overview

The Grey Erlang SDK provides a complete Erlang/OTP client for the Grey Multi-Tenant API using gRPC transport. The implementation follows OTP best practices with proper supervision, gen_server behaviors, and idiomatic Erlang patterns.

## Architecture

```
grey_sdk_app (application)
    └── grey_sdk_sup (supervisor)
            └── grey_client (gen_server) [dynamic children]
                    └── grey_channel (gen_server)
```

## Module Structure

### Core Modules

| Module | Type | Description |
|--------|------|-------------|
| `grey_sdk` | API | Main entry point with convenience functions |
| `grey_sdk_app` | Application | OTP application behaviour |
| `grey_sdk_sup` | Supervisor | Dynamic child management |
| `grey_client` | gen_server | Client façade aggregating all operations |
| `grey_channel` | gen_server | gRPC channel and auth state management |

### Configuration

| Module | Description |
|--------|-------------|
| `grey_options` | Configuration options record and helpers |
| `grey_error` | Normalized error handling |

### Domain Modules

| Module | Operations |
|--------|------------|
| `grey_auth` | login/3, logout/1, refresh/2 |
| `grey_user` | get_user/1, get_user/2 |
| `grey_projects` | list_projects/1-2, create_project/2-3 |
| `grey_query` | query/2, query/3 |
| `grey_mutation` | mutate/2, mutate/3 |

### gRPC Stubs

| Module | Purpose |
|--------|---------|
| `grey_grpc_auth` | Auth service stub |
| `grey_grpc_user` | User service stub |
| `grey_grpc_projects` | Projects service stub |
| `grey_grpc_query` | Query service stub |
| `grey_grpc_mutation` | Mutation service stub |

## Records

Defined in `include/grey_sdk.hrl`:

```erlang
-record(grey_error, {code, message, details}).
-record(grey_options, {host, port, use_tls, timeout_ms, metadata}).
-record(auth_data, {access_token, refresh_token, expires_in}).
-record(grey_user, {id, email, name, avatar, metadata}).
-record(grey_project, {id, name, description, created_at, updated_at, metadata}).
-record(projects_data, {projects, total, page, page_size}).
-record(query_data, {data, metadata}).
-record(mutation_data, {success, data, metadata}).
```

## Error Handling

All operations return standard Erlang tuples:
- `{ok, Result}` for success
- `{error, #grey_error{}}` for failures

Error codes:
- `unauthorized` - Authentication required
- `forbidden` - Permission denied
- `not_found` - Resource not found
- `validation_error` - Input validation failed
- `network_error` - Network failure
- `timeout` - Request timeout
- `server_error` - Server-side error
- `unknown` - Unclassified error

## Usage Example

```erlang
%% Start the application
application:ensure_all_started(grey_sdk).

%% Create options
Options = grey_options:local(8080),
%% Or for production:
%% Options = grey_options:production(<<"api.grey.com">>),

%% Start a named client
{ok, _Pid} = grey_sdk:start_client(my_client, Options),

%% Login
{ok, AuthData} = grey_sdk:login(my_client, <<"user@example.com">>, <<"password">>),

%% Check authentication
true = grey_sdk:is_authenticated(my_client),

%% Get current user
{ok, User} = grey_sdk:get_user(my_client),
io:format("Hello, ~s!~n", [User#grey_user.name]),

%% List projects
{ok, ProjectsData} = grey_sdk:list_projects(my_client, #{page => 1, page_size => 10}),
lists:foreach(fun(P) ->
    io:format("Project: ~s~n", [P#grey_project.name])
end, ProjectsData#projects_data.projects),

%% Create a project
{ok, NewProject} = grey_sdk:create_project(my_client, <<"My Project">>, #{
    description => <<"A new project">>
}),

%% Execute a query
{ok, QueryResult} = grey_sdk:query(my_client, <<"/api/data">>, #{
    params => #{filter => <<"active">>}
}),

%% Execute a mutation
{ok, MutationResult} = grey_sdk:mutate(my_client, <<"/api/update">>, #{
    method => <<"PUT">>,
    body => #{name => <<"Updated Name">>}
}),

%% Logout
ok = grey_sdk:logout(my_client),

%% Stop client
grey_sdk:stop_client(my_client).
```

## Direct Client Usage

For more control, use `grey_client` directly:

```erlang
Options = grey_options:new(<<"localhost">>, 8080, false),
{ok, Client} = grey_client:start_link(Options),

{ok, _} = grey_client:login(Client, <<"user@example.com">>, <<"password">>),
{ok, User} = grey_client:get_user(Client),

grey_client:stop(Client).
```

## OTP Integration

The SDK integrates with OTP supervision trees:

```erlang
%% In your application supervisor
init([]) ->
    SupFlags = #{strategy => one_for_one},
    ChildSpecs = [
        #{
            id => grey_sdk,
            start => {grey_sdk_app, start, [normal, []]},
            type => supervisor
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

## Building

```bash
rebar3 compile
```

## Testing

```bash
rebar3 eunit
rebar3 ct
```

## Dependencies

- `grpcbox` - gRPC client
- `jsx` - JSON encoding/decoding

## Checklist

- [x] gRPC transport
- [x] Error normalization
- [x] Auth: login, logout, refresh
- [x] User: getUser
- [x] Projects: listProjects, createProject
- [x] Query: query
- [x] Mutation: mutate
- [x] OTP application behaviour
- [x] Supervision tree
- [x] gen_server clients
- [x] Records for data types
- [x] Comprehensive specs
- [ ] Protobuf code generation (stub implementation)
- [ ] Unit tests
- [ ] Common Test suites
