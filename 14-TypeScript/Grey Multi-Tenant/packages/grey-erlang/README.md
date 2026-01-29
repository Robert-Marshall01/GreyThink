# Grey Multi-Tenant SDK - Erlang

Erlang SDK for the Grey Multi-Tenant platform using gRPC transport.

## Installation

Add the dependency to your `rebar.config`:

```erlang
{deps, [
    {grey_sdk, {git, "https://github.com/grey/grey-erlang.git", {tag, "0.1.0"}}}
]}.
```

Then run:

```bash
rebar3 get-deps
rebar3 compile
```

## Usage

```erlang
%% Start the application
application:ensure_all_started(grey_sdk).

%% Create client options
Options = grey_options:new(<<"api.grey.example.com">>, 443, true),

%% Start a client
{ok, Client} = grey_client:start_link(Options),

%% Login
case grey_auth:login(Client, <<"user@example.com">>, <<"password">>) of
    {ok, AuthData} ->
        io:format("Logged in: ~p~n", [maps:get(access_token, AuthData)]),
        
        %% Fetch user
        case grey_user:get_user(Client) of
            {ok, User} ->
                io:format("User: ~p~n", [maps:get(name, User)]);
            {error, Error} ->
                io:format("Error: ~p~n", [grey_error:message(Error)])
        end;
        
    {error, Error} ->
        io:format("Login failed: ~p~n", [grey_error:message(Error)])
end,

%% Stop client
grey_client:stop(Client).
```

## Pattern Matching

```erlang
handle_login(Client, Email, Password) ->
    case grey_auth:login(Client, Email, Password) of
        {ok, #{access_token := Token}} ->
            {ok, Token};
        {error, #grey_error{code = <<"VALIDATION_ERROR">>, message = Msg}} ->
            {error, {validation, Msg}};
        {error, #grey_error{code = <<"UNAUTHORIZED">>}} ->
            {error, unauthorized};
        {error, Error} ->
            {error, {unknown, grey_error:message(Error)}}
    end.
```

## Features

- **gRPC Transport**: Fast, type-safe binary protocol
- **OTP Compliant**: gen_server-based client with supervision support
- **Records**: Idiomatic Erlang records for structured data
- **Error Normalization**: Consistent `#grey_error{}` across all operations

## API

### grey_auth

- `login(Client, Email, Password)` - `{ok, AuthData} | {error, GreyError}`
- `logout(Client)` - `ok | {error, GreyError}`
- `refresh(Client, RefreshToken)` - `{ok, AuthData} | {error, GreyError}`

### grey_user

- `get_user(Client)` - `{ok, User} | {error, GreyError}`
- `get_user(Client, Options)` - `{ok, User} | {error, GreyError}`

### grey_projects

- `list_projects(Client)` - `{ok, ProjectsData} | {error, GreyError}`
- `list_projects(Client, Options)` - `{ok, ProjectsData} | {error, GreyError}`
- `create_project(Client, Name)` - `{ok, Project} | {error, GreyError}`
- `create_project(Client, Name, Options)` - `{ok, Project} | {error, GreyError}`

### grey_query

- `query(Client, Endpoint)` - `{ok, QueryData} | {error, GreyError}`
- `query(Client, Endpoint, Options)` - `{ok, QueryData} | {error, GreyError}`

### grey_mutation

- `mutate(Client, Endpoint)` - `{ok, MutationData} | {error, GreyError}`
- `mutate(Client, Endpoint, Options)` - `{ok, MutationData} | {error, GreyError}`

## License

MIT
