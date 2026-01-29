%%%-------------------------------------------------------------------
%%% @doc Grey SDK - Erlang client for Grey Multi-Tenant API.
%%%
%%% This module provides convenience functions for working with the
%%% Grey SDK. For full functionality, use grey_client directly.
%%%
%%% == Quick Start ==
%%% ```
%%% %% Start the application
%%% application:start(grey_sdk).
%%%
%%% %% Create options and start a client
%%% Options = grey_options:local(8080),
%%% {ok, Client} = grey_sdk:start_client(my_client, Options),
%%%
%%% %% Authenticate
%%% {ok, AuthData} = grey_sdk:login(my_client, <<"user@example.com">>, <<"password">>),
%%%
%%% %% Use the SDK
%%% {ok, User} = grey_sdk:get_user(my_client),
%%% {ok, Projects} = grey_sdk:list_projects(my_client),
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(grey_sdk).

-include("grey_sdk.hrl").

%% Client management
-export([
    start_client/1,
    start_client/2,
    stop_client/1,
    get_client/1
]).

%% Auth operations
-export([
    login/3,
    logout/1,
    refresh/2,
    is_authenticated/1
]).

%% User operations
-export([
    get_user/1,
    get_user/2
]).

%% Projects operations
-export([
    list_projects/1,
    list_projects/2,
    create_project/2,
    create_project/3
]).

%% Query operations
-export([
    query/2,
    query/3
]).

%% Mutation operations
-export([
    mutate/2,
    mutate/3
]).

%%====================================================================
%% Client management
%%====================================================================

%% @doc Starts a client with default name.
-spec start_client(#grey_options{}) -> {ok, pid()} | {error, term()}.
start_client(Options) ->
    grey_sdk_app:start_client(Options).

%% @doc Starts a named client.
-spec start_client(atom(), #grey_options{}) -> {ok, pid()} | {error, term()}.
start_client(Name, Options) ->
    grey_sdk_app:start_client(Name, Options).

%% @doc Stops a named client.
-spec stop_client(atom()) -> ok | {error, term()}.
stop_client(Name) ->
    grey_sdk_app:stop_client(Name).

%% @doc Gets the pid of a named client.
-spec get_client(atom()) -> {ok, pid()} | {error, not_found}.
get_client(Name) ->
    grey_sdk_app:get_client(Name).

%%====================================================================
%% Auth operations
%%====================================================================

%% @doc Authenticates with email and password.
-spec login(atom() | pid(), binary(), binary()) -> {ok, #auth_data{}} | {error, #grey_error{}}.
login(ClientOrName, Email, Password) ->
    with_client(ClientOrName, fun(Client) ->
        grey_client:login(Client, Email, Password)
    end).

%% @doc Logs out the current user.
-spec logout(atom() | pid()) -> ok | {error, #grey_error{}}.
logout(ClientOrName) ->
    with_client(ClientOrName, fun(Client) ->
        grey_client:logout(Client)
    end).

%% @doc Refreshes authentication with a refresh token.
-spec refresh(atom() | pid(), binary()) -> {ok, #auth_data{}} | {error, #grey_error{}}.
refresh(ClientOrName, RefreshToken) ->
    with_client(ClientOrName, fun(Client) ->
        grey_client:refresh(Client, RefreshToken)
    end).

%% @doc Checks if the client is authenticated.
-spec is_authenticated(atom() | pid()) -> boolean().
is_authenticated(ClientOrName) ->
    with_client(ClientOrName, fun(Client) ->
        grey_client:is_authenticated(Client)
    end).

%%====================================================================
%% User operations
%%====================================================================

%% @doc Gets the current user.
-spec get_user(atom() | pid()) -> {ok, #grey_user{}} | {error, #grey_error{}}.
get_user(ClientOrName) ->
    get_user(ClientOrName, #{}).

-spec get_user(atom() | pid(), map()) -> {ok, #grey_user{}} | {error, #grey_error{}}.
get_user(ClientOrName, Opts) ->
    with_client(ClientOrName, fun(Client) ->
        grey_client:get_user(Client, Opts)
    end).

%%====================================================================
%% Projects operations
%%====================================================================

%% @doc Lists projects.
-spec list_projects(atom() | pid()) -> {ok, #projects_data{}} | {error, #grey_error{}}.
list_projects(ClientOrName) ->
    list_projects(ClientOrName, #{}).

-spec list_projects(atom() | pid(), map()) -> {ok, #projects_data{}} | {error, #grey_error{}}.
list_projects(ClientOrName, Opts) ->
    with_client(ClientOrName, fun(Client) ->
        grey_client:list_projects(Client, Opts)
    end).

%% @doc Creates a project.
-spec create_project(atom() | pid(), binary()) -> {ok, #grey_project{}} | {error, #grey_error{}}.
create_project(ClientOrName, Name) ->
    create_project(ClientOrName, Name, #{}).

-spec create_project(atom() | pid(), binary(), map()) -> {ok, #grey_project{}} | {error, #grey_error{}}.
create_project(ClientOrName, Name, Opts) ->
    with_client(ClientOrName, fun(Client) ->
        grey_client:create_project(Client, Name, Opts)
    end).

%%====================================================================
%% Query operations
%%====================================================================

%% @doc Executes a query.
-spec query(atom() | pid(), binary()) -> {ok, #query_data{}} | {error, #grey_error{}}.
query(ClientOrName, Endpoint) ->
    query(ClientOrName, Endpoint, #{}).

-spec query(atom() | pid(), binary(), map()) -> {ok, #query_data{}} | {error, #grey_error{}}.
query(ClientOrName, Endpoint, Opts) ->
    with_client(ClientOrName, fun(Client) ->
        grey_client:query(Client, Endpoint, Opts)
    end).

%%====================================================================
%% Mutation operations
%%====================================================================

%% @doc Executes a mutation.
-spec mutate(atom() | pid(), binary()) -> {ok, #mutation_data{}} | {error, #grey_error{}}.
mutate(ClientOrName, Endpoint) ->
    mutate(ClientOrName, Endpoint, #{}).

-spec mutate(atom() | pid(), binary(), map()) -> {ok, #mutation_data{}} | {error, #grey_error{}}.
mutate(ClientOrName, Endpoint, Opts) ->
    with_client(ClientOrName, fun(Client) ->
        grey_client:mutate(Client, Endpoint, Opts)
    end).

%%====================================================================
%% Internal functions
%%====================================================================

with_client(Client, Fun) when is_pid(Client) ->
    Fun(Client);
with_client(Name, Fun) when is_atom(Name) ->
    case get_client(Name) of
        {ok, Client} -> Fun(Client);
        {error, not_found} -> {error, grey_error:new(not_found, <<"Client not found">>)}
    end.
