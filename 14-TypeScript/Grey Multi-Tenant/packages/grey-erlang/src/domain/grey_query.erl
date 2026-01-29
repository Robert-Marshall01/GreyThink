%%%-------------------------------------------------------------------
%%% @doc Grey SDK Query domain module.
%%%
%%% Provides query operations for the Grey SDK.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_query).

-include("grey_sdk.hrl").

%% API exports
-export([
    query/2,
    query/3
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Executes a query to the specified endpoint.
-spec query(pid(), binary()) -> {ok, #query_data{}} | {error, #grey_error{}}.
query(Channel, Endpoint) ->
    query(Channel, Endpoint, #{}).

%% @doc Executes a query with options.
%% Options:
%%   - params: map() - Query parameters
%%   - require_auth: boolean() - Whether auth is required (default: true)
%%   - headers: map() - Additional headers
-spec query(pid(), binary(), map()) -> {ok, #query_data{}} | {error, #grey_error{}}.
query(Channel, Endpoint, Opts) when is_binary(Endpoint), is_map(Opts) ->
    RequireAuth = maps:get(require_auth, Opts, true),
    
    case validate_endpoint(Endpoint) of
        {error, _} = Error ->
            Error;
        ok ->
            case RequireAuth of
                true ->
                    case grey_channel:is_authenticated(Channel) of
                        false ->
                            {error, grey_error:unauthorized(<<"User not authenticated">>)};
                        true ->
                            execute_query(Channel, Endpoint, Opts)
                    end;
                false ->
                    execute_query(Channel, Endpoint, Opts)
            end
    end;
query(_Channel, Endpoint, _Opts) when not is_binary(Endpoint) ->
    {error, grey_error:validation(<<"endpoint must be a binary">>)};
query(_Channel, _Endpoint, _Opts) ->
    {error, grey_error:validation(<<"opts must be a map">>)}.

%%====================================================================
%% Internal functions
%%====================================================================

execute_query(Channel, Endpoint, Opts) ->
    case grey_grpc_query:query(Channel, Endpoint, Opts) of
        {ok, _QueryData} = Success ->
            Success;
        {error, Reason} ->
            {error, grey_error:from_any(Reason)}
    end.

validate_endpoint(Endpoint) ->
    case byte_size(Endpoint) of
        0 ->
            {error, grey_error:validation(<<"endpoint cannot be empty">>)};
        _ ->
            ok
    end.
