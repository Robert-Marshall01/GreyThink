%%%-------------------------------------------------------------------
%%% @doc Grey SDK Mutation domain module.
%%%
%%% Provides mutation operations for the Grey SDK.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_mutation).

-include("grey_sdk.hrl").

%% API exports
-export([
    mutate/2,
    mutate/3
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Executes a mutation to the specified endpoint.
-spec mutate(pid(), binary()) -> {ok, #mutation_data{}} | {error, #grey_error{}}.
mutate(Channel, Endpoint) ->
    mutate(Channel, Endpoint, #{}).

%% @doc Executes a mutation with options.
%% Options:
%%   - method: binary() - HTTP method (default: <<"POST">>)
%%   - body: term() - Request body
%%   - headers: map() - Additional headers
-spec mutate(pid(), binary(), map()) -> {ok, #mutation_data{}} | {error, #grey_error{}}.
mutate(Channel, Endpoint, Opts) when is_binary(Endpoint), is_map(Opts) ->
    case grey_channel:is_authenticated(Channel) of
        false ->
            {error, grey_error:unauthorized(<<"User not authenticated">>)};
        true ->
            case validate_mutation(Endpoint, Opts) of
                {error, _} = Error ->
                    Error;
                ok ->
                    execute_mutation(Channel, Endpoint, Opts)
            end
    end;
mutate(_Channel, Endpoint, _Opts) when not is_binary(Endpoint) ->
    {error, grey_error:validation(<<"endpoint must be a binary">>)};
mutate(_Channel, _Endpoint, _Opts) ->
    {error, grey_error:validation(<<"opts must be a map">>)}.

%%====================================================================
%% Internal functions
%%====================================================================

execute_mutation(Channel, Endpoint, Opts) ->
    case grey_grpc_mutation:mutate(Channel, Endpoint, Opts) of
        {ok, _MutationData} = Success ->
            Success;
        {error, Reason} ->
            {error, grey_error:from_any(Reason)}
    end.

validate_mutation(Endpoint, Opts) ->
    case byte_size(Endpoint) of
        0 ->
            {error, grey_error:validation(<<"endpoint cannot be empty">>)};
        _ ->
            validate_method(Opts)
    end.

validate_method(Opts) ->
    Method = maps:get(method, Opts, <<"POST">>),
    ValidMethods = [<<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>],
    case lists:member(Method, ValidMethods) of
        true -> ok;
        false ->
            {error, grey_error:validation(<<"method must be POST, PUT, PATCH, or DELETE">>)}
    end.
