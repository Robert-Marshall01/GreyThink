%%%-------------------------------------------------------------------
%%% @doc Grey SDK Mutation gRPC service stub.
%%%
%%% This is a placeholder stub. In a real implementation, this would
%%% use generated protobuf client stubs from .proto files.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_grpc_mutation).

-include("grey_sdk.hrl").

%% API exports
-export([
    mutate/2,
    mutate/3
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Executes a mutation via gRPC.
-spec mutate(pid(), binary()) -> {ok, #mutation_data{}} | {error, term()}.
mutate(Channel, Endpoint) ->
    mutate(Channel, Endpoint, #{}).

-spec mutate(pid(), binary(), map()) -> {ok, #mutation_data{}} | {error, term()}.
mutate(Channel, Endpoint, Opts) ->
    Method = maps:get(method, Opts, <<"POST">>),
    Body = maps:get(body, Opts, undefined),
    
    simulate_authenticated_grpc_call(Channel, "mutation.Mutate", fun() ->
        #mutation_data{
            success = true,
            data = #{
                endpoint => Endpoint,
                method => Method,
                received => Body,
                stub => true
            },
            metadata = undefined
        }
    end).

%%====================================================================
%% Internal functions
%%====================================================================

simulate_authenticated_grpc_call(Channel, _Method, ResultFn) ->
    case grey_channel:is_authenticated(Channel) of
        false ->
            {error, grey_error:unauthorized()};
        true ->
            case grey_channel:get_channel(Channel) of
                {ok, _} ->
                    timer:sleep(1),
                    {ok, ResultFn()};
                {error, _} = Error ->
                    Error
            end
    end.
