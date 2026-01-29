%%%-------------------------------------------------------------------
%%% @doc Grey SDK User gRPC service stub.
%%%
%%% This is a placeholder stub. In a real implementation, this would
%%% use generated protobuf client stubs from .proto files.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_grpc_user).

-include("grey_sdk.hrl").

%% API exports
-export([
    get_user/1,
    get_user/2
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Gets the current user via gRPC.
-spec get_user(pid()) -> {ok, #grey_user{}} | {error, term()}.
get_user(Channel) ->
    get_user(Channel, #{}).

-spec get_user(pid(), map()) -> {ok, #grey_user{}} | {error, term()}.
get_user(Channel, _Opts) ->
    simulate_authenticated_grpc_call(Channel, "user.GetUser", fun() ->
        #grey_user{
            id = <<"stub_user_id">>,
            email = <<"user@example.com">>,
            name = <<"Stub User">>,
            avatar = undefined,
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
