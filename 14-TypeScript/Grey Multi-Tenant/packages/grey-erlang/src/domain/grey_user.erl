%%%-------------------------------------------------------------------
%%% @doc Grey SDK User domain module.
%%%
%%% Provides user operations for the Grey SDK.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_user).

-include("grey_sdk.hrl").

%% API exports
-export([
    get_user/1,
    get_user/2
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Gets the currently authenticated user.
-spec get_user(pid()) -> {ok, #grey_user{}} | {error, #grey_error{}}.
get_user(Channel) ->
    get_user(Channel, #{}).

%% @doc Gets the currently authenticated user with options.
%% Options:
%%   - force_refresh: boolean() - Force refresh from server
-spec get_user(pid(), map()) -> {ok, #grey_user{}} | {error, #grey_error{}}.
get_user(Channel, Opts) when is_map(Opts) ->
    case grey_channel:is_authenticated(Channel) of
        false ->
            {error, grey_error:unauthorized(<<"User not authenticated">>)};
        true ->
            case grey_grpc_user:get_user(Channel, Opts) of
                {ok, _User} = Success ->
                    Success;
                {error, Reason} ->
                    {error, grey_error:from_any(Reason)}
            end
    end;
get_user(_Channel, _Opts) ->
    {error, grey_error:validation(<<"opts must be a map">>)}.
