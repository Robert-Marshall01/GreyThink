%%%-------------------------------------------------------------------
%%% @doc Grey SDK Auth domain module.
%%%
%%% Provides authentication operations for the Grey SDK.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_auth).

-include("grey_sdk.hrl").

%% API exports
-export([
    login/3,
    logout/1,
    refresh/2
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Authenticates a user with email and password.
%% On success, stores the access token in the channel.
-spec login(pid(), binary(), binary()) -> {ok, #auth_data{}} | {error, #grey_error{}}.
login(Channel, Email, Password) when is_binary(Email), is_binary(Password) ->
    case validate_credentials(Email, Password) of
        ok ->
            case grey_grpc_auth:login(Channel, Email, Password) of
                {ok, AuthData} = Success ->
                    grey_channel:set_access_token(Channel, AuthData#auth_data.access_token),
                    Success;
                {error, Reason} ->
                    {error, grey_error:from_any(Reason)}
            end;
        {error, _} = Error ->
            Error
    end;
login(_Channel, _Email, _Password) ->
    {error, grey_error:validation(<<"email and password must be binaries">>)}.

%% @doc Logs out the current user.
%% Clears the access token from the channel.
-spec logout(pid()) -> ok | {error, #grey_error{}}.
logout(Channel) ->
    case grey_channel:is_authenticated(Channel) of
        false ->
            {error, grey_error:unauthorized()};
        true ->
            case grey_grpc_auth:logout(Channel) of
                {ok, _} ->
                    grey_channel:set_access_token(Channel, undefined),
                    ok;
                ok ->
                    grey_channel:set_access_token(Channel, undefined),
                    ok;
                {error, Reason} ->
                    {error, grey_error:from_any(Reason)}
            end
    end.

%% @doc Refreshes the authentication tokens using a refresh token.
-spec refresh(pid(), binary()) -> {ok, #auth_data{}} | {error, #grey_error{}}.
refresh(Channel, RefreshToken) when is_binary(RefreshToken) ->
    case grey_grpc_auth:refresh(Channel, RefreshToken) of
        {ok, AuthData} = Success ->
            grey_channel:set_access_token(Channel, AuthData#auth_data.access_token),
            Success;
        {error, Reason} ->
            {error, grey_error:from_any(Reason)}
    end;
refresh(_Channel, _RefreshToken) ->
    {error, grey_error:validation(<<"refresh_token must be a binary">>)}.

%%====================================================================
%% Internal functions
%%====================================================================

validate_credentials(Email, Password) ->
    case byte_size(Email) of
        0 ->
            {error, grey_error:validation(<<"email cannot be empty">>)};
        _ ->
            case byte_size(Password) of
                0 ->
                    {error, grey_error:validation(<<"password cannot be empty">>)};
                _ ->
                    ok
            end
    end.
