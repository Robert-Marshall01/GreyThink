%%%-------------------------------------------------------------------
%%% @doc Grey SDK OTP Application.
%%%
%%% This module implements the OTP application behaviour for the
%%% Grey SDK. It starts the supervision tree and can optionally
%%% start a default client.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_sdk_app).

-behaviour(application).

-include("grey_sdk.hrl").

%% Application callbacks
-export([
    start/2,
    stop/1
]).

%% API
-export([
    start_client/1,
    start_client/2,
    stop_client/1,
    get_client/1
]).

%%====================================================================
%% Application callbacks
%%====================================================================

start(_StartType, _StartArgs) ->
    grey_sdk_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts a named client under the supervisor.
-spec start_client(#grey_options{}) -> {ok, pid()} | {error, term()}.
start_client(Options) ->
    start_client(default, Options).

%% @doc Starts a client with a given name under the supervisor.
-spec start_client(atom(), #grey_options{}) -> {ok, pid()} | {error, term()}.
start_client(Name, Options) ->
    grey_sdk_sup:start_client(Name, Options).

%% @doc Stops a named client.
-spec stop_client(atom()) -> ok | {error, term()}.
stop_client(Name) ->
    grey_sdk_sup:stop_client(Name).

%% @doc Gets the pid of a named client.
-spec get_client(atom()) -> {ok, pid()} | {error, not_found}.
get_client(Name) ->
    grey_sdk_sup:get_client(Name).
