%%%-------------------------------------------------------------------
%%% @doc Grey SDK Supervisor.
%%%
%%% This supervisor manages Grey SDK clients. Clients can be started
%%% dynamically and are registered by name for easy lookup.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_sdk_sup).

-behaviour(supervisor).

-include("grey_sdk.hrl").

%% API
-export([
    start_link/0,
    start_client/2,
    stop_client/1,
    get_client/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% @doc Starts a client with the given name and options.
-spec start_client(atom(), #grey_options{}) -> {ok, pid()} | {error, term()}.
start_client(Name, Options) ->
    ChildId = client_id(Name),
    ChildSpec = #{
        id => ChildId,
        start => {grey_client, start_link, [Options, [{local, Name}]]},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [grey_client]
    },
    case supervisor:start_child(?SUPERVISOR, ChildSpec) of
        {ok, Pid} -> {ok, Pid};
        {ok, Pid, _Info} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, _} = Error -> Error
    end.

%% @doc Stops a client by name.
-spec stop_client(atom()) -> ok | {error, term()}.
stop_client(Name) ->
    ChildId = client_id(Name),
    case supervisor:terminate_child(?SUPERVISOR, ChildId) of
        ok ->
            supervisor:delete_child(?SUPERVISOR, ChildId);
        {error, _} = Error ->
            Error
    end.

%% @doc Gets the pid of a client by name.
-spec get_client(atom()) -> {ok, pid()} | {error, not_found}.
get_client(Name) ->
    case whereis(Name) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

client_id(Name) ->
    list_to_atom("grey_client_" ++ atom_to_list(Name)).
