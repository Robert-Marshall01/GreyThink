%%%-------------------------------------------------------------------
%%% @doc Grey SDK Client - main façade module.
%%%
%%% This gen_server provides a unified interface to all Grey SDK
%%% functionality. It manages the channel and delegates to domain
%%% modules.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_client).

-behaviour(gen_server).

-include("grey_sdk.hrl").

%% API exports
-export([
    start_link/1,
    start_link/2,
    stop/1,
    
    %% Auth
    login/3,
    logout/1,
    refresh/2,
    is_authenticated/1,
    
    %% User
    get_user/1,
    get_user/2,
    
    %% Projects
    list_projects/1,
    list_projects/2,
    create_project/2,
    create_project/3,
    
    %% Query
    query/2,
    query/3,
    
    %% Mutation
    mutate/2,
    mutate/3,
    
    %% Utilities
    get_options/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    options :: #grey_options{},
    channel :: pid() | undefined
}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the client with options.
-spec start_link(#grey_options{}) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    start_link(Options, []).

-spec start_link(#grey_options{}, list()) -> {ok, pid()} | {error, term()}.
start_link(Options, GenServerOpts) ->
    gen_server:start_link(?MODULE, Options, GenServerOpts).

%% @doc Stops the client.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Authenticates with email and password.
-spec login(pid(), binary(), binary()) -> {ok, #auth_data{}} | {error, #grey_error{}}.
login(Pid, Email, Password) ->
    gen_server:call(Pid, {login, Email, Password}).

%% @doc Logs out the current user.
-spec logout(pid()) -> ok | {error, #grey_error{}}.
logout(Pid) ->
    gen_server:call(Pid, logout).

%% @doc Refreshes authentication with a refresh token.
-spec refresh(pid(), binary()) -> {ok, #auth_data{}} | {error, #grey_error{}}.
refresh(Pid, RefreshToken) ->
    gen_server:call(Pid, {refresh, RefreshToken}).

%% @doc Checks if the client is authenticated.
-spec is_authenticated(pid()) -> boolean().
is_authenticated(Pid) ->
    gen_server:call(Pid, is_authenticated).

%% @doc Gets the current user.
-spec get_user(pid()) -> {ok, #grey_user{}} | {error, #grey_error{}}.
get_user(Pid) ->
    get_user(Pid, #{}).

-spec get_user(pid(), map()) -> {ok, #grey_user{}} | {error, #grey_error{}}.
get_user(Pid, Opts) ->
    gen_server:call(Pid, {get_user, Opts}).

%% @doc Lists projects.
-spec list_projects(pid()) -> {ok, #projects_data{}} | {error, #grey_error{}}.
list_projects(Pid) ->
    list_projects(Pid, #{}).

-spec list_projects(pid(), map()) -> {ok, #projects_data{}} | {error, #grey_error{}}.
list_projects(Pid, Opts) ->
    gen_server:call(Pid, {list_projects, Opts}).

%% @doc Creates a project.
-spec create_project(pid(), binary()) -> {ok, #grey_project{}} | {error, #grey_error{}}.
create_project(Pid, Name) ->
    create_project(Pid, Name, #{}).

-spec create_project(pid(), binary(), map()) -> {ok, #grey_project{}} | {error, #grey_error{}}.
create_project(Pid, Name, Opts) ->
    gen_server:call(Pid, {create_project, Name, Opts}).

%% @doc Executes a query.
-spec query(pid(), binary()) -> {ok, #query_data{}} | {error, #grey_error{}}.
query(Pid, Endpoint) ->
    query(Pid, Endpoint, #{}).

-spec query(pid(), binary(), map()) -> {ok, #query_data{}} | {error, #grey_error{}}.
query(Pid, Endpoint, Opts) ->
    gen_server:call(Pid, {query, Endpoint, Opts}).

%% @doc Executes a mutation.
-spec mutate(pid(), binary()) -> {ok, #mutation_data{}} | {error, #grey_error{}}.
mutate(Pid, Endpoint) ->
    mutate(Pid, Endpoint, #{}).

-spec mutate(pid(), binary(), map()) -> {ok, #mutation_data{}} | {error, #grey_error{}}.
mutate(Pid, Endpoint, Opts) ->
    gen_server:call(Pid, {mutate, Endpoint, Opts}).

%% @doc Gets the client options.
-spec get_options(pid()) -> #grey_options{}.
get_options(Pid) ->
    gen_server:call(Pid, get_options).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    State = #state{
        options = Options,
        channel = undefined
    },
    {ok, State, {continue, start_channel}}.

handle_continue(start_channel, State) ->
    case grey_channel:start_link(State#state.options) of
        {ok, Channel} ->
            {noreply, State#state{channel = Channel}};
        {error, Reason} ->
            error_logger:error_msg("Failed to start Grey channel: ~p~n", [Reason]),
            {stop, Reason, State}
    end.

handle_call({login, Email, Password}, _From, #state{channel = Channel} = State) ->
    Result = grey_auth:login(Channel, Email, Password),
    {reply, Result, State};

handle_call(logout, _From, #state{channel = Channel} = State) ->
    Result = grey_auth:logout(Channel),
    {reply, Result, State};

handle_call({refresh, RefreshToken}, _From, #state{channel = Channel} = State) ->
    Result = grey_auth:refresh(Channel, RefreshToken),
    {reply, Result, State};

handle_call(is_authenticated, _From, #state{channel = Channel} = State) ->
    Result = grey_channel:is_authenticated(Channel),
    {reply, Result, State};

handle_call({get_user, Opts}, _From, #state{channel = Channel} = State) ->
    Result = grey_user:get_user(Channel, Opts),
    {reply, Result, State};

handle_call({list_projects, Opts}, _From, #state{channel = Channel} = State) ->
    Result = grey_projects:list_projects(Channel, Opts),
    {reply, Result, State};

handle_call({create_project, Name, Opts}, _From, #state{channel = Channel} = State) ->
    Result = grey_projects:create_project(Channel, Name, Opts),
    {reply, Result, State};

handle_call({query, Endpoint, Opts}, _From, #state{channel = Channel} = State) ->
    Result = grey_query:query(Channel, Endpoint, Opts),
    {reply, Result, State};

handle_call({mutate, Endpoint, Opts}, _From, #state{channel = Channel} = State) ->
    Result = grey_mutation:mutate(Channel, Endpoint, Opts),
    {reply, Result, State};

handle_call(get_options, _From, #state{options = Options} = State) ->
    {reply, Options, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel = undefined}) ->
    ok;
terminate(_Reason, #state{channel = Channel}) ->
    grey_channel:stop(Channel),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
