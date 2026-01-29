%%%-------------------------------------------------------------------
%%% @doc Grey SDK gRPC channel management.
%%%
%%% This gen_server manages the gRPC channel connection,
%%% authentication state, and provides metadata for requests.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_channel).

-behaviour(gen_server).

-include("grey_sdk.hrl").

%% API exports
-export([
    start_link/1,
    stop/1,
    get_channel/1,
    set_access_token/2,
    get_access_token/1,
    is_authenticated/1,
    auth_metadata/1,
    timeout/1
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
    channel :: term() | undefined,
    access_token :: binary() | undefined
}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the channel manager.
-spec start_link(#grey_options{}) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc Stops the channel manager.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Gets the gRPC channel.
-spec get_channel(pid()) -> {ok, term()} | {error, term()}.
get_channel(Pid) ->
    gen_server:call(Pid, get_channel).

%% @doc Sets the access token.
-spec set_access_token(pid(), binary() | undefined) -> ok.
set_access_token(Pid, Token) ->
    gen_server:cast(Pid, {set_access_token, Token}).

%% @doc Gets the current access token.
-spec get_access_token(pid()) -> binary() | undefined.
get_access_token(Pid) ->
    gen_server:call(Pid, get_access_token).

%% @doc Checks if authenticated.
-spec is_authenticated(pid()) -> boolean().
is_authenticated(Pid) ->
    gen_server:call(Pid, is_authenticated).

%% @doc Gets metadata with authorization header.
-spec auth_metadata(pid()) -> map().
auth_metadata(Pid) ->
    gen_server:call(Pid, auth_metadata).

%% @doc Gets the request timeout.
-spec timeout(pid()) -> pos_integer().
timeout(Pid) ->
    gen_server:call(Pid, timeout).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    State = #state{
        options = Options,
        channel = undefined,
        access_token = undefined
    },
    {ok, State, {continue, connect}}.

handle_continue(connect, State) ->
    case connect(State#state.options) of
        {ok, Channel} ->
            {noreply, State#state{channel = Channel}};
        {error, Reason} ->
            error_logger:warning_msg("Failed to connect to Grey API: ~p~n", [Reason]),
            {noreply, State}
    end.

handle_call(get_channel, _From, #state{channel = undefined} = State) ->
    case connect(State#state.options) of
        {ok, Channel} ->
            {reply, {ok, Channel}, State#state{channel = Channel}};
        {error, _} = Error ->
            {reply, Error, State}
    end;
handle_call(get_channel, _From, #state{channel = Channel} = State) ->
    {reply, {ok, Channel}, State};

handle_call(get_access_token, _From, #state{access_token = Token} = State) ->
    {reply, Token, State};

handle_call(is_authenticated, _From, #state{access_token = Token} = State) ->
    {reply, Token =/= undefined, State};

handle_call(auth_metadata, _From, #state{access_token = Token} = State) ->
    Metadata = case Token of
        undefined -> #{};
        _ -> #{<<"authorization">> => <<"Bearer ", Token/binary>>}
    end,
    {reply, Metadata, State};

handle_call(timeout, _From, #state{options = Options} = State) ->
    {reply, grey_options:timeout_ms(Options), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({set_access_token, Token}, State) ->
    {noreply, State#state{access_token = Token}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel = undefined}) ->
    ok;
terminate(_Reason, #state{channel = Channel}) ->
    catch grpcbox_channel:stop(Channel),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

connect(Options) ->
    Endpoint = grey_options:endpoint(Options),
    UseTls = grey_options:use_tls(Options),
    
    TransportOpts = case UseTls of
        true -> #{ssl => true};
        false -> #{}
    end,
    
    case grpcbox_channel:start_link(Endpoint, TransportOpts) of
        {ok, Channel} -> {ok, Channel};
        {error, Reason} -> {error, Reason}
    end.
