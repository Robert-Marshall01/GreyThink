%%%-------------------------------------------------------------------
%%% @doc Grey SDK configuration options module.
%%%
%%% Provides configuration options for the Grey SDK client.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_options).

-include("grey_sdk.hrl").

%% API exports
-export([
    new/3,
    new/4,
    new/5,
    local/0,
    local/1,
    production/1,
    host/1,
    port/1,
    use_tls/1,
    timeout_ms/1,
    metadata/1,
    endpoint/1,
    to_map/1
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Creates new options with required parameters.
-spec new(binary(), pos_integer(), boolean()) -> #grey_options{}.
new(Host, Port, UseTls) ->
    new(Host, Port, UseTls, 30000).

%% @doc Creates new options with timeout.
-spec new(binary(), pos_integer(), boolean(), pos_integer()) -> #grey_options{}.
new(Host, Port, UseTls, TimeoutMs) ->
    new(Host, Port, UseTls, TimeoutMs, #{}).

%% @doc Creates new options with all parameters.
-spec new(binary(), pos_integer(), boolean(), pos_integer(), map()) -> #grey_options{}.
new(Host, Port, UseTls, TimeoutMs, Metadata) ->
    validate_host(Host),
    validate_port(Port),
    validate_timeout(TimeoutMs),
    #grey_options{
        host = Host,
        port = Port,
        use_tls = UseTls,
        timeout_ms = TimeoutMs,
        metadata = Metadata
    }.

%% @doc Creates options for local development.
-spec local() -> #grey_options{}.
local() ->
    local(50051).

-spec local(pos_integer()) -> #grey_options{}.
local(Port) ->
    new(<<"localhost">>, Port, false).

%% @doc Creates options for production.
-spec production(binary()) -> #grey_options{}.
production(Host) ->
    new(Host, 443, true).

%% @doc Gets the host.
-spec host(#grey_options{}) -> binary().
host(#grey_options{host = Host}) ->
    Host.

%% @doc Gets the port.
-spec port(#grey_options{}) -> pos_integer().
port(#grey_options{port = Port}) ->
    Port.

%% @doc Gets the TLS setting.
-spec use_tls(#grey_options{}) -> boolean().
use_tls(#grey_options{use_tls = UseTls}) ->
    UseTls.

%% @doc Gets the timeout in milliseconds.
-spec timeout_ms(#grey_options{}) -> pos_integer().
timeout_ms(#grey_options{timeout_ms = TimeoutMs}) ->
    TimeoutMs.

%% @doc Gets the metadata.
-spec metadata(#grey_options{}) -> map().
metadata(#grey_options{metadata = Metadata}) ->
    Metadata.

%% @doc Gets the full endpoint string.
-spec endpoint(#grey_options{}) -> binary().
endpoint(#grey_options{host = Host, port = Port}) ->
    list_to_binary(io_lib:format("~s:~p", [Host, Port])).

%% @doc Converts options to a map.
-spec to_map(#grey_options{}) -> map().
to_map(#grey_options{host = Host, port = Port, use_tls = UseTls,
                     timeout_ms = TimeoutMs, metadata = Metadata}) ->
    #{
        host => Host,
        port => Port,
        use_tls => UseTls,
        timeout_ms => TimeoutMs,
        metadata => Metadata
    }.

%%====================================================================
%% Internal functions
%%====================================================================

validate_host(Host) when is_binary(Host), byte_size(Host) > 0 ->
    ok;
validate_host(_) ->
    error(badarg).

validate_port(Port) when is_integer(Port), Port > 0, Port =< 65535 ->
    ok;
validate_port(_) ->
    error(badarg).

validate_timeout(Timeout) when is_integer(Timeout), Timeout > 0 ->
    ok;
validate_timeout(_) ->
    error(badarg).
