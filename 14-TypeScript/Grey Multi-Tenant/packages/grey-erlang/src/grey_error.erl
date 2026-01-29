%%%-------------------------------------------------------------------
%%% @doc Grey SDK error handling module.
%%%
%%% Provides normalized error structures and utilities for all Grey SDK
%%% operations.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_error).

-include("grey_sdk.hrl").

%% API exports
-export([
    new/2,
    new/3,
    unauthorized/0,
    unauthorized/1,
    validation/1,
    not_found/0,
    not_found/1,
    network/0,
    network/1,
    timeout/0,
    timeout/1,
    server_error/0,
    server_error/1,
    unknown/0,
    unknown/1,
    from_grpc_error/1,
    from_any/1,
    code/1,
    message/1,
    details/1,
    to_map/1
]).

%% Error code constants
-define(UNAUTHORIZED, <<"UNAUTHORIZED">>).
-define(FORBIDDEN, <<"FORBIDDEN">>).
-define(NOT_FOUND, <<"NOT_FOUND">>).
-define(VALIDATION_ERROR, <<"VALIDATION_ERROR">>).
-define(NETWORK_ERROR, <<"NETWORK_ERROR">>).
-define(TIMEOUT, <<"TIMEOUT">>).
-define(SERVER_ERROR, <<"SERVER_ERROR">>).
-define(UNKNOWN, <<"UNKNOWN">>).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Creates a new error with code and message.
-spec new(binary(), binary()) -> #grey_error{}.
new(Code, Message) ->
    #grey_error{code = Code, message = Message, details = undefined}.

%% @doc Creates a new error with code, message, and details.
-spec new(binary(), binary(), map()) -> #grey_error{}.
new(Code, Message, Details) ->
    #grey_error{code = Code, message = Message, details = Details}.

%% @doc Creates an unauthorized error.
-spec unauthorized() -> #grey_error{}.
unauthorized() ->
    unauthorized(<<"Not authenticated">>).

-spec unauthorized(binary()) -> #grey_error{}.
unauthorized(Message) ->
    new(?UNAUTHORIZED, Message).

%% @doc Creates a validation error.
-spec validation(binary()) -> #grey_error{}.
validation(Message) ->
    new(?VALIDATION_ERROR, Message).

%% @doc Creates a not found error.
-spec not_found() -> #grey_error{}.
not_found() ->
    not_found(<<"Resource not found">>).

-spec not_found(binary()) -> #grey_error{}.
not_found(Message) ->
    new(?NOT_FOUND, Message).

%% @doc Creates a network error.
-spec network() -> #grey_error{}.
network() ->
    network(<<"Network error">>).

-spec network(binary()) -> #grey_error{}.
network(Message) ->
    new(?NETWORK_ERROR, Message).

%% @doc Creates a timeout error.
-spec timeout() -> #grey_error{}.
timeout() ->
    timeout(<<"Request timed out">>).

-spec timeout(binary()) -> #grey_error{}.
timeout(Message) ->
    new(?TIMEOUT, Message).

%% @doc Creates a server error.
-spec server_error() -> #grey_error{}.
server_error() ->
    server_error(<<"Internal server error">>).

-spec server_error(binary()) -> #grey_error{}.
server_error(Message) ->
    new(?SERVER_ERROR, Message).

%% @doc Creates an unknown error.
-spec unknown() -> #grey_error{}.
unknown() ->
    unknown(<<"Unknown error">>).

-spec unknown(binary()) -> #grey_error{}.
unknown(Message) ->
    new(?UNKNOWN, Message).

%% @doc Creates a GreyError from a gRPC error.
-spec from_grpc_error(term()) -> #grey_error{}.
from_grpc_error({grpc_error, Status, Message}) ->
    Code = grpc_status_to_code(Status),
    new(Code, to_binary(Message), #{grpc_status => Status});
from_grpc_error({error, {grpc_error, Status, Message}}) ->
    from_grpc_error({grpc_error, Status, Message});
from_grpc_error(Other) ->
    from_any(Other).

%% @doc Creates a GreyError from any term.
-spec from_any(term()) -> #grey_error{}.
from_any(#grey_error{} = Error) ->
    Error;
from_any({grpc_error, _, _} = Error) ->
    from_grpc_error(Error);
from_any(Error) when is_binary(Error) ->
    unknown(Error);
from_any(Error) when is_list(Error) ->
    unknown(list_to_binary(Error));
from_any(Error) when is_atom(Error) ->
    unknown(atom_to_binary(Error, utf8));
from_any(Error) ->
    unknown(list_to_binary(io_lib:format("~p", [Error]))).

%% @doc Gets the error code.
-spec code(#grey_error{}) -> binary().
code(#grey_error{code = Code}) ->
    Code.

%% @doc Gets the error message.
-spec message(#grey_error{}) -> binary().
message(#grey_error{message = Message}) ->
    Message.

%% @doc Gets the error details.
-spec details(#grey_error{}) -> map() | undefined.
details(#grey_error{details = Details}) ->
    Details.

%% @doc Converts error to a map.
-spec to_map(#grey_error{}) -> map().
to_map(#grey_error{code = Code, message = Message, details = Details}) ->
    Map = #{code => Code, message => Message},
    case Details of
        undefined -> Map;
        _ -> Map#{details => Details}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% Maps gRPC status codes to Grey error codes.
grpc_status_to_code(unauthenticated) -> ?UNAUTHORIZED;
grpc_status_to_code(permission_denied) -> ?FORBIDDEN;
grpc_status_to_code(not_found) -> ?NOT_FOUND;
grpc_status_to_code(invalid_argument) -> ?VALIDATION_ERROR;
grpc_status_to_code(failed_precondition) -> ?VALIDATION_ERROR;
grpc_status_to_code(unavailable) -> ?NETWORK_ERROR;
grpc_status_to_code(deadline_exceeded) -> ?TIMEOUT;
grpc_status_to_code(internal) -> ?SERVER_ERROR;
grpc_status_to_code(unknown) -> ?UNKNOWN;
grpc_status_to_code(_) -> ?UNKNOWN.

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) -> list_to_binary(io_lib:format("~p", [V])).
