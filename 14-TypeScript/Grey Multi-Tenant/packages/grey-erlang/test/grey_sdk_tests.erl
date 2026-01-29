%%%-------------------------------------------------------------------
%%% @doc Grey SDK basic tests.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_sdk_tests).

-include_lib("eunit/include/eunit.hrl").
-include("grey_sdk.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% grey_options tests
%%====================================================================

grey_options_test_() ->
    {"grey_options tests", [
        {"new/3 creates options with defaults", fun() ->
            Opts = grey_options:new(<<"localhost">>, 8080, false),
            ?assertEqual(<<"localhost">>, grey_options:host(Opts)),
            ?assertEqual(8080, grey_options:port(Opts)),
            ?assertEqual(false, grey_options:use_tls(Opts)),
            ?assertEqual(30000, grey_options:timeout_ms(Opts))
        end},
        
        {"new/4 sets timeout", fun() ->
            Opts = grey_options:new(<<"localhost">>, 8080, true, 5000),
            ?assertEqual(true, grey_options:use_tls(Opts)),
            ?assertEqual(5000, grey_options:timeout_ms(Opts))
        end},
        
        {"local/1 creates local options", fun() ->
            Opts = grey_options:local(9000),
            ?assertEqual(<<"localhost">>, grey_options:host(Opts)),
            ?assertEqual(9000, grey_options:port(Opts)),
            ?assertEqual(false, grey_options:use_tls(Opts))
        end},
        
        {"production/1 creates production options", fun() ->
            Opts = grey_options:production(<<"api.grey.com">>),
            ?assertEqual(<<"api.grey.com">>, grey_options:host(Opts)),
            ?assertEqual(443, grey_options:port(Opts)),
            ?assertEqual(true, grey_options:use_tls(Opts))
        end},
        
        {"endpoint/1 formats correctly", fun() ->
            Opts = grey_options:new(<<"example.com">>, 8080, false),
            ?assertEqual(<<"example.com:8080">>, grey_options:endpoint(Opts))
        end},
        
        {"to_map/1 converts to map", fun() ->
            Opts = grey_options:new(<<"localhost">>, 8080, false),
            Map = grey_options:to_map(Opts),
            ?assertEqual(<<"localhost">>, maps:get(host, Map)),
            ?assertEqual(8080, maps:get(port, Map))
        end}
    ]}.

%%====================================================================
%% grey_error tests
%%====================================================================

grey_error_test_() ->
    {"grey_error tests", [
        {"new/2 creates error", fun() ->
            Err = grey_error:new(not_found, <<"Resource not found">>),
            ?assertEqual(not_found, grey_error:code(Err)),
            ?assertEqual(<<"Resource not found">>, grey_error:message(Err)),
            ?assertEqual(undefined, grey_error:details(Err))
        end},
        
        {"new/3 creates error with details", fun() ->
            Details = #{field => <<"id">>},
            Err = grey_error:new(validation_error, <<"Invalid input">>, Details),
            ?assertEqual(validation_error, grey_error:code(Err)),
            ?assertEqual(Details, grey_error:details(Err))
        end},
        
        {"unauthorized/0 creates unauthorized error", fun() ->
            Err = grey_error:unauthorized(),
            ?assertEqual(unauthorized, grey_error:code(Err))
        end},
        
        {"validation/1 creates validation error", fun() ->
            Err = grey_error:validation(<<"Email is required">>),
            ?assertEqual(validation_error, grey_error:code(Err)),
            ?assertEqual(<<"Email is required">>, grey_error:message(Err))
        end},
        
        {"to_map/1 converts to map", fun() ->
            Err = grey_error:new(timeout, <<"Request timed out">>),
            Map = grey_error:to_map(Err),
            ?assertEqual(timeout, maps:get(code, Map)),
            ?assertEqual(<<"Request timed out">>, maps:get(message, Map))
        end},
        
        {"from_any/1 with grey_error passthrough", fun() ->
            Original = grey_error:new(forbidden, <<"Access denied">>),
            Result = grey_error:from_any(Original),
            ?assertEqual(forbidden, grey_error:code(Result))
        end},
        
        {"from_any/1 with atom", fun() ->
            Result = grey_error:from_any(timeout),
            ?assertEqual(timeout, grey_error:code(Result))
        end},
        
        {"from_any/1 with binary", fun() ->
            Result = grey_error:from_any(<<"Something went wrong">>),
            ?assertEqual(unknown, grey_error:code(Result)),
            ?assertEqual(<<"Something went wrong">>, grey_error:message(Result))
        end}
    ]}.

%%====================================================================
%% Integration placeholder tests
%%====================================================================

integration_placeholder_test_() ->
    {"Integration tests (placeholders)", [
        {"client lifecycle", fun() ->
            %% This would require a running server
            %% For now, just verify modules are loadable
            ?assert(is_list(grey_client:module_info()))
        end},
        
        {"channel module exists", fun() ->
            ?assert(is_list(grey_channel:module_info()))
        end},
        
        {"domain modules exist", fun() ->
            ?assert(is_list(grey_auth:module_info())),
            ?assert(is_list(grey_user:module_info())),
            ?assert(is_list(grey_projects:module_info())),
            ?assert(is_list(grey_query:module_info())),
            ?assert(is_list(grey_mutation:module_info()))
        end}
    ]}.
