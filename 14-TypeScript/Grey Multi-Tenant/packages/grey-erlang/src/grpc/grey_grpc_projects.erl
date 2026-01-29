%%%-------------------------------------------------------------------
%%% @doc Grey SDK Projects gRPC service stub.
%%%
%%% This is a placeholder stub. In a real implementation, this would
%%% use generated protobuf client stubs from .proto files.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_grpc_projects).

-include("grey_sdk.hrl").

%% API exports
-export([
    list_projects/1,
    list_projects/2,
    create_project/2,
    create_project/3
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Lists projects via gRPC.
-spec list_projects(pid()) -> {ok, #projects_data{}} | {error, term()}.
list_projects(Channel) ->
    list_projects(Channel, #{}).

-spec list_projects(pid(), map()) -> {ok, #projects_data{}} | {error, term()}.
list_projects(Channel, Opts) ->
    Page = maps:get(page, Opts, 1),
    PageSize = maps:get(page_size, Opts, 20),
    
    simulate_authenticated_grpc_call(Channel, "projects.ListProjects", fun() ->
        #projects_data{
            projects = [
                #grey_project{
                    id = <<"project_1">>,
                    name = <<"Stub Project 1">>,
                    description = <<"A stub project for testing">>
                },
                #grey_project{
                    id = <<"project_2">>,
                    name = <<"Stub Project 2">>,
                    description = undefined
                }
            ],
            total = 2,
            page = Page,
            page_size = PageSize
        }
    end).

%% @doc Creates a project via gRPC.
-spec create_project(pid(), binary()) -> {ok, #grey_project{}} | {error, term()}.
create_project(Channel, Name) ->
    create_project(Channel, Name, #{}).

-spec create_project(pid(), binary(), map()) -> {ok, #grey_project{}} | {error, term()}.
create_project(Channel, Name, Opts) ->
    Description = maps:get(description, Opts, undefined),
    
    simulate_authenticated_grpc_call(Channel, "projects.CreateProject", fun() ->
        #grey_project{
            id = <<"new_project_id">>,
            name = Name,
            description = Description,
            created_at = iso8601_now(),
            updated_at = undefined,
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

iso8601_now() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                  [Y, M, D, H, Mi, S])).
