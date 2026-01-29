%%%-------------------------------------------------------------------
%%% @doc Grey SDK Projects domain module.
%%%
%%% Provides project operations for the Grey SDK.
%%% @end
%%%-------------------------------------------------------------------
-module(grey_projects).

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

%% @doc Lists all projects for the authenticated user.
-spec list_projects(pid()) -> {ok, #projects_data{}} | {error, #grey_error{}}.
list_projects(Channel) ->
    list_projects(Channel, #{}).

%% @doc Lists projects with pagination options.
%% Options:
%%   - page: pos_integer() - Page number (default: 1)
%%   - page_size: pos_integer() - Items per page (default: 20)
%%   - filter: map() - Filter criteria
-spec list_projects(pid(), map()) -> {ok, #projects_data{}} | {error, #grey_error{}}.
list_projects(Channel, Opts) when is_map(Opts) ->
    case grey_channel:is_authenticated(Channel) of
        false ->
            {error, grey_error:unauthorized(<<"User not authenticated">>)};
        true ->
            case validate_list_opts(Opts) of
                ok ->
                    case grey_grpc_projects:list_projects(Channel, Opts) of
                        {ok, _ProjectsData} = Success ->
                            Success;
                        {error, Reason} ->
                            {error, grey_error:from_any(Reason)}
                    end;
                {error, _} = Error ->
                    Error
            end
    end;
list_projects(_Channel, _Opts) ->
    {error, grey_error:validation(<<"opts must be a map">>)}.

%% @doc Creates a new project with the given name.
-spec create_project(pid(), binary()) -> {ok, #grey_project{}} | {error, #grey_error{}}.
create_project(Channel, Name) ->
    create_project(Channel, Name, #{}).

%% @doc Creates a new project with name and options.
%% Options:
%%   - description: binary() - Project description
%%   - metadata: map() - Additional metadata
-spec create_project(pid(), binary(), map()) -> {ok, #grey_project{}} | {error, #grey_error{}}.
create_project(Channel, Name, Opts) when is_binary(Name), is_map(Opts) ->
    case grey_channel:is_authenticated(Channel) of
        false ->
            {error, grey_error:unauthorized(<<"User not authenticated">>)};
        true ->
            case validate_project_name(Name) of
                ok ->
                    case grey_grpc_projects:create_project(Channel, Name, Opts) of
                        {ok, _Project} = Success ->
                            Success;
                        {error, Reason} ->
                            {error, grey_error:from_any(Reason)}
                    end;
                {error, _} = Error ->
                    Error
            end
    end;
create_project(_Channel, Name, _Opts) when not is_binary(Name) ->
    {error, grey_error:validation(<<"name must be a binary">>)};
create_project(_Channel, _Name, _Opts) ->
    {error, grey_error:validation(<<"opts must be a map">>)}.

%%====================================================================
%% Internal functions
%%====================================================================

validate_list_opts(Opts) ->
    Page = maps:get(page, Opts, 1),
    PageSize = maps:get(page_size, Opts, 20),
    
    case is_integer(Page) andalso Page > 0 of
        false ->
            {error, grey_error:validation(<<"page must be a positive integer">>)};
        true ->
            case is_integer(PageSize) andalso PageSize > 0 andalso PageSize =< 100 of
                false ->
                    {error, grey_error:validation(<<"page_size must be between 1 and 100">>)};
                true ->
                    ok
            end
    end.

validate_project_name(Name) ->
    case byte_size(Name) of
        0 ->
            {error, grey_error:validation(<<"project name cannot be empty">>)};
        N when N > 255 ->
            {error, grey_error:validation(<<"project name cannot exceed 255 characters">>)};
        _ ->
            ok
    end.
