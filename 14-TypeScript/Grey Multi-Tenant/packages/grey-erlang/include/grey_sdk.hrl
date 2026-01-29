%% @doc Record definitions for Grey SDK.

%% Grey error record
-record(grey_error, {
    code :: binary(),
    message :: binary(),
    details :: map() | undefined
}).

%% Grey options record
-record(grey_options, {
    host :: binary(),
    port :: pos_integer(),
    use_tls :: boolean(),
    timeout_ms :: pos_integer(),
    metadata :: map()
}).

%% Auth data record
-record(auth_data, {
    access_token :: binary(),
    refresh_token :: binary(),
    expires_in :: non_neg_integer()
}).

%% User record
-record(grey_user, {
    id :: binary(),
    email :: binary(),
    name :: binary(),
    avatar :: binary() | undefined,
    metadata :: map() | undefined
}).

%% Project record
-record(grey_project, {
    id :: binary(),
    name :: binary(),
    description :: binary() | undefined,
    created_at :: binary() | undefined,
    updated_at :: binary() | undefined,
    metadata :: map() | undefined
}).

%% Projects data record
-record(projects_data, {
    projects :: [#grey_project{}],
    total :: non_neg_integer(),
    page :: non_neg_integer(),
    page_size :: non_neg_integer()
}).

%% Query data record
-record(query_data, {
    data :: term(),
    metadata :: map() | undefined
}).

%% Mutation data record
-record(mutation_data, {
    success :: boolean(),
    data :: term() | undefined,
    metadata :: map() | undefined
}).
