"""
HTTP client for the Grey SDK using HTTP.jl.
"""
module HttpClient

export Client, get, post, put, patch, delete, set_auth_token!, clear_auth_token!

using HTTP
using JSON3
using ..Options
using ..GreyError
using ..Results

"""
HTTP client for making API requests.

# Fields
- `options::GreyOptions`: Configuration options
- `auth_token::Union{String, Nothing}`: Authentication token
"""
mutable struct Client
    options::Options.GreyOptions
    auth_token::Union{String, Nothing}
    
    function Client(options::Options.GreyOptions)
        new(options, nothing)
    end
end

"""Set the authentication token."""
function set_auth_token!(client::Client, token::String)
    client.auth_token = token
end

"""Clear the authentication token."""
function clear_auth_token!(client::Client)
    client.auth_token = nothing
end

"""Build request headers."""
function build_headers(client::Client, extra::Dict{String, String}=Dict{String, String}())::Vector{Pair{String, String}}
    headers = Pair{String, String}[]
    
    # Default headers
    push!(headers, "Content-Type" => "application/json")
    push!(headers, "Accept" => "application/json")
    
    # Add auth token if present
    if client.auth_token !== nothing
        push!(headers, "Authorization" => "Bearer $(client.auth_token)")
    end
    
    # Add options headers
    for (k, v) in client.options.headers
        push!(headers, k => v)
    end
    
    # Add extra headers
    for (k, v) in extra
        push!(headers, k => v)
    end
    
    headers
end

"""Build the full URL for a path."""
function build_url(client::Client, path::String)::String
    base = Options.base_url(client.options)
    normalized_path = startswith(path, "/") ? path : "/" * path
    base * normalized_path
end

"""Parse the response body."""
function parse_body(body::Vector{UInt8})::Union{Dict{String, Any}, Nothing}
    if isempty(body)
        return nothing
    end
    
    try
        JSON3.read(String(body), Dict{String, Any})
    catch
        nothing
    end
end

"""Execute an HTTP request."""
function execute(
    client::Client,
    method::Symbol,
    path::String;
    body::Union{Dict, Nothing}=nothing,
    headers::Dict{String, String}=Dict{String, String}(),
    query::Dict{String, String}=Dict{String, String}()
)
    url = build_url(client, path)
    req_headers = build_headers(client, headers)
    timeout = client.options.timeout
    
    try
        response = if body !== nothing
            json_body = JSON3.write(body)
            HTTP.request(
                String(method),
                url,
                req_headers,
                json_body;
                query=query,
                connect_timeout=timeout,
                readtimeout=timeout,
                status_exception=false
            )
        else
            HTTP.request(
                String(method),
                url,
                req_headers;
                query=query,
                connect_timeout=timeout,
                readtimeout=timeout,
                status_exception=false
            )
        end
        
        status = response.status
        parsed_body = parse_body(response.body)
        
        if 200 <= status < 300
            Results.ok(parsed_body)
        else
            Results.err(GreyError.from_response(status, parsed_body))
        end
        
    catch ex
        if ex isa HTTP.TimeoutError
            Results.err(GreyError.timeout("Request to $path timed out"))
        elseif ex isa HTTP.ConnectError
            Results.err(GreyError.network("Failed to connect to server"))
        else
            Results.err(GreyError.from_exception(ex))
        end
    end
end

"""
    get(client::Client, path::String; headers, query) -> Result

Make a GET request.
"""
function get(
    client::Client,
    path::String;
    headers::Dict{String, String}=Dict{String, String}(),
    query::Dict{String, String}=Dict{String, String}()
)
    execute(client, :GET, path; headers=headers, query=query)
end

"""
    post(client::Client, path::String, body; headers) -> Result

Make a POST request.
"""
function post(
    client::Client,
    path::String,
    body::Union{Dict, Nothing}=nothing;
    headers::Dict{String, String}=Dict{String, String}()
)
    execute(client, :POST, path; body=body, headers=headers)
end

"""
    put(client::Client, path::String, body; headers) -> Result

Make a PUT request.
"""
function put(
    client::Client,
    path::String,
    body::Union{Dict, Nothing}=nothing;
    headers::Dict{String, String}=Dict{String, String}()
)
    execute(client, :PUT, path; body=body, headers=headers)
end

"""
    patch(client::Client, path::String, body; headers) -> Result

Make a PATCH request.
"""
function patch(
    client::Client,
    path::String,
    body::Union{Dict, Nothing}=nothing;
    headers::Dict{String, String}=Dict{String, String}()
)
    execute(client, :PATCH, path; body=body, headers=headers)
end

"""
    delete(client::Client, path::String; headers) -> Result

Make a DELETE request.
"""
function delete(
    client::Client,
    path::String;
    headers::Dict{String, String}=Dict{String, String}()
)
    execute(client, :DELETE, path; headers=headers)
end

end # module
