"""
Configuration options for the Grey SDK.
"""
module Options

export GreyOptions, local_options, production_options, base_url, with_timeout, with_headers

"""
Configuration options for the Grey SDK client.

# Fields
- `host::String`: Server hostname
- `port::Int`: Server port
- `use_tls::Bool`: Whether to use HTTPS
- `timeout::Int`: Request timeout in seconds
- `headers::Dict{String, String}`: Additional headers
"""
struct GreyOptions
    host::String
    port::Int
    use_tls::Bool
    timeout::Int
    headers::Dict{String, String}
    
    function GreyOptions(;
        host::String,
        port::Int=443,
        use_tls::Bool=true,
        timeout::Int=30,
        headers::Dict{String, String}=Dict{String, String}()
    )
        new(host, port, use_tls, timeout, headers)
    end
end

"""
    local_options(port::Int=8080) -> GreyOptions

Create options for local development.
"""
function local_options(port::Int=8080)::GreyOptions
    GreyOptions(
        host="localhost",
        port=port,
        use_tls=false,
        timeout=30
    )
end

"""
    production_options(host::String, port::Int=443) -> GreyOptions

Create options for production.
"""
function production_options(host::String, port::Int=443)::GreyOptions
    GreyOptions(
        host=host,
        port=port,
        use_tls=true,
        timeout=30
    )
end

"""
    base_url(options::GreyOptions) -> String

Get the base URL for HTTP requests.
"""
function base_url(options::GreyOptions)::String
    scheme = options.use_tls ? "https" : "http"
    "$(scheme)://$(options.host):$(options.port)"
end

"""
    with_timeout(options::GreyOptions, timeout::Int) -> GreyOptions

Create new options with a different timeout.
"""
function with_timeout(options::GreyOptions, timeout::Int)::GreyOptions
    GreyOptions(
        host=options.host,
        port=options.port,
        use_tls=options.use_tls,
        timeout=timeout,
        headers=copy(options.headers)
    )
end

"""
    with_headers(options::GreyOptions, new_headers::Dict) -> GreyOptions

Create new options with additional headers merged.
"""
function with_headers(options::GreyOptions, new_headers::Dict{String, String})::GreyOptions
    merged = merge(options.headers, new_headers)
    GreyOptions(
        host=options.host,
        port=options.port,
        use_tls=options.use_tls,
        timeout=options.timeout,
        headers=merged
    )
end

end # module
