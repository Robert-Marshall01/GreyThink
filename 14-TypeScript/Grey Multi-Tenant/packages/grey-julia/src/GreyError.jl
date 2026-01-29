"""
Normalized error type for the Grey SDK.
"""
module GreyError

export Error, 
       unauthorized, forbidden, not_found, validation, 
       network, timeout, server, unknown,
       from_response, from_exception

using ..ErrorCodes

"""
Represents a normalized error in the Grey SDK.

# Fields
- `code::Symbol`: Error code
- `message::String`: Error message
- `details::Union{Dict, Nothing}`: Additional error details
"""
struct Error
    code::Symbol
    message::String
    details::Union{Dict{String, Any}, Nothing}
    
    function Error(code::Symbol, message::String, details::Union{Dict, Nothing}=nothing)
        new(ErrorCodes.normalize(code), message, details)
    end
end

"""Create an unauthorized error."""
function unauthorized(message::String="Authentication required")::Error
    Error(ErrorCodes.UNAUTHORIZED, message)
end

"""Create a forbidden error."""
function forbidden(message::String="Permission denied")::Error
    Error(ErrorCodes.FORBIDDEN, message)
end

"""Create a not found error."""
function not_found(message::String="Resource not found")::Error
    Error(ErrorCodes.NOT_FOUND, message)
end

"""Create a validation error."""
function validation(message::String, details::Union{Dict, Nothing}=nothing)::Error
    Error(ErrorCodes.VALIDATION_ERROR, message, details)
end

"""Create a network error."""
function network(message::String="Network error occurred")::Error
    Error(ErrorCodes.NETWORK_ERROR, message)
end

"""Create a timeout error."""
function timeout(message::String="Request timed out")::Error
    Error(ErrorCodes.TIMEOUT, message)
end

"""Create a server error."""
function server(message::String="Server error occurred")::Error
    Error(ErrorCodes.SERVER_ERROR, message)
end

"""Create an unknown error."""
function unknown(message::String="An unknown error occurred")::Error
    Error(ErrorCodes.UNKNOWN, message)
end

"""
    from_response(status::Int, body::Dict) -> Error

Create an error from an HTTP response.
"""
function from_response(status::Int, body::Union{Dict, Nothing}=nothing)::Error
    code = ErrorCodes.from_http_status(status)
    message = if body !== nothing
        get(body, "message", get(body, "error", "HTTP error $status"))
    else
        "HTTP error $status"
    end
    details = body !== nothing ? get(body, "details", nothing) : nothing
    
    Error(code, message, details)
end

"""
    from_exception(ex::Exception) -> Error

Create an error from an exception.
"""
function from_exception(ex::Exception)::Error
    msg = string(ex)
    
    if occursin(r"timeout"i, msg)
        timeout(msg)
    elseif occursin(r"connect|connection|network"i, msg)
        network(msg)
    else
        unknown(msg)
    end
end

end # module
