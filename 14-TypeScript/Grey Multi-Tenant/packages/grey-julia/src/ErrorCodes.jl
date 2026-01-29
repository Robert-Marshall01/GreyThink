"""
Error codes for the Grey SDK.
"""
module ErrorCodes

export ErrorCode, 
       UNAUTHORIZED, FORBIDDEN, NOT_FOUND, VALIDATION_ERROR,
       NETWORK_ERROR, TIMEOUT, SERVER_ERROR, UNKNOWN,
       from_http_status, is_valid, normalize

"""Error code type"""
const ErrorCode = Symbol

# Error code constants
const UNAUTHORIZED = :unauthorized
const FORBIDDEN = :forbidden
const NOT_FOUND = :not_found
const VALIDATION_ERROR = :validation_error
const NETWORK_ERROR = :network_error
const TIMEOUT = :timeout
const SERVER_ERROR = :server_error
const UNKNOWN = :unknown

# All valid error codes
const VALID_CODES = Set([
    UNAUTHORIZED, FORBIDDEN, NOT_FOUND, VALIDATION_ERROR,
    NETWORK_ERROR, TIMEOUT, SERVER_ERROR, UNKNOWN
])

"""
    from_http_status(status::Int) -> ErrorCode

Convert an HTTP status code to an error code.
"""
function from_http_status(status::Int)::ErrorCode
    if status == 401
        return UNAUTHORIZED
    elseif status == 403
        return FORBIDDEN
    elseif status == 404
        return NOT_FOUND
    elseif status in (400, 422)
        return VALIDATION_ERROR
    elseif status in (408, 504)
        return TIMEOUT
    elseif 500 <= status < 600
        return SERVER_ERROR
    else
        return UNKNOWN
    end
end

"""
    is_valid(code::ErrorCode) -> Bool

Check if an error code is valid.
"""
function is_valid(code::ErrorCode)::Bool
    code in VALID_CODES
end

"""
    normalize(code::ErrorCode) -> ErrorCode

Normalize an error code to a valid value.
"""
function normalize(code::ErrorCode)::ErrorCode
    is_valid(code) ? code : UNKNOWN
end

end # module
