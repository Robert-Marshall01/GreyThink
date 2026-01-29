"""
Result type for Grey SDK operations.
"""
module Results

export Result, is_ok, is_err, unwrap, unwrap_or, map_result, then_result, catch_result

using ..GreyError

"""
Represents the result of an operation that can succeed or fail.

# Fields
- `ok::Bool`: Whether the result is successful
- `data::Any`: The success data (nothing if error)
- `error::Union{GreyError.Error, Nothing}`: The error (nothing if success)
"""
struct Result{T}
    ok::Bool
    data::Union{T, Nothing}
    error::Union{GreyError.Error, Nothing}
    
    function Result{T}(; ok::Bool, data::Union{T, Nothing}=nothing, error::Union{GreyError.Error, Nothing}=nothing) where T
        new{T}(ok, data, error)
    end
end

# Convenience constructor
function Result(; ok::Bool, data=nothing, error=nothing)
    Result{typeof(data)}(ok=ok, data=data, error=error)
end

"""Create a successful result."""
function ok(data::T) where T
    Result{T}(ok=true, data=data)
end

"""Create a failed result."""
function err(error::GreyError.Error)
    Result{Nothing}(ok=false, error=error)
end

"""Create a failed result from a string."""
function err(message::String)
    Result{Nothing}(ok=false, error=GreyError.unknown(message))
end

"""Check if a result is successful."""
function is_ok(result::Result)::Bool
    result.ok
end

"""Check if a result is an error."""
function is_err(result::Result)::Bool
    !result.ok
end

"""
    unwrap(result::Result) -> T

Get the data or throw an error.
"""
function unwrap(result::Result)
    if is_err(result)
        throw(ErrorException("Unwrap failed: $(result.error.message)"))
    end
    result.data
end

"""
    unwrap_or(result::Result, default) -> T

Get the data or return the default value.
"""
function unwrap_or(result::Result{T}, default::T) where T
    is_ok(result) ? result.data : default
end

function unwrap_or(result::Result, default)
    is_ok(result) ? result.data : default
end

"""
    map_result(result::Result, fn::Function) -> Result

Map the data if successful.
"""
function map_result(result::Result, fn::Function)
    if is_err(result)
        return result
    end
    
    try
        ok(fn(result.data))
    catch ex
        err(GreyError.from_exception(ex))
    end
end

"""
    then_result(result::Result, fn::Function) -> Result

Chain another operation if successful.
"""
function then_result(result::Result, fn::Function)
    if is_err(result)
        return result
    end
    
    try
        res = fn(result.data)
        if res isa Result
            res
        else
            ok(res)
        end
    catch ex
        err(GreyError.from_exception(ex))
    end
end

"""
    catch_result(result::Result, fn::Function) -> Result

Handle error if failed.
"""
function catch_result(result::Result, fn::Function)
    if is_ok(result)
        return result
    end
    
    try
        res = fn(result.error)
        if res isa Result
            res
        else
            ok(res)
        end
    catch ex
        err(GreyError.from_exception(ex))
    end
end

# Make Result iterable for pattern matching
Base.iterate(r::Result) = is_ok(r) ? (r.data, nothing) : nothing
Base.iterate(r::Result, state) = nothing

end # module
