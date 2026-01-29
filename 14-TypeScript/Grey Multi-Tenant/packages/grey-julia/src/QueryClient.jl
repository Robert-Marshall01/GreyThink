"""
GraphQL-style query client for the Grey SDK.
"""
module QueryClient

export query, batch_query

using ..HttpClient
using ..GreyError
using ..Results

"""
    query(client::HttpClient.Client, query_string::String; variables, operation_name) -> Result

Execute a single query.
"""
function query(
    client::HttpClient.Client,
    query_string::String;
    variables::Union{Dict, Nothing}=nothing,
    operation_name::Union{String, Nothing}=nothing
)
    # Validate inputs
    if isempty(strip(query_string))
        return Results.err(GreyError.validation("Query string is required"))
    end
    
    body = Dict{String, Any}(
        "query" => query_string
    )
    
    if variables !== nothing
        body["variables"] = variables
    end
    
    if operation_name !== nothing
        body["operationName"] = operation_name
    end
    
    HttpClient.post(client, "/graphql", body)
end

"""
    batch_query(client::HttpClient.Client, queries::Vector) -> Result

Execute multiple queries in a single request.
Each query should be a Dict with "query" and optionally "variables" and "operationName".
"""
function batch_query(client::HttpClient.Client, queries::Vector)
    # Validate inputs
    if isempty(queries)
        return Results.err(GreyError.validation("At least one query is required"))
    end
    
    # Validate each query
    for (i, q) in enumerate(queries)
        if !haskey(q, "query") || isempty(strip(get(q, "query", "")))
            return Results.err(GreyError.validation("Query $i must have a non-empty 'query' field"))
        end
    end
    
    HttpClient.post(client, "/graphql/batch", Dict("queries" => queries))
end

end # module
