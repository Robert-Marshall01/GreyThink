"""
GraphQL-style mutation client for the Grey SDK.
"""
module MutationClient

export mutate, batch_mutate

using ..HttpClient
using ..GreyError
using ..Results

"""
    mutate(client::HttpClient.Client, mutation_string::String; variables, operation_name) -> Result

Execute a single mutation.
"""
function mutate(
    client::HttpClient.Client,
    mutation_string::String;
    variables::Union{Dict, Nothing}=nothing,
    operation_name::Union{String, Nothing}=nothing
)
    # Validate inputs
    if isempty(strip(mutation_string))
        return Results.err(GreyError.validation("Mutation string is required"))
    end
    
    body = Dict{String, Any}(
        "mutation" => mutation_string
    )
    
    if variables !== nothing
        body["variables"] = variables
    end
    
    if operation_name !== nothing
        body["operationName"] = operation_name
    end
    
    HttpClient.post(client, "/graphql/mutate", body)
end

"""
    batch_mutate(client::HttpClient.Client, mutations::Vector) -> Result

Execute multiple mutations in a single request.
Each mutation should be a Dict with "mutation" and optionally "variables" and "operationName".
"""
function batch_mutate(client::HttpClient.Client, mutations::Vector)
    # Validate inputs
    if isempty(mutations)
        return Results.err(GreyError.validation("At least one mutation is required"))
    end
    
    # Validate each mutation
    for (i, m) in enumerate(mutations)
        if !haskey(m, "mutation") || isempty(strip(get(m, "mutation", "")))
            return Results.err(GreyError.validation("Mutation $i must have a non-empty 'mutation' field"))
        end
    end
    
    HttpClient.post(client, "/graphql/mutate/batch", Dict("mutations" => mutations))
end

end # module
