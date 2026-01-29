"""
Projects client for the Grey SDK.
"""
module ProjectsClient

export list_projects, get_project, create_project, update_project, delete_project

using ..HttpClient
using ..GreyError
using ..Results

"""
    list_projects(client::HttpClient.Client; page, per_page, sort_by, sort_order) -> Result

List all projects with optional pagination.
"""
function list_projects(
    client::HttpClient.Client;
    page::Int=1,
    per_page::Int=20,
    sort_by::String="created_at",
    sort_order::String="desc"
)
    # Validate inputs
    if page < 1
        return Results.err(GreyError.validation("Page must be >= 1"))
    end
    
    if per_page < 1 || per_page > 100
        return Results.err(GreyError.validation("Per page must be between 1 and 100"))
    end
    
    query = Dict(
        "page" => string(page),
        "per_page" => string(per_page),
        "sort_by" => sort_by,
        "sort_order" => sort_order
    )
    
    HttpClient.get(client, "/projects"; query=query)
end

"""
    get_project(client::HttpClient.Client, project_id::String) -> Result

Get a project by ID.
"""
function get_project(client::HttpClient.Client, project_id::String)
    # Validate inputs
    if isempty(strip(project_id))
        return Results.err(GreyError.validation("Project ID is required"))
    end
    
    HttpClient.get(client, "/projects/$(project_id)")
end

"""
    create_project(client::HttpClient.Client, name::String; description, metadata) -> Result

Create a new project.
"""
function create_project(
    client::HttpClient.Client,
    name::String;
    description::String="",
    metadata::Union{Dict, Nothing}=nothing
)
    # Validate inputs
    if isempty(strip(name))
        return Results.err(GreyError.validation("Project name is required"))
    end
    
    body = Dict{String, Any}(
        "name" => name
    )
    
    if !isempty(description)
        body["description"] = description
    end
    
    if metadata !== nothing
        body["metadata"] = metadata
    end
    
    HttpClient.post(client, "/projects", body)
end

"""
    update_project(client::HttpClient.Client, project_id::String; name, description, metadata) -> Result

Update an existing project.
"""
function update_project(
    client::HttpClient.Client,
    project_id::String;
    name::Union{String, Nothing}=nothing,
    description::Union{String, Nothing}=nothing,
    metadata::Union{Dict, Nothing}=nothing
)
    # Validate inputs
    if isempty(strip(project_id))
        return Results.err(GreyError.validation("Project ID is required"))
    end
    
    body = Dict{String, Any}()
    
    if name !== nothing
        body["name"] = name
    end
    
    if description !== nothing
        body["description"] = description
    end
    
    if metadata !== nothing
        body["metadata"] = metadata
    end
    
    if isempty(body)
        return Results.err(GreyError.validation("At least one field must be provided for update"))
    end
    
    HttpClient.patch(client, "/projects/$(project_id)", body)
end

"""
    delete_project(client::HttpClient.Client, project_id::String) -> Result

Delete a project by ID.
"""
function delete_project(client::HttpClient.Client, project_id::String)
    # Validate inputs
    if isempty(strip(project_id))
        return Results.err(GreyError.validation("Project ID is required"))
    end
    
    HttpClient.delete(client, "/projects/$(project_id)")
end

end # module
