"""
User client for the Grey SDK.
"""
module UserClient

export get_user, get_current_user

using ..HttpClient
using ..GreyError
using ..Results

"""
    get_user(client::HttpClient.Client, user_id::String) -> Result

Get a user by ID.
"""
function get_user(client::HttpClient.Client, user_id::String)
    # Validate inputs
    if isempty(strip(user_id))
        return Results.err(GreyError.validation("User ID is required"))
    end
    
    HttpClient.get(client, "/users/$(user_id)")
end

"""
    get_current_user(client::HttpClient.Client) -> Result

Get the current authenticated user.
"""
function get_current_user(client::HttpClient.Client)
    HttpClient.get(client, "/users/me")
end

end # module
