"""
Authentication client for the Grey SDK.
"""
module AuthClient

export login, logout, refresh

using ..HttpClient
using ..GreyError
using ..Results

"""
    login(client::HttpClient.Client, username::String, password::String) -> Result

Authenticate with username and password.
Returns tokens on success.
"""
function login(client::HttpClient.Client, username::String, password::String)
    # Validate inputs
    if isempty(strip(username))
        return Results.err(GreyError.validation("Username is required"))
    end
    
    if isempty(strip(password))
        return Results.err(GreyError.validation("Password is required"))
    end
    
    result = HttpClient.post(client, "/auth/login", Dict(
        "username" => username,
        "password" => password
    ))
    
    if Results.is_ok(result) && result.data !== nothing
        token = get(result.data, "access_token", nothing)
        if token !== nothing
            HttpClient.set_auth_token!(client, token)
        end
    end
    
    result
end

"""
    logout(client::HttpClient.Client) -> Result

Log out the current user.
"""
function logout(client::HttpClient.Client)
    result = HttpClient.post(client, "/auth/logout")
    
    # Clear token regardless of result
    HttpClient.clear_auth_token!(client)
    
    result
end

"""
    refresh(client::HttpClient.Client, refresh_token::String) -> Result

Refresh the access token using a refresh token.
"""
function refresh(client::HttpClient.Client, refresh_token::String)
    # Validate inputs
    if isempty(strip(refresh_token))
        return Results.err(GreyError.validation("Refresh token is required"))
    end
    
    result = HttpClient.post(client, "/auth/refresh", Dict(
        "refresh_token" => refresh_token
    ))
    
    if Results.is_ok(result) && result.data !== nothing
        token = get(result.data, "access_token", nothing)
        if token !== nothing
            HttpClient.set_auth_token!(client, token)
        end
    end
    
    result
end

end # module
