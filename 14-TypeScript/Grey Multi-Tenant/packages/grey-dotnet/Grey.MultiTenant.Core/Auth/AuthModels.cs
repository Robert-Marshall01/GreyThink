using System.Text.Json.Serialization;

namespace Grey.MultiTenant.Core.Auth;

/// <summary>
/// Request model for user login.
/// </summary>
public sealed record LoginRequest
{
    [JsonPropertyName("email")]
    public required string Email { get; init; }

    [JsonPropertyName("password")]
    public required string Password { get; init; }

    [JsonPropertyName("tenantId")]
    public string? TenantId { get; init; }

    [JsonPropertyName("rememberMe")]
    public bool RememberMe { get; init; }
}

/// <summary>
/// Result model for successful login.
/// </summary>
public sealed record LoginResult
{
    [JsonPropertyName("user")]
    public required Models.UserModel User { get; init; }

    [JsonPropertyName("tokens")]
    public required Models.TokenModel Tokens { get; init; }

    [JsonPropertyName("expiresAt")]
    public DateTimeOffset? ExpiresAt { get; init; }
}

/// <summary>
/// Request model for user registration.
/// </summary>
public sealed record RegisterRequest
{
    [JsonPropertyName("email")]
    public required string Email { get; init; }

    [JsonPropertyName("password")]
    public required string Password { get; init; }

    [JsonPropertyName("name")]
    public string? Name { get; init; }

    [JsonPropertyName("tenantId")]
    public string? TenantId { get; init; }

    [JsonPropertyName("metadata")]
    public Dictionary<string, object>? Metadata { get; init; }
}

/// <summary>
/// Result model for token refresh.
/// </summary>
public sealed record RefreshResult
{
    [JsonPropertyName("tokens")]
    public required Models.TokenModel Tokens { get; init; }

    [JsonPropertyName("expiresAt")]
    public DateTimeOffset? ExpiresAt { get; init; }
}

/// <summary>
/// Represents the current authentication state.
/// </summary>
public sealed record AuthState
{
    public bool IsAuthenticated { get; init; }
    public Models.UserModel? User { get; init; }
    public Models.TokenModel? Tokens { get; init; }
    public bool IsLoading { get; init; }
    public string? Error { get; init; }
}
