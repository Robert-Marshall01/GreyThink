using System.Text.Json.Serialization;

namespace Grey.MultiTenant.Core.Models;

/// <summary>
/// Represents authentication tokens.
/// </summary>
public sealed record TokenModel
{
    [JsonPropertyName("accessToken")]
    public required string AccessToken { get; init; }

    [JsonPropertyName("refreshToken")]
    public string? RefreshToken { get; init; }

    [JsonPropertyName("tokenType")]
    public string TokenType { get; init; } = "Bearer";

    [JsonPropertyName("expiresIn")]
    public int? ExpiresIn { get; init; }

    [JsonPropertyName("expiresAt")]
    public DateTimeOffset? ExpiresAt { get; init; }

    [JsonPropertyName("scope")]
    public string? Scope { get; init; }
}

/// <summary>
/// Represents token claims extracted from a JWT.
/// </summary>
public sealed record TokenClaims
{
    public string? Subject { get; init; }
    public string? Email { get; init; }
    public string? TenantId { get; init; }
    public string? Role { get; init; }
    public DateTimeOffset? IssuedAt { get; init; }
    public DateTimeOffset? ExpiresAt { get; init; }
    public Dictionary<string, object>? CustomClaims { get; init; }
}
