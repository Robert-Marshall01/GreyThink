using System.Text.Json.Serialization;

namespace Grey.MultiTenant.Core.Models;

/// <summary>
/// Represents a user in the Grey Multi-Tenant system.
/// </summary>
public sealed record UserModel
{
    [JsonPropertyName("id")]
    public required string Id { get; init; }

    [JsonPropertyName("email")]
    public required string Email { get; init; }

    [JsonPropertyName("name")]
    public string? Name { get; init; }

    [JsonPropertyName("avatar")]
    public string? Avatar { get; init; }

    [JsonPropertyName("role")]
    public string? Role { get; init; }

    [JsonPropertyName("tenantId")]
    public string? TenantId { get; init; }

    [JsonPropertyName("metadata")]
    public Dictionary<string, object>? Metadata { get; init; }

    [JsonPropertyName("createdAt")]
    public DateTimeOffset? CreatedAt { get; init; }

    [JsonPropertyName("updatedAt")]
    public DateTimeOffset? UpdatedAt { get; init; }
}
