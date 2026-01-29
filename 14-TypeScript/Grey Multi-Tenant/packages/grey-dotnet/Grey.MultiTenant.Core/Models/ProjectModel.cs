using System.Text.Json.Serialization;

namespace Grey.MultiTenant.Core.Models;

/// <summary>
/// Represents a project in the Grey Multi-Tenant system.
/// </summary>
public sealed record ProjectModel
{
    [JsonPropertyName("id")]
    public required string Id { get; init; }

    [JsonPropertyName("name")]
    public required string Name { get; init; }

    [JsonPropertyName("description")]
    public string? Description { get; init; }

    [JsonPropertyName("tenantId")]
    public required string TenantId { get; init; }

    [JsonPropertyName("ownerId")]
    public required string OwnerId { get; init; }

    [JsonPropertyName("status")]
    public string? Status { get; init; }

    [JsonPropertyName("metadata")]
    public Dictionary<string, object>? Metadata { get; init; }

    [JsonPropertyName("createdAt")]
    public DateTimeOffset? CreatedAt { get; init; }

    [JsonPropertyName("updatedAt")]
    public DateTimeOffset? UpdatedAt { get; init; }
}
