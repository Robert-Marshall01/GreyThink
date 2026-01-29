using System.Text.Json.Serialization;

namespace Grey.MultiTenant.Core.User;

/// <summary>
/// Request model for updating a user.
/// </summary>
public sealed record UpdateUserRequest
{
    [JsonPropertyName("name")]
    public string? Name { get; init; }

    [JsonPropertyName("avatar")]
    public string? Avatar { get; init; }

    [JsonPropertyName("metadata")]
    public Dictionary<string, object>? Metadata { get; init; }
}

/// <summary>
/// Request model for fetching a user by ID.
/// </summary>
public sealed record GetUserRequest
{
    [JsonPropertyName("userId")]
    public required string UserId { get; init; }
}

/// <summary>
/// Represents the current user state.
/// </summary>
public sealed record UserState
{
    public Models.UserModel? User { get; init; }
    public bool IsLoading { get; init; }
    public string? Error { get; init; }
}
