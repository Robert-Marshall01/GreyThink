using System.Text.Json.Serialization;

namespace Grey.MultiTenant.Core.Models;

/// <summary>
/// Represents an API error response.
/// </summary>
public sealed record ErrorModel
{
    [JsonPropertyName("code")]
    public string? Code { get; init; }

    [JsonPropertyName("message")]
    public required string Message { get; init; }

    [JsonPropertyName("details")]
    public string? Details { get; init; }

    [JsonPropertyName("field")]
    public string? Field { get; init; }

    [JsonPropertyName("statusCode")]
    public int? StatusCode { get; init; }

    [JsonPropertyName("timestamp")]
    public DateTimeOffset? Timestamp { get; init; }

    [JsonPropertyName("traceId")]
    public string? TraceId { get; init; }
}

/// <summary>
/// Represents validation errors for a specific field.
/// </summary>
public sealed record ValidationError
{
    [JsonPropertyName("field")]
    public required string Field { get; init; }

    [JsonPropertyName("message")]
    public required string Message { get; init; }

    [JsonPropertyName("code")]
    public string? Code { get; init; }
}

/// <summary>
/// Represents a collection of validation errors.
/// </summary>
public sealed record ValidationErrorResponse
{
    [JsonPropertyName("message")]
    public string Message { get; init; } = "Validation failed";

    [JsonPropertyName("errors")]
    public IReadOnlyList<ValidationError> Errors { get; init; } = Array.Empty<ValidationError>();
}
