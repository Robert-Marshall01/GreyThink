using System.Text.Json.Serialization;

namespace Grey.MultiTenant.Core.Mutation;

/// <summary>
/// Request model for executing a mutation.
/// </summary>
public sealed record MutationRequest
{
    [JsonPropertyName("endpoint")]
    public required string Endpoint { get; init; }

    [JsonPropertyName("method")]
    public string Method { get; init; } = "POST";

    [JsonPropertyName("body")]
    public object? Body { get; init; }

    [JsonPropertyName("headers")]
    public Dictionary<string, string>? Headers { get; init; }
}

/// <summary>
/// Result model for a mutation execution.
/// </summary>
public sealed record MutationResult<T>
{
    [JsonPropertyName("data")]
    public T? Data { get; init; }

    [JsonPropertyName("success")]
    public bool Success { get; init; }

    [JsonPropertyName("error")]
    public string? Error { get; init; }

    [JsonPropertyName("statusCode")]
    public int StatusCode { get; init; }
}

/// <summary>
/// Non-generic mutation result for dynamic scenarios.
/// </summary>
public sealed record MutationResult
{
    [JsonPropertyName("data")]
    public object? Data { get; init; }

    [JsonPropertyName("success")]
    public bool Success { get; init; }

    [JsonPropertyName("error")]
    public string? Error { get; init; }

    [JsonPropertyName("statusCode")]
    public int StatusCode { get; init; }
}

/// <summary>
/// Options for mutation execution.
/// </summary>
public sealed record MutationOptions
{
    public TimeSpan? Timeout { get; init; }
    public int RetryCount { get; init; } = 0;
    public TimeSpan? RetryDelay { get; init; }
    public bool ThrowOnError { get; init; } = true;
    public CancellationToken CancellationToken { get; init; }
}

/// <summary>
/// Represents the current mutation state.
/// </summary>
public sealed record MutationState<T>
{
    public T? Data { get; init; }
    public bool IsMutating { get; init; }
    public bool IsSuccess { get; init; }
    public bool IsError { get; init; }
    public string? Error { get; init; }
}
