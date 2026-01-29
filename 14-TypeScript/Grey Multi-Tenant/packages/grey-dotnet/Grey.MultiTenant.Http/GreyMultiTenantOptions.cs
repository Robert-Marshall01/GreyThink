namespace Grey.MultiTenant.Http;

/// <summary>
/// Configuration options for Grey Multi-Tenant HTTP clients.
/// </summary>
public sealed class GreyMultiTenantOptions
{
    /// <summary>
    /// The base URL for the Grey API.
    /// </summary>
    public required string BaseUrl { get; set; }

    /// <summary>
    /// Optional tenant ID for multi-tenant scenarios.
    /// </summary>
    public string? TenantId { get; set; }

    /// <summary>
    /// HTTP request timeout.
    /// </summary>
    public TimeSpan Timeout { get; set; } = TimeSpan.FromSeconds(30);

    /// <summary>
    /// Number of retry attempts for transient failures.
    /// </summary>
    public int RetryCount { get; set; } = 3;

    /// <summary>
    /// Delay between retry attempts.
    /// </summary>
    public TimeSpan RetryDelay { get; set; } = TimeSpan.FromMilliseconds(500);

    /// <summary>
    /// Whether to automatically refresh tokens on 401 responses.
    /// </summary>
    public bool AutoRefreshTokens { get; set; } = true;

    /// <summary>
    /// Custom headers to include in all requests.
    /// </summary>
    public Dictionary<string, string> DefaultHeaders { get; set; } = new();

    /// <summary>
    /// API version string.
    /// </summary>
    public string? ApiVersion { get; set; }
}
