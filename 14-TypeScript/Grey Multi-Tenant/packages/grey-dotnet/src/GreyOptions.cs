// =============================================================================
// Grey Multi-Tenant SDK - .NET
// GreyOptions.cs
//
// Configuration options for the Grey SDK.
// DI-bindable for use with IOptions pattern.
// =============================================================================

namespace Grey.Sdk;

/// <summary>
/// Configuration options for the Grey Multi-Tenant SDK.
/// Bind this to configuration or configure via delegate in DI.
/// </summary>
public sealed class GreyOptions
{
    /// <summary>
    /// The base URL for the Grey API.
    /// </summary>
    public string ApiUrl { get; set; } = string.Empty;

    /// <summary>
    /// Optional API key for authentication.
    /// </summary>
    public string? ApiKey { get; set; }

    /// <summary>
    /// HTTP request timeout.
    /// </summary>
    public TimeSpan Timeout { get; set; } = TimeSpan.FromSeconds(30);
}
