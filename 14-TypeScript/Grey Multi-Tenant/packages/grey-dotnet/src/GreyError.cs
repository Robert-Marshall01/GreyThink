// =============================================================================
// Grey Multi-Tenant SDK - .NET
// GreyError.cs
//
// Normalized error shape for all Grey services.
// =============================================================================

namespace Grey.Sdk;

/// <summary>
/// Normalized error shape for all Grey operations.
/// Services must never throw raw errors; normalize them into this type.
/// </summary>
public sealed class GreyError
{
    /// <summary>
    /// Human-readable error message.
    /// </summary>
    public string Message { get; set; } = string.Empty;

    /// <summary>
    /// Optional error code for programmatic handling.
    /// </summary>
    public string? Code { get; set; }

    /// <summary>
    /// Optional HTTP status code.
    /// </summary>
    public int? Status { get; set; }

    /// <summary>
    /// Optional raw error data for debugging.
    /// </summary>
    public object? Raw { get; set; }

    /// <summary>
    /// Creates a GreyError from an exception.
    /// </summary>
    public static GreyError FromException(Exception ex)
    {
        return new GreyError
        {
            Message = ex.Message,
            Raw = ex,
        };
    }

    /// <summary>
    /// Creates a GreyError with a message.
    /// </summary>
    public static GreyError FromMessage(string message)
    {
        return new GreyError
        {
            Message = message,
        };
    }
}
