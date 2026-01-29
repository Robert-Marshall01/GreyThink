using System.Net;

namespace Grey.MultiTenant.Core.Exceptions;

/// <summary>
/// Base exception for all Grey API errors.
/// </summary>
public class ApiException : Exception
{
    public HttpStatusCode StatusCode { get; }
    public string? ErrorCode { get; }
    public string? RawResponse { get; }
    public string? TraceId { get; }

    public ApiException(string message) : base(message)
    {
        StatusCode = HttpStatusCode.InternalServerError;
    }

    public ApiException(string message, HttpStatusCode statusCode) : base(message)
    {
        StatusCode = statusCode;
    }

    public ApiException(
        string message,
        HttpStatusCode statusCode,
        string? errorCode = null,
        string? rawResponse = null,
        string? traceId = null,
        Exception? innerException = null)
        : base(message, innerException)
    {
        StatusCode = statusCode;
        ErrorCode = errorCode;
        RawResponse = rawResponse;
        TraceId = traceId;
    }

    public bool IsUnauthorized => StatusCode == HttpStatusCode.Unauthorized;
    public bool IsForbidden => StatusCode == HttpStatusCode.Forbidden;
    public bool IsNotFound => StatusCode == HttpStatusCode.NotFound;
    public bool IsServerError => (int)StatusCode >= 500;
    public bool IsClientError => (int)StatusCode >= 400 && (int)StatusCode < 500;
}
