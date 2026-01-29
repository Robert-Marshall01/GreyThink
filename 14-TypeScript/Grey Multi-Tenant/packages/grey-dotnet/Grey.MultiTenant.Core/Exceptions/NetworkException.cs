using System.Net;

namespace Grey.MultiTenant.Core.Exceptions;

/// <summary>
/// Exception for network-related errors.
/// </summary>
public class NetworkException : ApiException
{
    public bool IsTimeout { get; }
    public bool IsOffline { get; }

    public NetworkException(string message, bool isTimeout = false, bool isOffline = false)
        : base(message, HttpStatusCode.ServiceUnavailable, "NETWORK_ERROR")
    {
        IsTimeout = isTimeout;
        IsOffline = isOffline;
    }

    public NetworkException(
        string message,
        bool isTimeout,
        bool isOffline,
        Exception? innerException)
        : base(message, HttpStatusCode.ServiceUnavailable, "NETWORK_ERROR", null, null, innerException)
    {
        IsTimeout = isTimeout;
        IsOffline = isOffline;
    }

    public bool IsRetryable => !IsOffline;
}
