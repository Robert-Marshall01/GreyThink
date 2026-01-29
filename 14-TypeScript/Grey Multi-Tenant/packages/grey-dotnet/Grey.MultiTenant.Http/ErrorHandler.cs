using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using Grey.MultiTenant.Core.Exceptions;
using Grey.MultiTenant.Core.Models;

namespace Grey.MultiTenant.Http;

/// <summary>
/// Helper class for normalizing HTTP errors into Grey exceptions.
/// </summary>
internal static class ErrorHandler
{
    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        PropertyNameCaseInsensitive = true
    };

    /// <summary>
    /// Ensures the response is successful, throwing appropriate exceptions otherwise.
    /// </summary>
    public static async Task EnsureSuccessAsync(HttpResponseMessage response, CancellationToken cancellationToken = default)
    {
        if (response.IsSuccessStatusCode)
            return;

        var rawResponse = await response.Content.ReadAsStringAsync(cancellationToken);
        var statusCode = response.StatusCode;

        // Try to parse error response
        ErrorModel? errorModel = null;
        ValidationErrorResponse? validationErrors = null;

        try
        {
            if (statusCode == HttpStatusCode.BadRequest || statusCode == HttpStatusCode.UnprocessableEntity)
            {
                validationErrors = JsonSerializer.Deserialize<ValidationErrorResponse>(rawResponse, JsonOptions);
            }
            
            if (validationErrors?.Errors.Count > 0)
            {
                throw new ValidationException(
                    validationErrors.Message,
                    validationErrors.Errors,
                    rawResponse);
            }

            errorModel = JsonSerializer.Deserialize<ErrorModel>(rawResponse, JsonOptions);
        }
        catch (JsonException)
        {
            // Failed to parse error response, use raw message
        }

        var message = errorModel?.Message ?? GetDefaultMessage(statusCode);
        var errorCode = errorModel?.Code;
        var traceId = errorModel?.TraceId ?? response.Headers.TryGetValues("X-Trace-Id", out var traceValues) 
            ? traceValues.FirstOrDefault() 
            : null;

        // Handle authentication errors
        if (statusCode == HttpStatusCode.Unauthorized)
        {
            var authErrorType = DetermineAuthErrorType(errorCode);
            throw new AuthException(message, authErrorType, errorCode, rawResponse);
        }

        if (statusCode == HttpStatusCode.Forbidden)
        {
            throw new AuthException(message, AuthErrorType.PermissionDenied, errorCode, rawResponse);
        }

        throw new ApiException(message, statusCode, errorCode, rawResponse, traceId);
    }

    private static AuthErrorType DetermineAuthErrorType(string? errorCode)
    {
        return errorCode?.ToUpperInvariant() switch
        {
            "INVALID_CREDENTIALS" => AuthErrorType.InvalidCredentials,
            "TOKEN_EXPIRED" => AuthErrorType.TokenExpired,
            "TOKEN_INVALID" => AuthErrorType.TokenInvalid,
            "SESSION_EXPIRED" => AuthErrorType.SessionExpired,
            "REFRESH_FAILED" => AuthErrorType.RefreshFailed,
            "ACCOUNT_LOCKED" => AuthErrorType.AccountLocked,
            "ACCOUNT_DISABLED" => AuthErrorType.AccountDisabled,
            _ => AuthErrorType.Unknown
        };
    }

    private static string GetDefaultMessage(HttpStatusCode statusCode)
    {
        return statusCode switch
        {
            HttpStatusCode.BadRequest => "Bad request",
            HttpStatusCode.Unauthorized => "Unauthorized",
            HttpStatusCode.Forbidden => "Forbidden",
            HttpStatusCode.NotFound => "Not found",
            HttpStatusCode.Conflict => "Conflict",
            HttpStatusCode.UnprocessableEntity => "Validation failed",
            HttpStatusCode.TooManyRequests => "Too many requests",
            HttpStatusCode.InternalServerError => "Internal server error",
            HttpStatusCode.BadGateway => "Bad gateway",
            HttpStatusCode.ServiceUnavailable => "Service unavailable",
            HttpStatusCode.GatewayTimeout => "Gateway timeout",
            _ => $"HTTP error {(int)statusCode}"
        };
    }

    /// <summary>
    /// Wraps HTTP exceptions into Grey exceptions.
    /// </summary>
    public static Exception WrapException(Exception ex)
    {
        return ex switch
        {
            ApiException => ex,
            AuthException => ex,
            ValidationException => ex,
            NetworkException => ex,
            HttpRequestException httpEx when httpEx.InnerException is TaskCanceledException =>
                new NetworkException("Request timed out", isTimeout: true, isOffline: false, httpEx),
            HttpRequestException httpEx =>
                new NetworkException(httpEx.Message, isTimeout: false, isOffline: true, httpEx),
            TaskCanceledException tcEx when !tcEx.CancellationToken.IsCancellationRequested =>
                new NetworkException("Request timed out", isTimeout: true, isOffline: false, tcEx),
            _ => new ApiException($"Unexpected error: {ex.Message}", HttpStatusCode.InternalServerError, null, null, null, ex)
        };
    }
}
