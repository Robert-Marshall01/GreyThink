using System.Net;

namespace Grey.MultiTenant.Core.Exceptions;

/// <summary>
/// Exception for authentication-related errors.
/// </summary>
public class AuthException : ApiException
{
    public AuthErrorType ErrorType { get; }

    public AuthException(string message, AuthErrorType errorType = AuthErrorType.Unknown)
        : base(message, GetStatusCodeForErrorType(errorType))
    {
        ErrorType = errorType;
    }

    public AuthException(
        string message,
        AuthErrorType errorType,
        string? errorCode = null,
        string? rawResponse = null,
        Exception? innerException = null)
        : base(message, GetStatusCodeForErrorType(errorType), errorCode, rawResponse, null, innerException)
    {
        ErrorType = errorType;
    }

    private static HttpStatusCode GetStatusCodeForErrorType(AuthErrorType errorType) => errorType switch
    {
        AuthErrorType.InvalidCredentials => HttpStatusCode.Unauthorized,
        AuthErrorType.TokenExpired => HttpStatusCode.Unauthorized,
        AuthErrorType.TokenInvalid => HttpStatusCode.Unauthorized,
        AuthErrorType.SessionExpired => HttpStatusCode.Unauthorized,
        AuthErrorType.RefreshFailed => HttpStatusCode.Unauthorized,
        AuthErrorType.AccountLocked => HttpStatusCode.Forbidden,
        AuthErrorType.AccountDisabled => HttpStatusCode.Forbidden,
        AuthErrorType.PermissionDenied => HttpStatusCode.Forbidden,
        _ => HttpStatusCode.InternalServerError
    };

    public bool IsTokenExpired => ErrorType == AuthErrorType.TokenExpired;
    public bool IsSessionExpired => ErrorType == AuthErrorType.SessionExpired;
    public bool NeedsReauthentication => ErrorType is 
        AuthErrorType.TokenExpired or 
        AuthErrorType.TokenInvalid or 
        AuthErrorType.SessionExpired or
        AuthErrorType.RefreshFailed;
}

/// <summary>
/// Types of authentication errors.
/// </summary>
public enum AuthErrorType
{
    Unknown,
    InvalidCredentials,
    TokenExpired,
    TokenInvalid,
    SessionExpired,
    RefreshFailed,
    AccountLocked,
    AccountDisabled,
    PermissionDenied
}
