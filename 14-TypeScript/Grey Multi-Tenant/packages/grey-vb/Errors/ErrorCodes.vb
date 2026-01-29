' Grey SDK for Visual Basic - Error Codes
' Standardized error codes for SDK operations.

Namespace Grey.Errors

    ''' <summary>
    ''' Standard error codes used throughout the Grey SDK.
    ''' </summary>
    Public Module ErrorCodes

        ''' <summary>Authentication required or token invalid.</summary>
        Public Const Unauthorized As String = "unauthorized"

        ''' <summary>Access denied to the requested resource.</summary>
        Public Const Forbidden As String = "forbidden"

        ''' <summary>Requested resource was not found.</summary>
        Public Const NotFound As String = "not_found"

        ''' <summary>Request validation failed.</summary>
        Public Const ValidationError As String = "validation_error"

        ''' <summary>Network connectivity issue.</summary>
        Public Const NetworkError As String = "network_error"

        ''' <summary>Request timed out.</summary>
        Public Const Timeout As String = "timeout"

        ''' <summary>Server returned an error.</summary>
        Public Const ServerError As String = "server_error"

        ''' <summary>Unknown or unexpected error.</summary>
        Public Const Unknown As String = "unknown"

        ''' <summary>
        ''' Converts an HTTP status code to the corresponding error code.
        ''' </summary>
        ''' <param name="statusCode">The HTTP status code.</param>
        ''' <returns>The corresponding error code string.</returns>
        Public Function FromHttpStatus(statusCode As Integer) As String
            Select Case statusCode
                Case 401
                    Return Unauthorized
                Case 403
                    Return Forbidden
                Case 404
                    Return NotFound
                Case 400, 422
                    Return ValidationError
                Case 408
                    Return Timeout
                Case 500 To 599
                    Return ServerError
                Case Else
                    Return Unknown
            End Select
        End Function

        ''' <summary>
        ''' Checks if the error code represents a retryable error.
        ''' </summary>
        ''' <param name="code">The error code to check.</param>
        ''' <returns>True if the error is retryable.</returns>
        Public Function IsRetryable(code As String) As Boolean
            Select Case code
                Case NetworkError, Timeout, ServerError
                    Return True
                Case Else
                    Return False
            End Select
        End Function

        ''' <summary>
        ''' Gets a human-readable message for an error code.
        ''' </summary>
        ''' <param name="code">The error code.</param>
        ''' <returns>Human-readable error message.</returns>
        Public Function GetMessage(code As String) As String
            Select Case code
                Case Unauthorized
                    Return "Authentication required or token invalid"
                Case Forbidden
                    Return "Access denied to the requested resource"
                Case NotFound
                    Return "Requested resource was not found"
                Case ValidationError
                    Return "Request validation failed"
                Case NetworkError
                    Return "Network connectivity issue"
                Case Timeout
                    Return "Request timed out"
                Case ServerError
                    Return "Server returned an error"
                Case Else
                    Return "Unknown or unexpected error"
            End Select
        End Function

    End Module

End Namespace
