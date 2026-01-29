' Grey SDK for Visual Basic - Error Class
' Normalized error structure for SDK operations.

Namespace Grey.Errors

    ''' <summary>
    ''' Represents a normalized error from the Grey SDK.
    ''' </summary>
    Public Class GreyError
        Inherits Exception

        ''' <summary>Gets the error code.</summary>
        Public ReadOnly Property Code As String

        ''' <summary>Gets additional error details.</summary>
        Public ReadOnly Property Details As String

        ''' <summary>
        ''' Creates a new GreyError instance.
        ''' </summary>
        ''' <param name="code">The error code.</param>
        ''' <param name="message">The error message.</param>
        ''' <param name="details">Additional error details.</param>
        Public Sub New(code As String, message As String, Optional details As String = Nothing)
            MyBase.New(message)
            Me.Code = code
            Me.Details = details
        End Sub

        ''' <summary>
        ''' Creates a new GreyError with an inner exception.
        ''' </summary>
        Public Sub New(code As String, message As String, innerException As Exception)
            MyBase.New(message, innerException)
            Me.Code = code
            Me.Details = If(innerException?.Message, Nothing)
        End Sub

        ' Factory Methods

        ''' <summary>Creates an unauthorized error.</summary>
        Public Shared Function Unauthorized(Optional message As String = Nothing) As GreyError
            Return New GreyError(
                ErrorCodes.Unauthorized,
                If(message, ErrorCodes.GetMessage(ErrorCodes.Unauthorized)))
        End Function

        ''' <summary>Creates a forbidden error.</summary>
        Public Shared Function Forbidden(Optional message As String = Nothing) As GreyError
            Return New GreyError(
                ErrorCodes.Forbidden,
                If(message, ErrorCodes.GetMessage(ErrorCodes.Forbidden)))
        End Function

        ''' <summary>Creates a not found error.</summary>
        Public Shared Function NotFound(Optional message As String = Nothing) As GreyError
            Return New GreyError(
                ErrorCodes.NotFound,
                If(message, ErrorCodes.GetMessage(ErrorCodes.NotFound)))
        End Function

        ''' <summary>Creates a validation error.</summary>
        Public Shared Function ValidationError(message As String, Optional details As String = Nothing) As GreyError
            Return New GreyError(ErrorCodes.ValidationError, message, details)
        End Function

        ''' <summary>Creates a network error.</summary>
        Public Shared Function NetworkError(Optional message As String = Nothing, Optional innerEx As Exception = Nothing) As GreyError
            If innerEx IsNot Nothing Then
                Return New GreyError(
                    ErrorCodes.NetworkError,
                    If(message, ErrorCodes.GetMessage(ErrorCodes.NetworkError)),
                    innerEx)
            End If
            Return New GreyError(
                ErrorCodes.NetworkError,
                If(message, ErrorCodes.GetMessage(ErrorCodes.NetworkError)))
        End Function

        ''' <summary>Creates a timeout error.</summary>
        Public Shared Function Timeout(Optional message As String = Nothing) As GreyError
            Return New GreyError(
                ErrorCodes.Timeout,
                If(message, ErrorCodes.GetMessage(ErrorCodes.Timeout)))
        End Function

        ''' <summary>Creates a server error.</summary>
        Public Shared Function ServerError(Optional message As String = Nothing, Optional details As String = Nothing) As GreyError
            Return New GreyError(
                ErrorCodes.ServerError,
                If(message, ErrorCodes.GetMessage(ErrorCodes.ServerError)),
                details)
        End Function

        ''' <summary>Creates an error from an HTTP status code.</summary>
        Public Shared Function FromHttpStatus(statusCode As Integer, Optional message As String = Nothing, Optional details As String = Nothing) As GreyError
            Dim code = ErrorCodes.FromHttpStatus(statusCode)
            Return New GreyError(
                code,
                If(message, ErrorCodes.GetMessage(code)),
                details)
        End Function

        ''' <summary>Checks if this error is retryable.</summary>
        Public Function IsRetryable() As Boolean
            Return ErrorCodes.IsRetryable(Code)
        End Function

        ''' <summary>Returns a string representation of the error.</summary>
        Public Overrides Function ToString() As String
            If String.IsNullOrEmpty(Details) Then
                Return $"GreyError [{Code}]: {Message}"
            Else
                Return $"GreyError [{Code}]: {Message} - {Details}"
            End If
        End Function

    End Class

End Namespace
