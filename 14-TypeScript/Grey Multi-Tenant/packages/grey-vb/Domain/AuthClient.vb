' Grey SDK for Visual Basic - Auth Client
' Authentication operations: login, logout, refresh.

Imports Grey.Errors
Imports Grey.Http

Namespace Grey.Domain

    ''' <summary>
    ''' Authentication tokens returned from login/refresh.
    ''' </summary>
    Public Class AuthTokens
        ''' <summary>Gets or sets the access token.</summary>
        Public Property AccessToken As String

        ''' <summary>Gets or sets the refresh token.</summary>
        Public Property RefreshToken As String

        ''' <summary>Gets or sets the token expiration in seconds.</summary>
        Public Property ExpiresIn As Integer
    End Class

    ''' <summary>
    ''' Client for authentication operations.
    ''' </summary>
    Public Class AuthClient

        Private ReadOnly _httpClient As GreyHttpClient

        ''' <summary>
        ''' Creates a new AuthClient instance.
        ''' </summary>
        ''' <param name="httpClient">The HTTP client to use.</param>
        Public Sub New(httpClient As GreyHttpClient)
            _httpClient = httpClient
        End Sub

        ''' <summary>
        ''' Authenticates a user with username and password.
        ''' </summary>
        ''' <param name="username">The username.</param>
        ''' <param name="password">The password.</param>
        ''' <returns>Result containing auth tokens on success.</returns>
        Public Async Function LoginAsync(username As String, password As String) As Task(Of Result(Of AuthTokens))
            ' Validate inputs
            If String.IsNullOrWhiteSpace(username) Then
                Return Result(Of AuthTokens).Fail(
                    GreyError.ValidationError("Username is required"))
            End If

            If String.IsNullOrWhiteSpace(password) Then
                Return Result(Of AuthTokens).Fail(
                    GreyError.ValidationError("Password is required"))
            End If

            Dim body = New With {
                .username = username,
                .password = password
            }

            Return Await _httpClient.PostAsync(Of AuthTokens)("/api/auth/login", body)
        End Function

        ''' <summary>
        ''' Logs out the current user.
        ''' </summary>
        ''' <returns>Result indicating success or failure.</returns>
        Public Async Function LogoutAsync() As Task(Of Result(Of Boolean))
            Return Await _httpClient.PostAsync(Of Boolean)("/api/auth/logout", Nothing)
        End Function

        ''' <summary>
        ''' Refreshes the authentication token.
        ''' </summary>
        ''' <param name="refreshToken">The refresh token.</param>
        ''' <returns>Result containing new auth tokens on success.</returns>
        Public Async Function RefreshAsync(refreshToken As String) As Task(Of Result(Of AuthTokens))
            ' Validate inputs
            If String.IsNullOrWhiteSpace(refreshToken) Then
                Return Result(Of AuthTokens).Fail(
                    GreyError.ValidationError("Refresh token is required"))
            End If

            Dim body = New With {
                .refreshToken = refreshToken
            }

            Return Await _httpClient.PostAsync(Of AuthTokens)("/api/auth/refresh", body)
        End Function

    End Class

End Namespace
