' Grey SDK for Visual Basic - User Client
' User operations: getUser.

Imports Grey.Errors
Imports Grey.Http

Namespace Grey.Domain

    ''' <summary>
    ''' Represents a user in the system.
    ''' </summary>
    Public Class User
        ''' <summary>Gets or sets the user ID.</summary>
        Public Property Id As String

        ''' <summary>Gets or sets the username.</summary>
        Public Property Username As String

        ''' <summary>Gets or sets the email address.</summary>
        Public Property Email As String

        ''' <summary>Gets or sets the display name.</summary>
        Public Property DisplayName As String

        ''' <summary>Gets or sets the user roles.</summary>
        Public Property Roles As List(Of String)

        ''' <summary>Gets or sets when the user was created.</summary>
        Public Property CreatedAt As DateTime

        ''' <summary>Gets or sets when the user was last updated.</summary>
        Public Property UpdatedAt As DateTime
    End Class

    ''' <summary>
    ''' Client for user operations.
    ''' </summary>
    Public Class UserClient

        Private ReadOnly _httpClient As GreyHttpClient

        ''' <summary>
        ''' Creates a new UserClient instance.
        ''' </summary>
        ''' <param name="httpClient">The HTTP client to use.</param>
        Public Sub New(httpClient As GreyHttpClient)
            _httpClient = httpClient
        End Sub

        ''' <summary>
        ''' Gets a user by ID.
        ''' </summary>
        ''' <param name="userId">The user ID.</param>
        ''' <returns>Result containing the user on success.</returns>
        Public Async Function GetUserAsync(userId As String) As Task(Of Result(Of User))
            ' Validate inputs
            If String.IsNullOrWhiteSpace(userId) Then
                Return Result(Of User).Fail(
                    GreyError.ValidationError("User ID is required"))
            End If

            Return Await _httpClient.GetAsync(Of User)($"/api/users/{Uri.EscapeDataString(userId)}")
        End Function

        ''' <summary>
        ''' Gets the currently authenticated user.
        ''' </summary>
        ''' <returns>Result containing the current user on success.</returns>
        Public Async Function GetCurrentUserAsync() As Task(Of Result(Of User))
            Return Await _httpClient.GetAsync(Of User)("/api/users/me")
        End Function

    End Class

End Namespace
