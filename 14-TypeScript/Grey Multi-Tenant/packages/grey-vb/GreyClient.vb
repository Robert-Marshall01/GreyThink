' Grey SDK for Visual Basic - Main Client
' Entry point facade providing access to all domain clients.

Imports Grey.Config
Imports Grey.Domain
Imports Grey.Errors
Imports Grey.Http

Namespace Grey

    ''' <summary>
    ''' Main Grey SDK client providing access to all API operations.
    ''' </summary>
    Public Class GreyClient
        Implements IDisposable

        Private ReadOnly _httpClient As GreyHttpClient
        Private ReadOnly _options As Options
        Private _disposed As Boolean = False

        ''' <summary>Gets the authentication client.</summary>
        Public ReadOnly Property Auth As AuthClient

        ''' <summary>Gets the user client.</summary>
        Public ReadOnly Property Users As UserClient

        ''' <summary>Gets the projects client.</summary>
        Public ReadOnly Property Projects As ProjectsClient

        ''' <summary>Gets the query client.</summary>
        Public ReadOnly Property Queries As QueryClient

        ''' <summary>Gets the mutation client.</summary>
        Public ReadOnly Property Mutations As MutationClient

        ''' <summary>
        ''' Creates a new GreyClient instance.
        ''' </summary>
        ''' <param name="options">Configuration options.</param>
        Public Sub New(options As Options)
            _options = options
            _httpClient = New GreyHttpClient(options)

            Auth = New AuthClient(_httpClient)
            Users = New UserClient(_httpClient)
            Projects = New ProjectsClient(_httpClient)
            Queries = New QueryClient(_httpClient)
            Mutations = New MutationClient(_httpClient)
        End Sub

        ''' <summary>
        ''' Creates a client for local development.
        ''' </summary>
        ''' <param name="port">Optional port number (default: 8080).</param>
        ''' <returns>Configured GreyClient instance.</returns>
        Public Shared Function Local(Optional port As Integer = 8080) As GreyClient
            Return New GreyClient(Options.Local(port))
        End Function

        ''' <summary>
        ''' Creates a client for production use.
        ''' </summary>
        ''' <param name="host">The production API host.</param>
        ''' <param name="port">Optional port number (default: 443).</param>
        ''' <returns>Configured GreyClient instance.</returns>
        Public Shared Function Production(host As String, Optional port As Integer = 443) As GreyClient
            Return New GreyClient(Options.Production(host, port))
        End Function

        ''' <summary>
        ''' Creates a new client with the specified auth token.
        ''' </summary>
        ''' <param name="token">The authentication token.</param>
        ''' <returns>A new GreyClient instance with the token set.</returns>
        Public Function WithAuthToken(token As String) As GreyClient
            Return New GreyClient(_options.WithAuthToken(token))
        End Function

        ''' <summary>
        ''' Creates a new client with the specified timeout.
        ''' </summary>
        ''' <param name="seconds">The timeout in seconds.</param>
        ''' <returns>A new GreyClient instance with the timeout set.</returns>
        Public Function WithTimeout(seconds As Integer) As GreyClient
            Return New GreyClient(_options.WithTimeout(seconds))
        End Function

        ''' <summary>
        ''' Creates a new client with an additional header.
        ''' </summary>
        ''' <param name="name">The header name.</param>
        ''' <param name="value">The header value.</param>
        ''' <returns>A new GreyClient instance with the header added.</returns>
        Public Function WithHeader(name As String, value As String) As GreyClient
            Return New GreyClient(_options.WithHeader(name, value))
        End Function

        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not _disposed Then
                If disposing Then
                    _httpClient?.Dispose()
                End If
                _disposed = True
            End If
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub

    End Class

End Namespace
