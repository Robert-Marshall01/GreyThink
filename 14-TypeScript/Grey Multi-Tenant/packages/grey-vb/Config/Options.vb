' Grey SDK for Visual Basic - Options
' Configuration options for the SDK client.

Namespace Grey.Config

    ''' <summary>
    ''' Configuration options for the Grey SDK client.
    ''' </summary>
    Public Class Options

        ''' <summary>Gets or sets the API host.</summary>
        Public Property Host As String

        ''' <summary>Gets or sets the API port.</summary>
        Public Property Port As Integer

        ''' <summary>Gets or sets whether to use TLS (HTTPS).</summary>
        Public Property UseTls As Boolean

        ''' <summary>Gets or sets the request timeout in seconds.</summary>
        Public Property TimeoutSeconds As Integer

        ''' <summary>Gets or sets the authentication token.</summary>
        Public Property AuthToken As String

        ''' <summary>Gets or sets custom headers to include with requests.</summary>
        Public Property CustomHeaders As Dictionary(Of String, String)

        ''' <summary>
        ''' Creates a new Options instance with default values.
        ''' </summary>
        Public Sub New()
            Host = "localhost"
            Port = 8080
            UseTls = False
            TimeoutSeconds = 30
            AuthToken = Nothing
            CustomHeaders = New Dictionary(Of String, String)()
        End Sub

        ''' <summary>
        ''' Creates options for local development.
        ''' </summary>
        ''' <param name="port">Optional port number (default: 8080).</param>
        ''' <returns>Configured Options instance.</returns>
        Public Shared Function Local(Optional port As Integer = 8080) As Options
            Return New Options() With {
                .Host = "localhost",
                .Port = port,
                .UseTls = False,
                .TimeoutSeconds = 30
            }
        End Function

        ''' <summary>
        ''' Creates options for production use.
        ''' </summary>
        ''' <param name="host">The production API host.</param>
        ''' <param name="port">Optional port number (default: 443).</param>
        ''' <returns>Configured Options instance.</returns>
        Public Shared Function Production(host As String, Optional port As Integer = 443) As Options
            Return New Options() With {
                .Host = host,
                .Port = port,
                .UseTls = True,
                .TimeoutSeconds = 30
            }
        End Function

        ''' <summary>
        ''' Gets the base URL constructed from the options.
        ''' </summary>
        ''' <returns>The base URL string.</returns>
        Public Function GetBaseUrl() As String
            Dim scheme = If(UseTls, "https", "http")
            Return $"{scheme}://{Host}:{Port}"
        End Function

        ''' <summary>
        ''' Creates a copy of this Options with a new auth token.
        ''' </summary>
        ''' <param name="token">The authentication token.</param>
        ''' <returns>A new Options instance with the token set.</returns>
        Public Function WithAuthToken(token As String) As Options
            Return New Options() With {
                .Host = Me.Host,
                .Port = Me.Port,
                .UseTls = Me.UseTls,
                .TimeoutSeconds = Me.TimeoutSeconds,
                .AuthToken = token,
                .CustomHeaders = New Dictionary(Of String, String)(Me.CustomHeaders)
            }
        End Function

        ''' <summary>
        ''' Creates a copy of this Options with a new timeout.
        ''' </summary>
        ''' <param name="seconds">The timeout in seconds.</param>
        ''' <returns>A new Options instance with the timeout set.</returns>
        Public Function WithTimeout(seconds As Integer) As Options
            Return New Options() With {
                .Host = Me.Host,
                .Port = Me.Port,
                .UseTls = Me.UseTls,
                .TimeoutSeconds = seconds,
                .AuthToken = Me.AuthToken,
                .CustomHeaders = New Dictionary(Of String, String)(Me.CustomHeaders)
            }
        End Function

        ''' <summary>
        ''' Creates a copy of this Options with an additional header.
        ''' </summary>
        ''' <param name="name">The header name.</param>
        ''' <param name="value">The header value.</param>
        ''' <returns>A new Options instance with the header added.</returns>
        Public Function WithHeader(name As String, value As String) As Options
            Dim result = New Options() With {
                .Host = Me.Host,
                .Port = Me.Port,
                .UseTls = Me.UseTls,
                .TimeoutSeconds = Me.TimeoutSeconds,
                .AuthToken = Me.AuthToken,
                .CustomHeaders = New Dictionary(Of String, String)(Me.CustomHeaders)
            }
            result.CustomHeaders(name) = value
            Return result
        End Function

    End Class

End Namespace
