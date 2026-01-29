' Grey SDK for Visual Basic - HTTP Client
' Shared HTTP client for making API requests.

Imports System.Net.Http
Imports System.Text
Imports System.Text.Json
Imports Grey.Config
Imports Grey.Errors

Namespace Grey.Http

    ''' <summary>
    ''' HTTP client for making API requests.
    ''' </summary>
    Public Class GreyHttpClient
        Implements IDisposable

        Private ReadOnly _httpClient As HttpClient
        Private ReadOnly _options As Options
        Private _disposed As Boolean = False

        ''' <summary>
        ''' Creates a new GreyHttpClient instance.
        ''' </summary>
        ''' <param name="options">Configuration options.</param>
        Public Sub New(options As Options)
            _options = options
            _httpClient = New HttpClient() With {
                .BaseAddress = New Uri(options.GetBaseUrl()),
                .Timeout = TimeSpan.FromSeconds(options.TimeoutSeconds)
            }

            ' Set default headers
            _httpClient.DefaultRequestHeaders.Add("Accept", "application/json")

            ' Add auth token if present
            If Not String.IsNullOrEmpty(options.AuthToken) Then
                _httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {options.AuthToken}")
            End If

            ' Add custom headers
            For Each header In options.CustomHeaders
                _httpClient.DefaultRequestHeaders.Add(header.Key, header.Value)
            Next
        End Sub

        ''' <summary>
        ''' Performs a GET request.
        ''' </summary>
        Public Async Function GetAsync(Of T)(path As String) As Task(Of Result(Of T))
            Return Await SendAsync(Of T)(HttpMethod.Get, path, Nothing)
        End Function

        ''' <summary>
        ''' Performs a POST request with a JSON body.
        ''' </summary>
        Public Async Function PostAsync(Of T)(path As String, body As Object) As Task(Of Result(Of T))
            Return Await SendAsync(Of T)(HttpMethod.Post, path, body)
        End Function

        ''' <summary>
        ''' Performs a PUT request with a JSON body.
        ''' </summary>
        Public Async Function PutAsync(Of T)(path As String, body As Object) As Task(Of Result(Of T))
            Return Await SendAsync(Of T)(HttpMethod.Put, path, body)
        End Function

        ''' <summary>
        ''' Performs a PATCH request with a JSON body.
        ''' </summary>
        Public Async Function PatchAsync(Of T)(path As String, body As Object) As Task(Of Result(Of T))
            Return Await SendAsync(Of T)(New HttpMethod("PATCH"), path, body)
        End Function

        ''' <summary>
        ''' Performs a DELETE request.
        ''' </summary>
        Public Async Function DeleteAsync(Of T)(path As String) As Task(Of Result(Of T))
            Return Await SendAsync(Of T)(HttpMethod.Delete, path, Nothing)
        End Function

        ''' <summary>
        ''' Performs a DELETE request with no response body.
        ''' </summary>
        Public Async Function DeleteAsync(path As String) As Task(Of Result(Of Boolean))
            Return Await SendAsync(Of Boolean)(HttpMethod.Delete, path, Nothing)
        End Function

        ''' <summary>
        ''' Sends an HTTP request and returns the result.
        ''' </summary>
        Private Async Function SendAsync(Of T)(method As HttpMethod, path As String, body As Object) As Task(Of Result(Of T))
            Try
                Dim request = New HttpRequestMessage(method, path)

                ' Add JSON body if present
                If body IsNot Nothing Then
                    Dim jsonOptions = New JsonSerializerOptions() With {
                        .PropertyNamingPolicy = JsonNamingPolicy.CamelCase
                    }
                    Dim json = JsonSerializer.Serialize(body, jsonOptions)
                    request.Content = New StringContent(json, Encoding.UTF8, "application/json")
                End If

                Dim response = Await _httpClient.SendAsync(request)
                Dim responseBody = Await response.Content.ReadAsStringAsync()

                If response.IsSuccessStatusCode Then
                    Return ParseSuccessResponse(Of T)(responseBody)
                Else
                    Return ParseErrorResponse(Of T)(CInt(response.StatusCode), responseBody)
                End If

            Catch ex As TaskCanceledException
                Return Result(Of T).Fail(GreyError.Timeout($"Request to {path} timed out"))

            Catch ex As HttpRequestException
                Return Result(Of T).Fail(GreyError.NetworkError($"Network error requesting {path}", ex))

            Catch ex As Exception
                Return Result(Of T).Fail(New GreyError(ErrorCodes.Unknown, $"Unexpected error: {ex.Message}", ex.ToString()))
            End Try
        End Function

        ''' <summary>
        ''' Parses a successful response.
        ''' </summary>
        Private Function ParseSuccessResponse(Of T)(responseBody As String) As Result(Of T)
            Try
                ' Handle empty responses
                If String.IsNullOrWhiteSpace(responseBody) Then
                    If GetType(T) Is GetType(Boolean) Then
                        Return Result(Of T).Ok(CType(CObj(True), T))
                    End If
                    Return Result(Of T).Ok(Nothing)
                End If

                Dim jsonOptions = New JsonSerializerOptions() With {
                    .PropertyNameCaseInsensitive = True
                }
                Dim value = JsonSerializer.Deserialize(Of T)(responseBody, jsonOptions)
                Return Result(Of T).Ok(value)

            Catch ex As JsonException
                Return Result(Of T).Fail(New GreyError(
                    ErrorCodes.Unknown,
                    "Failed to parse response",
                    ex.Message))
            End Try
        End Function

        ''' <summary>
        ''' Parses an error response.
        ''' </summary>
        Private Function ParseErrorResponse(Of T)(statusCode As Integer, responseBody As String) As Result(Of T)
            Dim errorCode = ErrorCodes.FromHttpStatus(statusCode)
            Dim errorMessage = ErrorCodes.GetMessage(errorCode)
            Dim details As String = Nothing

            ' Try to extract error details from response body
            If Not String.IsNullOrWhiteSpace(responseBody) Then
                Try
                    Dim jsonDoc = JsonDocument.Parse(responseBody)
                    Dim root = jsonDoc.RootElement

                    ' Try to get message from response
                    If root.TryGetProperty("message", errorProp) OrElse
                       root.TryGetProperty("error", errorProp) Then
                        errorMessage = errorProp.GetString()
                    End If

                    ' Try to get details from response
                    If root.TryGetProperty("details", detailsProp) Then
                        details = detailsProp.ToString()
                    End If
                Catch
                    ' If we can't parse the error body, use the raw response
                    details = responseBody
                End Try
            End If

            Return Result(Of T).Fail(New GreyError(errorCode, errorMessage, details))
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
