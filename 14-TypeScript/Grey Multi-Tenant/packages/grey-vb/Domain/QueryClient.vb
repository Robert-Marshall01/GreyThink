' Grey SDK for Visual Basic - Query Client
' Query operations for read-only data access.

Imports Grey.Errors
Imports Grey.Http

Namespace Grey.Domain

    ''' <summary>
    ''' Request for executing a query.
    ''' </summary>
    Public Class QueryRequest
        ''' <summary>Gets or sets the query string.</summary>
        Public Property Query As String

        ''' <summary>Gets or sets query variables.</summary>
        Public Property Variables As Dictionary(Of String, Object)

        ''' <summary>Gets or sets the operation name.</summary>
        Public Property OperationName As String
    End Class

    ''' <summary>
    ''' Response from a query execution.
    ''' </summary>
    Public Class QueryResponse
        ''' <summary>Gets or sets the query result data.</summary>
        Public Property Data As Object

        ''' <summary>Gets or sets any errors.</summary>
        Public Property Errors As List(Of QueryError)

        ''' <summary>Gets or sets query extensions/metadata.</summary>
        Public Property Extensions As Dictionary(Of String, Object)
    End Class

    ''' <summary>
    ''' An error from query execution.
    ''' </summary>
    Public Class QueryError
        ''' <summary>Gets or sets the error message.</summary>
        Public Property Message As String

        ''' <summary>Gets or sets the error path.</summary>
        Public Property Path As List(Of String)

        ''' <summary>Gets or sets error extensions.</summary>
        Public Property Extensions As Dictionary(Of String, Object)
    End Class

    ''' <summary>
    ''' Request for batch queries.
    ''' </summary>
    Public Class BatchQueryRequest
        ''' <summary>Gets or sets the list of queries.</summary>
        Public Property Queries As List(Of QueryRequest)
    End Class

    ''' <summary>
    ''' Response from batch query execution.
    ''' </summary>
    Public Class BatchQueryResponse
        ''' <summary>Gets or sets the list of results.</summary>
        Public Property Results As List(Of QueryResponse)
    End Class

    ''' <summary>
    ''' Client for query operations.
    ''' </summary>
    Public Class QueryClient

        Private ReadOnly _httpClient As GreyHttpClient

        ''' <summary>
        ''' Creates a new QueryClient instance.
        ''' </summary>
        ''' <param name="httpClient">The HTTP client to use.</param>
        Public Sub New(httpClient As GreyHttpClient)
            _httpClient = httpClient
        End Sub

        ''' <summary>
        ''' Executes a query.
        ''' </summary>
        ''' <param name="query">The query string.</param>
        ''' <param name="variables">Optional query variables.</param>
        ''' <param name="operationName">Optional operation name.</param>
        ''' <returns>Result containing the query response on success.</returns>
        Public Async Function QueryAsync(
            query As String,
            Optional variables As Dictionary(Of String, Object) = Nothing,
            Optional operationName As String = Nothing) As Task(Of Result(Of QueryResponse))

            ' Validate inputs
            If String.IsNullOrWhiteSpace(query) Then
                Return Result(Of QueryResponse).Fail(
                    GreyError.ValidationError("Query string is required"))
            End If

            Dim request = New QueryRequest() With {
                .Query = query,
                .Variables = variables,
                .OperationName = operationName
            }

            Return Await _httpClient.PostAsync(Of QueryResponse)("/api/query", request)
        End Function

        ''' <summary>
        ''' Executes a query with a typed request.
        ''' </summary>
        ''' <param name="request">The query request.</param>
        ''' <returns>Result containing the query response on success.</returns>
        Public Async Function QueryAsync(request As QueryRequest) As Task(Of Result(Of QueryResponse))
            ' Validate inputs
            If request Is Nothing Then
                Return Result(Of QueryResponse).Fail(
                    GreyError.ValidationError("Query request is required"))
            End If

            If String.IsNullOrWhiteSpace(request.Query) Then
                Return Result(Of QueryResponse).Fail(
                    GreyError.ValidationError("Query string is required"))
            End If

            Return Await _httpClient.PostAsync(Of QueryResponse)("/api/query", request)
        End Function

        ''' <summary>
        ''' Executes multiple queries in a batch.
        ''' </summary>
        ''' <param name="queries">The list of query requests.</param>
        ''' <returns>Result containing the batch response on success.</returns>
        Public Async Function BatchQueryAsync(queries As List(Of QueryRequest)) As Task(Of Result(Of BatchQueryResponse))
            ' Validate inputs
            If queries Is Nothing OrElse queries.Count = 0 Then
                Return Result(Of BatchQueryResponse).Fail(
                    GreyError.ValidationError("At least one query is required for batch"))
            End If

            ' Validate each query
            For i = 0 To queries.Count - 1
                If queries(i) Is Nothing Then
                    Return Result(Of BatchQueryResponse).Fail(
                        GreyError.ValidationError($"Query at index {i} is null"))
                End If

                If String.IsNullOrWhiteSpace(queries(i).Query) Then
                    Return Result(Of BatchQueryResponse).Fail(
                        GreyError.ValidationError($"Query at index {i} has empty query string"))
                End If
            Next

            Dim request = New BatchQueryRequest() With {
                .Queries = queries
            }

            Return Await _httpClient.PostAsync(Of BatchQueryResponse)("/api/query/batch", request)
        End Function

    End Class

End Namespace
