' Grey SDK for Visual Basic - Mutation Client
' Mutation operations for data modification.

Imports Grey.Errors
Imports Grey.Http

Namespace Grey.Domain

    ''' <summary>
    ''' Request for executing a mutation.
    ''' </summary>
    Public Class MutationRequest
        ''' <summary>Gets or sets the mutation string.</summary>
        Public Property Mutation As String

        ''' <summary>Gets or sets mutation variables.</summary>
        Public Property Variables As Dictionary(Of String, Object)

        ''' <summary>Gets or sets the operation name.</summary>
        Public Property OperationName As String
    End Class

    ''' <summary>
    ''' Response from a mutation execution.
    ''' </summary>
    Public Class MutationResponse
        ''' <summary>Gets or sets the mutation result data.</summary>
        Public Property Data As Object

        ''' <summary>Gets or sets any errors.</summary>
        Public Property Errors As List(Of MutationError)

        ''' <summary>Gets or sets mutation extensions/metadata.</summary>
        Public Property Extensions As Dictionary(Of String, Object)
    End Class

    ''' <summary>
    ''' An error from mutation execution.
    ''' </summary>
    Public Class MutationError
        ''' <summary>Gets or sets the error message.</summary>
        Public Property Message As String

        ''' <summary>Gets or sets the error path.</summary>
        Public Property Path As List(Of String)

        ''' <summary>Gets or sets error extensions.</summary>
        Public Property Extensions As Dictionary(Of String, Object)
    End Class

    ''' <summary>
    ''' Request for batch mutations.
    ''' </summary>
    Public Class BatchMutationRequest
        ''' <summary>Gets or sets the list of mutations.</summary>
        Public Property Mutations As List(Of MutationRequest)
    End Class

    ''' <summary>
    ''' Response from batch mutation execution.
    ''' </summary>
    Public Class BatchMutationResponse
        ''' <summary>Gets or sets the list of results.</summary>
        Public Property Results As List(Of MutationResponse)
    End Class

    ''' <summary>
    ''' Client for mutation operations.
    ''' </summary>
    Public Class MutationClient

        Private ReadOnly _httpClient As GreyHttpClient

        ''' <summary>
        ''' Creates a new MutationClient instance.
        ''' </summary>
        ''' <param name="httpClient">The HTTP client to use.</param>
        Public Sub New(httpClient As GreyHttpClient)
            _httpClient = httpClient
        End Sub

        ''' <summary>
        ''' Executes a mutation.
        ''' </summary>
        ''' <param name="mutation">The mutation string.</param>
        ''' <param name="variables">Optional mutation variables.</param>
        ''' <param name="operationName">Optional operation name.</param>
        ''' <returns>Result containing the mutation response on success.</returns>
        Public Async Function MutateAsync(
            mutation As String,
            Optional variables As Dictionary(Of String, Object) = Nothing,
            Optional operationName As String = Nothing) As Task(Of Result(Of MutationResponse))

            ' Validate inputs
            If String.IsNullOrWhiteSpace(mutation) Then
                Return Result(Of MutationResponse).Fail(
                    GreyError.ValidationError("Mutation string is required"))
            End If

            Dim request = New MutationRequest() With {
                .Mutation = mutation,
                .Variables = variables,
                .OperationName = operationName
            }

            Return Await _httpClient.PostAsync(Of MutationResponse)("/api/mutate", request)
        End Function

        ''' <summary>
        ''' Executes a mutation with a typed request.
        ''' </summary>
        ''' <param name="request">The mutation request.</param>
        ''' <returns>Result containing the mutation response on success.</returns>
        Public Async Function MutateAsync(request As MutationRequest) As Task(Of Result(Of MutationResponse))
            ' Validate inputs
            If request Is Nothing Then
                Return Result(Of MutationResponse).Fail(
                    GreyError.ValidationError("Mutation request is required"))
            End If

            If String.IsNullOrWhiteSpace(request.Mutation) Then
                Return Result(Of MutationResponse).Fail(
                    GreyError.ValidationError("Mutation string is required"))
            End If

            Return Await _httpClient.PostAsync(Of MutationResponse)("/api/mutate", request)
        End Function

        ''' <summary>
        ''' Executes multiple mutations in a batch.
        ''' </summary>
        ''' <param name="mutations">The list of mutation requests.</param>
        ''' <returns>Result containing the batch response on success.</returns>
        Public Async Function BatchMutateAsync(mutations As List(Of MutationRequest)) As Task(Of Result(Of BatchMutationResponse))
            ' Validate inputs
            If mutations Is Nothing OrElse mutations.Count = 0 Then
                Return Result(Of BatchMutationResponse).Fail(
                    GreyError.ValidationError("At least one mutation is required for batch"))
            End If

            ' Validate each mutation
            For i = 0 To mutations.Count - 1
                If mutations(i) Is Nothing Then
                    Return Result(Of BatchMutationResponse).Fail(
                        GreyError.ValidationError($"Mutation at index {i} is null"))
                End If

                If String.IsNullOrWhiteSpace(mutations(i).Mutation) Then
                    Return Result(Of BatchMutationResponse).Fail(
                        GreyError.ValidationError($"Mutation at index {i} has empty mutation string"))
                End If
            Next

            Dim request = New BatchMutationRequest() With {
                .Mutations = mutations
            }

            Return Await _httpClient.PostAsync(Of BatchMutationResponse)("/api/mutate/batch", request)
        End Function

    End Class

End Namespace
