' Grey SDK for Visual Basic - Result Type
' A discriminated union representing success or failure.

Namespace Grey.Errors

    ''' <summary>
    ''' Represents the result of an operation that may succeed or fail.
    ''' </summary>
    ''' <typeparam name="T">The type of the success value.</typeparam>
    Public Class Result(Of T)

        Private ReadOnly _value As T
        Private ReadOnly _error As GreyError
        Private ReadOnly _isSuccess As Boolean

        ''' <summary>Gets whether the result is a success.</summary>
        Public ReadOnly Property IsSuccess As Boolean
            Get
                Return _isSuccess
            End Get
        End Property

        ''' <summary>Gets whether the result is a failure.</summary>
        Public ReadOnly Property IsFailure As Boolean
            Get
                Return Not _isSuccess
            End Get
        End Property

        ''' <summary>Gets the success value. Throws if result is a failure.</summary>
        Public ReadOnly Property Value As T
            Get
                If Not _isSuccess Then
                    Throw New InvalidOperationException("Cannot access Value on a failed Result. Check IsSuccess first.")
                End If
                Return _value
            End Get
        End Property

        ''' <summary>Gets the error. Throws if result is a success.</summary>
        Public ReadOnly Property [Error] As GreyError
            Get
                If _isSuccess Then
                    Throw New InvalidOperationException("Cannot access Error on a successful Result. Check IsFailure first.")
                End If
                Return _error
            End Get
        End Property

        ' Private constructors - use factory methods
        Private Sub New(value As T)
            _value = value
            _error = Nothing
            _isSuccess = True
        End Sub

        Private Sub New(err As GreyError)
            _value = Nothing
            _error = err
            _isSuccess = False
        End Sub

        ''' <summary>Creates a successful result.</summary>
        Public Shared Function Ok(value As T) As Result(Of T)
            Return New Result(Of T)(value)
        End Function

        ''' <summary>Creates a failed result.</summary>
        Public Shared Function Fail(err As GreyError) As Result(Of T)
            Return New Result(Of T)(err)
        End Function

        ''' <summary>Creates a failed result from error components.</summary>
        Public Shared Function Fail(code As String, message As String, Optional details As String = Nothing) As Result(Of T)
            Return New Result(Of T)(New GreyError(code, message, details))
        End Function

        ''' <summary>Gets the value or a default if failed.</summary>
        Public Function GetValueOrDefault(defaultValue As T) As T
            If _isSuccess Then
                Return _value
            End If
            Return defaultValue
        End Function

        ''' <summary>Gets the value or throws the error as an exception.</summary>
        Public Function GetValueOrThrow() As T
            If _isSuccess Then
                Return _value
            End If
            Throw _error
        End Function

        ''' <summary>Maps a successful value to a new result.</summary>
        Public Function Map(Of TNew)(mapper As Func(Of T, TNew)) As Result(Of TNew)
            If _isSuccess Then
                Return Result(Of TNew).Ok(mapper(_value))
            End If
            Return Result(Of TNew).Fail(_error)
        End Function

        ''' <summary>Flat maps a successful value to a new result.</summary>
        Public Function FlatMap(Of TNew)(mapper As Func(Of T, Result(Of TNew))) As Result(Of TNew)
            If _isSuccess Then
                Return mapper(_value)
            End If
            Return Result(Of TNew).Fail(_error)
        End Function

        ''' <summary>Executes an action on success.</summary>
        Public Function OnSuccess(action As Action(Of T)) As Result(Of T)
            If _isSuccess Then
                action(_value)
            End If
            Return Me
        End Function

        ''' <summary>Executes an action on failure.</summary>
        Public Function OnFailure(action As Action(Of GreyError)) As Result(Of T)
            If Not _isSuccess Then
                action(_error)
            End If
            Return Me
        End Function

        ''' <summary>Pattern matches on the result.</summary>
        Public Function Match(Of TResult)(onSuccess As Func(Of T, TResult), onFailure As Func(Of GreyError, TResult)) As TResult
            If _isSuccess Then
                Return onSuccess(_value)
            End If
            Return onFailure(_error)
        End Function

        Public Overrides Function ToString() As String
            If _isSuccess Then
                Return $"Ok({_value})"
            End If
            Return $"Fail({_error})"
        End Function

    End Class

    ''' <summary>
    ''' Result type for operations with no return value.
    ''' </summary>
    Public Class Result
        Inherits Result(Of Boolean)

        Private Sub New(success As Boolean)
            MyBase.New()
        End Sub

        ''' <summary>Creates a successful result with no value.</summary>
        Public Shared Shadows Function Ok() As Result(Of Boolean)
            Return Result(Of Boolean).Ok(True)
        End Function

    End Class

End Namespace
