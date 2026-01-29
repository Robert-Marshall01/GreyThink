' Grey SDK for Visual Basic - Result Tests

Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports Grey.Errors

<TestClass>
Public Class ResultTests

    <TestMethod>
    Public Sub Ok_Creates_Successful_Result()
        Dim result = Result(Of String).Ok("test value")
        
        Assert.IsTrue(result.IsSuccess)
        Assert.IsFalse(result.IsFailure)
        Assert.AreEqual("test value", result.Value)
    End Sub

    <TestMethod>
    Public Sub Fail_Creates_Failed_Result()
        Dim err = GreyError.ValidationError("test error")
        Dim result = Result(Of String).Fail(err)
        
        Assert.IsFalse(result.IsSuccess)
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("test error", result.Error.Message)
    End Sub

    <TestMethod>
    <ExpectedException(GetType(InvalidOperationException))>
    Public Sub Value_Throws_On_Failed_Result()
        Dim result = Result(Of String).Fail(GreyError.ValidationError("error"))
        Dim value = result.Value ' Should throw
    End Sub

    <TestMethod>
    <ExpectedException(GetType(InvalidOperationException))>
    Public Sub Error_Throws_On_Successful_Result()
        Dim result = Result(Of String).Ok("value")
        Dim err = result.Error ' Should throw
    End Sub

    <TestMethod>
    Public Sub GetValueOrDefault_Returns_Value_On_Success()
        Dim result = Result(Of String).Ok("test value")
        Assert.AreEqual("test value", result.GetValueOrDefault("default"))
    End Sub

    <TestMethod>
    Public Sub GetValueOrDefault_Returns_Default_On_Failure()
        Dim result = Result(Of String).Fail(GreyError.ValidationError("error"))
        Assert.AreEqual("default", result.GetValueOrDefault("default"))
    End Sub

    <TestMethod>
    Public Sub GetValueOrThrow_Returns_Value_On_Success()
        Dim result = Result(Of String).Ok("test value")
        Assert.AreEqual("test value", result.GetValueOrThrow())
    End Sub

    <TestMethod>
    <ExpectedException(GetType(GreyError))>
    Public Sub GetValueOrThrow_Throws_On_Failure()
        Dim result = Result(Of String).Fail(GreyError.ValidationError("error"))
        result.GetValueOrThrow() ' Should throw
    End Sub

    <TestMethod>
    Public Sub Map_Transforms_Successful_Value()
        Dim result = Result(Of Integer).Ok(5)
        Dim mapped = result.Map(Function(x) x * 2)
        
        Assert.IsTrue(mapped.IsSuccess)
        Assert.AreEqual(10, mapped.Value)
    End Sub

    <TestMethod>
    Public Sub Map_Propagates_Error()
        Dim err = GreyError.ValidationError("error")
        Dim result = Result(Of Integer).Fail(err)
        Dim mapped = result.Map(Function(x) x * 2)
        
        Assert.IsTrue(mapped.IsFailure)
        Assert.AreEqual("error", mapped.Error.Message)
    End Sub

    <TestMethod>
    Public Sub Match_Calls_OnSuccess_For_Success()
        Dim result = Result(Of Integer).Ok(5)
        Dim matched = result.Match(
            Function(x) $"value: {x}",
            Function(e) $"error: {e.Message}"
        )
        
        Assert.AreEqual("value: 5", matched)
    End Sub

    <TestMethod>
    Public Sub Match_Calls_OnFailure_For_Failure()
        Dim result = Result(Of Integer).Fail(GreyError.ValidationError("test error"))
        Dim matched = result.Match(
            Function(x) $"value: {x}",
            Function(e) $"error: {e.Message}"
        )
        
        Assert.AreEqual("error: test error", matched)
    End Sub

    <TestMethod>
    Public Sub OnSuccess_Executes_Action_On_Success()
        Dim executed = False
        Dim result = Result(Of Integer).Ok(5)
        
        result.OnSuccess(Sub(x) executed = True)
        
        Assert.IsTrue(executed)
    End Sub

    <TestMethod>
    Public Sub OnSuccess_Does_Not_Execute_On_Failure()
        Dim executed = False
        Dim result = Result(Of Integer).Fail(GreyError.ValidationError("error"))
        
        result.OnSuccess(Sub(x) executed = True)
        
        Assert.IsFalse(executed)
    End Sub

    <TestMethod>
    Public Sub OnFailure_Executes_Action_On_Failure()
        Dim executed = False
        Dim result = Result(Of Integer).Fail(GreyError.ValidationError("error"))
        
        result.OnFailure(Sub(e) executed = True)
        
        Assert.IsTrue(executed)
    End Sub

    <TestMethod>
    Public Sub OnFailure_Does_Not_Execute_On_Success()
        Dim executed = False
        Dim result = Result(Of Integer).Ok(5)
        
        result.OnFailure(Sub(e) executed = True)
        
        Assert.IsFalse(executed)
    End Sub

End Class
