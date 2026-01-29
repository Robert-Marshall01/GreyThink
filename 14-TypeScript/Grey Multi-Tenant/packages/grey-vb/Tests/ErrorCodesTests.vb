' Grey SDK for Visual Basic - Error Codes Tests

Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports Grey.Errors

<TestClass>
Public Class ErrorCodesTests

    <TestMethod>
    Public Sub FromHttpStatus_Returns_Unauthorized_For_401()
        Dim result = ErrorCodes.FromHttpStatus(401)
        Assert.AreEqual(ErrorCodes.Unauthorized, result)
    End Sub

    <TestMethod>
    Public Sub FromHttpStatus_Returns_Forbidden_For_403()
        Dim result = ErrorCodes.FromHttpStatus(403)
        Assert.AreEqual(ErrorCodes.Forbidden, result)
    End Sub

    <TestMethod>
    Public Sub FromHttpStatus_Returns_NotFound_For_404()
        Dim result = ErrorCodes.FromHttpStatus(404)
        Assert.AreEqual(ErrorCodes.NotFound, result)
    End Sub

    <TestMethod>
    Public Sub FromHttpStatus_Returns_ValidationError_For_400()
        Dim result = ErrorCodes.FromHttpStatus(400)
        Assert.AreEqual(ErrorCodes.ValidationError, result)
    End Sub

    <TestMethod>
    Public Sub FromHttpStatus_Returns_ValidationError_For_422()
        Dim result = ErrorCodes.FromHttpStatus(422)
        Assert.AreEqual(ErrorCodes.ValidationError, result)
    End Sub

    <TestMethod>
    Public Sub FromHttpStatus_Returns_Timeout_For_408()
        Dim result = ErrorCodes.FromHttpStatus(408)
        Assert.AreEqual(ErrorCodes.Timeout, result)
    End Sub

    <TestMethod>
    Public Sub FromHttpStatus_Returns_ServerError_For_500()
        Dim result = ErrorCodes.FromHttpStatus(500)
        Assert.AreEqual(ErrorCodes.ServerError, result)
    End Sub

    <TestMethod>
    Public Sub FromHttpStatus_Returns_ServerError_For_503()
        Dim result = ErrorCodes.FromHttpStatus(503)
        Assert.AreEqual(ErrorCodes.ServerError, result)
    End Sub

    <TestMethod>
    Public Sub FromHttpStatus_Returns_Unknown_For_Unrecognized()
        Dim result = ErrorCodes.FromHttpStatus(418) ' I'm a teapot
        Assert.AreEqual(ErrorCodes.Unknown, result)
    End Sub

    <TestMethod>
    Public Sub IsRetryable_Returns_True_For_NetworkError()
        Assert.IsTrue(ErrorCodes.IsRetryable(ErrorCodes.NetworkError))
    End Sub

    <TestMethod>
    Public Sub IsRetryable_Returns_True_For_Timeout()
        Assert.IsTrue(ErrorCodes.IsRetryable(ErrorCodes.Timeout))
    End Sub

    <TestMethod>
    Public Sub IsRetryable_Returns_True_For_ServerError()
        Assert.IsTrue(ErrorCodes.IsRetryable(ErrorCodes.ServerError))
    End Sub

    <TestMethod>
    Public Sub IsRetryable_Returns_False_For_Unauthorized()
        Assert.IsFalse(ErrorCodes.IsRetryable(ErrorCodes.Unauthorized))
    End Sub

    <TestMethod>
    Public Sub IsRetryable_Returns_False_For_ValidationError()
        Assert.IsFalse(ErrorCodes.IsRetryable(ErrorCodes.ValidationError))
    End Sub

    <TestMethod>
    Public Sub GetMessage_Returns_Message_For_Each_Code()
        Assert.IsNotNull(ErrorCodes.GetMessage(ErrorCodes.Unauthorized))
        Assert.IsNotNull(ErrorCodes.GetMessage(ErrorCodes.Forbidden))
        Assert.IsNotNull(ErrorCodes.GetMessage(ErrorCodes.NotFound))
        Assert.IsNotNull(ErrorCodes.GetMessage(ErrorCodes.ValidationError))
        Assert.IsNotNull(ErrorCodes.GetMessage(ErrorCodes.NetworkError))
        Assert.IsNotNull(ErrorCodes.GetMessage(ErrorCodes.Timeout))
        Assert.IsNotNull(ErrorCodes.GetMessage(ErrorCodes.ServerError))
        Assert.IsNotNull(ErrorCodes.GetMessage(ErrorCodes.Unknown))
    End Sub

End Class
