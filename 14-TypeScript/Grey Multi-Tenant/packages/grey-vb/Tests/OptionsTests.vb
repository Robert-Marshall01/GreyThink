' Grey SDK for Visual Basic - Options Tests

Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports Grey.Config

<TestClass>
Public Class OptionsTests

    <TestMethod>
    Public Sub Local_Creates_Options_With_Defaults()
        Dim options = Options.Local()
        
        Assert.AreEqual("localhost", options.Host)
        Assert.AreEqual(8080, options.Port)
        Assert.IsFalse(options.UseTls)
        Assert.AreEqual(30, options.TimeoutSeconds)
    End Sub

    <TestMethod>
    Public Sub Local_Accepts_Custom_Port()
        Dim options = Options.Local(3000)
        
        Assert.AreEqual("localhost", options.Host)
        Assert.AreEqual(3000, options.Port)
    End Sub

    <TestMethod>
    Public Sub Production_Creates_Options_With_TLS()
        Dim options = Options.Production("api.example.com")
        
        Assert.AreEqual("api.example.com", options.Host)
        Assert.AreEqual(443, options.Port)
        Assert.IsTrue(options.UseTls)
    End Sub

    <TestMethod>
    Public Sub Production_Accepts_Custom_Port()
        Dim options = Options.Production("api.example.com", 8443)
        
        Assert.AreEqual("api.example.com", options.Host)
        Assert.AreEqual(8443, options.Port)
    End Sub

    <TestMethod>
    Public Sub GetBaseUrl_Returns_Http_For_Non_TLS()
        Dim options = Options.Local(8080)
        Assert.AreEqual("http://localhost:8080", options.GetBaseUrl())
    End Sub

    <TestMethod>
    Public Sub GetBaseUrl_Returns_Https_For_TLS()
        Dim options = Options.Production("api.example.com")
        Assert.AreEqual("https://api.example.com:443", options.GetBaseUrl())
    End Sub

    <TestMethod>
    Public Sub WithAuthToken_Returns_New_Options_With_Token()
        Dim original = Options.Local()
        Dim withToken = original.WithAuthToken("test-token")
        
        ' Original should be unchanged
        Assert.IsNull(original.AuthToken)
        
        ' New options should have token
        Assert.AreEqual("test-token", withToken.AuthToken)
        
        ' Other properties should be preserved
        Assert.AreEqual(original.Host, withToken.Host)
        Assert.AreEqual(original.Port, withToken.Port)
    End Sub

    <TestMethod>
    Public Sub WithTimeout_Returns_New_Options_With_Timeout()
        Dim original = Options.Local()
        Dim withTimeout = original.WithTimeout(60)
        
        ' Original should be unchanged
        Assert.AreEqual(30, original.TimeoutSeconds)
        
        ' New options should have new timeout
        Assert.AreEqual(60, withTimeout.TimeoutSeconds)
    End Sub

    <TestMethod>
    Public Sub WithHeader_Returns_New_Options_With_Header()
        Dim original = Options.Local()
        Dim withHeader = original.WithHeader("X-Custom", "value")
        
        ' Original should be unchanged
        Assert.IsFalse(original.CustomHeaders.ContainsKey("X-Custom"))
        
        ' New options should have header
        Assert.IsTrue(withHeader.CustomHeaders.ContainsKey("X-Custom"))
        Assert.AreEqual("value", withHeader.CustomHeaders("X-Custom"))
    End Sub

End Class
