' Grey SDK for Visual Basic - Validation Tests

Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports Grey
Imports Grey.Config
Imports Grey.Domain

<TestClass>
Public Class ValidationTests

    Private _client As GreyClient

    <TestInitialize>
    Public Sub Setup()
        _client = GreyClient.Local()
    End Sub

    <TestCleanup>
    Public Sub Cleanup()
        _client?.Dispose()
    End Sub

    ' Auth Validation Tests

    <TestMethod>
    Public Async Function Auth_Login_Validates_Empty_Username() As Task
        Dim result = Await _client.Auth.LoginAsync("", "password")
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("Username"))
    End Function

    <TestMethod>
    Public Async Function Auth_Login_Validates_Empty_Password() As Task
        Dim result = Await _client.Auth.LoginAsync("user", "")
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("Password"))
    End Function

    <TestMethod>
    Public Async Function Auth_Refresh_Validates_Empty_Token() As Task
        Dim result = Await _client.Auth.RefreshAsync("")
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("Refresh token"))
    End Function

    ' User Validation Tests

    <TestMethod>
    Public Async Function User_GetUser_Validates_Empty_Id() As Task
        Dim result = Await _client.Users.GetUserAsync("")
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("User ID"))
    End Function

    ' Projects Validation Tests

    <TestMethod>
    Public Async Function Projects_List_Validates_Invalid_Page() As Task
        Dim options = New ListProjectsOptions() With {.Page = 0}
        Dim result = Await _client.Projects.ListProjectsAsync(options)
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("Page"))
    End Function

    <TestMethod>
    Public Async Function Projects_List_Validates_Invalid_PerPage() As Task
        Dim options = New ListProjectsOptions() With {.PerPage = 0}
        Dim result = Await _client.Projects.ListProjectsAsync(options)
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("PerPage"))
    End Function

    <TestMethod>
    Public Async Function Projects_Get_Validates_Empty_Id() As Task
        Dim result = Await _client.Projects.GetProjectAsync("")
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("Project ID"))
    End Function

    <TestMethod>
    Public Async Function Projects_Create_Validates_Null_Data() As Task
        Dim result = Await _client.Projects.CreateProjectAsync(Nothing)
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
    End Function

    <TestMethod>
    Public Async Function Projects_Create_Validates_Empty_Name() As Task
        Dim data = New CreateProjectData() With {.Name = ""}
        Dim result = Await _client.Projects.CreateProjectAsync(data)
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("name"))
    End Function

    <TestMethod>
    Public Async Function Projects_Delete_Validates_Empty_Id() As Task
        Dim result = Await _client.Projects.DeleteProjectAsync("")
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
    End Function

    ' Query Validation Tests

    <TestMethod>
    Public Async Function Query_Validates_Empty_Query() As Task
        Dim result = Await _client.Queries.QueryAsync("")
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("Query"))
    End Function

    <TestMethod>
    Public Async Function Query_Batch_Validates_Empty_List() As Task
        Dim result = Await _client.Queries.BatchQueryAsync(New List(Of QueryRequest)())
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
    End Function

    ' Mutation Validation Tests

    <TestMethod>
    Public Async Function Mutation_Validates_Empty_Mutation() As Task
        Dim result = Await _client.Mutations.MutateAsync("")
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
        Assert.IsTrue(result.Error.Message.Contains("Mutation"))
    End Function

    <TestMethod>
    Public Async Function Mutation_Batch_Validates_Empty_List() As Task
        Dim result = Await _client.Mutations.BatchMutateAsync(New List(Of MutationRequest)())
        
        Assert.IsTrue(result.IsFailure)
        Assert.AreEqual("validation_error", result.Error.Code)
    End Function

End Class
