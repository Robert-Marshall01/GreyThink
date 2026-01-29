using Test
using GreySDK
using GreySDK.Results
using GreySDK.AuthClient
using GreySDK.UserClient
using GreySDK.ProjectsClient
using GreySDK.QueryClient
using GreySDK.MutationClient
using GreySDK.HttpClient

@testset "Input Validation" begin
    # Create a client for validation tests
    client = HttpClient.Client(GreySDK.local_options(8080))
    
    @testset "AuthClient validation" begin
        # Empty username
        result = AuthClient.login(client, "", "password")
        @test is_err(result)
        @test result.error.code == :validation_error
        @test occursin("Username", result.error.message)
        
        # Empty password
        result = AuthClient.login(client, "user", "")
        @test is_err(result)
        @test result.error.code == :validation_error
        @test occursin("Password", result.error.message)
        
        # Empty refresh token
        result = AuthClient.refresh(client, "")
        @test is_err(result)
        @test result.error.code == :validation_error
    end
    
    @testset "UserClient validation" begin
        # Empty user ID
        result = UserClient.get_user(client, "")
        @test is_err(result)
        @test result.error.code == :validation_error
        @test occursin("User ID", result.error.message)
    end
    
    @testset "ProjectsClient validation" begin
        # Empty project ID
        result = ProjectsClient.get_project(client, "")
        @test is_err(result)
        @test result.error.code == :validation_error
        
        # Empty project name
        result = ProjectsClient.create_project(client, "")
        @test is_err(result)
        @test result.error.code == :validation_error
        
        # Invalid pagination
        result = ProjectsClient.list_projects(client; page=0)
        @test is_err(result)
        @test result.error.code == :validation_error
        
        result = ProjectsClient.list_projects(client; per_page=200)
        @test is_err(result)
        @test result.error.code == :validation_error
        
        # Empty update (no fields)
        result = ProjectsClient.update_project(client, "proj-1")
        @test is_err(result)
        @test result.error.code == :validation_error
    end
    
    @testset "QueryClient validation" begin
        # Empty query
        result = QueryClient.query(client, "")
        @test is_err(result)
        @test result.error.code == :validation_error
        
        # Empty batch
        result = QueryClient.batch_query(client, [])
        @test is_err(result)
        @test result.error.code == :validation_error
        
        # Invalid batch query
        result = QueryClient.batch_query(client, [Dict("query" => "")])
        @test is_err(result)
        @test result.error.code == :validation_error
    end
    
    @testset "MutationClient validation" begin
        # Empty mutation
        result = MutationClient.mutate(client, "")
        @test is_err(result)
        @test result.error.code == :validation_error
        
        # Empty batch
        result = MutationClient.batch_mutate(client, [])
        @test is_err(result)
        @test result.error.code == :validation_error
        
        # Invalid batch mutation
        result = MutationClient.batch_mutate(client, [Dict("mutation" => "")])
        @test is_err(result)
        @test result.error.code == :validation_error
    end
end
