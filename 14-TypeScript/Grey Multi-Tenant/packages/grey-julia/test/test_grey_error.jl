using Test
using GreySDK.GreyError
using GreySDK.ErrorCodes

@testset "GreyError" begin
    @testset "Error construction" begin
        err = Error(UNAUTHORIZED, "Auth required")
        @test err.code == :unauthorized
        @test err.message == "Auth required"
        @test err.details === nothing
        
        err_with_details = Error(VALIDATION_ERROR, "Invalid", Dict{String,Any}("field" => "name"))
        @test err_with_details.code == :validation_error
        @test err_with_details.details["field"] == "name"
    end
    
    @testset "Error factories" begin
        @test unauthorized().code == :unauthorized
        @test unauthorized("Custom").message == "Custom"
        
        @test forbidden().code == :forbidden
        @test not_found().code == :not_found
        @test validation("Error").code == :validation_error
        @test network().code == :network_error
        @test timeout().code == :timeout
        @test server().code == :server_error
        @test unknown().code == :unknown
    end
    
    @testset "from_response" begin
        err = from_response(401, Dict("message" => "Token expired"))
        @test err.code == :unauthorized
        @test err.message == "Token expired"
        
        err_no_body = from_response(500, nothing)
        @test err_no_body.code == :server_error
        @test err_no_body.message == "HTTP error 500"
        
        err_with_details = from_response(400, Dict(
            "message" => "Validation failed",
            "details" => Dict("field" => "email")
        ))
        @test err_with_details.code == :validation_error
        @test err_with_details.details["field"] == "email"
    end
    
    @testset "from_exception" begin
        timeout_err = from_exception(ErrorException("Request timeout occurred"))
        @test timeout_err.code == :timeout
        
        network_err = from_exception(ErrorException("Connection refused"))
        @test network_err.code == :network_error
        
        unknown_err = from_exception(ErrorException("Something broke"))
        @test unknown_err.code == :unknown
    end
end
