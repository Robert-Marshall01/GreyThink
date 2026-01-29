using Test
using GreySDK.ErrorCodes

@testset "ErrorCodes" begin
    @testset "from_http_status" begin
        @test from_http_status(401) == UNAUTHORIZED
        @test from_http_status(403) == FORBIDDEN
        @test from_http_status(404) == NOT_FOUND
        @test from_http_status(400) == VALIDATION_ERROR
        @test from_http_status(422) == VALIDATION_ERROR
        @test from_http_status(408) == TIMEOUT
        @test from_http_status(504) == TIMEOUT
        @test from_http_status(500) == SERVER_ERROR
        @test from_http_status(502) == SERVER_ERROR
        @test from_http_status(503) == SERVER_ERROR
        @test from_http_status(200) == UNKNOWN
        @test from_http_status(301) == UNKNOWN
    end
    
    @testset "is_valid" begin
        @test is_valid(UNAUTHORIZED) == true
        @test is_valid(FORBIDDEN) == true
        @test is_valid(NOT_FOUND) == true
        @test is_valid(VALIDATION_ERROR) == true
        @test is_valid(NETWORK_ERROR) == true
        @test is_valid(TIMEOUT) == true
        @test is_valid(SERVER_ERROR) == true
        @test is_valid(UNKNOWN) == true
        @test is_valid(:invalid_code) == false
    end
    
    @testset "normalize" begin
        @test normalize(UNAUTHORIZED) == UNAUTHORIZED
        @test normalize(:invalid_code) == UNKNOWN
    end
end
