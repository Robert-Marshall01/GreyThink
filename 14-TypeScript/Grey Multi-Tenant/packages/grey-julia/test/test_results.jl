using Test
using GreySDK.Results
using GreySDK.GreyError

@testset "Results" begin
    @testset "ok result" begin
        result = Results.ok("success")
        @test is_ok(result) == true
        @test is_err(result) == false
        @test result.data == "success"
        @test result.error === nothing
    end
    
    @testset "err result" begin
        error = GreyError.unauthorized("Not authenticated")
        result = Results.err(error)
        @test is_ok(result) == false
        @test is_err(result) == true
        @test result.data === nothing
        @test result.error.code == :unauthorized
        
        # From string
        result_str = Results.err("Something went wrong")
        @test is_err(result_str) == true
        @test result_str.error.code == :unknown
    end
    
    @testset "unwrap" begin
        ok_result = Results.ok(42)
        @test unwrap(ok_result) == 42
        
        err_result = Results.err(GreyError.forbidden())
        @test_throws ErrorException unwrap(err_result)
    end
    
    @testset "unwrap_or" begin
        ok_result = Results.ok(42)
        @test unwrap_or(ok_result, 0) == 42
        
        err_result = Results.err(GreyError.forbidden())
        @test unwrap_or(err_result, 0) == 0
    end
    
    @testset "map_result" begin
        ok_result = Results.ok(10)
        mapped = map_result(ok_result, x -> x * 2)
        @test is_ok(mapped) == true
        @test mapped.data == 20
        
        err_result = Results.err(GreyError.forbidden())
        mapped_err = map_result(err_result, x -> x * 2)
        @test is_err(mapped_err) == true
        
        # Exception in map
        throws = map_result(ok_result, x -> error("boom"))
        @test is_err(throws) == true
    end
    
    @testset "then_result" begin
        ok_result = Results.ok(10)
        chained = then_result(ok_result, x -> Results.ok(x + 5))
        @test is_ok(chained) == true
        @test chained.data == 15
        
        # Chain returns non-Result
        chained2 = then_result(ok_result, x -> x + 5)
        @test is_ok(chained2) == true
        @test chained2.data == 15
        
        # Chain on error
        err_result = Results.err(GreyError.forbidden())
        chained_err = then_result(err_result, x -> Results.ok(x + 5))
        @test is_err(chained_err) == true
    end
    
    @testset "catch_result" begin
        err_result = Results.err(GreyError.not_found())
        caught = catch_result(err_result, e -> Results.ok("default"))
        @test is_ok(caught) == true
        @test caught.data == "default"
        
        ok_result = Results.ok(42)
        not_caught = catch_result(ok_result, e -> Results.ok("default"))
        @test is_ok(not_caught) == true
        @test not_caught.data == 42
    end
end
