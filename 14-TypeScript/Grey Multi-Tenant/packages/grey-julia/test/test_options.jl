using Test
using GreySDK.Options

@testset "Options" begin
    @testset "GreyOptions construction" begin
        opts = GreyOptions(host="api.grey.com")
        @test opts.host == "api.grey.com"
        @test opts.port == 443
        @test opts.use_tls == true
        @test opts.timeout == 30
        @test isempty(opts.headers)
        
        opts_custom = GreyOptions(
            host="localhost",
            port=8080,
            use_tls=false,
            timeout=60,
            headers=Dict("X-Custom" => "value")
        )
        @test opts_custom.host == "localhost"
        @test opts_custom.port == 8080
        @test opts_custom.use_tls == false
        @test opts_custom.timeout == 60
        @test opts_custom.headers["X-Custom"] == "value"
    end
    
    @testset "local_options" begin
        opts = local_options()
        @test opts.host == "localhost"
        @test opts.port == 8080
        @test opts.use_tls == false
        
        opts_custom_port = local_options(3000)
        @test opts_custom_port.port == 3000
    end
    
    @testset "production_options" begin
        opts = production_options("api.grey.com")
        @test opts.host == "api.grey.com"
        @test opts.port == 443
        @test opts.use_tls == true
        
        opts_custom_port = production_options("api.grey.com", 8443)
        @test opts_custom_port.port == 8443
    end
    
    @testset "base_url" begin
        local_opts = local_options(8080)
        @test base_url(local_opts) == "http://localhost:8080"
        
        prod_opts = production_options("api.grey.com")
        @test base_url(prod_opts) == "https://api.grey.com:443"
    end
    
    @testset "with_timeout" begin
        opts = local_options()
        new_opts = with_timeout(opts, 120)
        @test new_opts.timeout == 120
        @test opts.timeout == 30  # Original unchanged
    end
    
    @testset "with_headers" begin
        opts = local_options()
        new_opts = with_headers(opts, Dict("Authorization" => "Bearer token"))
        @test new_opts.headers["Authorization"] == "Bearer token"
        @test isempty(opts.headers)  # Original unchanged
    end
end
