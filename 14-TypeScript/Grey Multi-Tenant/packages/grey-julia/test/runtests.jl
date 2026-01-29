using Test
using GreySDK

@testset "GreySDK Tests" begin
    include("test_error_codes.jl")
    include("test_grey_error.jl")
    include("test_results.jl")
    include("test_options.jl")
    include("test_validation.jl")
end
