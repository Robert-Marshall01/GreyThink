classdef ResultTest < matlab.unittest.TestCase
    %RESULTTEST Tests for grey.error.Result
    
    methods (Test)
        function testSuccessResult(testCase)
            result = grey.error.Result.success('data');
            testCase.assertTrue(result.isOk());
            testCase.assertFalse(result.isErr());
            testCase.assertEqual(result.unwrap(), 'data');
        end
        
        function testFailureResult(testCase)
            err = grey.error.GreyError.unauthorized();
            result = grey.error.Result.failure(err);
            testCase.assertFalse(result.isOk());
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'unauthorized');
        end
        
        function testFailureFromString(testCase)
            result = grey.error.Result.failure('Something went wrong');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'unknown');
        end
        
        function testUnwrapSuccess(testCase)
            result = grey.error.Result.success(42);
            testCase.assertEqual(result.unwrap(), 42);
        end
        
        function testUnwrapError(testCase)
            result = grey.error.Result.failure(grey.error.GreyError.forbidden());
            testCase.assertError(@() result.unwrap(), 'Grey:Result:UnwrapError');
        end
        
        function testUnwrapOrSuccess(testCase)
            result = grey.error.Result.success(42);
            testCase.assertEqual(result.unwrapOr(0), 42);
        end
        
        function testUnwrapOrFailure(testCase)
            result = grey.error.Result.failure(grey.error.GreyError.forbidden());
            testCase.assertEqual(result.unwrapOr(0), 0);
        end
        
        function testMapSuccess(testCase)
            result = grey.error.Result.success(10);
            mapped = result.map(@(x) x * 2);
            testCase.assertTrue(mapped.isOk());
            testCase.assertEqual(mapped.unwrap(), 20);
        end
        
        function testMapOnError(testCase)
            result = grey.error.Result.failure(grey.error.GreyError.forbidden());
            mapped = result.map(@(x) x * 2);
            testCase.assertTrue(mapped.isErr());
        end
        
        function testAndThenSuccess(testCase)
            result = grey.error.Result.success(10);
            chained = result.andThen(@(x) grey.error.Result.success(x + 5));
            testCase.assertTrue(chained.isOk());
            testCase.assertEqual(chained.unwrap(), 15);
        end
        
        function testAndThenReturnsNonResult(testCase)
            result = grey.error.Result.success(10);
            chained = result.andThen(@(x) x + 5);
            testCase.assertTrue(chained.isOk());
            testCase.assertEqual(chained.unwrap(), 15);
        end
        
        function testAndThenOnError(testCase)
            result = grey.error.Result.failure(grey.error.GreyError.forbidden());
            chained = result.andThen(@(x) grey.error.Result.success(x + 5));
            testCase.assertTrue(chained.isErr());
        end
        
        function testOrElseOnError(testCase)
            result = grey.error.Result.failure(grey.error.GreyError.notFound());
            caught = result.orElse(@(err) grey.error.Result.success('default'));
            testCase.assertTrue(caught.isOk());
            testCase.assertEqual(caught.unwrap(), 'default');
        end
        
        function testOrElseOnSuccess(testCase)
            result = grey.error.Result.success(42);
            caught = result.orElse(@(err) grey.error.Result.success('default'));
            testCase.assertTrue(caught.isOk());
            testCase.assertEqual(caught.unwrap(), 42);
        end
    end
end
