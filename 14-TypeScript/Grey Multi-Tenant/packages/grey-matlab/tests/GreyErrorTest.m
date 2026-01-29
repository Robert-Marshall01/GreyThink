classdef GreyErrorTest < matlab.unittest.TestCase
    %GREYERRORTEST Tests for grey.error.GreyError
    
    methods (Test)
        function testConstruction(testCase)
            err = grey.error.GreyError(grey.error.ErrorCodes.UNAUTHORIZED, 'Auth required');
            testCase.assertEqual(err.code, 'unauthorized');
            testCase.assertEqual(err.message, 'Auth required');
            testCase.assertEmpty(err.details);
        end
        
        function testConstructionWithDetails(testCase)
            details = struct('field', 'email');
            err = grey.error.GreyError(grey.error.ErrorCodes.VALIDATION_ERROR, 'Invalid', details);
            testCase.assertEqual(err.code, 'validation_error');
            testCase.assertEqual(err.details.field, 'email');
        end
        
        function testUnauthorizedFactory(testCase)
            err = grey.error.GreyError.unauthorized();
            testCase.assertEqual(err.code, 'unauthorized');
            testCase.assertEqual(err.message, 'Authentication required');
        end
        
        function testUnauthorizedFactoryCustomMessage(testCase)
            err = grey.error.GreyError.unauthorized('Token expired');
            testCase.assertEqual(err.message, 'Token expired');
        end
        
        function testForbiddenFactory(testCase)
            err = grey.error.GreyError.forbidden();
            testCase.assertEqual(err.code, 'forbidden');
        end
        
        function testNotFoundFactory(testCase)
            err = grey.error.GreyError.notFound();
            testCase.assertEqual(err.code, 'not_found');
        end
        
        function testValidationFactory(testCase)
            err = grey.error.GreyError.validation('Invalid input');
            testCase.assertEqual(err.code, 'validation_error');
        end
        
        function testNetworkFactory(testCase)
            err = grey.error.GreyError.network();
            testCase.assertEqual(err.code, 'network_error');
        end
        
        function testTimeoutFactory(testCase)
            err = grey.error.GreyError.timeout();
            testCase.assertEqual(err.code, 'timeout');
        end
        
        function testServerFactory(testCase)
            err = grey.error.GreyError.server();
            testCase.assertEqual(err.code, 'server_error');
        end
        
        function testUnknownFactory(testCase)
            err = grey.error.GreyError.unknown();
            testCase.assertEqual(err.code, 'unknown');
        end
        
        function testFromResponse(testCase)
            body = struct('message', 'Token expired');
            err = grey.error.GreyError.fromResponse(401, body);
            testCase.assertEqual(err.code, 'unauthorized');
            testCase.assertEqual(err.message, 'Token expired');
        end
        
        function testFromResponseNoBody(testCase)
            err = grey.error.GreyError.fromResponse(500, struct());
            testCase.assertEqual(err.code, 'server_error');
            testCase.assertEqual(err.message, 'HTTP error 500');
        end
        
        function testFromException(testCase)
            ex = MException('Test:Error', 'Connection refused');
            err = grey.error.GreyError.fromException(ex);
            testCase.assertEqual(err.code, 'network_error');
        end
        
        function testFromExceptionTimeout(testCase)
            ex = MException('Test:Error', 'Request timeout');
            err = grey.error.GreyError.fromException(ex);
            testCase.assertEqual(err.code, 'timeout');
        end
    end
end
