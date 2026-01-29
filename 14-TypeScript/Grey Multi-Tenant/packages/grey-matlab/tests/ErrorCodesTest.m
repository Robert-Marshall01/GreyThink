classdef ErrorCodesTest < matlab.unittest.TestCase
    %ERRORCODESTEST Tests for grey.error.ErrorCodes
    
    methods (Test)
        function testFromHttpStatus401(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(401);
            testCase.assertEqual(code, grey.error.ErrorCodes.UNAUTHORIZED);
        end
        
        function testFromHttpStatus403(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(403);
            testCase.assertEqual(code, grey.error.ErrorCodes.FORBIDDEN);
        end
        
        function testFromHttpStatus404(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(404);
            testCase.assertEqual(code, grey.error.ErrorCodes.NOT_FOUND);
        end
        
        function testFromHttpStatus400(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(400);
            testCase.assertEqual(code, grey.error.ErrorCodes.VALIDATION_ERROR);
        end
        
        function testFromHttpStatus422(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(422);
            testCase.assertEqual(code, grey.error.ErrorCodes.VALIDATION_ERROR);
        end
        
        function testFromHttpStatus408(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(408);
            testCase.assertEqual(code, grey.error.ErrorCodes.TIMEOUT);
        end
        
        function testFromHttpStatus504(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(504);
            testCase.assertEqual(code, grey.error.ErrorCodes.TIMEOUT);
        end
        
        function testFromHttpStatus500(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(500);
            testCase.assertEqual(code, grey.error.ErrorCodes.SERVER_ERROR);
        end
        
        function testFromHttpStatus502(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(502);
            testCase.assertEqual(code, grey.error.ErrorCodes.SERVER_ERROR);
        end
        
        function testFromHttpStatus200(testCase)
            code = grey.error.ErrorCodes.fromHttpStatus(200);
            testCase.assertEqual(code, grey.error.ErrorCodes.UNKNOWN);
        end
        
        function testIsValidTrue(testCase)
            testCase.assertTrue(grey.error.ErrorCodes.isValid(grey.error.ErrorCodes.UNAUTHORIZED));
            testCase.assertTrue(grey.error.ErrorCodes.isValid(grey.error.ErrorCodes.FORBIDDEN));
            testCase.assertTrue(grey.error.ErrorCodes.isValid(grey.error.ErrorCodes.NOT_FOUND));
        end
        
        function testIsValidFalse(testCase)
            testCase.assertFalse(grey.error.ErrorCodes.isValid('invalid_code'));
        end
        
        function testNormalizeValid(testCase)
            code = grey.error.ErrorCodes.normalize(grey.error.ErrorCodes.UNAUTHORIZED);
            testCase.assertEqual(code, grey.error.ErrorCodes.UNAUTHORIZED);
        end
        
        function testNormalizeInvalid(testCase)
            code = grey.error.ErrorCodes.normalize('invalid');
            testCase.assertEqual(code, grey.error.ErrorCodes.UNKNOWN);
        end
    end
end
