classdef OptionsTest < matlab.unittest.TestCase
    %OPTIONSTEST Tests for grey.config.Options
    
    methods (Test)
        function testDefaultConstruction(testCase)
            opts = grey.config.Options('api.grey.com');
            testCase.assertEqual(opts.host, 'api.grey.com');
            testCase.assertEqual(opts.port, 443);
            testCase.assertTrue(opts.useTls);
            testCase.assertEqual(opts.timeout, 30);
        end
        
        function testFullConstruction(testCase)
            headers = struct('X_Custom', 'value');
            opts = grey.config.Options('localhost', 8080, false, 60, headers);
            testCase.assertEqual(opts.host, 'localhost');
            testCase.assertEqual(opts.port, 8080);
            testCase.assertFalse(opts.useTls);
            testCase.assertEqual(opts.timeout, 60);
            testCase.assertEqual(opts.headers.X_Custom, 'value');
        end
        
        function testLocalFactory(testCase)
            opts = grey.config.Options.local();
            testCase.assertEqual(opts.host, 'localhost');
            testCase.assertEqual(opts.port, 8080);
            testCase.assertFalse(opts.useTls);
        end
        
        function testLocalFactoryCustomPort(testCase)
            opts = grey.config.Options.local(3000);
            testCase.assertEqual(opts.port, 3000);
        end
        
        function testProductionFactory(testCase)
            opts = grey.config.Options.production('api.grey.com');
            testCase.assertEqual(opts.host, 'api.grey.com');
            testCase.assertEqual(opts.port, 443);
            testCase.assertTrue(opts.useTls);
        end
        
        function testProductionFactoryCustomPort(testCase)
            opts = grey.config.Options.production('api.grey.com', 8443);
            testCase.assertEqual(opts.port, 8443);
        end
        
        function testBaseUrlHttp(testCase)
            opts = grey.config.Options.local(8080);
            testCase.assertEqual(opts.baseUrl(), 'http://localhost:8080');
        end
        
        function testBaseUrlHttps(testCase)
            opts = grey.config.Options.production('api.grey.com');
            testCase.assertEqual(opts.baseUrl(), 'https://api.grey.com:443');
        end
        
        function testWithTimeout(testCase)
            opts = grey.config.Options.local();
            newOpts = opts.withTimeout(120);
            testCase.assertEqual(newOpts.timeout, 120);
            testCase.assertEqual(opts.timeout, 30); % Original unchanged
        end
        
        function testWithHeaders(testCase)
            opts = grey.config.Options.local();
            newOpts = opts.withHeaders(struct('Authorization', 'Bearer token'));
            testCase.assertEqual(newOpts.headers.Authorization, 'Bearer token');
            testCase.assertEmpty(fieldnames(opts.headers)); % Original unchanged
        end
    end
end
