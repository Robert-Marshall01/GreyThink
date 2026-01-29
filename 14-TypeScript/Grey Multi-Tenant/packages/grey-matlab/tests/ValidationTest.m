classdef ValidationTest < matlab.unittest.TestCase
    %VALIDATIONTEST Tests for input validation in domain clients
    
    properties
        httpClient
    end
    
    methods (TestMethodSetup)
        function setup(testCase)
            opts = grey.config.Options.local(8080);
            testCase.httpClient = grey.http.HttpClient(opts);
        end
    end
    
    methods (Test)
        % AuthClient validation
        function testLoginEmptyUsername(testCase)
            authClient = grey.domain.AuthClient(testCase.httpClient);
            result = authClient.login('', 'password');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
            testCase.assertSubstring(result.error().message, 'Username');
        end
        
        function testLoginEmptyPassword(testCase)
            authClient = grey.domain.AuthClient(testCase.httpClient);
            result = authClient.login('user', '');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
            testCase.assertSubstring(result.error().message, 'Password');
        end
        
        function testRefreshEmptyToken(testCase)
            authClient = grey.domain.AuthClient(testCase.httpClient);
            result = authClient.refresh('');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        % UserClient validation
        function testGetUserEmptyId(testCase)
            userClient = grey.domain.UserClient(testCase.httpClient);
            result = userClient.get('');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
            testCase.assertSubstring(result.error().message, 'User ID');
        end
        
        % ProjectsClient validation
        function testGetProjectEmptyId(testCase)
            projectsClient = grey.domain.ProjectsClient(testCase.httpClient);
            result = projectsClient.get('');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        function testCreateProjectEmptyName(testCase)
            projectsClient = grey.domain.ProjectsClient(testCase.httpClient);
            result = projectsClient.create('');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
            testCase.assertSubstring(result.error().message, 'name');
        end
        
        function testListProjectsInvalidPage(testCase)
            projectsClient = grey.domain.ProjectsClient(testCase.httpClient);
            result = projectsClient.list(struct('page', 0));
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        function testListProjectsInvalidPerPage(testCase)
            projectsClient = grey.domain.ProjectsClient(testCase.httpClient);
            result = projectsClient.list(struct('perPage', 200));
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        function testUpdateProjectEmptyId(testCase)
            projectsClient = grey.domain.ProjectsClient(testCase.httpClient);
            result = projectsClient.update('', struct('name', 'New Name'));
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        function testUpdateProjectEmptyUpdates(testCase)
            projectsClient = grey.domain.ProjectsClient(testCase.httpClient);
            result = projectsClient.update('proj-1', struct());
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        function testDeleteProjectEmptyId(testCase)
            projectsClient = grey.domain.ProjectsClient(testCase.httpClient);
            result = projectsClient.delete('');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        % QueryClient validation
        function testQueryEmptyString(testCase)
            queryClient = grey.domain.QueryClient(testCase.httpClient);
            result = queryClient.execute('');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        function testBatchQueryEmpty(testCase)
            queryClient = grey.domain.QueryClient(testCase.httpClient);
            result = queryClient.batch({});
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        function testBatchQueryInvalidItem(testCase)
            queryClient = grey.domain.QueryClient(testCase.httpClient);
            result = queryClient.batch({struct('query', '')});
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        % MutationClient validation
        function testMutationEmptyString(testCase)
            mutationClient = grey.domain.MutationClient(testCase.httpClient);
            result = mutationClient.execute('');
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        function testBatchMutationEmpty(testCase)
            mutationClient = grey.domain.MutationClient(testCase.httpClient);
            result = mutationClient.batch({});
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
        
        function testBatchMutationInvalidItem(testCase)
            mutationClient = grey.domain.MutationClient(testCase.httpClient);
            result = mutationClient.batch({struct('mutation', '')});
            testCase.assertTrue(result.isErr());
            testCase.assertEqual(result.error().code, 'validation_error');
        end
    end
end
