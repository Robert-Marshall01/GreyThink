classdef MutationClient < handle
    %MUTATIONCLIENT GraphQL-style mutation client for Grey SDK
    %   Handles mutation operations.
    
    properties (SetAccess = private)
        httpClient  % grey.http.HttpClient
    end
    
    methods
        function obj = MutationClient(httpClient)
            %MUTATIONCLIENT Construct mutation client
            %   client = MutationClient(httpClient)
            
            obj.httpClient = httpClient;
        end
        
        function result = execute(obj, mutationString, variables, operationName)
            %EXECUTE Execute a single mutation
            %   result = client.execute(mutationString)
            %   result = client.execute(mutationString, variables)
            %   result = client.execute(mutationString, variables, operationName)
            
            if nargin < 3
                variables = [];
            end
            if nargin < 4
                operationName = '';
            end
            
            % Validate inputs
            if isempty(strtrim(mutationString))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Mutation string is required'));
                return;
            end
            
            body = struct('mutation', mutationString);
            
            if ~isempty(variables) && isstruct(variables)
                body.variables = variables;
            end
            
            if ~isempty(operationName)
                body.operationName = operationName;
            end
            
            result = obj.httpClient.post('/graphql/mutate', body);
        end
        
        function result = batch(obj, mutations)
            %BATCH Execute multiple mutations in a single request
            %   result = client.batch(mutations)
            %
            %   mutations is a cell array of structs, each with:
            %   - mutation: Mutation string (required)
            %   - variables: Variables struct (optional)
            %   - operationName: Operation name (optional)
            
            % Validate inputs
            if isempty(mutations)
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('At least one mutation is required'));
                return;
            end
            
            % Validate each mutation
            if iscell(mutations)
                for i = 1:length(mutations)
                    m = mutations{i};
                    if ~isstruct(m) || ~isfield(m, 'mutation') || isempty(strtrim(m.mutation))
                        result = grey.error.Result.failure(...
                            grey.error.GreyError.validation(...
                                sprintf('Mutation %d must have a non-empty ''mutation'' field', i)));
                        return;
                    end
                end
            end
            
            body = struct('mutations', {mutations});
            result = obj.httpClient.post('/graphql/mutate/batch', body);
        end
    end
end
