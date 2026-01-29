classdef QueryClient < handle
    %QUERYCLIENT GraphQL-style query client for Grey SDK
    %   Handles query operations.
    
    properties (SetAccess = private)
        httpClient  % grey.http.HttpClient
    end
    
    methods
        function obj = QueryClient(httpClient)
            %QUERYCLIENT Construct query client
            %   client = QueryClient(httpClient)
            
            obj.httpClient = httpClient;
        end
        
        function result = execute(obj, queryString, variables, operationName)
            %EXECUTE Execute a single query
            %   result = client.execute(queryString)
            %   result = client.execute(queryString, variables)
            %   result = client.execute(queryString, variables, operationName)
            
            if nargin < 3
                variables = [];
            end
            if nargin < 4
                operationName = '';
            end
            
            % Validate inputs
            if isempty(strtrim(queryString))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Query string is required'));
                return;
            end
            
            body = struct('query', queryString);
            
            if ~isempty(variables) && isstruct(variables)
                body.variables = variables;
            end
            
            if ~isempty(operationName)
                body.operationName = operationName;
            end
            
            result = obj.httpClient.post('/graphql', body);
        end
        
        function result = batch(obj, queries)
            %BATCH Execute multiple queries in a single request
            %   result = client.batch(queries)
            %
            %   queries is a cell array of structs, each with:
            %   - query: Query string (required)
            %   - variables: Variables struct (optional)
            %   - operationName: Operation name (optional)
            
            % Validate inputs
            if isempty(queries)
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('At least one query is required'));
                return;
            end
            
            % Validate each query
            if iscell(queries)
                for i = 1:length(queries)
                    q = queries{i};
                    if ~isstruct(q) || ~isfield(q, 'query') || isempty(strtrim(q.query))
                        result = grey.error.Result.failure(...
                            grey.error.GreyError.validation(...
                                sprintf('Query %d must have a non-empty ''query'' field', i)));
                        return;
                    end
                end
            end
            
            body = struct('queries', {queries});
            result = obj.httpClient.post('/graphql/batch', body);
        end
    end
end
