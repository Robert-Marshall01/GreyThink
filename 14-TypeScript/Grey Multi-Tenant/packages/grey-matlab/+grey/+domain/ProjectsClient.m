classdef ProjectsClient < handle
    %PROJECTSCLIENT Projects client for Grey SDK
    %   Handles project-related operations.
    
    properties (SetAccess = private)
        httpClient  % grey.http.HttpClient
    end
    
    methods
        function obj = ProjectsClient(httpClient)
            %PROJECTSCLIENT Construct projects client
            %   client = ProjectsClient(httpClient)
            
            obj.httpClient = httpClient;
        end
        
        function result = list(obj, options)
            %LIST List all projects with optional pagination
            %   result = client.list()
            %   result = client.list(options)
            %
            %   Options struct fields:
            %   - page: Page number (default: 1)
            %   - perPage: Items per page (default: 20)
            %   - sortBy: Sort field (default: 'created_at')
            %   - sortOrder: Sort order (default: 'desc')
            
            if nargin < 2
                options = struct();
            end
            
            % Set defaults
            page = 1;
            perPage = 20;
            sortBy = 'created_at';
            sortOrder = 'desc';
            
            if isfield(options, 'page')
                page = options.page;
            end
            if isfield(options, 'perPage')
                perPage = options.perPage;
            end
            if isfield(options, 'sortBy')
                sortBy = options.sortBy;
            end
            if isfield(options, 'sortOrder')
                sortOrder = options.sortOrder;
            end
            
            % Validate
            if page < 1
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Page must be >= 1'));
                return;
            end
            
            if perPage < 1 || perPage > 100
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Per page must be between 1 and 100'));
                return;
            end
            
            query = struct(...
                'page', page, ...
                'per_page', perPage, ...
                'sort_by', sortBy, ...
                'sort_order', sortOrder);
            
            result = obj.httpClient.get('/projects', query);
        end
        
        function result = get(obj, projectId)
            %GET Get a project by ID
            %   result = client.get(projectId)
            
            % Validate inputs
            if isempty(strtrim(projectId))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Project ID is required'));
                return;
            end
            
            path = sprintf('/projects/%s', projectId);
            result = obj.httpClient.get(path);
        end
        
        function result = create(obj, name, description, metadata)
            %CREATE Create a new project
            %   result = client.create(name)
            %   result = client.create(name, description)
            %   result = client.create(name, description, metadata)
            
            if nargin < 3
                description = '';
            end
            if nargin < 4
                metadata = [];
            end
            
            % Validate inputs
            if isempty(strtrim(name))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Project name is required'));
                return;
            end
            
            body = struct('name', name);
            
            if ~isempty(description)
                body.description = description;
            end
            
            if ~isempty(metadata) && isstruct(metadata)
                body.metadata = metadata;
            end
            
            result = obj.httpClient.post('/projects', body);
        end
        
        function result = update(obj, projectId, updates)
            %UPDATE Update an existing project
            %   result = client.update(projectId, updates)
            %
            %   Updates struct fields:
            %   - name: New name
            %   - description: New description
            %   - metadata: New metadata
            
            % Validate inputs
            if isempty(strtrim(projectId))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Project ID is required'));
                return;
            end
            
            if ~isstruct(updates) || isempty(fieldnames(updates))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation(...
                        'At least one field must be provided for update'));
                return;
            end
            
            path = sprintf('/projects/%s', projectId);
            result = obj.httpClient.patch(path, updates);
        end
        
        function result = delete(obj, projectId)
            %DELETE Delete a project by ID
            %   result = client.delete(projectId)
            
            % Validate inputs
            if isempty(strtrim(projectId))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Project ID is required'));
                return;
            end
            
            path = sprintf('/projects/%s', projectId);
            result = obj.httpClient.delete(path);
        end
    end
end
