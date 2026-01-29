classdef HttpClient < handle
    %HTTPCLIENT HTTP client for Grey SDK using webread/webwrite
    %   Makes HTTP requests to the Grey API.
    
    properties (SetAccess = private)
        options         % grey.config.Options
        authToken char  % Authentication token
    end
    
    methods
        function obj = HttpClient(options)
            %HTTPCLIENT Construct HTTP client
            %   client = HttpClient(options)
            
            obj.options = options;
            obj.authToken = '';
        end
        
        function setAuthToken(obj, token)
            %SETAUTHTOKEN Set the authentication token
            obj.authToken = token;
        end
        
        function clearAuthToken(obj)
            %CLEARAUTHTOKEN Clear the authentication token
            obj.authToken = '';
        end
        
        function result = get(obj, path, query)
            %GET Make a GET request
            %   result = client.get(path)
            %   result = client.get(path, queryParams)
            
            if nargin < 3
                query = struct();
            end
            
            result = obj.request('GET', path, [], query);
        end
        
        function result = post(obj, path, body)
            %POST Make a POST request
            %   result = client.post(path)
            %   result = client.post(path, body)
            
            if nargin < 3
                body = [];
            end
            
            result = obj.request('POST', path, body);
        end
        
        function result = put(obj, path, body)
            %PUT Make a PUT request
            %   result = client.put(path)
            %   result = client.put(path, body)
            
            if nargin < 3
                body = [];
            end
            
            result = obj.request('PUT', path, body);
        end
        
        function result = patch(obj, path, body)
            %PATCH Make a PATCH request
            %   result = client.patch(path)
            %   result = client.patch(path, body)
            
            if nargin < 3
                body = [];
            end
            
            result = obj.request('PATCH', path, body);
        end
        
        function result = delete(obj, path)
            %DELETE Make a DELETE request
            %   result = client.delete(path)
            
            result = obj.request('DELETE', path);
        end
    end
    
    methods (Access = private)
        function url = buildUrl(obj, path)
            %BUILDURL Build full URL from path
            
            baseUrl = obj.options.baseUrl();
            
            if ~startsWith(path, '/')
                path = ['/' path];
            end
            
            url = [baseUrl path];
        end
        
        function opts = buildWebOptions(obj, method)
            %BUILDWEBOPTIONS Build weboptions for request
            
            opts = weboptions();
            opts.MediaType = 'application/json';
            opts.ContentType = 'json';
            opts.Timeout = obj.options.timeout;
            opts.RequestMethod = method;
            
            % Build headers
            headers = {'Content-Type', 'application/json'; ...
                       'Accept', 'application/json'};
            
            % Add auth token if present
            if ~isempty(obj.authToken)
                headers = [headers; {'Authorization', ['Bearer ' obj.authToken]}];
            end
            
            % Add custom headers from options
            optHeaders = obj.options.headers;
            fields = fieldnames(optHeaders);
            for i = 1:length(fields)
                headers = [headers; {fields{i}, optHeaders.(fields{i})}]; %#ok<AGROW>
            end
            
            opts.HeaderFields = headers;
        end
        
        function result = request(obj, method, path, body, query)
            %REQUEST Execute an HTTP request
            
            if nargin < 4
                body = [];
            end
            if nargin < 5
                query = struct();
            end
            
            url = obj.buildUrl(path);
            opts = obj.buildWebOptions(method);
            
            try
                % Add query parameters
                queryFields = fieldnames(query);
                if ~isempty(queryFields)
                    queryParts = cell(1, length(queryFields) * 2);
                    for i = 1:length(queryFields)
                        queryParts{2*i-1} = queryFields{i};
                        val = query.(queryFields{i});
                        if isnumeric(val)
                            val = num2str(val);
                        end
                        queryParts{2*i} = val;
                    end
                    opts.ArrayFormat = 'csv';
                end
                
                % Execute request based on method
                if strcmp(method, 'GET') || strcmp(method, 'DELETE')
                    if ~isempty(queryFields)
                        response = webread(url, queryParts{:}, opts);
                    else
                        response = webread(url, opts);
                    end
                else
                    % POST, PUT, PATCH
                    if isempty(body)
                        body = struct();
                    end
                    response = webwrite(url, body, opts);
                end
                
                result = grey.error.Result.success(response);
                
            catch ex
                % Handle HTTP errors
                if contains(ex.identifier, 'HTTP')
                    % Try to extract status code from message
                    status = obj.extractStatusCode(ex.message);
                    
                    % Try to parse error response
                    errorBody = obj.parseErrorBody(ex);
                    
                    err = grey.error.GreyError.fromResponse(status, errorBody);
                else
                    err = grey.error.GreyError.fromException(ex);
                end
                
                result = grey.error.Result.failure(err);
            end
        end
        
        function status = extractStatusCode(~, message)
            %EXTRACTSTATUSCODE Extract HTTP status code from error message
            
            % Try to find a 3-digit status code in the message
            tokens = regexp(message, '(\d{3})', 'tokens');
            if ~isempty(tokens)
                status = str2double(tokens{1}{1});
            else
                status = 500; % Default to server error
            end
        end
        
        function body = parseErrorBody(~, ex)
            %PARSEERRORBODY Parse error response body
            
            body = struct();
            
            % Try to extract message from exception
            if isprop(ex, 'message')
                body.message = ex.message;
            end
        end
    end
end
