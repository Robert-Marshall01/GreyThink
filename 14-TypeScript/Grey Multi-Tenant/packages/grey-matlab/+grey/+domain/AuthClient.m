classdef AuthClient < handle
    %AUTHCLIENT Authentication client for Grey SDK
    %   Handles login, logout, and token refresh operations.
    
    properties (SetAccess = private)
        httpClient  % grey.http.HttpClient
    end
    
    methods
        function obj = AuthClient(httpClient)
            %AUTHCLIENT Construct auth client
            %   client = AuthClient(httpClient)
            
            obj.httpClient = httpClient;
        end
        
        function result = login(obj, username, password)
            %LOGIN Authenticate with username and password
            %   result = client.login(username, password)
            
            % Validate inputs
            if isempty(strtrim(username))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Username is required'));
                return;
            end
            
            if isempty(strtrim(password))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Password is required'));
                return;
            end
            
            body = struct('username', username, 'password', password);
            result = obj.httpClient.post('/auth/login', body);
            
            % Set auth token if successful
            if result.isOk()
                data = result.unwrap();
                if isstruct(data) && isfield(data, 'access_token')
                    obj.httpClient.setAuthToken(data.access_token);
                end
            end
        end
        
        function result = logout(obj)
            %LOGOUT Log out the current user
            %   result = client.logout()
            
            result = obj.httpClient.post('/auth/logout');
            
            % Clear token regardless of result
            obj.httpClient.clearAuthToken();
        end
        
        function result = refresh(obj, refreshToken)
            %REFRESH Refresh the access token
            %   result = client.refresh(refreshToken)
            
            % Validate inputs
            if isempty(strtrim(refreshToken))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('Refresh token is required'));
                return;
            end
            
            body = struct('refresh_token', refreshToken);
            result = obj.httpClient.post('/auth/refresh', body);
            
            % Set new auth token if successful
            if result.isOk()
                data = result.unwrap();
                if isstruct(data) && isfield(data, 'access_token')
                    obj.httpClient.setAuthToken(data.access_token);
                end
            end
        end
    end
end
