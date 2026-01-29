classdef UserClient < handle
    %USERCLIENT User client for Grey SDK
    %   Handles user-related operations.
    
    properties (SetAccess = private)
        httpClient  % grey.http.HttpClient
    end
    
    methods
        function obj = UserClient(httpClient)
            %USERCLIENT Construct user client
            %   client = UserClient(httpClient)
            
            obj.httpClient = httpClient;
        end
        
        function result = get(obj, userId)
            %GET Get a user by ID
            %   result = client.get(userId)
            
            % Validate inputs
            if isempty(strtrim(userId))
                result = grey.error.Result.failure(...
                    grey.error.GreyError.validation('User ID is required'));
                return;
            end
            
            path = sprintf('/users/%s', userId);
            result = obj.httpClient.get(path);
        end
        
        function result = getCurrent(obj)
            %GETCURRENT Get the current authenticated user
            %   result = client.getCurrent()
            
            result = obj.httpClient.get('/users/me');
        end
    end
end
