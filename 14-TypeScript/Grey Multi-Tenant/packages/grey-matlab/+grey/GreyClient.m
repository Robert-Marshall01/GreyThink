classdef GreyClient < handle
    %GREYCLIENT Main client for Grey SDK
    %   Provides access to all Grey API functionality.
    %
    %   Example:
    %       client = grey.GreyClient(grey.config.Options.local(8080));
    %       result = client.auth.login('user', 'pass');
    %       if result.isOk()
    %           projects = client.projects.list().unwrap();
    %       end
    
    properties (SetAccess = private)
        % Domain clients
        auth        % grey.domain.AuthClient
        user        % grey.domain.UserClient
        projects    % grey.domain.ProjectsClient
        query       % grey.domain.QueryClient
        mutation    % grey.domain.MutationClient
    end
    
    properties (Access = private)
        httpClient  % grey.http.HttpClient
        options     % grey.config.Options
    end
    
    methods
        function obj = GreyClient(options)
            %GREYCLIENT Construct Grey SDK client
            %   client = GreyClient(options)
            %
            %   Example:
            %       client = GreyClient(grey.config.Options.local(8080));
            %       client = GreyClient(grey.config.Options.production('api.grey.com'));
            
            if nargin < 1
                error('Grey:Client:InvalidOptions', ...
                    'Options are required. Use grey.config.Options.local() or .production()');
            end
            
            obj.options = options;
            obj.httpClient = grey.http.HttpClient(options);
            
            % Initialize domain clients
            obj.auth = grey.domain.AuthClient(obj.httpClient);
            obj.user = grey.domain.UserClient(obj.httpClient);
            obj.projects = grey.domain.ProjectsClient(obj.httpClient);
            obj.query = grey.domain.QueryClient(obj.httpClient);
            obj.mutation = grey.domain.MutationClient(obj.httpClient);
        end
        
        function setAuthToken(obj, token)
            %SETAUTHTOKEN Manually set the authentication token
            %   client.setAuthToken(token)
            
            obj.httpClient.setAuthToken(token);
        end
        
        function clearAuthToken(obj)
            %CLEARAUTHTOKEN Clear the authentication token
            %   client.clearAuthToken()
            
            obj.httpClient.clearAuthToken();
        end
        
        function opts = getOptions(obj)
            %GETOPTIONS Get the current options
            %   opts = client.getOptions()
            
            opts = obj.options;
        end
    end
    
    methods (Static)
        function client = local(port)
            %LOCAL Create a client for local development
            %   client = GreyClient.local()
            %   client = GreyClient.local(port)
            
            if nargin < 1
                port = 8080;
            end
            
            client = grey.GreyClient(grey.config.Options.local(port));
        end
        
        function client = production(host, port)
            %PRODUCTION Create a client for production
            %   client = GreyClient.production(host)
            %   client = GreyClient.production(host, port)
            
            if nargin < 2
                port = 443;
            end
            
            client = grey.GreyClient(grey.config.Options.production(host, port));
        end
    end
end
