classdef Options
    %OPTIONS Configuration options for Grey SDK
    %   Stores connection settings for the Grey API.
    
    properties (SetAccess = private)
        host char           % Server hostname
        port double         % Server port
        useTls logical      % Whether to use HTTPS
        timeout double      % Request timeout in seconds
        headers struct      % Additional headers
    end
    
    methods
        function obj = Options(host, port, useTls, timeout, headers)
            %OPTIONS Construct options
            %   opts = Options(host)
            %   opts = Options(host, port)
            %   opts = Options(host, port, useTls)
            %   opts = Options(host, port, useTls, timeout)
            %   opts = Options(host, port, useTls, timeout, headers)
            
            obj.host = host;
            
            if nargin < 2
                obj.port = 443;
            else
                obj.port = port;
            end
            
            if nargin < 3
                obj.useTls = true;
            else
                obj.useTls = useTls;
            end
            
            if nargin < 4
                obj.timeout = 30;
            else
                obj.timeout = timeout;
            end
            
            if nargin < 5
                obj.headers = struct();
            else
                obj.headers = headers;
            end
        end
        
        function url = baseUrl(obj)
            %BASEURL Get the base URL for HTTP requests
            
            if obj.useTls
                scheme = 'https';
            else
                scheme = 'http';
            end
            
            url = sprintf('%s://%s:%d', scheme, obj.host, obj.port);
        end
        
        function newOpts = withTimeout(obj, timeout)
            %WITHTIMEOUT Create new options with different timeout
            
            newOpts = grey.config.Options(obj.host, obj.port, obj.useTls, ...
                timeout, obj.headers);
        end
        
        function newOpts = withHeaders(obj, newHeaders)
            %WITHHEADERS Create new options with additional headers
            
            % Merge headers
            merged = obj.headers;
            fields = fieldnames(newHeaders);
            for i = 1:length(fields)
                merged.(fields{i}) = newHeaders.(fields{i});
            end
            
            newOpts = grey.config.Options(obj.host, obj.port, obj.useTls, ...
                obj.timeout, merged);
        end
    end
    
    methods (Static)
        function opts = local(port)
            %LOCAL Create options for local development
            %   opts = Options.local()
            %   opts = Options.local(port)
            
            if nargin < 1
                port = 8080;
            end
            
            opts = grey.config.Options('localhost', port, false, 30);
        end
        
        function opts = production(host, port)
            %PRODUCTION Create options for production
            %   opts = Options.production(host)
            %   opts = Options.production(host, port)
            
            if nargin < 2
                port = 443;
            end
            
            opts = grey.config.Options(host, port, true, 30);
        end
    end
end
