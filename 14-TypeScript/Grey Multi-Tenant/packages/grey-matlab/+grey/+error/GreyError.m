classdef GreyError
    %GREYERROR Normalized error type for Grey SDK
    %   Represents an error with code, message, and optional details.
    
    properties (SetAccess = private)
        code char      % Error code
        message char   % Error message
        details        % Additional error details (struct or empty)
    end
    
    methods
        function obj = GreyError(code, message, details)
            %GREYERROR Construct an error
            %   err = GreyError(code, message)
            %   err = GreyError(code, message, details)
            
            obj.code = grey.error.ErrorCodes.normalize(code);
            obj.message = message;
            
            if nargin < 3
                obj.details = [];
            else
                obj.details = details;
            end
        end
    end
    
    methods (Static)
        function err = unauthorized(message)
            %UNAUTHORIZED Create unauthorized error
            if nargin < 1
                message = 'Authentication required';
            end
            err = grey.error.GreyError(grey.error.ErrorCodes.UNAUTHORIZED, message);
        end
        
        function err = forbidden(message)
            %FORBIDDEN Create forbidden error
            if nargin < 1
                message = 'Permission denied';
            end
            err = grey.error.GreyError(grey.error.ErrorCodes.FORBIDDEN, message);
        end
        
        function err = notFound(message)
            %NOTFOUND Create not found error
            if nargin < 1
                message = 'Resource not found';
            end
            err = grey.error.GreyError(grey.error.ErrorCodes.NOT_FOUND, message);
        end
        
        function err = validation(message, details)
            %VALIDATION Create validation error
            if nargin < 2
                details = [];
            end
            err = grey.error.GreyError(grey.error.ErrorCodes.VALIDATION_ERROR, message, details);
        end
        
        function err = network(message)
            %NETWORK Create network error
            if nargin < 1
                message = 'Network error occurred';
            end
            err = grey.error.GreyError(grey.error.ErrorCodes.NETWORK_ERROR, message);
        end
        
        function err = timeout(message)
            %TIMEOUT Create timeout error
            if nargin < 1
                message = 'Request timed out';
            end
            err = grey.error.GreyError(grey.error.ErrorCodes.TIMEOUT, message);
        end
        
        function err = server(message)
            %SERVER Create server error
            if nargin < 1
                message = 'Server error occurred';
            end
            err = grey.error.GreyError(grey.error.ErrorCodes.SERVER_ERROR, message);
        end
        
        function err = unknown(message)
            %UNKNOWN Create unknown error
            if nargin < 1
                message = 'An unknown error occurred';
            end
            err = grey.error.GreyError(grey.error.ErrorCodes.UNKNOWN, message);
        end
        
        function err = fromResponse(status, body)
            %FROMRESPONSE Create error from HTTP response
            %   err = GreyError.fromResponse(status, body)
            
            code = grey.error.ErrorCodes.fromHttpStatus(status);
            
            if isstruct(body) && isfield(body, 'message')
                message = body.message;
            elseif isstruct(body) && isfield(body, 'error')
                message = body.error;
            else
                message = sprintf('HTTP error %d', status);
            end
            
            if isstruct(body) && isfield(body, 'details')
                details = body.details;
            else
                details = [];
            end
            
            err = grey.error.GreyError(code, message, details);
        end
        
        function err = fromException(ex)
            %FROMEXCEPTION Create error from MATLAB exception
            %   err = GreyError.fromException(ex)
            
            msg = ex.message;
            
            if contains(lower(msg), 'timeout')
                err = grey.error.GreyError.timeout(msg);
            elseif contains(lower(msg), {'connect', 'connection', 'network'})
                err = grey.error.GreyError.network(msg);
            else
                err = grey.error.GreyError.unknown(msg);
            end
        end
    end
end
