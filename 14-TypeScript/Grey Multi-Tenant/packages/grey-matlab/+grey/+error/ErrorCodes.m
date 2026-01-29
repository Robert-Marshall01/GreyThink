classdef ErrorCodes
    %ERRORCODES Error code constants for Grey SDK
    %   Defines standard error codes used throughout the SDK.
    
    properties (Constant)
        UNAUTHORIZED = 'unauthorized'
        FORBIDDEN = 'forbidden'
        NOT_FOUND = 'not_found'
        VALIDATION_ERROR = 'validation_error'
        NETWORK_ERROR = 'network_error'
        TIMEOUT = 'timeout'
        SERVER_ERROR = 'server_error'
        UNKNOWN = 'unknown'
    end
    
    methods (Static)
        function code = fromHttpStatus(status)
            %FROMHTTPSTATUS Convert HTTP status code to error code
            %   code = ErrorCodes.fromHttpStatus(status)
            
            if status == 401
                code = grey.error.ErrorCodes.UNAUTHORIZED;
            elseif status == 403
                code = grey.error.ErrorCodes.FORBIDDEN;
            elseif status == 404
                code = grey.error.ErrorCodes.NOT_FOUND;
            elseif status == 400 || status == 422
                code = grey.error.ErrorCodes.VALIDATION_ERROR;
            elseif status == 408 || status == 504
                code = grey.error.ErrorCodes.TIMEOUT;
            elseif status >= 500 && status < 600
                code = grey.error.ErrorCodes.SERVER_ERROR;
            else
                code = grey.error.ErrorCodes.UNKNOWN;
            end
        end
        
        function valid = isValid(code)
            %ISVALID Check if error code is valid
            %   valid = ErrorCodes.isValid(code)
            
            validCodes = {
                grey.error.ErrorCodes.UNAUTHORIZED, ...
                grey.error.ErrorCodes.FORBIDDEN, ...
                grey.error.ErrorCodes.NOT_FOUND, ...
                grey.error.ErrorCodes.VALIDATION_ERROR, ...
                grey.error.ErrorCodes.NETWORK_ERROR, ...
                grey.error.ErrorCodes.TIMEOUT, ...
                grey.error.ErrorCodes.SERVER_ERROR, ...
                grey.error.ErrorCodes.UNKNOWN
            };
            
            valid = any(strcmp(code, validCodes));
        end
        
        function normalized = normalize(code)
            %NORMALIZE Normalize error code to valid value
            %   normalized = ErrorCodes.normalize(code)
            
            if grey.error.ErrorCodes.isValid(code)
                normalized = code;
            else
                normalized = grey.error.ErrorCodes.UNKNOWN;
            end
        end
    end
end
