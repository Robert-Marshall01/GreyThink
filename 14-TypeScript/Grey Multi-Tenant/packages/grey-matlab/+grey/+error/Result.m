classdef Result
    %RESULT Result type for Grey SDK operations
    %   Represents the result of an operation that can succeed or fail.
    
    properties (SetAccess = private)
        ok logical      % Whether the result is successful
        data            % Success data (empty if error)
        err             % Error (empty if success)
    end
    
    methods
        function obj = Result(ok, data, err)
            %RESULT Construct a result
            %   For internal use - use Result.success or Result.failure
            
            obj.ok = ok;
            obj.data = data;
            obj.err = err;
        end
        
        function result = isOk(obj)
            %ISOK Check if result is successful
            result = obj.ok;
        end
        
        function result = isErr(obj)
            %ISERR Check if result is an error
            result = ~obj.ok;
        end
        
        function data = unwrap(obj)
            %UNWRAP Get the data or throw error
            if obj.isErr()
                error('Grey:Result:UnwrapError', ...
                    'Cannot unwrap error result: %s', obj.err.message);
            end
            data = obj.data;
        end
        
        function data = unwrapOr(obj, default)
            %UNWRAPOR Get the data or return default
            if obj.isOk()
                data = obj.data;
            else
                data = default;
            end
        end
        
        function err = error(obj)
            %ERROR Get the error (empty if success)
            err = obj.err;
        end
        
        function result = map(obj, fn)
            %MAP Transform the data if successful
            %   result = result.map(@(data) transform(data))
            
            if obj.isErr()
                result = obj;
                return;
            end
            
            try
                newData = fn(obj.data);
                result = grey.error.Result.success(newData);
            catch ex
                result = grey.error.Result.failure(...
                    grey.error.GreyError.fromException(ex));
            end
        end
        
        function result = andThen(obj, fn)
            %ANDTHEN Chain another operation if successful
            %   result = result.andThen(@(data) anotherOperation(data))
            
            if obj.isErr()
                result = obj;
                return;
            end
            
            try
                result = fn(obj.data);
                % If fn doesn't return a Result, wrap it
                if ~isa(result, 'grey.error.Result')
                    result = grey.error.Result.success(result);
                end
            catch ex
                result = grey.error.Result.failure(...
                    grey.error.GreyError.fromException(ex));
            end
        end
        
        function result = orElse(obj, fn)
            %ORELSE Handle error if failed
            %   result = result.orElse(@(err) handleError(err))
            
            if obj.isOk()
                result = obj;
                return;
            end
            
            try
                result = fn(obj.err);
                % If fn doesn't return a Result, wrap it
                if ~isa(result, 'grey.error.Result')
                    result = grey.error.Result.success(result);
                end
            catch ex
                result = grey.error.Result.failure(...
                    grey.error.GreyError.fromException(ex));
            end
        end
    end
    
    methods (Static)
        function result = success(data)
            %SUCCESS Create a successful result
            %   result = Result.success(data)
            
            result = grey.error.Result(true, data, []);
        end
        
        function result = failure(err)
            %FAILURE Create a failed result
            %   result = Result.failure(greyError)
            %   result = Result.failure('error message')
            
            if ischar(err) || isstring(err)
                err = grey.error.GreyError.unknown(char(err));
            end
            
            result = grey.error.Result(false, [], err);
        end
    end
end
