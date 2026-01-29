//
//  GREYErrorCodes.m
//  GreySDK
//
//  Standard error codes for the Grey SDK.
//

#import "GREYErrorCodes.h"

NSString * const GREYErrorDomain = @"io.grey.sdk";
NSString * const GREYErrorDetailsKey = @"GREYErrorDetails";

@implementation GREYErrorCodes

+ (GREYErrorCode)fromHTTPStatus:(NSInteger)statusCode {
    switch (statusCode) {
        case 401:
            return GREYErrorCodeUnauthorized;
        case 403:
            return GREYErrorCodeForbidden;
        case 404:
            return GREYErrorCodeNotFound;
        case 400:
        case 422:
            return GREYErrorCodeValidationError;
        case 408:
            return GREYErrorCodeTimeout;
        default:
            if (statusCode >= 500 && statusCode < 600) {
                return GREYErrorCodeServerError;
            }
            return GREYErrorCodeUnknown;
    }
}

+ (GREYErrorCode)fromGRPCStatus:(NSInteger)statusCode {
    switch (statusCode) {
        case 1:  // CANCELLED
            return GREYErrorCodeUnknown;
        case 2:  // UNKNOWN
            return GREYErrorCodeUnknown;
        case 3:  // INVALID_ARGUMENT
            return GREYErrorCodeValidationError;
        case 4:  // DEADLINE_EXCEEDED
            return GREYErrorCodeTimeout;
        case 5:  // NOT_FOUND
            return GREYErrorCodeNotFound;
        case 6:  // ALREADY_EXISTS
            return GREYErrorCodeValidationError;
        case 7:  // PERMISSION_DENIED
            return GREYErrorCodeForbidden;
        case 8:  // RESOURCE_EXHAUSTED
            return GREYErrorCodeServerError;
        case 9:  // FAILED_PRECONDITION
            return GREYErrorCodeValidationError;
        case 10: // ABORTED
            return GREYErrorCodeUnknown;
        case 11: // OUT_OF_RANGE
            return GREYErrorCodeValidationError;
        case 12: // UNIMPLEMENTED
            return GREYErrorCodeUnknown;
        case 13: // INTERNAL
            return GREYErrorCodeServerError;
        case 14: // UNAVAILABLE
            return GREYErrorCodeNetworkError;
        case 15: // DATA_LOSS
            return GREYErrorCodeServerError;
        case 16: // UNAUTHENTICATED
            return GREYErrorCodeUnauthorized;
        default:
            return GREYErrorCodeUnknown;
    }
}

+ (BOOL)isRetryable:(GREYErrorCode)code {
    switch (code) {
        case GREYErrorCodeNetworkError:
        case GREYErrorCodeTimeout:
        case GREYErrorCodeServerError:
            return YES;
        default:
            return NO;
    }
}

+ (NSString *)messageForCode:(GREYErrorCode)code {
    switch (code) {
        case GREYErrorCodeUnauthorized:
            return @"Authentication required or token invalid";
        case GREYErrorCodeForbidden:
            return @"Access denied to the requested resource";
        case GREYErrorCodeNotFound:
            return @"Requested resource was not found";
        case GREYErrorCodeValidationError:
            return @"Request validation failed";
        case GREYErrorCodeNetworkError:
            return @"Network connectivity issue";
        case GREYErrorCodeTimeout:
            return @"Request timed out";
        case GREYErrorCodeServerError:
            return @"Server returned an error";
        case GREYErrorCodeUnknown:
        default:
            return @"Unknown or unexpected error";
    }
}

+ (NSString *)stringForCode:(GREYErrorCode)code {
    switch (code) {
        case GREYErrorCodeUnauthorized:
            return @"unauthorized";
        case GREYErrorCodeForbidden:
            return @"forbidden";
        case GREYErrorCodeNotFound:
            return @"not_found";
        case GREYErrorCodeValidationError:
            return @"validation_error";
        case GREYErrorCodeNetworkError:
            return @"network_error";
        case GREYErrorCodeTimeout:
            return @"timeout";
        case GREYErrorCodeServerError:
            return @"server_error";
        case GREYErrorCodeUnknown:
        default:
            return @"unknown";
    }
}

@end
