//
//  GREYErrorCodes.h
//  GreySDK
//
//  Standard error codes for the Grey SDK.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

/// Error domain for Grey SDK errors
extern NSString * const GREYErrorDomain;

/// Standard error codes used throughout the Grey SDK.
typedef NS_ENUM(NSInteger, GREYErrorCode) {
    /// Authentication required or token invalid
    GREYErrorCodeUnauthorized = 401,
    /// Access denied to the requested resource
    GREYErrorCodeForbidden = 403,
    /// Requested resource was not found
    GREYErrorCodeNotFound = 404,
    /// Request validation failed
    GREYErrorCodeValidationError = 422,
    /// Network connectivity issue
    GREYErrorCodeNetworkError = 1001,
    /// Request timed out
    GREYErrorCodeTimeout = 1002,
    /// Server returned an error
    GREYErrorCodeServerError = 500,
    /// Unknown or unexpected error
    GREYErrorCodeUnknown = 0
};

/// User info key for error details
extern NSString * const GREYErrorDetailsKey;

/**
 * Utility functions for error codes.
 */
@interface GREYErrorCodes : NSObject

/**
 * Convert an HTTP status code to a Grey error code.
 * @param statusCode The HTTP status code.
 * @return The corresponding GREYErrorCode.
 */
+ (GREYErrorCode)fromHTTPStatus:(NSInteger)statusCode;

/**
 * Convert a gRPC status code to a Grey error code.
 * @param statusCode The gRPC status code.
 * @return The corresponding GREYErrorCode.
 */
+ (GREYErrorCode)fromGRPCStatus:(NSInteger)statusCode;

/**
 * Check if an error code represents a retryable error.
 * @param code The error code to check.
 * @return YES if the error is retryable.
 */
+ (BOOL)isRetryable:(GREYErrorCode)code;

/**
 * Get a human-readable message for an error code.
 * @param code The error code.
 * @return Human-readable error message.
 */
+ (NSString *)messageForCode:(GREYErrorCode)code;

/**
 * Get the string representation of an error code.
 * @param code The error code.
 * @return String representation (e.g., "unauthorized").
 */
+ (NSString *)stringForCode:(GREYErrorCode)code;

@end

NS_ASSUME_NONNULL_END
